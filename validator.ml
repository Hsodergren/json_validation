open Brr
open Jsch
open React

module Validation = struct
  type t = [
    | `Valid
    | `Invalid of (Jsonpath.t * string) list
    | `Parsing
    | `Empty
  ] [@@deriving eq]

  let merge v1 v2 =
    match v1,v2 with
    | `Valid,`Valid -> `Valid
    | `Empty,a | a,`Empty -> a
    | `Parsing,_ | _,`Parsing -> `Parsing
    | `Invalid a, `Invalid b  -> `Invalid (a @ b)
    | `Invalid l,_ | _,`Invalid l-> `Invalid l

  let to_class =  function
    | `Valid -> "valid"
    | `Invalid _ -> "invalid"
    | `Parsing -> "parsing"
    | `Empty -> "empty"
end

type v =
  | Simple of string
  | Array of v list
  | Object of (string * v) list [@@deriving show, eq]

let kind = function
  | Simple _ -> "simple"
  | Array _ -> "array"
  | Object _ -> "object"

type t = { schema : Schema.t; v : v}

type action = [ `Update of Jsonpath.t * string
              | `AddItem of Jsonpath.t
              | `AddField of Jsonpath.t * string
              | `RemField of Jsonpath.t ]

let schema {schema;_} = schema
let v {v;_} = v

let equal {v=v1;_} {v=v2;_} = equal_v v1 v2

let pp fmt {v;_} = pp_v fmt v
let show {v;_} = show_v v

let find_schema name = function
  | Schema.Props props -> List.assoc_opt name props
  | PatProps props ->
    List.find_map (fun (re,_,s) -> if Re.execp re name then Some s else None) props

let make schema json =
  let rec make_v ({Schema.value;_}) json =
    match value,json with
    | _,`Int i -> Simple (string_of_int i)
    | _,`Float f -> Simple (string_of_float f)
    | _,`String s -> Simple s
    | _,`Bool b -> Simple (string_of_bool b)
    | (Number _ | Integer _ | String _ | Boolean),`Null -> Simple ""
    | Array {items;_}, `List l -> Array (List.map (make_v items) l)
    | Array _, `Null -> Array []
    | Object {properties=Props props;_}, (`Assoc _ | `Null as json) ->
      let objs = (List.map (fun (name,sch) ->
          let json = (try Yojson.Safe.Util.member name json with _ -> `Null) in
          name, make_v sch json
        ) props)
      in
      Object objs
    | Object {properties=PatProps _ as props;_}, (`Assoc js) ->
      let objs =
        List.filter_map (fun (name,json) ->
            Option.map (fun sch -> name,make_v sch json) (find_schema name props)
          ) js
      in
      Object objs
    | _ -> failwith "unhandled json"
  in
  {schema; v=make_v schema json}

let to_yojson {schema;v} =
  let list l = `List l in
  let assoc l = `Assoc l in
  let a f ff b ts =
    let l =
      List.map f ts
      |> List.filter ff
    in
    match l with
    | [] -> `Null
    | _ -> b l
  in
  let rec aux {Schema.value;_} v =
    match value,v with
    | (Number _ | Integer _ | String _ | Boolean), Simple "" -> `Null
    | Number _, Simple s -> `Float (float_of_string s)
    | Integer _, Simple s -> `Int (int_of_string s)
    | String _, Simple s -> `String s
    | Boolean, Simple s -> `Bool (bool_of_string s)
    | Array {items;_}, Array ts -> a (aux items) (function `Null -> false | _ -> true) list ts
    | Object {properties=Props props;_}, Object ts ->
        a (fun (s,t) -> s,aux (List.assoc s props) t)
          (function | (_,`Null) -> false | _ -> true)
          assoc
          ts
    | Object {properties=PatProps props;_}, Object ts ->
      let f (s,t) =
            let (_,_,schema) = List.find (fun (re,_,_) -> Re.execp re s) props in
            s, aux schema t
      in
      a f (function | (_,`Null) -> false | _ -> true) assoc ts
    | _ -> failwith "asd implemented"
  in
  try
    Some (aux schema v)
  with _ ->
    None

let get path t =
  let rec aux path t =
    match path,t with
    | [], _ -> Some t
    | `Object s::tl,Object ts -> aux tl (List.assoc s ts)
    | `Index i::tl,Array ts -> aux tl (List.nth ts i)
    | _ -> None
  in
  Option.map (fun v -> {t with v}) (aux (Jsonpath.to_list path) t.v)

let get_arr path t =
  let {v;_} = Option.get (get path t) in
  match v with
  | Array ts -> ts
  | _ -> failwith "array not on path"

let get_obj path t =
  let {v;_} = Option.get (get path t) in
  match v with
  | Object ts -> ts
  | _ -> failwith "object not on path"

let rec assoc_update f k l =
  match l with
  | [] -> []
  | (s,v) as hd::tl ->
    if s = k
    then (s,f v)::tl
    else hd::assoc_update f k tl

let rec nth_update f i l =
  match l with
  | [] -> []
  | hd::tl when i = 0 -> f hd::tl
  | hd::tl -> hd::nth_update f (i-1) tl

let rec remove_item i = function
  | [] -> []
  | _::tl when i = 0 -> tl
  | hd::tl -> hd::remove_item (i-1) tl

let rem_field p ({v;schema} as t) =
  let rec aux p {Schema.value;_} t  =
    match p,t,value with
    | [],Simple _,_ -> t
    | [`Index i], Array (ts),_ -> Array (remove_item i ts)
    | `Index i::tl,Array (ts),Array {items;_} -> Array(nth_update (aux tl items) i ts)
    | [`Object s],Object (ts), _ -> Object (List.remove_assoc s ts)
    | `Object s::tl,Object (ts),Object {properties=Props props;_} ->
      let schema = List.assoc s props in
      Object (assoc_update (aux tl schema) s ts)
    | _ -> failwith "invalid path"
  in
  let t = {t with v=aux (Jsonpath.to_list p) schema v} in
  t

let add_item p ({v;schema} as t) =
  let rec aux p {Schema.value;_} t  =
    match p,t,value with
    | `Index i::tl,Array (ts),Array {items;_} -> Array(nth_update (aux tl items) i ts)
    | [], Array (ts),Array {items;_} ->
      let {v;_} = make items `Null in
      Array(v::ts)
    | `Object s::tl,Object (ts),Object {properties=Props props;_} ->
      let schema = List.assoc s props in
      Object (assoc_update (aux tl schema) s ts)
    | `Object s::tl,Object (ts),Object {properties=PatProps _ as props;_} -> begin
        match find_schema s props with
        | Some schema -> Object (assoc_update (aux tl schema) s ts)
        | None -> failwith "didnt find schema"
      end
    | [],_,_ -> failwith "path doesn't lead to array"
    | _ -> failwith "invalid path"
  in
  let t = {t with v=aux (Jsonpath.to_list p) schema v} in
  t

let add_field p str ({v;schema} as t) =
  let rec aux p {Schema.value;_} t  =
    match p,t,value with
    | `Index i::tl,Array (ts),Array {items;_} -> Array(nth_update (aux tl items) i ts)
    | [], Object (ts),Object {properties;_} ->
      let {v;_} = make (Option.get (find_schema str properties)) `Null in
      Object((str,v)::ts)
    | `Object s::tl,Object (ts),Object {properties=Props props;_} ->
      let schema = List.assoc s props in
      Object (assoc_update (aux tl schema) s ts)
    | `Object s::tl,Object (ts),Object {properties=PatProps _ as props;_} -> begin
        match find_schema s props with
        | Some schema -> Object (assoc_update (aux tl schema) s ts)
        | None -> failwith "didnt find schema"
      end
    | _ -> failwith "invalid path"
  in
  let t = {t with v=aux (Jsonpath.to_list p) schema v} in
  t

let update ({v;_} as t) path str =
  let rec aux path t =
    match path,t with
    | [],Simple _ -> Simple str
    | `Object s::tl, Object (ts) -> Object (assoc_update (aux tl) s ts)
    | `Index i::tl, Array(ts) -> Array (nth_update (aux tl) i ts)
    | _ -> Console.log [Jstr.v "invalid path"]; failwith "error"
  in
  {t with v=aux (Jsonpath.to_list path) v}

let float s = float_of_string_opt s |> Option.map (fun s -> `Float s)
let bool s = bool_of_string_opt s |> Option.map (fun s -> `Bool s)
let int s = int_of_string_opt s |> Option.map (fun s -> `Int s)
let string s = Some (`String s)

let validate : Schema.t -> string -> Jsonpath.t -> Validation.t =
  fun ({value;_} as schema) str path ->
  let v conv =
    match conv str with
    | None -> `Parsing
    | Some j ->
      match Schema.validate j schema with
      | `Valid -> `Valid
      | `Invalid a -> `Invalid (List.map (fun (_,s) -> path,s) a)
  in
  if str = "" then `Empty
  else match value with
    | Number _ -> v float
    | Integer _ -> v int
    | Boolean -> v bool
    | String _ -> v string
    | _ -> Console.log [Jstr.v "cannot validate arrays or objects"]; failwith "error"

let add_button el v =
  let el = El.button [el] in
  let e,send_e = E.create () in
  Ev.listen Ev.click (fun _ -> send_e v) (El.as_target el);
  e,el

let visible_fields {v;_} re =
  let rec aux set path = function
    | Simple _ -> []
    | Array vs ->
      let _,child_set =List.fold_left (fun (i,set) v ->
          let s = aux [] (Jsonpath.add path (`Index i)) v in
          (i+1,s @ set)
        ) (0,set) vs
      in
      set @ child_set
    | Object vs ->
      let child_set = List.fold_left (fun set (str,v) ->
          let path = Jsonpath.add path (`Object str) in
          let s = aux [] path v in
          match s with
          | [] -> if Re.execp re str then path::s @ set else s @ set
          | _ -> path::s @ set
        ) (set) vs
      in
      set @ child_set
  in
  aux [Jsonpath.empty] Jsonpath.empty v

let regex_input regexes =
  let but = El.button [El.txt' "add"] in
  let input = El.input ~at:[At.class' (Jstr.v "reg_input")] () in
  let regex_txt = El.span [] in
  let e,send_e = E.create () in
  let cur,send_cur = S.create "" in
  let cur_a = S.map (fun str ->
      match List.find_opt (fun (re,_,_) -> Re.execp re str) regexes with
      | Some (_re,_reg_str,_) -> El.set_at (Jstr.v "disabled") None but; Some _reg_str
      | None -> El.set_at (Jstr.v "disabled") (Some (Jstr.v "true")) but; None
    ) cur
  in
  ignore @@ S.trace (function
      | Some regex_str ->
        El.set_children regex_txt [El.txt' regex_str];
      | None ->
        El.set_children regex_txt [];
    ) cur_a;
  Ev.listen Ev.click (fun _ ->
      send_e (S.value cur);
      send_cur "";
      El.set_prop El.Prop.value (Jstr.v "") input
    ) (El.as_target but);
  Ev.listen Ev.keyup (fun _ ->
      let v = Jstr.to_string @@ El.prop El.Prop.value input in
      send_cur v
    ) (El.as_target input);
  e,El.div [but;input;regex_txt]

let simple_input ?(disabled=false) schema path value =
  let at = [At.value (Jstr.v value)] in
  let el = El.input ~at:(if disabled then At.disabled::at else at) () in
  let e, send_e = S.create (validate schema value path) in
  let update, send_update = E.create () in
  let target = El.as_target el in
  Ev.listen Ev.keyup (fun _ ->
      let cur_str = El.prop El.Prop.value el in
      send_e (validate schema (Jstr.to_string cur_str) path);
    ) target;
  Ev.listen Ev.keyup (fun _ ->
      let cur_str = El.prop El.Prop.value el in
      send_update (Jstr.to_string cur_str)
    ) target;
  update, e, el

let schema_to_short {Schema.value;_} =
  match value with
  | Boolean -> "B"
  | Integer _ -> "I"
  | Number _ -> "F"
  | String _ -> "S"
  | Object _ -> "O"
  | Array _ -> "A"

let prev_if_err f start =
  let last = ref start in
  fun v ->
    try
      last := f v;
      !last
    with _ -> !last

let view ?(disabled=false) ?(handle_required=true) ?(id="") ?(search=S.const "") model_s =
  let search_re = S.map (fun str -> Re.Posix.re str |> Re.compile) search in
  let visibles = S.l2 visible_fields model_s search_re in
  let (<+>) = Jsonpath.add in
  let set_attr signal to_str attr el =
    S.trace (fun v ->
      El.set_at (Jstr.v attr) (Some (Jstr.v @@ to_str v)) el
      ) signal
    |> ignore
  in
  let set_class signal to_str el = set_attr signal to_str "class" el in
  let list_eq_len l1 l2 = List.length l1 = List.length l2 in
  let (>>=) = S.bind in
  let rec aux model ({Schema.value;_} as schema) path =
    match model, value with
    | Simple s, (Number _ | Integer _ | String _ | Boolean) ->
      let updated,s,el = simple_input ~disabled schema path s in
      s, E.map (fun s -> `Update (path,s)) updated , [el]
    | Array _ts, Array {items;_} ->
      let add_e,but_el = add_button (El.txt' "+") (`AddItem path) in
      let ee,send_e = E.create () in
      let e = E.switch add_e ee in
      let at = [At.class' (Jstr.v "list")] in
      let parent = El.ul ~at [] in
      let s = S.map ~eq:list_eq_len
          (prev_if_err (get_arr path) []) model_s >>= fun (vs) ->
        let validss,ac_evs, els =
          List.mapi (fun i t ->
              let path = path <+> `Index i in
              let a,b,el = aux t items path in
              let add_e,but_el = add_button (El.txt' "-") (`RemField path) in
              a,E.select[add_e;b],if disabled then el else but_el::el
            ) vs
          |> Util.unzip3
        in
        send_e (E.select (add_e::ac_evs));
        let el = List.map El.li els in
        El.set_children parent (if disabled then el else but_el::el);
        S.merge Validation.merge `Empty validss
      in
      s, e, [parent]
    | Object _ts, Object ({required;properties}) ->
      let get_schema props str =
        match props with
        | Schema.Props props -> List.assoc str props
        | PatProps props -> Option.get @@
          List.find_map (fun (re,_,sch) -> if Re.execp re str then Some sch else None) props
      in
      let is_patprops = match properties with | Schema.Props _ -> false | PatProps _ -> true in
      let but_e,but_el = match properties with
        | PatProps props -> regex_input props
        | Props _ -> E.never,El.div []
      in
      let add_e = E.map (fun str -> `AddField (path,str)) but_e in
      let ee, _send_e = E.create () in
      let e = E.switch add_e ee in
      let at = [At.class' (Jstr.v "object")] in
      let parent = El.div ~at [] in
      let s = S.map ~eq:list_eq_len (prev_if_err (get_obj path) []) model_s >>= fun vs ->
        let validss,_ac_evs, els =
          List.map (fun (s,t) ->
              let path = path <+> `Object s in
              let schema = get_schema properties s in
              let v,b,e = aux t schema path in
              let err_if_req l =
                if List.mem s l
                then S.map (function | `Empty -> `Invalid [path, "required missing"] | a -> a) v
                else v
              in
              let v =
                if handle_required
                then Option.fold required ~none:v ~some:err_if_req
                else v
              in
              let s = Printf.sprintf "%s (%s)" s (schema_to_short schema) in
              let rem_e,rem_el = add_button (El.txt' "-") (`RemField path) in
              let hdr = El.h4 (if is_patprops then [El.txt' s;rem_el] else [El.txt' s]) in
              let el = [hdr; El.div e] in
              let obj = El.div el in
              let _ = S.trace (fun paths ->
                  El.set_class (Jstr.v "hide") (not @@ List.mem path paths) obj) visibles
              in
              let () = set_class v Validation.to_class hdr in
              v,E.select [rem_e;b],obj
            ) vs
          |> Util.unzip3
        in
        El.set_children parent
          (if is_patprops then but_el::els else els);
        _send_e (E.select (add_e::_ac_evs));
        S.merge Validation.merge `Valid validss
      in
      s,e,[parent]
    | a,b ->
      Console.error Jstr.([v id;v @@ Jsonpath.to_string path ^" ::";
                           v @@ kind a; v "and";v @@ Schema.kind b; v "invalid"]);
      failwith "error"
  in
  let m = S.value model_s in
  aux (v m) (schema m) Jsonpath.empty
