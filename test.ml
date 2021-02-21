open Brr
open React
open Jsch
let schema = {|
{
  "type": "object",
  "properties": {
    "asd": { "type": "integer" },
    "bqwe": {"type": "string", "maxLength": 10, "minLength": 3 },
    "c": { "type": "array", "items": {"type": "string", "maxLength": 10, "minLength": 3 }},
    "d": {
    "type": "object",
     "properties": { "da": {"type": "integer" }, "db": {"type": "string", "maxLength": 10, "minLength": 3 }},
     "required": ["da"]
    },
    "e": { "type": "array", "items": { "type": "object", "properties": { "a" : { "type":"boolean" }, "b" : { "type": "number"}}}},
  "pats": { "type" : "object", "patternProperties" : {"^a.*$" : {"type": "string"}, "^q.*$" : {"type": "integer"}}}
  }
}
|}
let schema =
  match Jsch.Schema.of_string schema with
  | Ok x -> (Console.log [Jstr.v "success"];x)
  | Error s -> (Console.log [Jstr.v s]; failwith s)

let json = {|
{
"asd": 5,
"c": ["1","2","3"],
"d": {
  "da":1,
  "db":"asd"
  },
"e": [{"a":true, "b":1.2}, {"a": false, "b":5}],
"pats": {"aaaa":"qwe", "q":1}
}
|}
(* "asd": "qwe","bwe": 12 *)
let json = Yojson.Safe.from_string json
let json2 = {|
{
"asd": 5,
"c": ["1","2","3","4"],
"d": {
  "da":1,
  "db":"asd"
  },
"e": [{"a":true, "b":1.2}, {"a": false, "b":5}],
"pats": {"aaaa":"qwe", "q":1}
}
|}
(* "asd": "qwe","bwe": 12 *)
let json2 = Yojson.Safe.from_string json2
let (<+>) = Jsonpath.add

module Model : sig
  type t [@@deriving show]
  type v = private
    | Simple of string
    | Array of v list
    | Object of (string * v) list

  val schema: t -> Schema.t
  val v: t -> v

  val equal: t -> t -> bool

  (* val get: Jsonpath.t -> t -> t option *)
  val make: Schema.t -> Yojson.Safe.t -> t
  val update: t -> Jsonpath.t -> string -> t
  val add_field: Jsonpath.t -> t -> t
end = struct
  type v =
    | Simple of string
    | Array of v list
    | Object of (string * v) list [@@deriving show, eq]

  type t = { schema : Schema.t; v : v}

  let schema {schema;_} = schema
  let v {v;_} = v

  let equal {v=v1;_} {v=v2;_} = equal_v v1 v2

  let pp fmt {v;_} = pp_v fmt v
  let show {v;_} = show_v v

  let make schema json =
    let rec make_v {Schema.value;_} json =
      match value,json with
      | _,`Int i -> Simple (string_of_int i)
      | _,`Float f -> Simple (string_of_float f)
      | _,`String s -> Simple s
      | _,`Bool b -> Simple (string_of_bool b)
      | (Number _ | Integer _ | String _ | Boolean),`Null -> Simple ""
      | Array {items;_}, `List l -> Array (List.map (make_v items) l)
      | Object {properties=Props props;_}, (`Assoc _ | `Null as json) ->
        let objs = (List.map (fun (name,sch) ->
            let json = (try Yojson.Safe.Util.member name json with _ -> `Null) in
            name, make_v sch json
          ) props)
        in
        Object objs
      | Object {properties=PatProps props;_}, (`Assoc js) ->
        let find_schema name props =
          List.find_map (fun (re,_,s) -> if Re.execp re name then Some s else None) props
        in
        let objs =
          List.filter_map (fun (name,json) ->
              Option.map (fun sch -> name,make_v sch json) (find_schema name props)
            ) js
        in
        Object objs
      | _ -> failwith "unhandled json"
    in
    {schema; v=make_v schema json}

  (* let get path t =
   *   let rec aux ps t =
   *     match ps,t with
   *     | [],_ -> t
   *     | `Object s::tl, Object (ts) -> aux tl (List.assoc s ts)
   *     | `Index i::tl, Array (ts) -> aux tl (List.nth ts i)
   *     | _ -> Console.log ["error"];failwith "error"
   *   in
   *   Some (aux (Jsonpath.to_list path) t) *)

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

  let add_field p ({v;_} as t) =
    let rec aux p t =
      match p,t with
      | [],Simple _ -> t
      | `Index i::tl,Array (ts) -> Array(nth_update (aux tl) i ts)
      (* | [], Array (ts) -> Array(make `Null::ts) *)
      | `Object _s::_tl,Object (_ts) -> failwith "not implemented"
      | _ -> failwith "invalid path"
    in
    {t with v=aux (Jsonpath.to_list p) v}

  let update ({v;_} as t) path str =
    let rec aux path t =
      match path,t with
      | [],Simple _ -> Simple str
      | `Object s::tl, Object (ts) -> Object (assoc_update (aux tl) s ts)
      | `Index i::tl, Array(ts) -> Array (nth_update (aux tl) i ts)
      | _ -> Console.log [Jstr.v "invalid path"]; failwith "error"
    in
    {t with v=aux (Jsonpath.to_list path) v}
end

type action = [`Update of Jsonpath.t * string | `AddField of Jsonpath.t]

let apply_action action model =
  match action with
  | `Update (p,str) -> Model.update model p str
  | `AddField p -> Model.add_field p model

type validation = [
  | `Valid
  | `Invalid of (Jsonpath.t * string) list
  | `Parsing
  | `Empty
]

let valid_to_class =  function
  | `Valid -> "valid"
  | `Invalid _ -> "invalid"
  | `Parsing -> "parsing"
  | `Empty -> "empty"

let float s = float_of_string_opt s |> Option.map (fun s -> `Float s)
let bool s = bool_of_string_opt s |> Option.map (fun s -> `Bool s)
let int s = int_of_string_opt s |> Option.map (fun s -> `Int s)
let string s = Some (`String s)

let validate : Schema.t -> string -> Jsonpath.t -> validation =
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

let simple_input : Schema.t -> Jsonpath.t -> string -> string event * validation signal * El.t
  = fun schema path value ->
    let el = El.input ~at:[At.value (Jstr.v value)] () in
    let e, send_e = S.create (validate schema value path) in
    let update, send_update = E.create () in
    let target = El.as_target el in
    Ev.listen Ev.keyup (fun _ ->
        let cur_str = El.prop El.Prop.value el in
        send_e (validate schema (Jstr.to_string cur_str) path);
      ) target;
    Ev.listen Ev.change (fun _ ->
        let cur_str = El.prop El.Prop.value el in
        send_update (Jstr.to_string cur_str)
      ) target;
    update, e, el

let errors_to_str errs =
  List.map (fun (p,s) -> Printf.sprintf "%s : %s\n" (Jsonpath.to_string p) s) errs
  |> List.map Jstr.v

let rec unzip3 l =
  match l with
  | (x,y,z)::tl ->
    let xs,ys,zs = unzip3 tl in
    x::xs,y::ys,z::zs
  | [] -> [],[],[]

let merge_valid v1 v2 =
  match v1,v2 with
  | `Valid,`Valid -> `Valid
  | `Empty,a | a,`Empty -> a
  | `Parsing,_ | _,`Parsing -> `Parsing
  | `Invalid a, `Invalid b  -> `Invalid (a @ b)
  | `Invalid l,_ | _,`Invalid l-> `Invalid l

let create_ui model_s : validation signal * action event * El.t list =
  let set_class valid el =
    El.set_at (Jstr.v "class") (Some (Jstr.v @@ valid_to_class valid)) el
  in
  let rec aux model ({Schema.value;_} as schema) path =
    match model, value with
    | Model.Simple s, (Number _ | Integer _ | String _ | Boolean) ->
      let updated,s,el = simple_input schema path s in
      s, E.map (fun s -> `Update (path,s)) updated , [el]
    | Array ts, Array {items;_} ->
      let validss,ac_evs, els =
        List.mapi (fun i t -> aux t items (path <+> `Index i)) ts
        |> unzip3
      in
      let lis = List.map El.li els in
      let at = [At.class' (Jstr.v "list")] in
      S.merge merge_valid `Valid validss, E.select (ac_evs), [El.ul ~at lis]
    | Object ts, Object ({required;properties}) ->
      let get_schema props str =
        match props with
        | Schema.Props props -> List.assoc str props
        | PatProps props -> Option.get @@
          List.find_map (fun (re,_,sch) -> if Re.execp re str then Some sch else None) props
      in
      let validss,ac_evs, els =
        List.map (fun (s,t) ->
            let path = path <+> `Object s in
            let schema = get_schema properties s in
            let v,b,e = aux t schema path in
            let err_if_req l =
              if List.mem s l
              then S.map (function | `Empty -> `Invalid [path, "required missing"] | a -> a) v
              else v
            in
            let v = Option.fold required ~none:v ~some:err_if_req in
            let hdr = El.h4 [El.txt' s] in
            ignore @@ S.trace (fun v -> set_class v hdr) v;
            let at = [At.class' (Jstr.v "object")] in
            v,b,El.div ~at [hdr; El.div e]
          ) ts
        |> unzip3
      in
      let valids = S.merge merge_valid `Empty validss in
      valids,E.select ac_evs,els
    | _ -> Console.log ["invalid"]; failwith "invalid"
  in
  let model = S.value model_s in
  aux (Model.v model) (Model.schema model) Jsonpath.empty

let main model =
  let def model_s =
    let v,a,el = create_ui model_s in
    let apply_action = E.map apply_action a in
    let model_s' = S.accum ~eq:Model.equal apply_action (S.value model_s) in
    model_s',(model_s',v,el)
  in
  S.fix ~eq:Model.equal model def

let main () =
  let id = Jstr.v "main" in
  match Document.find_el_by_id G.document id with
  | None -> Console.(error ["no element with name main"])
  | Some el ->
    let jsons = [json;json2;json2] in
    let models = List.map (Model.make schema) jsons in
    let _ms,vs,uis = List.map main models |> unzip3 in
    let at = [At.class' (Jstr.v "validator")] in
    let s = S.merge merge_valid `Valid vs in
    ignore @@ S.trace (fun s ->
        let strs = match s with
          | `Empty -> ["empty"]
          | `Valid -> ["valid"]
          | `Invalid errs -> List.map (fun (p,s) -> Printf.sprintf "%s : %s" (Jsonpath.to_string p) s) errs
          | `Parsing -> ["pasing error"]
        in
        Console.log [Array.of_list @@ List.map Jstr.v strs]) s;
    let ui = List.map (fun u -> El.div ~at u) uis in
    El.set_children el [El.div ui]

let () = main ()
