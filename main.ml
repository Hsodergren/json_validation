open Brr
open React
open Jsch

module Model = Validator

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
    "arrarr": { "type": "array", "items": { "type": "array", "items": { "type": "number"}}},
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
"arrarr": [[]],
"pats": {"aaaa":"qwe", "q":1}
}
|}
(* "asd": "qwe","bwe": 12 *)
let json = Yojson.Safe.from_string json
let json2 = {|
{
"asd": 5,
"c": ["asd","qwe"],
"d": {
  "da":1,
  "db":"asd"
  },
"e": [{"a":true, "b":1.2}, {"a": false, "b":5}],
"arrarr": [[],[]],
"pats": {"aaaa":"qwe", "q":1}
}
|}
(* "asd": "qwe","bwe": 12 *)
let json2 = Yojson.Safe.from_string json2

let set_children signal f el =
  S.trace (fun v ->
      El.set_children el (f v)
    ) signal
  |> ignore

let trd (_,_,x) = x

let apply_action action model =
  match action with
  | `Update (p,str) -> Model.update model p str
  | `AddItem p -> Model.add_item p model
  | `AddField (p,s) -> Model.add_field p s model
  | `RemField p -> Model.rem_field p model

let errors_to_str errs =
  List.map (fun (p,s) -> Printf.sprintf "%s : %s\n" (Jsonpath.to_string p) s) errs
  |> List.map Jstr.v

let searchbar () =
  let el = El.input ~at:[At.class' (Jstr.v "searchbar"); At.placeholder (Jstr.v "Search")] () in
  let s,send_s = S.create "" in
  Ev.listen Ev.keyup (fun _ ->
      let v = El.prop El.Prop.value el in
      send_s (Jstr.to_string v)
    ) (El.as_target el);
  s,el

let (<*>) f o =
  match f,o with
  | Some f, Some o -> Some (f o)
  | _ -> None

let (<$>) = Option.map

let create_result ?(search=S.const "") models_s =
  match models_s with
  | [] -> El.div []
  | hd::_ ->
    let schema = Model.schema (S.value hd) in
    let jsons = List.map (S.map Model.to_yojson) models_s in
    let s = S.merge (fun a b -> Jsch.Merge.merge <$> a <*> b) (Some (`Assoc [])) jsons in
    let parent = El.div [] in
    set_children s (fun json ->
        match json with
        | Some json ->
          Console.log [Jstr.v @@ Yojson.Safe.to_string json];
          let model = Model.make schema json in
          S.const model
          |> Model.view ~disabled:true ~id:"result" ~search
          |> trd
        | None -> []
      ) parent;
    parent

let validator ?(disabled=false) ?(handle_required=true) ?(search=S.const "")model =
  let def model_s =
    let v,a,el = Model.view ~disabled ~handle_required ~search model_s in
    let apply_action = E.map apply_action a in
    let model_s' = S.accum ~eq:Model.equal apply_action (S.value model_s) in
    model_s',(model_s',v,el)
  in
  S.fix ~eq:Model.equal model def

let header () =
  let sb_s, sb_el = searchbar () in
  sb_s, El.div ~at:[At.class' (Jstr.v "header")] [sb_el]

let body schema jsons sb_s =
  let models,_vs,uis =
    List.mapi (fun i j -> Model.make schema j,i=0) jsons
    |> List.map (fun (m,b) -> validator ~handle_required:b ~search:sb_s m)
    |> Util.unzip3
  in
  let result_ui = create_result models ~search:sb_s in
  let validators = List.map (fun el ->
      El.div ~at:[At.class' (Jstr.v "validator")] el
    ) uis
  in
  let validators = El.div ~at:[At.class' (Jstr.v "validators")] validators in
  let result_ui = El.div ~at:[At.class' (Jstr.v "result")] [result_ui] in
  El.div ~at:[At.class' (Jstr.v "content")] [result_ui;validators]

let main_ui schema jsons =
  let sb_s, header_el = header () in
  let body_el = body schema jsons sb_s in
  El.div [header_el;body_el]

let main () =
  let id = Jstr.v "main" in
  match Document.find_el_by_id G.document id with
  | None -> Console.(error ["no element with name main"])
  | Some el ->
    let jsons = [json;json2;json2] in
    let ui = main_ui schema jsons in
    El.set_children el [ui]

let () = main ()
