let asdpath = Sys.argv.(1)

module T = struct
  let schema = {|
{
  "type": "object",
  "properties": {
    "asd": { "type": "integer", "description":"this is an integer value" },
    "bqwe": {"type": "string", "maxLength": 10, "minLength": 3, "description": "testtesttest"},
    "c": { "type": "array", "items": {"type": "string", "maxLength": 10, "minLength": 3 }},
    "d": {"description":"this is an integer value",
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
  type cfg = unit
  type t = (string * Types.Module.t) list

  let make () =
    let fs = Yojson.Safe.from_string in
    let schema,json,json2 = fs schema, fs json, fs json2 in
    [ "asd", {Types.Module.schema;jsons=["j1",json;"j2",json;"j3",json;"j4",json2]}
    ; "qwe", {Types.Module.schema;jsons=["asd",json;"qwe",json2;"zxc",json2;"fgh",json]}
    ; "poqwe", {Types.Module.schema;jsons=["base",json;"system",json2]}
    ]

  let rec assoc_update k v = function
    | [] -> []
    | (khd,_)::tl when khd = k -> (k,v)::tl
    | hd::tl -> hd::assoc_update k v tl

  let get_module t m = List.assoc_opt m t
  let get_module_list t = List.map fst t |> Types.ModuleList.of_list
  let save t m name = assoc_update name m t
end
module Serv = Vserver.Make(T)

let () = ignore (Lwt_main.run (Serv.start (T.make ()) (Fpath.v asdpath)))
