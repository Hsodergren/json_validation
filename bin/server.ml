(* open Lwt * open Cohttp *)
open Cohttp_lwt_unix

let asdpath = Sys.argv.(1)

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
let () = print_endline asdpath

let get_jsons () =
  let schema = Yojson.Safe.from_string schema in
  let json = Yojson.Safe.from_string json in
  let json2 = Yojson.Safe.from_string json2 in
  {Types.Module.schema=schema;jsons=[json;json2;json2;json;json;json;json;json;json]}

let index () = Routes.(empty @--> `Root)
let modul () = Routes.(s "module" / str /? nil @--> fun _ -> `Module (get_jsons ()))
let file () = Routes.(s "file" / str / str /? nil @--> fun dir fname -> `File (dir,fname))
let all = Routes.one_of [index (); modul ();file ()]

let opt_or_notfound o f =
  match o with
  | Some str -> f str
  | None -> Server.respond_not_found ()

let (>|?) = opt_or_notfound

let ok ~body =
  Server.respond_string ~status:`OK ~body ()

let res = function
  | Ok body -> ok ~body
  | Error str -> Server.respond_error ~status:`Bad_request ~body:str ()

let main =
  let callback _conn req _body =
    let path = req |> Request.uri |> Uri.path in
    Printf.printf "path: %s\n" path;
    Routes.match' all ~target:path >|? function
      | `Root -> Server.respond_redirect ~uri:(Uri.of_string "/file/index/index.html") ()
      | `Module m -> ok ~body:(Yojson.Safe.to_string (Types.Module.to_yojson m))
      | `File (dir,fname) ->
        let fname = Server.resolve_local_file ~docroot:asdpath ~uri:(Uri.of_string (dir ^"/"^fname)) in
        Server.respond_file ~fname ()
  in
  Server.create ~mode:(`TCP (`Port 8080)) (Server.make ~callback ())

let () = ignore (Lwt_main.run main)
