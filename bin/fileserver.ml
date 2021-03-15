let y2j_cmd f =
  Printf.sprintf {|python3 -c 'import sys, yaml, json; y=yaml.safe_load(open("%s")); print(json.dumps(y))'|} f

let j2y_cmd = "python3 -c 'import sys, yaml, json; y=json.loads(sys.stdin.read()); print(yaml.dump(y,default_flow_style=False))'"

let input_read_all inc =
  let data = Buffer.create 0 in
  let rec aux () =
    match input_char inc with
    | c -> Buffer.add_char data c; aux ()
    | exception End_of_file -> Buffer.to_bytes data |> Bytes.to_string
  in
  aux ()

let yml_to_yojson file =
  let inc = Unix.open_process_in @@ y2j_cmd file in
  let str = input_read_all inc in
  Yojson.Safe.from_string str

let yojson_to_yml json =
  let str = Yojson.Safe.to_string json in
  let (inc,outc) = Unix.open_process j2y_cmd in
  output_string outc str;
  close_out outc;
  input_read_all inc

module T = struct
  type data = {
    name: string;
    schema: string;
    files: (string * string) list
  } [@@deriving yojson]

  let name {name;_} = name

  type t = data list [@@deriving yojson]

  let make str =
    match of_yojson (Yojson.Safe.from_file str) with
    | Ok file -> file
    | Error err -> failwith err

  let rec find t m =
    match t with
    | [] -> None
    | {name;_} as hd::_ when name = m -> Some hd
    | _::tl -> find tl m

  let get_module t m =
    Option.map (fun {name=_;schema;files} ->
        Types.Module.v
          (yml_to_yojson schema)
          (List.map (fun (s,f) -> s,yml_to_yojson f) files))
      (find t m)

  let get_module_list t = Types.ModuleList.of_list @@ List.map name t

  let save t {Types.Module.jsons;_} name =
    (match List.find_opt (fun {name=n;_} -> n = name) t with
     | Some {files;_} -> List.iter (fun (name,file) ->
         match List.assoc_opt name jsons with
         | Some json ->
           let yml = yojson_to_yml json in
           let outc = open_out file in
           output_string outc yml;
           close_out outc
         | None -> ()
       ) files
     | None -> print_endline "not found");
    t
end
module Serv = Vserver.Make(T)

let run port conf ui =
  ignore (Lwt_main.run (Serv.start ~port (T.make conf) ui))

open Cmdliner

let port =
  let doc = "The port to use for the server" in
  Arg.(value & opt int 8080 & info ["p";"port"] ~doc)

let conf =
  let doc = "Path to the config file" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~doc ~docv:"CONF")

let ui =
  let doc = "Path to the ui directory" in
  Arg.(required & pos 1 (some dir) None & info [] ~doc ~docv:"UI")

let run_t = Term.(const run $ port $ conf $ ui)

let info =
  let doc = "Server that hosts file based yml editor" in
  Term.info "fileserver" ~doc ~exits:Term.default_error_exits

let () = Term.exit @@ Term.eval (run_t,info)
