open Cohttp_lwt_unix

module type S_Type = sig
  type t
  type cfg
  val make: cfg -> t

  val get_module: t -> string -> Types.Module.t option
  val get_module_list: t -> Types.ModuleList.t
end

module type S = sig
  module Type : S_Type

  val start: Type.cfg -> Fpath.t -> unit Lwt.t
end

module Make (T: S_Type) = struct
  module Type : S_Type with type t = T.t and type cfg = T.cfg = T

  let index () = Routes.(empty @--> `Root)
  let modul t = Routes.(s "module" / str /? nil @--> fun m -> `Module (T.get_module t m))
  let modul_list t = Routes.(s "modulelist" /? nil @--> `ModuleList (T.get_module_list t))
  let file () = Routes.(s "file" / str / str /? nil @--> fun dir fname -> `File (dir,fname))

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

  let start cfg root =
    let t = Type.make cfg in
    let all = Routes.one_of [index (); modul t; modul_list t; file ()] in

    let callback _conn req _body =
      let path = req |> Request.uri |> Uri.path in
      Printf.printf "path: %s\n" path;
      Routes.match' all ~target:path >|? function
      | `Root -> Server.respond_redirect ~uri:(Uri.of_string "/file/index/index.html") ()
      | `Module m -> begin
          match m with
        | Some m -> ok ~body:(Yojson.Safe.to_string (Types.Module.to_yojson m))
        | None -> Server.respond_not_found ()
        end
      | `ModuleList m -> ok ~body:(Yojson.Safe.to_string (Types.ModuleList.to_yojson m))
      | `File (dir,fname) ->
        let fname = Server.resolve_local_file
            ~docroot:(Fpath.to_string root)
            ~uri:(Uri.of_string (dir ^"/"^fname))
        in
        Server.respond_file ~fname ()
    in
    Server.create ~mode:(`TCP (`Port 8080)) (Server.make ~callback ())
end
