open Cohttp_lwt_unix

module type S_Type = sig
  type t

  val get_module: t -> string -> Types.Module.t option
  val get_module_list: t -> Types.ModuleList.t
  val save: t -> Types.Module.t -> string -> t
end

module type S = sig
  type t

  val start: ?port:int -> t -> Fpath.t -> unit Lwt.t
end

module Make (T: S_Type) : S with type t := T.t = struct

  let index = Routes.(empty @--> `Root)
  let modul = Routes.(s "module" / str /? nil @--> fun m -> `Module (m))
  let modul_list = Routes.(s "modulelist" /? nil @--> `ModuleList)
  let save = Routes.(s "save" / str /? nil @--> fun s -> `Save s)
  let file = Routes.(s "file" / str / str /? nil @--> fun dir fname -> `File (dir,fname))

  let opt_or_notfound o f =
    match o with
    | Some str -> f str
    | None -> Server.respond_not_found ()

  let (>|?) = opt_or_notfound

  let ok ~body =
    Server.respond_string ~status:`OK ~body ()

  let start ?(port=8080) t root =
    let t = ref t in
    let all = Routes.one_of [index; modul; modul_list; file; save] in

    let (let*) = Lwt.bind in
    let callback _conn req _body =
      let path = req |> Request.uri |> Uri.path in
      Printf.printf "path: %s\n" path;
      Routes.match' all ~target:path >|? function
      | `Root -> Server.respond_redirect ~uri:(Uri.of_string "/file/index/index.html") ()
      | `Module m -> begin
          match T.get_module !t m with
        | Some m -> ok ~body:(Yojson.Safe.to_string (Types.Module.to_yojson m))
        | None -> Server.respond_not_found ()
        end
      | `ModuleList -> ok ~body:(Yojson.Safe.to_string (Types.ModuleList.to_yojson (T.get_module_list !t)))
      | `File (dir,fname) ->
        let fname = Server.resolve_local_file
            ~docroot:(Fpath.to_string root)
            ~uri:(Uri.of_string (dir ^"/"^fname))
        in
        Server.respond_file ~fname ()
      | `Save mname ->
        let* content = Cohttp_lwt.Body.to_string _body in
        let data = Yojson.Safe.from_string content |> Types.Module.of_yojson in
        match data with
        | Ok m -> t := T.save !t m mname; ok ~body:""
        | Error str -> Server.respond_error ~status:`Bad_request ~body:str ()
    in
    Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())
end
