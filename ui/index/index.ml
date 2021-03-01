open Brr
open Brr_io

module Model = struct
  type t = string list
end

let view model =
  let ahrefs =
    model
    |> List.map Jstr.v
    |> List.map (fun s ->
      let uri = Jstr.(v "/file/validator/validator.html?module=" + s) in
      let at = [At.href uri] in
      El.li [El.a ~at [El.txt s]])
  in
  El.ul ahrefs

let get_ui () =
  let open Fut.Result_syntax in
  let open Fetch in
  let req = Request.v ~init:(Request.init ()) (Jstr.v "/modulelist") in
  let* resp = request req in
  let* body = Response.as_body resp |> Body.text in
  let ui =
    match Types.ModuleList.of_yojson (Yojson.Safe.from_string (Jstr.to_string body)) with
    | Ok m -> view (Types.ModuleList.to_list m)
    | Error str -> El.txt' str
  in
  Fut.return @@ Ok (ui)

let main () =
  let id = Jstr.v "main" in
  match Document.find_el_by_id G.document id with
  | None -> Console.(error ["no element with name main"])
  | Some el ->
    ignore @@ Fut.bind (get_ui ())
      (function
        | Ok ui -> Fut.return @@ El.set_children el [ui]
        | Error err -> Fut.return @@ El.set_children el [El.txt @@Jv.Error.message err]
      )

let () =
  main ()
