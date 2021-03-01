open Brr

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

let main model =
  let id = Jstr.v "main" in
  match Document.find_el_by_id G.document id with
  | None -> Console.(error ["no element with name main"])
  | Some el ->
    let m_view = view model in
    El.set_children el [m_view]

let () =
  main ["abc";"def"]
