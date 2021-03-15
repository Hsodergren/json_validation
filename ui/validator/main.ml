open Brr
open Brr_io
open React
open Jsch

module Model = Validator

let set_children signal f el =
  S.trace (fun v ->
      El.set_children el (f v)
    ) signal
  |> ignore

let trd (_,_,x) = x

let rec zip l1 l2 =
  match l1,l2 with
  | hd1::tl1,hd2::tl2 -> (hd1,hd2)::zip tl1 tl2
  | [], [] -> []
  | _ -> failwith "lists different lengths"

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
    let jsons = List.mapi (fun i sm -> S.map (Model.to_yojson ~handle_required:(i=0)) sm) models_s in
    let s = S.merge (fun a b -> Jsch.Merge.merge <$> a <*> b) (Some (`Assoc [])) jsons in
    let parent = El.div [] in
    set_children s (function
        | Some json ->
          let model = Model.make schema json in
          S.const model
          |> Model.view ~disabled:true ~id:"result" ~search
          |> trd
        | None -> []
      ) parent;
    parent

let validator ?(disabled=false) ?(handle_required=true) ?(search=S.const "") ~id model =
  let def model_s =
    let v,a,el = Model.view ~disabled ~handle_required ~search ~id model_s in
    let apply_action = E.map apply_action a in
    let model_s' = S.accum ~eq:Model.equal apply_action (S.value model_s) in
    model_s',(model_s',v,el)
  in
  S.fix ~eq:Model.equal model def

let save_button () =
  let but_el = El.button [El.txt' "Save"] in
  let e, send_e = E.create () in
  let c, send_click = E.create () in
  let _ = E.trace (fun b ->
      if b
      then El.set_at (Jstr.v "disabled") None but_el
      else El.set_at (Jstr.v "disabled") (Some (Jstr.v "true")) but_el
    ) e
  in
  Ev.listen Ev.click (fun _ ->
      send_click ()
    ) (El.as_target but_el);
  c, send_e, but_el

let header () =
  let sb_s, sb_el = searchbar () in
  let c, ef, save_but = save_button () in
  c, ef, sb_s, El.div ~at:[At.class' (Jstr.v "header")] [sb_el;save_but]

let save name models schema =
  let open Fetch in
  let open Fut.Result_syntax in
  let body =
    models
    |> List.mapi (fun i (name,m) -> name,Option.get (Model.to_yojson ~handle_required:(i=0) m))
    |> Types.Module.v schema
    |> Types.Module.to_yojson
    |> Yojson.Safe.to_string
    |> Jstr.v
    |> Body.of_jstr
  in
  let req = Request.v ~init:(Request.init ~body ~method':(Jstr.v "POST") ()) (Jstr.(v "/save/" + v name)) in
  let* resp = Fetch.request req in
  if Fetch.Response.ok resp
  then Fut.return @@ Ok ()
  else Fut.return @@ Error (Jv.Error.v (Jstr.v "error saving module"))

let body name schemajson jsons sb_s save_enable c =
  match Schema.of_yojson ~assume_object:true schemajson with
  | Error str -> El.txt' ("schema load error : " ^ str)
  | Ok schema ->
    let models,_vs,uis =
      List.mapi (fun i (name,j) -> name,Model.make schema j,i=0) jsons
      |> List.map (fun (id,m,b) -> validator ~handle_required:b ~search:sb_s ~id m)
      |> Util.unzip3
    in
    let result_ui = create_result models ~search:sb_s in
    let validators = List.map (fun el ->
        El.div ~at:[At.class' (Jstr.v "validator")] el
      ) uis
    in
    let _ = S.trace
        save_enable
        (S.merge (fun b v -> b && match v with | `Valid | `Empty -> true | _ -> false) true _vs)
    in
    let _ = E.trace (fun _ ->
        let models = zip (List.map fst jsons) (List.map S.value models) in
        match models with
        | [] -> Console.error ["no models to save"]
        | _::_ ->
          Console.log["saving"];
          save_enable false;
          let save_fut = save name models schemajson in
          Fut.await save_fut (function
              | Ok () -> Console.log["saving done"];
              | Error err -> Console.log [Jv.Error.message err]
            );
          save_enable true
      ) c
    in
    let validators = El.div ~at:[At.class' (Jstr.v "validators")] validators in
    let result_ui = El.div ~at:[At.class' (Jstr.v "result")] [result_ui] in
    El.div ~at:[At.class' (Jstr.v "content")] [result_ui;validators]

let main_ui name schema jsons =
  let c, save_enable, sb_s, header_el = header () in
  let body_el = body name schema jsons sb_s save_enable c in
  El.div [header_el;body_el]

let get_ui name =
  let open Fut.Result_syntax in
  let uri = Brr.Window.location Brr.G.window in
  let m = Brr.Uri.Params.of_jstr (Uri.query uri) |> Uri.Params.find (Jstr.v "module") in
  let open Fetch in
  let req = Request.v ~init:(Request.init ()) (Jstr.append (Jstr.v "/module/") (m |> Option.get)) in
  let* resp = Fetch.request req in
  let* body = Fetch.Response.as_body resp |> Fetch.Body.text in
  let ui =
    match Types.Module.of_yojson (Yojson.Safe.from_string (Jstr.to_string body)) with
    | Ok {schema;jsons} -> main_ui name schema jsons
    | Error str -> El.txt' str
  in
  Fut.return @@ Ok (ui)

let main () =
  let id = Jstr.v "main" in
  match Document.find_el_by_id G.document id with
  | None -> Console.(error ["no element with name main"])
  | Some el ->
    let uri = Brr.Window.location Brr.G.window in
    let name = Brr.Uri.Params.of_jstr (Uri.query uri) |> Uri.Params.find (Jstr.v "module") in
    match name with
    | Some name ->
      ignore @@
      Fut.bind (get_ui (Jstr.to_string name)) (function
          | Ok ui -> Fut.return @@ El.set_children el [ui]
          | Error str -> Fut.return @@ El.set_children el [El.txt (Jv.Error.message str)]
        )
    | None -> Console.error ["not module parameter"]

let () = ignore @@ main ()
