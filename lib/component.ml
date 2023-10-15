open Js_of_ocaml

let localStorageSet, localStorageGet =
  match Js.Optdef.to_option Dom_html.window##.localStorage with
  | None ->
     ((fun _ _ -> ()), fun _ -> None)
  | Some localStorage ->
      ( (fun key data -> localStorage##setItem key data),
        fun key -> Js.Opt.to_option (localStorage##getItem key) )

module type COMPONENT = sig
  type state
  type action

  val render : state -> action Html.t
  val update : action -> state -> state
  val initial : state
end

module type PERSISTENT = sig
  include COMPONENT

  val serialise : state -> string
  val deserialise : string -> state option
end

let of_persistent ~key (module P : PERSISTENT) =
  let module C =
    struct
      type state = P.state
      type action = P.action

      let render = P.render
      let update action state =
        let state = P.update action state in
        localStorageSet key (Js.string (P.serialise state));
        state

      let initial =
        match localStorageGet key with
        | None -> P.initial
        | Some data ->
           match P.deserialise (Js.to_string data) with
           | None -> P.initial
           | Some state -> state
    end
  in
  (module C : COMPONENT)

let no_persist (module P : PERSISTENT) =
  (module P : COMPONENT)

(* Attaches the component to the given parent node, and sets it
   running *)
let run parent (module C : COMPONENT) =
  let current_tree = ref None in
  let rec handler state action =
    let state = C.update action state in
    render state
  and render state =
    let html = C.render state in
    (match !current_tree with
     | None ->
        let realised_tree = Html.create ~handler:(handler state) ~parent html in
        current_tree := Some realised_tree
     | Some current ->
        let realised_tree =
          Html.update ~handler:(handler state) ~parent ~current html
        in
        current_tree := Some realised_tree);
    Js._false
  in
  render C.initial

(*
let run_persistent parent key (module C : PERSISTENT) =
  let key = Js.string key in
  let current_tree = ref None in
  let rec handler state action =
    let state = C.update action state in
    (* FIXME: take the onStateChange handler as a parameter *)
    localStorageSet key (Js.string (C.snooze state));
    render state
  and render state =
    let html = C.render state in
    (match !current_tree with
     | None ->
        let realised_tree = Html.create ~handler:(handler state) ~parent html in
        current_tree := Some realised_tree
     | Some current ->
        let realised_tree =
          Html.update ~handler:(handler state) ~parent ~current html
        in
        current_tree := Some realised_tree);
    Js._false
  in
  let state =
    Option.value
      ~default:C.initial
      (Option.bind (localStorageGet key)
       @@ fun data -> C.awaken (Js.to_string data))
  in
  render state
 *)

let attach parent_id component =
  let parent_id = Js.string parent_id in
  let node_opt = Dom_html.document##getElementById parent_id in
  match Js.Opt.to_option node_opt with
  | None -> () (* FIXME: throw an exception? *)
  | Some parent -> ignore (run parent component)

let attach_node_init parent component =
  let initial_str =
    match Js.Opt.to_option parent##.textContent with
    | None -> ""
    | Some str -> Js.to_string str
  in
  parent##.textContent := Js.Opt.empty;
  let component = component initial_str in
  ignore (run parent component)

(*
let attach_from_data ~parent_id ~initial component =
  let parent_id = Js.string parent_id in
  let node_opt = Dom_html.document##getElementById parent_id in
  match Js.Opt.to_option node_opt with
  | None -> () (* FIXME: throw an exception? *)
  | Some parent -> attach_node_init parent initial component
 *)

let attach_all name component =
  let divs =
    Dom.list_of_nodeList
      (Dom_html.document##getElementsByTagName (Js.string "div"))
  in
  List.iter
    (fun el ->
      match Js.Opt.to_option (el##getAttribute (Js.string "data-widget")) with
      | Some str when String.equal (Js.to_string str) name ->
         (match Js.Opt.to_option (el##getAttribute (Js.string "data-key")) with
          | Some key ->
             attach_node_init el (fun config -> of_persistent ~key (component config))
          | None ->
             attach_node_init el (fun config -> no_persist (component config)))
      | None | Some _ ->
         ())
    divs

let attach_download_all button_id =
  let keys =
    Dom_html.document##getElementsByTagName (Js.string "div")
    |> Dom.list_of_nodeList
    |> List.filter_map
         (fun el -> Js.Opt.to_option (el##getAttribute (Js.string "data-key")))
  in
  let get_data_url () =
    keys
    |> List.map
         (fun key ->
           Printf.sprintf "%s:%S"
             (Js.to_string key)
             (Option.fold (localStorageGet key)
                ~none:""
                ~some:Js.to_string))
    |> String.concat "\n"
    |> Base64.encode_exn
    |> Printf.sprintf "data:application/x-answers;base64,%s"
    |> Js.string
  in
  let button_id = Js.string button_id in
  Dom_html.document##getElementById button_id
  |> Fun.flip Js.Opt.iter
       (fun button ->
         let filename =
           Js.Opt.case
             (button##getAttribute (Js.string "data-filename"))
             (fun () -> Js.string "answers.txt")
             (fun nm -> nm)
         in
         ignore
           (Dom_html.addEventListener
              button
              Dom_html.Event.click
              (Dom_html.handler (fun _ ->
                   let data = get_data_url () in
                   let el = Dom_html.createA Dom_html.document in
                   el##setAttribute (Js.string "href") data;
                   el##setAttribute (Js.string "download") filename;
                   el##.style##.display := Js.string "none";
                   Dom.appendChild (Dom_html.document##.body) el;
                   el##click;
                   Dom.removeChild (Dom_html.document##.body) el;
                   Js._false))
              Js._false))

(*
module Cmd = struct
  (* FIXME: multiple events on the same object? *)
  type 'a t =
    | Cmd : {
        setup : unit -> (< .. > as 'o) Js.t;
        event : ('e #Dom.event Js.t as 'b) Dom.Event.typ;
        handle : 'o Js.t -> ('e #Dom.event Js.t as 'b) -> 'a option;
        prepare : 'o Js.t -> unit;
      }
        -> 'a t

  let read_file file f =
    Cmd
      {
        setup = (fun () -> new%js File.fileReader);
        event = File.ReaderEvent.loadend;
        handle =
          (fun fr _ ->
            (* FIXME: error reporting *)
            let content = File.CoerceTo.arrayBuffer fr##.result in
            match Js.Opt.to_option content with
            | None -> None
            | Some content -> f (Typed_array.String.of_arrayBuffer content));
        prepare = (fun fr -> fr##readAsArrayBuffer file);
      }

  (* Save to server... *)

  (* Retrieve from server... *)
end
 *)

(*
module type FREEZABLE = sig
  include S

  val update : action -> state -> state * action Cmd.t list
  val freeze : state -> string
  val thaw : string -> state option
end

module type SEXP_S = sig
  type state [@@deriving sexp]

  include S with type state := state
end

module Freezable_Of_Sexpable (C : SEXP_S) = struct
  type state = C.state
  type action = C.action

  let render = C.render
  let update action state = (C.update action state, [])
  let freeze state = Sexplib.Sexp.to_string (C.sexp_of_state state)

  let thaw string =
    match Sexplib.Sexp.of_string string with
    | exception _ -> None
    | sexp -> (
        match C.state_of_sexp sexp with
        | exception _ -> None
        | state -> Some state)
end

type 'state freezable = (module FREEZABLE with type state = 'state)

let run_persistent (type state) key parent
    (module C : FREEZABLE with type state = state) initial =
  let key = Js.string key in
  let current_tree = ref None in
  let current_state =
    ref
      (match localStorageGet key with
      | None -> initial
      | Some data -> (
          match C.thaw (Js.to_string data) with
          | None -> initial
          | Some state -> state))
  in
  let rec handler action =
    let state, extnls = C.update action !current_state in
    current_state := state;
    extnls
    |> List.iter (fun (Cmd.Cmd cmd) ->
           let obj = cmd.setup () in
           let _id =
             Dom.addEventListener obj cmd.event
               (Dom.handler (fun evd ->
                    match cmd.handle obj evd with
                    | None -> Js._true
                    | Some action -> handler action))
               Js._false
           in
           cmd.prepare obj);
    render ()
  and render () =
    let state = !current_state in
    let data = Js.string (C.freeze state) in
    localStorageSet key data;
    let html = C.render state in
    (match !current_tree with
    | None ->
        let realised_tree = Html.create ~handler ~parent html in
        current_tree := Some realised_tree
    | Some current ->
        let realised_tree =
          Html.update ~handler ~parent ~current html
        in
        current_tree := Some realised_tree);
    Js._false
  in
  render ()
(*
    match localStorageGet key with
    | None ->
       loop C.initial
    | Some data ->
       match C.thaw (Js.to_string data) with
         | None ->
            loop C.initial
         | Some state ->
            loop state
*)

let attach_persistent ~parent_id ~key ~initial component =
  let parent_id = Js.string parent_id in
  let node_opt = Dom_html.document##getElementById parent_id in
  match Js.Opt.to_option node_opt with
  | None -> () (* FIXME: throw an exception? *)
  | Some parent -> ignore (run_persistent key parent component initial)
 *)
