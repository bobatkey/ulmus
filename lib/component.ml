open Js_of_ocaml

module type S = sig
  type state
  type action

  val render : state -> action Html.t
  val update : action -> state -> state
end

type 'state t = (module S with type state = 'state)

(*
type impossible = { impossible : 'a. 'a }

let fixed html =
  let module C = struct
    type state = unit
    type action = impossible

    let render () = html
    let update action () = ()
    let initial = ()
  end
  in
  (module C : S)

let (^^) (module C1 : S) (module C2 : S) =
  let module C = struct
    type state = C1.state * C2.state
    type action =
      | C1 of C1.action
      | C2 of C2.action

    let render (s1, s2) =
      let html1 = Dynamic_HTML.map (fun a -> C1 a) (C1.render s1)
      and html2 = Dynamic_HTML.map (fun a -> C2 a) (C2.render s2)
      in
      Dynamic_HTML.(html1 ^^ html2)

    let update = function
      | C1 action -> fun (s1, s2) -> (C1.update action s1, s2)
      | C2 action -> fun (s1, s2) -> (s1, C2.update action s2)

    let initial =
      (C1.initial, C2.initial)
  end
  in
  (module C : S)
*)

let run (type state) parent (module C : S with type state = state) initial =
  let current_tree = ref None in
  let rec loop state =
    let handler action = loop (C.update action state) in
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
  loop initial

let attach ~parent_id ~initial component =
  let parent_id = Js.string parent_id in
  let node_opt = Dom_html.document##getElementById parent_id in
  match Js.Opt.to_option node_opt with
  | None -> () (* FIXME: throw an exception? *)
  | Some parent -> ignore (run parent component initial)

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

let localStorageSet, localStorageGet =
  match Js.Optdef.to_option Dom_html.window##.localStorage with
  | None -> ((fun _ _ -> ()), fun _ -> None)
  | Some localStorage ->
      ( (fun key data -> localStorage##setItem key data),
        fun key -> Js.Opt.to_option (localStorage##getItem key) )

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
