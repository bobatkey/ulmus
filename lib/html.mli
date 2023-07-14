(** Client-side HTML representation, with DOM-diffing *)

open Js_of_ocaml
include Html_sig.S

type key_modifiers = { alt : bool; shift : bool; ctrl : bool; meta : bool }

module A : sig
  include module type of A

  val autofocus : _ attribute
  val for_ : string -> _ attribute
end

module E : sig
  val onkeypress :
    (key_modifiers -> Uchar.t -> 'action option) -> 'action attribute

  val onkeydown :
    (key_modifiers -> Dom_html.Keyboard_code.t -> 'action option) ->
    'action attribute

  val onkeyup :
    (key_modifiers -> Dom_html.Keyboard_code.t -> 'action option) ->
    'action attribute

  val onclick : 'action -> 'action attribute
  val onclick_pos : (int -> int -> 'action) -> 'action attribute
  val ondoubleclick : 'action -> 'action attribute
  val oninput : (string -> 'action) -> 'action attribute
  val onchange : (string -> 'action) -> 'action attribute
  val onchange2 : (File.file Js.t -> 'action) -> 'action attribute
  val blur : 'action -> 'action attribute
end

type realised_tree

val create :
  handler:('action -> bool Js.t) ->
  parent:Dom_html.element Js.t ->
  'action t ->
  realised_tree

val update :
  handler:('action -> bool Js.t) ->
  parent:Dom_html.element Js.t ->
  current:realised_tree ->
  'action t ->
  realised_tree
