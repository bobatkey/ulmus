module Html : sig
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
      (key_modifiers -> Js_of_ocaml.Dom_html.Keyboard_code.t -> 'action option) ->
      'action attribute

    val onkeyup :
      (key_modifiers -> Js_of_ocaml.Dom_html.Keyboard_code.t -> 'action option) ->
      'action attribute

    val onclick : 'action -> 'action attribute
    val onclick_pos : (int -> int -> 'action) -> 'action attribute
    val ondoubleclick : 'action -> 'action attribute
    val oninput : (string -> 'action) -> 'action attribute
    val onchange : (string -> 'action) -> 'action attribute
    val onchange2 : (Js_of_ocaml.File.file Js_of_ocaml.Js.t -> 'action) -> 'action attribute
    val blur : 'action -> 'action attribute
  end
end

type 'a html = 'a Html.t

module type COMPONENT = sig
  type state
  type action

  val render : state -> action html
  val update : action -> state -> state
  val initial : state
end

module type PERSISTENT = sig
  include COMPONENT

  val serialise : state -> string
  val deserialise : string -> state option
end

val attach : string -> (module COMPONENT) -> unit

val attach_all : string -> (string -> (module PERSISTENT)) -> unit

val attach_download_button : string -> unit
