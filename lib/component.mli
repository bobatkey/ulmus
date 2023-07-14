(** Interactive components *)

module type S = sig
  type state
  type action

  val render : state -> action Html.t
  val update : action -> state -> state
end

type 'a t = (module S with type state = 'a)

val attach : parent_id:string -> initial:'state -> 'state t -> unit

module Cmd : sig
  type 'a t

  val read_file :
    #Js_of_ocaml.File.file Js_of_ocaml.Js.t -> (string -> 'a option) -> 'a t
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

module Freezable_Of_Sexpable (S : SEXP_S) :
  FREEZABLE with type state = S.state and type action = S.action

type 'state freezable = (module FREEZABLE with type state = 'state)

val attach_persistent :
  parent_id:string -> key:string -> initial:'state -> 'state freezable -> unit
