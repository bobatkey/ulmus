module Html = Html

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

let of_persistent = Component.of_persistent

let attach = Component.attach

let attach_all = Component.attach_all

let attach_download_button = Component.attach_download_all
