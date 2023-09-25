module Html = Html

type 'a html = 'a Html.t

module type S = sig
  type state
  type action

  val render : state -> action html
  val update : action -> state -> state
end

type 'a component = (module S with type state = 'a)

let attach = Component.attach
let attach_from_data = Component.attach_from_data
let attach_all = Component.attach_all
