open Internals

module type OPTION_ASSERT = sig
  type elem

  val none : elem option assertion

  val some : elem -> elem option assertion
end

let string_of printer opt =
  match opt with
  | Some x -> Printf.sprintf "Some %s" @@ printer x
  | None -> "None"
