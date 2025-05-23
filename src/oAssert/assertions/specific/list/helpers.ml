open Internals

module type LIST_ASSERT = sig
  type elem

  val empty : elem list assertion

  val of_length : int -> elem list assertion

  val equal_to : elem list -> elem list assertion

  val containing : elem -> elem list assertion
end

let string_of printer lst =
  Printf.sprintf "[%s]" @@ Stdlib.String.concat "; " @@ Stdlib.List.map printer lst
