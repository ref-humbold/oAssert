open Internals

module type TUPLE2_ASSERT = sig
  type fst_elem

  type snd_elem

  type tuple2 = fst_elem * snd_elem

  val equal_to : tuple2 -> tuple2 assertion

  val with_first : fst_elem -> tuple2 assertion

  val with_second : snd_elem -> tuple2 assertion
end

let string_of printer1 printer2 (x, y) = Printf.sprintf "(%s, %s)" (printer1 x) (printer2 y)
