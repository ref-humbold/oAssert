open Internals

module type TUPLE2_ASSERT = sig
  type fst_elem

  type snd_elem

  type tuple2 = fst_elem * snd_elem

  val equal_to : tuple2 -> tuple2 assertion

  val first : fst_elem -> tuple2 assertion

  val second : snd_elem -> tuple2 assertion
end
