open Internals

module type TUPLE3_ASSERT = sig
  type fst_elem

  type snd_elem

  type trd_elem

  type tuple3 = fst_elem * snd_elem * trd_elem

  val equal_to : tuple3 -> tuple3 assertion

  val with_first : fst_elem -> tuple3 assertion

  val with_second : snd_elem -> tuple3 assertion

  val with_third : trd_elem -> tuple3 assertion
end
