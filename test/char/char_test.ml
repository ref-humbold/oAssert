(* Tests: Char assertions. *)
open OUnit2
open Char_test_lettercase
open Char_test_lettertype

let char_Test = __MODULE__ >::: [char_test_lettercase; char_test_lettertype]

let _ = run_test_tt_main char_Test
