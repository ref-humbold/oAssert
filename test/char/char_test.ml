(* Tests: Char assertions. *)
open OUnit2
open Char_test_comparison
open Char_test_lettercase
open Char_test_values

let char_Test = __MODULE__ >::: [char_test_comparison; char_test_lettercase; char_test_values]

let _ = run_test_tt_main char_Test
