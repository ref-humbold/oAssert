(* Tests: List assertions. *)
open OUnit2
open List_test_values
open List_test_contain
open List_test_length
open List_test_matching

(* list_Test *)

let list_Test =
  __MODULE__ >::: [list_test_values; list_test_contain; list_test_matching; list_test_length]

let _ = run_test_tt_main list_Test
