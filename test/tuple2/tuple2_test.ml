(* Tests: Tuple (of 2 elements) assertions. *)
open OUnit2
open Tuple2_test_equal
open Tuple2_test_members

let tuple2_Test = __MODULE__ >::: [tuple2_test_equal; tuple2_test_members]

let _ = run_test_tt_main tuple2_Test
