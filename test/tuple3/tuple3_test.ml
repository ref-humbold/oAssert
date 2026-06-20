(* Tests: Tuple (of 3 elements) assertions. *)
open OUnit2
open Tuple3_test_equal
open Tuple3_test_members

(* tuple3_Test *)

let tuple3_Test = __MODULE__ >::: [tuple3_test_equal; tuple3_test_members]

let _ = run_test_tt_main tuple3_Test
