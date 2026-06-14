(* Tests: Int assertions. *)
open OUnit2
open Int_test_basic
open Int_test_comparison

let int_Test = __MODULE__ >::: [int_test_basic; int_test_comparison]

let _ = run_test_tt_main int_Test
