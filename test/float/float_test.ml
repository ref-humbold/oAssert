(* Tests: Float assertions. *)
open OUnit2
open Float_test_basic
open Float_test_comparison

let float_Test = __MODULE__ >::: [float_test_basic; float_test_comparison]

let _ = run_test_tt_main float_Test
