(* Tests: Assertion failed. *)
open OUnit2
open OAssert

let assertion_failed__when_function__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let message = "Custom failure message" in
    (* when *)
    let action () = assertion_failed message in
    (* then *)
    assert_that action @@ Is.raising (Assertion_failed message)

let assertion_failed__when_operator__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let message = "Custom failure message" in
    (* when *)
    let action () = !!!message in
    (* then *)
    assert_that action @@ Is.raising (Assertion_failed message)

let assertion_failed_Test_list =
  test_list
    [assertion_failed__when_function__then_failed; assertion_failed__when_operator__then_failed]

(* assertion_failed_Test *)

let assertion_failed_Test = __MODULE__ >::: [assertion_failed_Test_list]

let _ = run_test_tt_main assertion_failed_Test
