(* Tests: Assertion failing. *)
open OUnit2
open OAssert

let assert_fail__then_failed =
  "assert_fail__then_failed" >:: fun _ ->
    (* given *)
    let message = "Custom failure message" in
    (* when *)
    let action () = assert_fail ~msg:message in
    (* then *)
    assert_that action @@ Is.raising (Assertion_failed message)

let assert_fail_Test_list = test_list [assert_fail__then_failed]

(* assert_fail_Test *)

let assert_fail_Test = "Tests: Failing function" >::: [assert_fail_Test_list]

let _ = run_test_tt_main assert_fail_Test
