(* Tests: Bool assertions. *)
open OUnit2
open OAssert

(* is_true_Test_list *)

let is_true__when_actual_is_true__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that true Is.true_ in
    (* then *)
    assert_that action Is.raising_nothing

let is_true__when_actual_is_false__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that false Is.true_ in
    (* then *)
    assert_that action @@ Is.raising (Assertion_failed "Expected true, but was false")

let is_true_Test_list =
  test_list [is_true__when_actual_is_true__then_passed; is_true__when_actual_is_false__then_failed]

(* not_is_true_Test_list *)

let not_is_true__when_actual_is_true__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that true @@ Satisfies.not Is.true_ in
    (* then *)
    assert_that action @@ Is.raising (Assertion_failed "Expected value different than true")

let not_is_true__when_actual_is_false__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that false @@ Satisfies.not Is.true_ in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_true_Test_list =
  test_list
    [not_is_true__when_actual_is_true__then_failed; not_is_true__when_actual_is_false__then_passed]

(* is_false_Test_list *)

let is_false__when_actual_is_false__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that false Is.false_ in
    (* then *)
    assert_that action Is.raising_nothing

let is_false__when_actual_is_true__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that true Is.false_ in
    (* then *)
    assert_that action @@ Is.raising (Assertion_failed "Expected false, but was true")

let is_false_Test_list =
  test_list [is_false__when_actual_is_false__then_passed; is_false__when_actual_is_true__then_failed]

(* not_is_false_Test_list *)

let not_is_false__when_actual_is_false__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that false @@ Satisfies.not Is.false_ in
    (* then *)
    assert_that action @@ Is.raising (Assertion_failed "Expected value different than false")

let not_is_false__when_actual_is_true__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that true @@ Satisfies.not Is.false_ in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_false_Test_list =
  test_list
    [not_is_false__when_actual_is_false__then_failed; not_is_false__when_actual_is_true__then_passed]

(* bool_Test *)

let bool_Test =
  "Tests: Bool assertions"
  >::: [is_true_Test_list; not_is_true_Test_list; is_false_Test_list; not_is_false_Test_list]

let _ = run_test_tt_main bool_Test
