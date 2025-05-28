(* Tests: Int assertions. *)
open OUnit2
open OAssert

(* is_zero_Test_list *)

let is_zero__when_actual_is_zero__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that 0 Is.Int.zero in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let is_zero__when_actual_different_than_zero__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = 10 in
    (* when *)
    let exec () = assert_that value Is.Int.zero in
    (* then *)
    let expected = Assertion_failed (Printf.sprintf "Expected 0, but was %d" value) in
    assert_that exec @@ Is.raising expected

let is_zero_Test_list =
  test_list
    [ is_zero__when_actual_is_zero__then_passed;
      is_zero__when_actual_different_than_zero__then_failed ]

(* not_is_zero_Test_list *)

let not_is_zero__when_actual_is_zero__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that 0 @@ Satisfies.not Is.Int.zero in
    (* then *)
    let expected = Assertion_failed "Expected value different than 0" in
    assert_that exec @@ Is.raising expected

let not_is_zero__when_actual_different_than_zero__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that 10 @@ Satisfies.not Is.Int.zero in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_zero_Test_list =
  test_list
    [ not_is_zero__when_actual_is_zero__then_failed;
      not_is_zero__when_actual_different_than_zero__then_passed ]

(* is_positive_Test_list *)

let is_positive__when_actual_greater_than_zero__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that 10 Is.Int.positive in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let is_positive__when_actual_less_than_zero__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = -10 in
    (* when *)
    let exec () = assert_that value Is.Int.positive in
    (* then *)
    let expected = Assertion_failed (Printf.sprintf "Expected %d to be positive" value) in
    assert_that exec @@ Is.raising expected

let is_positive__when_actual_equal_to_zero__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = 0 in
    (* when *)
    let exec () = assert_that value Is.Int.positive in
    (* then *)
    let expected = Assertion_failed (Printf.sprintf "Expected %d to be positive" value) in
    assert_that exec @@ Is.raising expected

let is_positive_Test_list =
  test_list
    [ is_positive__when_actual_greater_than_zero__then_passed;
      is_positive__when_actual_less_than_zero__then_failed;
      is_positive__when_actual_equal_to_zero__then_failed ]

(* not_is_positive_Test_list *)

let not_is_positive__when_actual_greater_than_zero__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = 10 in
    (* when *)
    let exec () = assert_that value @@ Satisfies.not Is.Int.positive in
    (* then *)
    let expected = Assertion_failed (Printf.sprintf "Expected %d not to be positive" value) in
    assert_that exec @@ Is.raising expected

let not_is_positive__when_actual_less_than_zero__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that (-10) @@ Satisfies.not Is.Int.positive in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_positive__when_actual_equal_to_zero__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    (* when *)
    let exec () = assert_that 0 @@ Satisfies.not Is.Int.positive in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_positive_Test_list =
  test_list
    [ not_is_positive__when_actual_greater_than_zero__then_failed;
      not_is_positive__when_actual_less_than_zero__then_passed;
      not_is_positive__when_actual_equal_to_zero__then_passed ]

(* is_negative_Test_list *)

let is_negative__when_actual_less_than_zero__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that (-10) Is.Int.negative in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let is_negative__when_actual_greater_than_zero__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = 10 in
    (* when *)
    let exec () = assert_that value Is.Int.negative in
    (* then *)
    let expected = Assertion_failed (Printf.sprintf "Expected %d to be negative" value) in
    assert_that exec @@ Is.raising expected

let is_negative__when_actual_equal_to_zero__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = 0 in
    (* when *)
    let exec () = assert_that value Is.Int.negative in
    (* then *)
    let expected = Assertion_failed (Printf.sprintf "Expected %d to be negative" value) in
    assert_that exec @@ Is.raising expected

let is_negative_Test_list =
  test_list
    [ is_negative__when_actual_less_than_zero__then_passed;
      is_negative__when_actual_greater_than_zero__then_failed;
      is_negative__when_actual_equal_to_zero__then_failed ]

(* not_is_negative_Test_list *)

let not_is_negative__when_actual_less_than_zero__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = -10 in
    (* when *)
    let exec () = assert_that value @@ Satisfies.not Is.Int.negative in
    (* then *)
    let expected = Assertion_failed (Printf.sprintf "Expected %d not to be negative" value) in
    assert_that exec @@ Is.raising expected

let not_is_negative__when_actual_greater_than_zero__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that 10 @@ Satisfies.not Is.Int.negative in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_negative__when_actual_equal_to_zero__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    (* when *)
    let exec () = assert_that 0 @@ Satisfies.not Is.Int.negative in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_negative_Test_list =
  test_list
    [ not_is_negative__when_actual_less_than_zero__then_failed;
      not_is_negative__when_actual_greater_than_zero__then_passed;
      not_is_negative__when_actual_equal_to_zero__then_passed ]

(* is_equal_to_Test_list *)

let is_equal_to__when_actual_is_same__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = 10 in
    (* when *)
    let exec () = assert_that value @@ Is.Int.equal_to value in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let is_equal_to__when_actual_different__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value1 = 10 and value2 = 123 in
    (* when *)
    let exec () = assert_that value2 @@ Is.Int.equal_to value1 in
    (* then *)
    let expected = Assertion_failed (Printf.sprintf "Expected %d, but was %d" value1 value2) in
    assert_that exec @@ Is.raising expected

let is_equal_to_Test_list =
  test_list
    [is_equal_to__when_actual_is_same__then_passed; is_equal_to__when_actual_different__then_failed]

(* not_is_equal_to_Test_list *)

let not_is_equal_to__when_actual_is_same__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = 10 in
    (* when *)
    let exec () = assert_that value @@ Satisfies.not @@ Is.Int.equal_to value in
    (* then *)
    let expected = Assertion_failed (Printf.sprintf "Expected value different than %d" value) in
    assert_that exec @@ Is.raising expected

let not_is_equal_to__when_actual_is_different__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that 10 @@ Satisfies.not @@ Is.Int.equal_to 123 in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_equal_to_Test_list =
  test_list
    [ not_is_equal_to__when_actual_is_same__then_failed;
      not_is_equal_to__when_actual_is_different__then_passed ]

(* int_Test *)

let int_Test =
  __MODULE__
  >::: [ is_zero_Test_list;
         not_is_zero_Test_list;
         is_positive_Test_list;
         not_is_positive_Test_list;
         is_negative_Test_list;
         not_is_negative_Test_list;
         is_equal_to_Test_list;
         not_is_equal_to_Test_list ]

let _ = run_test_tt_main int_Test
