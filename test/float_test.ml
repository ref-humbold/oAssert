(* Tests: Int assertions. *)
open OUnit2
open OAssert

(* is_nan_Test_list *)

let is_nan__when_actual_is_nan__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that Float.nan Is.Float.nan in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let is_nan__when_actual_different_than_nan__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = 12.6 in
    (* when *)
    let exec () = assert_that value Is.Float.nan in
    (* then *)
    let expected =
      Assertion_failed (Printf.sprintf "Expected nan, but was %s" (Float.to_string value))
    in
    assert_that exec @@ Is.raising expected

let is_nan_Test_list =
  test_list
    [is_nan__when_actual_is_nan__then_passed; is_nan__when_actual_different_than_nan__then_failed]

(* not_is_nan_Test_list *)

let not_is_nan__when_actual_is_nan__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that Float.nan @@ Satisfies.not Is.Float.nan in
    (* then *)
    let expected = Assertion_failed "Expected value different than nan" in
    assert_that exec @@ Is.raising expected

let not_is_nan__when_actual_different_than_nan__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that 12.6 @@ Satisfies.not Is.Float.nan in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_nan_Test_list =
  test_list
    [ not_is_nan__when_actual_is_nan__then_failed;
      not_is_nan__when_actual_different_than_nan__then_passed ]

(* is_zero_Test_list *)

let is_zero__when_actual_is_zero__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that 0.0 Is.Float.zero in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let is_zero__when_actual_different_than_zero__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = 12.6 in
    (* when *)
    let exec () = assert_that value Is.Float.zero in
    (* then *)
    let expected =
      Assertion_failed (Printf.sprintf "Expected 0., but was %s" (Float.to_string value))
    in
    assert_that exec @@ Is.raising expected

let is_zero_Test_list =
  test_list
    [ is_zero__when_actual_is_zero__then_passed;
      is_zero__when_actual_different_than_zero__then_failed ]

(* not_is_zero_Test_list *)

let not_is_zero__when_actual_is_zero__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that 0.0 @@ Satisfies.not Is.Float.zero in
    (* then *)
    let expected = Assertion_failed "Expected value different than 0." in
    assert_that exec @@ Is.raising expected

let not_is_zero__when_actual_different_than_zero__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that 12.6 @@ Satisfies.not Is.Float.zero in
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
    let exec () = assert_that 12.6 Is.Float.positive in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let is_positive__when_actual_less_than_zero__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = -12.6 in
    (* when *)
    let exec () = assert_that value Is.Float.positive in
    (* then *)
    let expected =
      Assertion_failed (Printf.sprintf "Expected %s to be positive" (Float.to_string value))
    in
    assert_that exec @@ Is.raising expected

let is_positive__when_actual_equal_to_zero__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = 0.0 in
    (* when *)
    let exec () = assert_that value Is.Float.positive in
    (* then *)
    let expected =
      Assertion_failed (Printf.sprintf "Expected %s to be positive" (Float.to_string value))
    in
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
    let value = 12.6 in
    (* when *)
    let exec () = assert_that value @@ Satisfies.not Is.Float.positive in
    (* then *)
    let expected =
      Assertion_failed (Printf.sprintf "Expected %s not to be positive" (Float.to_string value))
    in
    assert_that exec @@ Is.raising expected

let not_is_positive__when_actual_less_than_zero__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that (-12.6) @@ Satisfies.not Is.Float.positive in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_positive__when_actual_equal_to_zero__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    (* when *)
    let exec () = assert_that 0.0 @@ Satisfies.not Is.Float.positive in
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
    let exec () = assert_that (-12.6) Is.Float.negative in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let is_negative__when_actual_greater_than_zero__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = 12.6 in
    (* when *)
    let exec () = assert_that value Is.Float.negative in
    (* then *)
    let expected =
      Assertion_failed (Printf.sprintf "Expected %s to be negative" (Float.to_string value))
    in
    assert_that exec @@ Is.raising expected

let is_negative__when_actual_equal_to_zero__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = 0.0 in
    (* when *)
    let exec () = assert_that value Is.Float.negative in
    (* then *)
    let expected =
      Assertion_failed (Printf.sprintf "Expected %s to be negative" (Float.to_string value))
    in
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
    let value = -12.6 in
    (* when *)
    let exec () = assert_that value @@ Satisfies.not Is.Float.negative in
    (* then *)
    let expected =
      Assertion_failed (Printf.sprintf "Expected %s not to be negative" (Float.to_string value))
    in
    assert_that exec @@ Is.raising expected

let not_is_negative__when_actual_greater_than_zero__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that 12.6 @@ Satisfies.not Is.Float.negative in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_negative__when_actual_equal_to_zero__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    (* when *)
    let exec () = assert_that 0.0 @@ Satisfies.not Is.Float.negative in
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
    let value = 12.6 in
    (* when *)
    let exec () = assert_that value @@ Is.Float.equal_to value in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let is_equal_to__when_actual_different__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value1 = 12.6 and value2 = 345.77 in
    (* when *)
    let exec () = assert_that value2 @@ Is.Float.equal_to value1 in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf "Expected %s, but was %s" (Float.to_string value1) (Float.to_string value2))
    in
    assert_that exec @@ Is.raising expected

let is_equal_to_Test_list =
  test_list
    [is_equal_to__when_actual_is_same__then_passed; is_equal_to__when_actual_different__then_failed]

(* not_is_equal_to_Test_list *)

let not_is_equal_to__when_actual_is_same__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = 12.6 in
    (* when *)
    let exec () = assert_that value @@ Satisfies.not @@ Is.Float.equal_to value in
    (* then *)
    let expected =
      Assertion_failed (Printf.sprintf "Expected value different than %s" (Float.to_string value))
    in
    assert_that exec @@ Is.raising expected

let not_is_equal_to__when_actual_is_different__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that 12.6 @@ Satisfies.not @@ Is.Float.equal_to 345.77 in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_equal_to_Test_list =
  test_list
    [ not_is_equal_to__when_actual_is_same__then_failed;
      not_is_equal_to__when_actual_is_different__then_passed ]

(* is_close_to_Test_list *)

let is_close_to__when_actual_is_same__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = 12.6 in
    (* when *)
    let exec () = assert_that value @@ Is.Float.close_to value ~diff:1e-3 in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let is_close_to__when_actual_is_in_difference__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that 12.600095 @@ Is.Float.close_to 12.6 ~diff:1e-3 in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let is_close_to__when_actual_different__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value1 = 12.6 and value2 = 345.77 and diff = 1e-3 in
    (* when *)
    let exec () = assert_that value2 @@ Is.Float.close_to value1 ~diff in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s to be close to %s with difference %s, but difference was %s"
           (Float.to_string value2)
           (Float.to_string value1)
           (Float.to_string diff)
           (Float.to_string @@ abs_float (value2 -. value1)) )
    in
    assert_that exec @@ Is.raising expected

let is_close_to_Test_list =
  test_list
    [ is_close_to__when_actual_is_same__then_passed;
      is_close_to__when_actual_is_in_difference__then_passed;
      is_close_to__when_actual_different__then_failed ]

(* not_is_close_to_Test_list *)

let not_is_close_to__when_actual_is_same__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = 12.6 and diff = 1e-3 in
    (* when *)
    let exec () = assert_that value @@ Satisfies.not @@ Is.Float.close_to value ~diff in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s not to be close to %s with difference %s"
           (Float.to_string value)
           (Float.to_string value)
           (Float.to_string diff) )
    in
    assert_that exec @@ Is.raising expected

let not_is_close_to__when_actual_is_in_difference__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value1 = 12.6 and value2 = 12.600095 and diff = 1e-3 in
    (* when *)
    let exec () = assert_that value2 @@ Satisfies.not @@ Is.Float.close_to value1 ~diff in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s not to be close to %s with difference %s"
           (Float.to_string value2)
           (Float.to_string value1)
           (Float.to_string diff) )
    in
    assert_that exec @@ Is.raising expected

let not_is_close_to__when_actual_is_different__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that 12.6 @@ Satisfies.not @@ Is.Float.close_to 345.77 ~diff:1e-3 in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_close_to_Test_list =
  test_list
    [ not_is_close_to__when_actual_is_same__then_failed;
      not_is_close_to__when_actual_is_in_difference__then_failed;
      not_is_close_to__when_actual_is_different__then_passed ]

(* is_greater_than_Test_list *)

let is_greater_than__when_actual_greater__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value1 = 12.6 and value2 = 345.77 in
    (* when *)
    let exec () = assert_that value2 @@ Is.Float.greater_than value1 in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let is_greater_than__when_actual_equal__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = 12.6 in
    (* when *)
    let exec () = assert_that value @@ Is.Float.greater_than value in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s to be greater than %s"
           (Float.to_string value)
           (Float.to_string value) )
    in
    assert_that exec @@ Is.raising expected

let is_greater_than__when_actual_less__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value1 = 12.6 and value2 = 345.77 in
    (* when *)
    let exec () = assert_that value1 @@ Is.Float.greater_than value2 in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s to be greater than %s"
           (Float.to_string value1)
           (Float.to_string value2) )
    in
    assert_that exec @@ Is.raising expected

let is_greater_than_Test_list =
  test_list
    [ is_greater_than__when_actual_greater__then_passed;
      is_greater_than__when_actual_equal__then_failed;
      is_greater_than__when_actual_less__then_failed ]

(* not_is_greater_than_Test_list *)

let not_is_greater_than__when_actual_greater__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value1 = 12.6 and value2 = 345.77 in
    (* when *)
    let exec () = assert_that value2 @@ Satisfies.not @@ Is.Float.greater_than value1 in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s not to be greater than %s"
           (Float.to_string value2)
           (Float.to_string value1) )
    in
    assert_that exec @@ Is.raising expected

let not_is_greater_than__when_actual_equal__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = 12.6 in
    (* when *)
    let exec () = assert_that value @@ Satisfies.not @@ Is.Float.greater_than value in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_greater_than__when_actual_less__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value1 = 12.6 and value2 = 345.77 in
    (* when *)
    let exec () = assert_that value1 @@ Satisfies.not @@ Is.Float.greater_than value2 in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_greater_than_Test_list =
  test_list
    [ not_is_greater_than__when_actual_greater__then_failed;
      not_is_greater_than__when_actual_equal__then_passed;
      not_is_greater_than__when_actual_less__then_passed ]

(* is_greater_than_or_equal_to_Test_list *)

let is_greater_than_or_equal_to__when_actual_greater__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value1 = 12.6 and value2 = 345.77 in
    (* when *)
    let exec () = assert_that value2 @@ Is.Float.greater_than_or_equal_to value1 in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let is_greater_than_or_equal_to__when_actual_equal__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = 12.6 in
    (* when *)
    let exec () = assert_that value @@ Is.Float.greater_than_or_equal_to value in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let is_greater_than_or_equal_to__when_actual_less__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value1 = 12.6 and value2 = 345.77 in
    (* when *)
    let exec () = assert_that value1 @@ Is.Float.greater_than_or_equal_to value2 in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s to be greater than or equal to %s"
           (Float.to_string value1)
           (Float.to_string value2) )
    in
    assert_that exec @@ Is.raising expected

let is_greater_than_or_equal_to_Test_list =
  test_list
    [ is_greater_than_or_equal_to__when_actual_greater__then_passed;
      is_greater_than_or_equal_to__when_actual_equal__then_passed;
      is_greater_than_or_equal_to__when_actual_less__then_failed ]

(* not_is_greater_than_or_equal_to_Test_list *)

let not_is_greater_than_or_equal_to__when_actual_greater__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value1 = 12.6 and value2 = 345.77 in
    (* when *)
    let exec () = assert_that value2 @@ Satisfies.not @@ Is.Float.greater_than_or_equal_to value1 in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s not to be greater than or equal to %s"
           (Float.to_string value2)
           (Float.to_string value1) )
    in
    assert_that exec @@ Is.raising expected

let not_is_greater_than_or_equal_to__when_actual_equal__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = 12.6 in
    (* when *)
    let exec () = assert_that value @@ Satisfies.not @@ Is.Float.greater_than_or_equal_to value in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s not to be greater than or equal to %s"
           (Float.to_string value)
           (Float.to_string value) )
    in
    assert_that exec @@ Is.raising expected

let not_is_greater_than_or_equal_to__when_actual_less__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value1 = 12.6 and value2 = 345.77 in
    (* when *)
    let exec () = assert_that value1 @@ Satisfies.not @@ Is.Float.greater_than_or_equal_to value2 in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_greater_than_or_equal_to_Test_list =
  test_list
    [ not_is_greater_than_or_equal_to__when_actual_greater__then_failed;
      not_is_greater_than_or_equal_to__when_actual_equal__then_failed;
      not_is_greater_than_or_equal_to__when_actual_less__then_passed ]

(* is_less_than_Test_list *)

let is_less_than__when_actual_less__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value1 = 12.6 and value2 = 345.77 in
    (* when *)
    let exec () = assert_that value1 @@ Is.Float.less_than value2 in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let is_less_than__when_actual_equal__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = 12.6 in
    (* when *)
    let exec () = assert_that value @@ Is.Float.less_than value in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s to be less than %s"
           (Float.to_string value)
           (Float.to_string value) )
    in
    assert_that exec @@ Is.raising expected

let is_less_than__when_actual_greater__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value1 = 12.6 and value2 = 345.77 in
    (* when *)
    let exec () = assert_that value2 @@ Is.Float.less_than value1 in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s to be less than %s"
           (Float.to_string value2)
           (Float.to_string value1) )
    in
    assert_that exec @@ Is.raising expected

let is_less_than_Test_list =
  test_list
    [ is_less_than__when_actual_less__then_passed;
      is_less_than__when_actual_equal__then_failed;
      is_less_than__when_actual_greater__then_failed ]

(* not_is_less_than_Test_list *)

let not_is_less_than__when_actual_less__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value1 = 12.6 and value2 = 345.77 in
    (* when *)
    let exec () = assert_that value1 @@ Satisfies.not @@ Is.Float.less_than value2 in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s not to be less than %s"
           (Float.to_string value1)
           (Float.to_string value2) )
    in
    assert_that exec @@ Is.raising expected

let not_is_less_than__when_actual_equal__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = 12.6 in
    (* when *)
    let exec () = assert_that value @@ Satisfies.not @@ Is.Float.less_than value in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_less_than__when_actual_greater__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value1 = 12.6 and value2 = 345.77 in
    (* when *)
    let exec () = assert_that value2 @@ Satisfies.not @@ Is.Float.less_than value1 in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_less_than_Test_list =
  test_list
    [ not_is_less_than__when_actual_less__then_failed;
      not_is_less_than__when_actual_equal__then_passed;
      not_is_less_than__when_actual_greater__then_passed ]

(* is_less_than_or_equal_to_Test_list *)

let is_less_than_or_equal_to__when_actual_less__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value1 = 12.6 and value2 = 345.77 in
    (* when *)
    let exec () = assert_that value1 @@ Is.Float.less_than_or_equal_to value2 in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let is_less_than_or_equal_to__when_actual_equal__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = 12.6 in
    (* when *)
    let exec () = assert_that value @@ Is.Float.less_than_or_equal_to value in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let is_less_than_or_equal_to__when_actual_less__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value1 = 12.6 and value2 = 345.77 in
    (* when *)
    let exec () = assert_that value2 @@ Is.Float.less_than_or_equal_to value1 in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s to be less than or equal to %s"
           (Float.to_string value2)
           (Float.to_string value1) )
    in
    assert_that exec @@ Is.raising expected

let is_less_than_or_equal_to_Test_list =
  test_list
    [ is_less_than_or_equal_to__when_actual_less__then_passed;
      is_less_than_or_equal_to__when_actual_equal__then_passed;
      is_less_than_or_equal_to__when_actual_less__then_failed ]

(* not_is_less_than_or_equal_to_Test_list *)

let not_is_less_than_or_equal_to__when_actual_less__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value1 = 12.6 and value2 = 345.77 in
    (* when *)
    let exec () = assert_that value1 @@ Satisfies.not @@ Is.Float.less_than_or_equal_to value2 in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s not to be less than or equal to %s"
           (Float.to_string value1)
           (Float.to_string value2) )
    in
    assert_that exec @@ Is.raising expected

let not_is_less_than_or_equal_to__when_actual_equal__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = 12.6 in
    (* when *)
    let exec () = assert_that value @@ Satisfies.not @@ Is.Float.less_than_or_equal_to value in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s not to be less than or equal to %s"
           (Float.to_string value)
           (Float.to_string value) )
    in
    assert_that exec @@ Is.raising expected

let not_is_less_than_or_equal_to__when_actual_greater__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value1 = 12.6 and value2 = 345.77 in
    (* when *)
    let exec () = assert_that value2 @@ Satisfies.not @@ Is.Float.less_than_or_equal_to value1 in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_less_than_or_equal_to_Test_list =
  test_list
    [ not_is_less_than_or_equal_to__when_actual_less__then_failed;
      not_is_less_than_or_equal_to__when_actual_equal__then_failed;
      not_is_less_than_or_equal_to__when_actual_greater__then_passed ]

(* float_Test *)

let float_Test =
  __MODULE__
  >::: [ is_nan_Test_list;
         not_is_nan_Test_list;
         is_zero_Test_list;
         not_is_zero_Test_list;
         is_positive_Test_list;
         not_is_positive_Test_list;
         is_negative_Test_list;
         not_is_negative_Test_list;
         is_equal_to_Test_list;
         not_is_equal_to_Test_list;
         is_close_to_Test_list;
         not_is_close_to_Test_list;
         not_is_equal_to_Test_list;
         is_greater_than_Test_list;
         not_is_greater_than_Test_list;
         is_greater_than_or_equal_to_Test_list;
         not_is_greater_than_or_equal_to_Test_list;
         is_less_than_Test_list;
         not_is_less_than_Test_list;
         is_less_than_or_equal_to_Test_list;
         not_is_less_than_or_equal_to_Test_list ]

let _ = run_test_tt_main float_Test
