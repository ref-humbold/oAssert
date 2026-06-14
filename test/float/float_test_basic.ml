(* Tests: Float assertions. *)
open OUnit2
open OAssert
open Float_params

(* is_nan_Test_list *)

let is_nan__when_actual_is_nan__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that Float.nan Is.Float.nan in
    (* then *)
    assert_that action Is.raising_nothing

let is_nan__when_actual_different_than_nan__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %F" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param Is.Float.nan in
      (* then *)
      let expected = Assertion_failed (Printf.sprintf "Expected NaN, but was %F" param) in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_positive

let is_nan_Test_list =
  test_list
    [is_nan__when_actual_is_nan__then_passed; is_nan__when_actual_different_than_nan__then_failed]

(* not_is_nan_Test_list *)

let not_is_nan__when_actual_is_nan__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that Float.nan @@ Satisfies.not Is.Float.nan in
    (* then *)
    let expected = Assertion_failed "Expected value other than NaN" in
    assert_that action @@ Is.raising expected

let not_is_nan__when_actual_different_than_nan__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %F" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Satisfies.not Is.Float.nan in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_positive

let not_is_nan_Test_list =
  test_list
    [ not_is_nan__when_actual_is_nan__then_failed;
      not_is_nan__when_actual_different_than_nan__then_passed ]

(* is_zero_Test_list *)

let is_zero__when_actual_is_zero__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that 0.0 Is.Float.zero in
    (* then *)
    assert_that action Is.raising_nothing

let is_zero__when_actual_different_than_zero__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %F" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param Is.Float.zero in
      (* then *)
      let expected = Assertion_failed (Printf.sprintf "Expected zero, but was %F" param) in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param (params_positive @ params_negative)

let is_zero_Test_list =
  test_list
    [ is_zero__when_actual_is_zero__then_passed;
      is_zero__when_actual_different_than_zero__then_failed ]

(* not_is_zero_Test_list *)

let not_is_zero__when_actual_is_zero__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that 0.0 @@ Satisfies.not Is.Float.zero in
    (* then *)
    let expected = Assertion_failed "Expected value other than zero" in
    assert_that action @@ Is.raising expected

let not_is_zero__when_actual_different_than_zero__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %F" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Satisfies.not Is.Float.zero in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param (params_positive @ params_negative)

let not_is_zero_Test_list =
  test_list
    [ not_is_zero__when_actual_is_zero__then_failed;
      not_is_zero__when_actual_different_than_zero__then_passed ]

(* is_positive_Test_list *)

let is_positive__when_actual_greater_than_zero__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %F" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param Is.Float.positive in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_positive

let is_positive__when_actual_less_than_zero__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %F" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param Is.Float.positive in
      (* then *)
      let expected = Assertion_failed (Printf.sprintf "Expected %F to be positive" param) in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_negative

let is_positive__when_actual_equal_to_zero__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = 0.0 in
    (* when *)
    let action () = assert_that value Is.Float.positive in
    (* then *)
    let expected = Assertion_failed (Printf.sprintf "Expected %F to be positive" value) in
    assert_that action @@ Is.raising expected

let is_positive_Test_list =
  test_list
    [ is_positive__when_actual_greater_than_zero__then_passed;
      is_positive__when_actual_less_than_zero__then_failed;
      is_positive__when_actual_equal_to_zero__then_failed ]

(* not_is_positive_Test_list *)

let not_is_positive__when_actual_greater_than_zero__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %F" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Satisfies.not Is.Float.positive in
      (* then *)
      let expected = Assertion_failed (Printf.sprintf "Expected %F not to be positive" param) in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_positive

let not_is_positive__when_actual_less_than_zero__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %F" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Satisfies.not Is.Float.positive in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_negative

let not_is_positive__when_actual_equal_to_zero__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    (* when *)
    let action () = assert_that 0.0 @@ Satisfies.not Is.Float.positive in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_positive_Test_list =
  test_list
    [ not_is_positive__when_actual_greater_than_zero__then_failed;
      not_is_positive__when_actual_less_than_zero__then_passed;
      not_is_positive__when_actual_equal_to_zero__then_passed ]

(* is_negative_Test_list *)

let is_negative__when_actual_less_than_zero__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %F" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param Is.Float.negative in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_negative

let is_negative__when_actual_greater_than_zero__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %F" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param Is.Float.negative in
      (* then *)
      let expected = Assertion_failed (Printf.sprintf "Expected %F to be negative" param) in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_positive

let is_negative__when_actual_equal_to_zero__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = 0.0 in
    (* when *)
    let action () = assert_that value Is.Float.negative in
    (* then *)
    let expected = Assertion_failed (Printf.sprintf "Expected %F to be negative" value) in
    assert_that action @@ Is.raising expected

let is_negative_Test_list =
  test_list
    [ is_negative__when_actual_less_than_zero__then_passed;
      is_negative__when_actual_greater_than_zero__then_failed;
      is_negative__when_actual_equal_to_zero__then_failed ]

(* not_is_negative_Test_list *)

let not_is_negative__when_actual_less_than_zero__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %F" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Satisfies.not Is.Float.negative in
      (* then *)
      let expected = Assertion_failed (Printf.sprintf "Expected %F not to be negative" param) in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_negative

let not_is_negative__when_actual_greater_than_zero__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %F" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Satisfies.not Is.Float.negative in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_positive

let not_is_negative__when_actual_equal_to_zero__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    (* when *)
    let action () = assert_that 0.0 @@ Satisfies.not Is.Float.negative in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_negative_Test_list =
  test_list
    [ not_is_negative__when_actual_less_than_zero__then_failed;
      not_is_negative__when_actual_greater_than_zero__then_passed;
      not_is_negative__when_actual_equal_to_zero__then_passed ]

let float_test_basic =
  test_list
    [ is_nan_Test_list;
      not_is_nan_Test_list;
      is_zero_Test_list;
      not_is_zero_Test_list;
      is_positive_Test_list;
      not_is_positive_Test_list;
      is_negative_Test_list;
      not_is_negative_Test_list ]
