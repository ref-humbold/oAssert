(* Tests: Int assertions. *)
open OUnit2
open OAssert
open Int_params

(* is_zero_Test_list *)

let is_zero__when_actual_is_zero__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that 0 Is.Int.zero in
    (* then *)
    assert_that action Is.raising_nothing

let is_zero__when_actual_different_than_zero__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %d" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param Is.Int.zero in
      (* then *)
      let expected = Assertion_failed (Printf.sprintf "Expected zero, but was %d" param) in
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
    let action () = assert_that 0 @@ Satisfies.not Is.Int.zero in
    (* then *)
    let expected = Assertion_failed "Expected value other than zero" in
    assert_that action @@ Is.raising expected

let not_is_zero__when_actual_different_than_zero__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %d" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Satisfies.not Is.Int.zero in
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
    let label = Printf.sprintf "%s %d" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param Is.Int.positive in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_positive

let is_positive__when_actual_less_than_zero__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %d" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param Is.Int.positive in
      (* then *)
      let expected = Assertion_failed (Printf.sprintf "Expected %d to be positive" param) in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_negative

let is_positive__when_actual_equal_to_zero__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = 0 in
    (* when *)
    let action () = assert_that value Is.Int.positive in
    (* then *)
    let expected = Assertion_failed (Printf.sprintf "Expected %d to be positive" value) in
    assert_that action @@ Is.raising expected

let is_positive_Test_list =
  test_list
    [ is_positive__when_actual_greater_than_zero__then_passed;
      is_positive__when_actual_less_than_zero__then_failed;
      is_positive__when_actual_equal_to_zero__then_failed ]

(* not_is_positive_Test_list *)

let not_is_positive__when_actual_greater_than_zero__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %d" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Satisfies.not Is.Int.positive in
      (* then *)
      let expected = Assertion_failed (Printf.sprintf "Expected %d not to be positive" param) in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_positive

let not_is_positive__when_actual_less_than_zero__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %d" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Satisfies.not Is.Int.positive in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_negative

let not_is_positive__when_actual_equal_to_zero__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    (* when *)
    let action () = assert_that 0 @@ Satisfies.not Is.Int.positive in
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
    let label = Printf.sprintf "%s %d" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param Is.Int.negative in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_negative

let is_negative__when_actual_greater_than_zero__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %d" __FUNCTION__ param in
    label >:: fun _ ->
      (* given *)
      let value = 10 in
      (* when *)
      let action () = assert_that value Is.Int.negative in
      (* then *)
      let expected = Assertion_failed (Printf.sprintf "Expected %d to be negative" value) in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_positive

let is_negative__when_actual_equal_to_zero__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = 0 in
    (* when *)
    let action () = assert_that value Is.Int.negative in
    (* then *)
    let expected = Assertion_failed (Printf.sprintf "Expected %d to be negative" value) in
    assert_that action @@ Is.raising expected

let is_negative_Test_list =
  test_list
    [ is_negative__when_actual_less_than_zero__then_passed;
      is_negative__when_actual_greater_than_zero__then_failed;
      is_negative__when_actual_equal_to_zero__then_failed ]

(* not_is_negative_Test_list *)

let not_is_negative__when_actual_less_than_zero__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %d" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Satisfies.not Is.Int.negative in
      (* then *)
      let expected = Assertion_failed (Printf.sprintf "Expected %d not to be negative" param) in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_negative

let not_is_negative__when_actual_greater_than_zero__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %d" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Satisfies.not Is.Int.negative in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_positive

let not_is_negative__when_actual_equal_to_zero__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    (* when *)
    let action () = assert_that 0 @@ Satisfies.not Is.Int.negative in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_negative_Test_list =
  test_list
    [ not_is_negative__when_actual_less_than_zero__then_failed;
      not_is_negative__when_actual_greater_than_zero__then_passed;
      not_is_negative__when_actual_equal_to_zero__then_passed ]

(* is_equal_to_Test_list *)

let is_equal_to__when_actual_is_same__then_passed =
  let with_param param =
    __FUNCTION__ >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Is.Int.equal_to param in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param (params_positive @ params_negative)

let is_equal_to__when_actual_different__then_failed =
  let with_param (param1, param2) =
    let label = Printf.sprintf "%s %d %d" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param1 @@ Is.Int.equal_to param2 in
      (* then *)
      let expected =
        Assertion_failed (Printf.sprintf "Expected %d to be equal to %d" param1 param2)
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_number_pairs

let is_equal_to_Test_list =
  test_list
    [is_equal_to__when_actual_is_same__then_passed; is_equal_to__when_actual_different__then_failed]

(* not_is_equal_to_Test_list *)

let not_is_equal_to__when_actual_is_same__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %d" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Satisfies.not @@ Is.Int.equal_to param in
      (* then *)
      let expected =
        Assertion_failed (Printf.sprintf "Expected %d not to be equal to %d" param param)
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param (params_positive @ params_negative)

let not_is_equal_to__when_actual_is_different__then_passed =
  let with_param (param1, param2) =
    let label = Printf.sprintf "%s %d %d" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param1 @@ Satisfies.not @@ Is.Int.equal_to param2 in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_number_pairs

let not_is_equal_to_Test_list =
  test_list
    [ not_is_equal_to__when_actual_is_same__then_failed;
      not_is_equal_to__when_actual_is_different__then_passed ]

(* is_greater_than_Test_list *)

let is_greater_than__when_actual_greater__then_passed =
  let with_param (param1, param2) =
    let label = Printf.sprintf "%s %d %d" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param2 @@ Is.Int.greater_than param1 in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_number_pairs

let is_greater_than__when_actual_equal__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %d" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Is.Int.greater_than param in
      (* then *)
      let expected =
        Assertion_failed (Printf.sprintf "Expected %d to be greater than %d" param param)
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_positive

let is_greater_than__when_actual_less__then_failed =
  let with_param (param1, param2) =
    let label = Printf.sprintf "%s %d %d" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param1 @@ Is.Int.greater_than param2 in
      (* then *)
      let expected =
        Assertion_failed (Printf.sprintf "Expected %d to be greater than %d" param1 param2)
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_number_pairs

let is_greater_than_Test_list =
  test_list
    [ is_greater_than__when_actual_greater__then_passed;
      is_greater_than__when_actual_equal__then_failed;
      is_greater_than__when_actual_less__then_failed ]

(* not_is_greater_than_Test_list *)

let not_is_greater_than__when_actual_greater__then_failed =
  let with_param (param1, param2) =
    let label = Printf.sprintf "%s %d %d" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param2 @@ Satisfies.not @@ Is.Int.greater_than param1 in
      (* then *)
      let expected =
        Assertion_failed (Printf.sprintf "Expected %d not to be greater than %d" param2 param1)
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_number_pairs

let not_is_greater_than__when_actual_equal__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %d" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Satisfies.not @@ Is.Int.greater_than param in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_positive

let not_is_greater_than__when_actual_less__then_passed =
  let with_param (param1, param2) =
    let label = Printf.sprintf "%s %d %d" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param1 @@ Satisfies.not @@ Is.Int.greater_than param2 in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_number_pairs

let not_is_greater_than_Test_list =
  test_list
    [ not_is_greater_than__when_actual_greater__then_failed;
      not_is_greater_than__when_actual_equal__then_passed;
      not_is_greater_than__when_actual_less__then_passed ]

(* is_greater_than_or_equal_to_Test_list *)

let is_greater_than_or_equal_to__when_actual_greater__then_passed =
  let with_param (param1, param2) =
    let label = Printf.sprintf "%s %d %d" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param2 @@ Is.Int.greater_than_or_equal_to param1 in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_number_pairs

let is_greater_than_or_equal_to__when_actual_equal__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %d" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Is.Int.greater_than_or_equal_to param in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_positive

let is_greater_than_or_equal_to__when_actual_less__then_failed =
  let with_param (param1, param2) =
    let label = Printf.sprintf "%s %d %d" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param1 @@ Is.Int.greater_than_or_equal_to param2 in
      (* then *)
      let expected =
        Assertion_failed
          (Printf.sprintf "Expected %d to be greater than or equal to %d" param1 param2)
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_number_pairs

let is_greater_than_or_equal_to_Test_list =
  test_list
    [ is_greater_than_or_equal_to__when_actual_greater__then_passed;
      is_greater_than_or_equal_to__when_actual_equal__then_passed;
      is_greater_than_or_equal_to__when_actual_less__then_failed ]

(* not_is_greater_than_or_equal_to_Test_list *)

let not_is_greater_than_or_equal_to__when_actual_greater__then_failed =
  let with_param (param1, param2) =
    let label = Printf.sprintf "%s %d %d" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param2 @@ Satisfies.not @@ Is.Int.greater_than_or_equal_to param1 in
      (* then *)
      let expected =
        Assertion_failed
          (Printf.sprintf "Expected %d not to be greater than or equal to %d" param2 param1)
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_number_pairs

let not_is_greater_than_or_equal_to__when_actual_equal__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %d" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Satisfies.not @@ Is.Int.greater_than_or_equal_to param in
      (* then *)
      let expected =
        Assertion_failed
          (Printf.sprintf "Expected %d not to be greater than or equal to %d" param param)
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_positive

let not_is_greater_than_or_equal_to__when_actual_less__then_passed =
  let with_param (param1, param2) =
    let label = Printf.sprintf "%s %d %d" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param1 @@ Satisfies.not @@ Is.Int.greater_than_or_equal_to param2 in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_number_pairs

let not_is_greater_than_or_equal_to_Test_list =
  test_list
    [ not_is_greater_than_or_equal_to__when_actual_greater__then_failed;
      not_is_greater_than_or_equal_to__when_actual_equal__then_failed;
      not_is_greater_than_or_equal_to__when_actual_less__then_passed ]

(* is_less_than_Test_list *)

let is_less_than__when_actual_less__then_passed =
  let with_param (param1, param2) =
    let label = Printf.sprintf "%s %d %d" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param1 @@ Is.Int.less_than param2 in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_number_pairs

let is_less_than__when_actual_equal__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %d" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Is.Int.less_than param in
      (* then *)
      let expected = Assertion_failed (Printf.sprintf "Expected %d to be less than %d" param param) in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_positive

let is_less_than__when_actual_greater__then_failed =
  let with_param (param1, param2) =
    let label = Printf.sprintf "%s %d %d" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param2 @@ Is.Int.less_than param1 in
      (* then *)
      let expected =
        Assertion_failed (Printf.sprintf "Expected %d to be less than %d" param2 param1)
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_number_pairs

let is_less_than_Test_list =
  test_list
    [ is_less_than__when_actual_less__then_passed;
      is_less_than__when_actual_equal__then_failed;
      is_less_than__when_actual_greater__then_failed ]

(* not_is_less_than_Test_list *)

let not_is_less_than__when_actual_less__then_failed =
  let with_param (param1, param2) =
    let label = Printf.sprintf "%s %d %d" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param1 @@ Satisfies.not @@ Is.Int.less_than param2 in
      (* then *)
      let expected =
        Assertion_failed (Printf.sprintf "Expected %d not to be less than %d" param1 param2)
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_number_pairs

let not_is_less_than__when_actual_equal__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %d" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Satisfies.not @@ Is.Int.less_than param in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_positive

let not_is_less_than__when_actual_greater__then_passed =
  let with_param (param1, param2) =
    let label = Printf.sprintf "%s %d %d" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param2 @@ Satisfies.not @@ Is.Int.less_than param1 in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_number_pairs

let not_is_less_than_Test_list =
  test_list
    [ not_is_less_than__when_actual_less__then_failed;
      not_is_less_than__when_actual_equal__then_passed;
      not_is_less_than__when_actual_greater__then_passed ]

(* is_less_than_or_equal_to_Test_list *)

let is_less_than_or_equal_to__when_actual_less__then_passed =
  let with_param (param1, param2) =
    let label = Printf.sprintf "%s %d %d" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param1 @@ Is.Int.less_than_or_equal_to param2 in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_number_pairs

let is_less_than_or_equal_to__when_actual_equal__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %d" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Is.Int.less_than_or_equal_to param in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_positive

let is_less_than_or_equal_to__when_actual_greater__then_failed =
  let with_param (param1, param2) =
    let label = Printf.sprintf "%s %d %d" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param2 @@ Is.Int.less_than_or_equal_to param1 in
      (* then *)
      let expected =
        Assertion_failed (Printf.sprintf "Expected %d to be less than or equal to %d" param2 param1)
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_number_pairs

let is_less_than_or_equal_to_Test_list =
  test_list
    [ is_less_than_or_equal_to__when_actual_less__then_passed;
      is_less_than_or_equal_to__when_actual_equal__then_passed;
      is_less_than_or_equal_to__when_actual_greater__then_failed ]

(* not_is_less_than_or_equal_to_Test_list *)

let not_is_less_than_or_equal_to__when_actual_less__then_failed =
  let with_param (param1, param2) =
    let label = Printf.sprintf "%s %d %d" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param1 @@ Satisfies.not @@ Is.Int.less_than_or_equal_to param2 in
      (* then *)
      let expected =
        Assertion_failed
          (Printf.sprintf "Expected %d not to be less than or equal to %d" param1 param2)
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_number_pairs

let not_is_less_than_or_equal_to__when_actual_equal__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %d" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Satisfies.not @@ Is.Int.less_than_or_equal_to param in
      (* then *)
      let expected =
        Assertion_failed (Printf.sprintf "Expected %d not to be less than or equal to %d" param param)
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_positive

let not_is_less_than_or_equal_to__when_actual_greater__then_passed =
  let with_param (param1, param2) =
    let label = Printf.sprintf "%s %d %d" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param2 @@ Satisfies.not @@ Is.Int.less_than_or_equal_to param1 in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_number_pairs

let not_is_less_than_or_equal_to_Test_list =
  test_list
    [ not_is_less_than_or_equal_to__when_actual_less__then_failed;
      not_is_less_than_or_equal_to__when_actual_equal__then_failed;
      not_is_less_than_or_equal_to__when_actual_greater__then_passed ]

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
         not_is_equal_to_Test_list;
         is_greater_than_Test_list;
         not_is_greater_than_Test_list;
         is_greater_than_or_equal_to_Test_list;
         not_is_greater_than_or_equal_to_Test_list;
         is_less_than_Test_list;
         not_is_less_than_Test_list;
         is_less_than_or_equal_to_Test_list;
         not_is_less_than_or_equal_to_Test_list ]

let _ = run_test_tt_main int_Test
