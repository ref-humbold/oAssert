(* Tests: Int assertions - comparison. *)
open OUnit2
open OAssert
open Int_params

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

(* int_test_comparison *)

let int_test_comparison =
  __MODULE__
  >::: [ is_equal_to_Test_list;
         not_is_equal_to_Test_list;
         is_greater_than_Test_list;
         not_is_greater_than_Test_list;
         is_greater_than_or_equal_to_Test_list;
         not_is_greater_than_or_equal_to_Test_list;
         is_less_than_Test_list;
         not_is_less_than_Test_list;
         is_less_than_or_equal_to_Test_list;
         not_is_less_than_or_equal_to_Test_list ]
