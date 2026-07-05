(* Tests: Char assertions - comparison. *)
open OUnit2
open OAssert
open Char_params

(* is_equal_to_Test_list *)

let is_equal_to__when_actual_same__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %C" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Is.Char.equal_to param in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_all

let is_equal_to__when_actual_different__then_failed =
  let with_param (param1, param2) =
    let label = Printf.sprintf "%s %C %C" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param1 @@ Is.Char.equal_to param2 in
      (* then *)
      let expected =
        Assertion_failed (Printf.sprintf "Expected %C to be equal to %C" param1 param2)
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_char_pairs

let is_equal_to_Test_list =
  test_list
    [is_equal_to__when_actual_same__then_passed; is_equal_to__when_actual_different__then_failed]

(* not_is_equal_to_Test_list *)

let not_is_equal_to__when_actual_same__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %C" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Satisfies.not @@ Is.Char.equal_to param in
      (* then *)
      let expected =
        Assertion_failed (Printf.sprintf "Expected %C not to be equal to %C" param param)
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_all

let not_is_equal_to__when_actual_different__then_passed =
  let with_param (param1, param2) =
    let label = Printf.sprintf "%s %C vs %C" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param1 @@ Satisfies.not @@ Is.Char.equal_to param2 in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_char_pairs

let not_is_equal_to_Test_list =
  test_list
    [ not_is_equal_to__when_actual_same__then_failed;
      not_is_equal_to__when_actual_different__then_passed ]

(* is_greater_than_Test_list *)

let is_greater_than__when_actual_greater__then_passed =
  let with_param (param1, param2) =
    let label = Printf.sprintf "%s %C %C" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param2 @@ Is.Char.greater_than param1 in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_char_pairs

let is_greater_than__when_actual_equal__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %C" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Is.Char.greater_than param in
      (* then *)
      let expected =
        Assertion_failed (Printf.sprintf "Expected %C to be greater than %C" param param)
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_all

let is_greater_than__when_actual_less__then_failed =
  let with_param (param1, param2) =
    let label = Printf.sprintf "%s %C %C" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param1 @@ Is.Char.greater_than param2 in
      (* then *)
      let expected =
        Assertion_failed (Printf.sprintf "Expected %C to be greater than %C" param1 param2)
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_char_pairs

let is_greater_than_Test_list =
  test_list
    [ is_greater_than__when_actual_greater__then_passed;
      is_greater_than__when_actual_equal__then_failed;
      is_greater_than__when_actual_less__then_failed ]

(* is_less_than_Test_list *)

let is_less_than__when_actual_less__then_passed =
  let with_param (param1, param2) =
    let label = Printf.sprintf "%s %C %C" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param1 @@ Is.Char.less_than param2 in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_char_pairs

let is_less_than__when_actual_equal__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %C" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Is.Char.less_than param in
      (* then *)
      let expected = Assertion_failed (Printf.sprintf "Expected %C to be less than %C" param param) in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_all

let is_less_than__when_actual_greater__then_failed =
  let with_param (param1, param2) =
    let label = Printf.sprintf "%s %C %C" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param2 @@ Is.Char.less_than param1 in
      (* then *)
      let expected =
        Assertion_failed (Printf.sprintf "Expected %C to be less than %C" param2 param1)
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_char_pairs

let is_less_than_Test_list =
  test_list
    [ is_less_than__when_actual_less__then_passed;
      is_less_than__when_actual_equal__then_failed;
      is_less_than__when_actual_greater__then_failed ]

(* is_greater_than_or_equal_to_Test_list *)

let is_greater_than_or_equal_to__when_actual_greater__then_passed =
  let with_param (param1, param2) =
    let label = Printf.sprintf "%s %C %C" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param2 @@ Is.Char.greater_than_or_equal_to param1 in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_char_pairs

let is_greater_than_or_equal_to__when_actual_equal__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %C" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Is.Char.greater_than_or_equal_to param in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_all

let is_greater_than_or_equal_to__when_actual_less__then_failed =
  let with_param (param1, param2) =
    let label = Printf.sprintf "%s %C %C" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param1 @@ Is.Char.greater_than_or_equal_to param2 in
      (* then *)
      let expected =
        Assertion_failed
          (Printf.sprintf "Expected %C to be greater than or equal to %C" param1 param2)
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_char_pairs

let is_greater_than_or_equal_to_Test_list =
  test_list
    [ is_greater_than_or_equal_to__when_actual_greater__then_passed;
      is_greater_than_or_equal_to__when_actual_equal__then_passed;
      is_greater_than_or_equal_to__when_actual_less__then_failed ]

(* is_less_than_or_equal_to_Test_list *)

let is_less_than_or_equal_to__when_actual_less__then_passed =
  let with_param (param1, param2) =
    let label = Printf.sprintf "%s %C %C" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param1 @@ Is.Char.less_than_or_equal_to param2 in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_char_pairs

let is_less_than_or_equal_to__when_actual_equal__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %C" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Is.Char.less_than_or_equal_to param in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_all

let is_less_than_or_equal_to__when_actual_greater__then_failed =
  let with_param (param1, param2) =
    let label = Printf.sprintf "%s %C %C" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param2 @@ Is.Char.less_than_or_equal_to param1 in
      (* then *)
      let expected =
        Assertion_failed (Printf.sprintf "Expected %C to be less than or equal to %C" param2 param1)
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_char_pairs

let is_less_than_or_equal_to_Test_list =
  test_list
    [ is_less_than_or_equal_to__when_actual_less__then_passed;
      is_less_than_or_equal_to__when_actual_equal__then_passed;
      is_less_than_or_equal_to__when_actual_greater__then_failed ]

(* char_test_comparison *)
let char_test_comparison =
  __MODULE__
  >::: [ is_equal_to_Test_list;
         not_is_equal_to_Test_list;
         is_greater_than_Test_list;
         is_less_than_Test_list;
         is_greater_than_or_equal_to_Test_list;
         is_less_than_or_equal_to_Test_list ]
