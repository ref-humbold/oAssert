(* Tests: List length assertions. *)
open OUnit2
open OAssert
module ListVal = Values.List.Of (Values.Int)
module IsList = Is.List.Of (Values.Int)

(* is_length_equal_to_Test_list *)

let is_length_equal_to__when_actual_has_specified_length__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that [1; 2; 3; 4] @@ IsList.Length.equal_to 4 in
    (* then *)
    assert_that action Is.raising_nothing

let is_length_equal_to__when_actual_shorter__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [1; 2; 3; 4] and length = 10 in
    (* when *)
    let action () = assert_that value @@ IsList.Length.equal_to length in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s to have length %d, but was %d"
           (ListVal.to_string value)
           length
           (List.length value) )
    in
    assert_that action @@ Is.raising expected

let is_length_equal_to__when_actual_longer__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [1; 2; 3; 4] and length = 3 in
    (* when *)
    let action () = assert_that value @@ IsList.Length.equal_to length in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s to have length %d, but was %d"
           (ListVal.to_string value)
           length
           (List.length value) )
    in
    assert_that action @@ Is.raising expected

let is_length_equal_to_Test_list =
  test_list
    [ is_length_equal_to__when_actual_has_specified_length__then_passed;
      is_length_equal_to__when_actual_shorter__then_failed;
      is_length_equal_to__when_actual_longer__then_failed ]

(* not_is_length_equal_to_Test_list *)

let not_is_length_equal_to__when_actual_has_specified_length__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [1; 2; 3; 4] and length = 4 in
    (* when *)
    let action () = assert_that value @@ Satisfies.not @@ IsList.Length.equal_to length in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf "Expected %s not to have length %d" (ListVal.to_string value) length)
    in
    assert_that action @@ Is.raising expected

let not_is_length_equal_to__when_actual_shorter__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that [1; 2; 3; 4] @@ Satisfies.not @@ IsList.Length.equal_to 10 in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_length_equal_to__when_actual_longer__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that [1; 2; 3; 4] @@ Satisfies.not @@ IsList.Length.equal_to 3 in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_length_equal_to_Test_list =
  test_list
    [ not_is_length_equal_to__when_actual_has_specified_length__then_failed;
      not_is_length_equal_to__when_actual_shorter__then_passed;
      not_is_length_equal_to__when_actual_longer__then_passed ]

(* list_length_Test *)
let list_length_Test =
  __MODULE__ >::: [is_length_equal_to_Test_list; not_is_length_equal_to_Test_list]

let _ = run_test_tt_main list_length_Test
