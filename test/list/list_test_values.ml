(* Tests: List assertions - values. *)
open OUnit2
open OAssert
module ListVal = Values.List.Of (Values.Int)
module IsList = Is.List.Of (Values.Int)

(* is_empty_Test_list *)

let is_empty__when_actual_empty__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that [] IsList.empty in
    (* then *)
    assert_that action Is.raising_nothing

let is_empty__when_actual_not_empty__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [1; 2; 3; 4] in
    (* when *)
    let action () = assert_that value IsList.empty in
    (* then *)
    let expected =
      Assertion_failed (Printf.sprintf "Expected empty list, but was %s" (ListVal.to_string value))
    in
    assert_that action @@ Is.raising expected

let is_empty_Test_list =
  test_list [is_empty__when_actual_empty__then_passed; is_empty__when_actual_not_empty__then_failed]

(* not_is_empty_Test_list *)

let not_is_empty__when_actual_empty__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that [] @@ Satisfies.not IsList.empty in
    (* then *)
    let expected = Assertion_failed "Expected non-empty list" in
    assert_that action @@ Is.raising expected

let not_is_empty__when_actual_not_empty__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that [1; 2; 3; 4] @@ Satisfies.not IsList.empty in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_empty_Test_list =
  test_list
    [not_is_empty__when_actual_empty__then_failed; not_is_empty__when_actual_not_empty__then_passed]

(* is_equal_to_Test_list *)

let is_equal_to__when_same_elements__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [1; 2; 3; 4] in
    (* when *)
    let action () = assert_that value @@ IsList.equal_to value in
    (* then *)
    assert_that action Is.raising_nothing

let is_equal_to__when_different_elements__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [1; 2; 3; 4] and value' = [1; 3; 5; 7] in
    (* when *)
    let action () = assert_that value @@ IsList.equal_to value' in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s to be equal to %s"
           (ListVal.to_string value)
           (ListVal.to_string value') )
    in
    assert_that action @@ Is.raising expected

let is_equal_to__when_actual_longer__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [1; 2; 3; 4] and value' = [1; 2; 3] in
    (* when *)
    let action () = assert_that value @@ IsList.equal_to value' in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s to be equal to %s"
           (ListVal.to_string value)
           (ListVal.to_string value') )
    in
    assert_that action @@ Is.raising expected

let is_equal_to__when_actual_shorter__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [1; 2; 3; 4] and value' = [1; 2; 3; 4; 5; 6; 7] in
    (* when *)
    let action () = assert_that value @@ IsList.equal_to value' in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s to be equal to %s"
           (ListVal.to_string value)
           (ListVal.to_string value') )
    in
    assert_that action @@ Is.raising expected

let is_equal_to_Test_list =
  test_list
    [ is_equal_to__when_same_elements__then_passed;
      is_equal_to__when_different_elements__then_failed;
      is_equal_to__when_actual_longer__then_failed;
      is_equal_to__when_actual_shorter__then_failed ]

(* not_is_equal_to_Test_list *)

let not_is_equal_to__when_same_elements__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [1; 2; 3; 4] in
    (* when *)
    let action () = assert_that value @@ Satisfies.not @@ IsList.equal_to value in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s not to be equal to %s"
           (ListVal.to_string value)
           (ListVal.to_string value) )
    in
    assert_that action @@ Is.raising expected

let not_is_equal_to__when_different_elements__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that [1; 2; 3; 4] @@ Satisfies.not @@ IsList.equal_to [1; 3; 5; 7] in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_equal_to__when_actual_longer__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that [1; 2; 3; 4] @@ Satisfies.not @@ IsList.equal_to [1; 2; 3] in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_equal_to__when_actual_shorter__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () =
      assert_that [1; 2; 3; 4] @@ Satisfies.not @@ IsList.equal_to [1; 2; 3; 4; 5; 6; 7]
    in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_equal_to_Test_list =
  test_list
    [ not_is_equal_to__when_same_elements__then_failed;
      not_is_equal_to__when_different_elements__then_passed;
      not_is_equal_to__when_actual_longer__then_passed;
      not_is_equal_to__when_actual_shorter__then_passed ]

(* list_test_values *)
let list_test_values =
  __MODULE__
  >::: [is_empty_Test_list; not_is_empty_Test_list; is_equal_to_Test_list; not_is_equal_to_Test_list]
