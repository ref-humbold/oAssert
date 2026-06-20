(* Tests: List assertions - matching. *)
open OUnit2
open OAssert
module ListVal = Values.List.Of (Values.Int)
module IsList = Is.List.Of (Values.Int)

(* is_all_matching_Test_list *)

let is_all_matching__when_all_elements_matched__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that [1; 2; 3; 4; 5; 6; 7; 8; 9] @@ IsList.all_matching (fun e -> e > 0) in
    (* then *)
    assert_that action Is.raising_nothing

let is_all_matching__when_some_elements_not_matched__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [1; 2; 3; 4; 5; 6; 7; 8; 9] in
    (* when *)
    let action () = assert_that value @@ IsList.all_matching (fun e -> e mod 2 = 0) in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s to have all elements matching given predicate, but %s did not match"
           (ListVal.to_string value)
           (ListVal.to_string [1; 3; 5; 7; 9]) )
    in
    assert_that action @@ Is.raising expected

let is_all_matching_Test_list =
  test_list
    [ is_all_matching__when_all_elements_matched__then_passed;
      is_all_matching__when_some_elements_not_matched__then_failed ]

(* not_is_all_matching_Test_list *)

let not_is_all_matching__when_all_elements_matched__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [1; 2; 3; 4; 5; 6; 7; 8; 9] in
    (* when *)
    let action () = assert_that value @@ Satisfies.not @@ IsList.all_matching (fun e -> e > 0) in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s not to have all elements matching given predicate"
           (ListVal.to_string value) )
    in
    assert_that action @@ Is.raising expected

let not_is_all_matching__when_some_elements_not_matched__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () =
      assert_that [1; 2; 3; 4; 5; 6; 7; 8; 9]
      @@ Satisfies.not
      @@ IsList.all_matching (fun e -> e mod 2 = 0)
    in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_all_matching_Test_list =
  test_list
    [ not_is_all_matching__when_all_elements_matched__then_failed;
      not_is_all_matching__when_some_elements_not_matched__then_passed ]

(* is_any_matching_Test_list *)

let is_any_matching__when_some_elements_matched__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () =
      assert_that [1; 2; 3; 4; 5; 6; 7; 8; 9] @@ IsList.any_matching (fun e -> e mod 2 = 0)
    in
    (* then *)
    assert_that action Is.raising_nothing

let is_any_matching__when_all_elements_not_matched__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [1; 2; 3; 4; 5; 6; 7; 8; 9] in
    (* when *)
    let action () = assert_that value @@ IsList.any_matching (fun e -> e < 0) in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s to have any element matching given predicate, but none matched"
           (ListVal.to_string value) )
    in
    assert_that action @@ Is.raising expected

let is_any_matching_Test_list =
  test_list
    [ is_any_matching__when_some_elements_matched__then_passed;
      is_any_matching__when_all_elements_not_matched__then_failed ]

(* not_is_any_matching_Test_list *)

let not_is_any_matching__when_some_elements_matched__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [1; 2; 3; 4; 5; 6; 7; 8; 9] in
    (* when *)
    let action () =
      assert_that value @@ Satisfies.not @@ IsList.any_matching (fun e -> e mod 2 = 0)
    in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s not to have any element matching given predicate"
           (ListVal.to_string value) )
    in
    assert_that action @@ Is.raising expected

let not_is_any_matching__when_all_elements_not_matched__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () =
      assert_that [1; 2; 3; 4; 5; 6; 7; 8; 9] @@ Satisfies.not @@ IsList.any_matching (fun e -> e < 0)
    in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_any_matching_Test_list =
  test_list
    [ not_is_any_matching__when_some_elements_matched__then_failed;
      not_is_any_matching__when_all_elements_not_matched__then_passed ]

(* list_test_matching *)

let list_test_matching =
  __MODULE__
  >::: [ is_all_matching_Test_list;
         not_is_all_matching_Test_list;
         is_any_matching_Test_list;
         not_is_any_matching_Test_list ]
