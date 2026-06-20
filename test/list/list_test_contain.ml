(* Tests: List assertions - containing. *)
open OUnit2
open OAssert
module ListVal = Values.List.Of (Values.Int)
module IsList = Is.List.Of (Values.Int)

(* is_containing_Test_list *)

let is_containing__when_element_present__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that [1; 2; 3; 4] @@ IsList.containing 2 in
    (* then *)
    assert_that action Is.raising_nothing

let is_containing__when_element_absent__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [1; 2; 3; 4] and element = 10 in
    (* when *)
    let action () = assert_that value @@ IsList.containing element in
    (* then *)
    let expected =
      Assertion_failed (Printf.sprintf "Expected %s to contain %d" (ListVal.to_string value) element)
    in
    assert_that action @@ Is.raising expected

let is_containing_Test_list =
  test_list
    [ is_containing__when_element_present__then_passed;
      is_containing__when_element_absent__then_failed ]

(* not_is_containing_Test_list *)

let not_is_containing__when_element_present__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [1; 2; 3; 4] and element = 2 in
    (* when *)
    let action () = assert_that value @@ Satisfies.not @@ IsList.containing element in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf "Expected %s not to contain %d" (ListVal.to_string value) element)
    in
    assert_that action @@ Is.raising expected

let not_is_containing__when_element_absent__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that [1; 2; 3; 4] @@ Satisfies.not @@ IsList.containing 10 in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_containing_Test_list =
  test_list
    [ not_is_containing__when_element_present__then_failed;
      not_is_containing__when_element_absent__then_passed ]

(* is_containing_all_Test_list *)

let is_containing_all__when_all_elements_present__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that [1; 2; 3; 4; 5; 6; 7; 8; 9] @@ IsList.containing_all [2; 4; 6; 8] in
    (* then *)
    assert_that action Is.raising_nothing

let is_containing_all__when_some_elements_absent__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [1; 2; 3; 4; 5; 6; 7; 8; 9] and elements = [4; 12; 8; 20] in
    (* when *)
    let action () = assert_that value @@ IsList.containing_all elements in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s to contain all values of %s, but %s are missing"
           (ListVal.to_string value)
           (ListVal.to_string elements)
           (ListVal.to_string [12; 20]) )
    in
    assert_that action @@ Is.raising expected

let is_containing_all_Test_list =
  test_list
    [ is_containing_all__when_all_elements_present__then_passed;
      is_containing_all__when_some_elements_absent__then_failed ]

(* not_is_containing_all_Test_list *)

let not_is_containing_all__when_all_elements_present__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [1; 2; 3; 4; 5; 6; 7; 8; 9] and elements = [2; 4; 6; 8] in
    (* when *)
    let action () = assert_that value @@ Satisfies.not @@ IsList.containing_all elements in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s not to contain all values of %s"
           (ListVal.to_string value)
           (ListVal.to_string elements) )
    in
    assert_that action @@ Is.raising expected

let not_is_containing_all__when_some_elements_absent__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () =
      assert_that [1; 2; 3; 4; 5; 6; 7; 8; 9] @@ Satisfies.not @@ IsList.containing_all [4; 12; 8; 20]
    in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_containing_all_Test_list =
  test_list
    [ not_is_containing_all__when_all_elements_present__then_failed;
      not_is_containing_all__when_some_elements_absent__then_passed ]

(* is_containing_any_Test_list *)

let is_containing_any__when_some_elements_present__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that [1; 2; 3; 4; 5; 6; 7; 8; 9] @@ IsList.containing_any [4; 12; 8; 20] in
    (* then *)
    assert_that action Is.raising_nothing

let is_containing_any__when_all_elements_absent__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [1; 2; 3; 4; 5; 6; 7; 8; 9] and elements = [12; 14; 16; 18] in
    (* when *)
    let action () = assert_that value @@ IsList.containing_any elements in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s to contain any value of %s, but none was found"
           (ListVal.to_string value)
           (ListVal.to_string elements) )
    in
    assert_that action @@ Is.raising expected

let is_containing_any_Test_list =
  test_list
    [ is_containing_any__when_some_elements_present__then_passed;
      is_containing_any__when_all_elements_absent__then_failed ]

(* not_is_containing_any_Test_list *)

let not_is_containing_any__when_some_elements_present__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [1; 2; 3; 4; 5; 6; 7; 8; 9] and elements = [4; 12; 8; 20] in
    (* when *)
    let action () = assert_that value @@ Satisfies.not @@ IsList.containing_any elements in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s not to contain any value of %s"
           (ListVal.to_string value)
           (ListVal.to_string elements) )
    in
    assert_that action @@ Is.raising expected

let not_is_containing_any__when_all_elements_absent__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () =
      assert_that [1; 2; 3; 4; 5; 6; 7; 8; 9]
      @@ Satisfies.not
      @@ IsList.containing_any [12; 14; 16; 18]
    in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_containing_any_Test_list =
  test_list
    [ not_is_containing_any__when_some_elements_present__then_failed;
      not_is_containing_any__when_all_elements_absent__then_passed ]

(* list_test_contain *)

let list_test_contain =
  __MODULE__
  >::: [ is_containing_Test_list;
         not_is_containing_Test_list;
         is_containing_all_Test_list;
         not_is_containing_all_Test_list;
         is_containing_any_Test_list;
         not_is_containing_any_Test_list ]
