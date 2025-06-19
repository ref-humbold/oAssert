(* Tests: List assertions. *)
open OUnit2
open OAssert
module ListVal = Values.List.Of (Values.Int)
module IsList = Is.List.Of (Values.Int)

(* is_empty_Test_list *)

let is_empty__when_actual_empty__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that [] IsList.empty in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let is_empty__when_actual_not_empty__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [1; 2; 3; 4] in
    (* when *)
    let exec () = assert_that value IsList.empty in
    (* then *)
    let expected =
      Assertion_failed (Printf.sprintf "Expected empty list, but was %s" (ListVal.to_string value))
    in
    assert_that exec @@ Is.raising expected

let is_empty_Test_list =
  test_list [is_empty__when_actual_empty__then_passed; is_empty__when_actual_not_empty__then_failed]

(* not_is_empty_Test_list *)

let not_is_empty__when_actual_empty__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that [] @@ Satisfies.not IsList.empty in
    (* then *)
    let expected = Assertion_failed "Expected value different than empty list" in
    assert_that exec @@ Is.raising expected

let not_is_empty__when_actual_not_empty__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that [1; 2; 3; 4] @@ Satisfies.not IsList.empty in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_empty_Test_list =
  test_list
    [not_is_empty__when_actual_empty__then_failed; not_is_empty__when_actual_not_empty__then_passed]

(* is_of_length_Test_list *)

let is_of_length__when_actual_has_specified_length__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that [1; 2; 3; 4] @@ IsList.of_length 4 in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let is_of_length__when_actual_shorter__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [1; 2; 3; 4] and length = 10 in
    (* when *)
    let exec () = assert_that value @@ IsList.of_length length in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s to have length %d, but was %d"
           (ListVal.to_string value)
           length
           (List.length value) )
    in
    assert_that exec @@ Is.raising expected

let is_of_length__when_actual_longer__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [1; 2; 3; 4] and length = 3 in
    (* when *)
    let exec () = assert_that value @@ IsList.of_length length in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s to have length %d, but was %d"
           (ListVal.to_string value)
           length
           (List.length value) )
    in
    assert_that exec @@ Is.raising expected

let is_of_length_Test_list =
  test_list
    [ is_of_length__when_actual_has_specified_length__then_passed;
      is_of_length__when_actual_shorter__then_failed;
      is_of_length__when_actual_longer__then_failed ]

(* not_is_of_length_Test_list *)

let not_is_of_length__when_actual_has_specified_length__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [1; 2; 3; 4] and length = 4 in
    (* when *)
    let exec () = assert_that value @@ Satisfies.not @@ IsList.of_length length in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf "Expected %s not to have length %d" (ListVal.to_string value) length)
    in
    assert_that exec @@ Is.raising expected

let not_is_of_length__when_actual_shorter__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that [1; 2; 3; 4] @@ Satisfies.not @@ IsList.of_length 10 in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_of_length__when_actual_longer__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that [1; 2; 3; 4] @@ Satisfies.not @@ IsList.of_length 3 in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_of_length_Test_list =
  test_list
    [ not_is_of_length__when_actual_has_specified_length__then_failed;
      not_is_of_length__when_actual_shorter__then_passed;
      not_is_of_length__when_actual_longer__then_passed ]

(* is_equal_to_Test_list *)

let is_equal_to__when_same_elements__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [1; 2; 3; 4] in
    (* when *)
    let exec () = assert_that value @@ IsList.equal_to value in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let is_equal_to__when_different_elements__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [1; 2; 3; 4] and value' = [1; 3; 5; 7] in
    (* when *)
    let exec () = assert_that value @@ IsList.equal_to value' in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s, but was %s"
           (ListVal.to_string value')
           (ListVal.to_string value) )
    in
    assert_that exec @@ Is.raising expected

let is_equal_to__when_actual_longer__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [1; 2; 3; 4] and value' = [1; 2; 3] in
    (* when *)
    let exec () = assert_that value @@ IsList.equal_to value' in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s, but was %s"
           (ListVal.to_string value')
           (ListVal.to_string value) )
    in
    assert_that exec @@ Is.raising expected

let is_equal_to__when_actual_shorter__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [1; 2; 3; 4] and value' = [1; 2; 3; 4; 5; 6; 7] in
    (* when *)
    let exec () = assert_that value @@ IsList.equal_to value' in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s, but was %s"
           (ListVal.to_string value')
           (ListVal.to_string value) )
    in
    assert_that exec @@ Is.raising expected

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
    let exec () = assert_that value @@ Satisfies.not @@ IsList.equal_to value in
    (* then *)
    let expected =
      Assertion_failed (Printf.sprintf "Expected value different than %s" (ListVal.to_string value))
    in
    assert_that exec @@ Is.raising expected

let not_is_equal_to__when_different_elements__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that [1; 2; 3; 4] @@ Satisfies.not @@ IsList.equal_to [1; 3; 5; 7] in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_equal_to__when_actual_longer__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that [1; 2; 3; 4] @@ Satisfies.not @@ IsList.equal_to [1; 2; 3] in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_equal_to__when_actual_shorter__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () =
      assert_that [1; 2; 3; 4] @@ Satisfies.not @@ IsList.equal_to [1; 2; 3; 4; 5; 6; 7]
    in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_equal_to_Test_list =
  test_list
    [ not_is_equal_to__when_same_elements__then_failed;
      not_is_equal_to__when_different_elements__then_passed;
      not_is_equal_to__when_actual_longer__then_passed;
      not_is_equal_to__when_actual_shorter__then_passed ]

(* is_containing_Test_list *)

let is_containing__when_element_present__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that [1; 2; 3; 4] @@ IsList.containing 2 in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let is_containing__when_element_absent__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [1; 2; 3; 4] and element = 10 in
    (* when *)
    let exec () = assert_that value @@ IsList.containing element in
    (* then *)
    let expected =
      Assertion_failed (Printf.sprintf "Expected %s to contain %d" (ListVal.to_string value) element)
    in
    assert_that exec @@ Is.raising expected

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
    let exec () = assert_that value @@ Satisfies.not @@ IsList.containing element in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf "Expected %s not to contain %d" (ListVal.to_string value) element)
    in
    assert_that exec @@ Is.raising expected

let not_is_containing__when_element_absent__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that [1; 2; 3; 4] @@ Satisfies.not @@ IsList.containing 10 in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_containing_Test_list =
  test_list
    [ not_is_containing__when_element_present__then_failed;
      not_is_containing__when_element_absent__then_passed ]

(* is_containing_all_Test_list *)

let is_containing_all__when_all_elements_present__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that [1; 2; 3; 4; 5; 6; 7; 8; 9] @@ IsList.containing_all [2; 4; 6; 8] in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let is_containing_all__when_some_elements_absent__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [1; 2; 3; 4; 5; 6; 7; 8; 9] and elements = [4; 12; 8; 20] in
    (* when *)
    let exec () = assert_that value @@ IsList.containing_all elements in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s to contain all values of %s, but %s are missing"
           (ListVal.to_string value)
           (ListVal.to_string elements)
           (ListVal.to_string [12; 20]) )
    in
    assert_that exec @@ Is.raising expected

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
    let exec () = assert_that value @@ Satisfies.not @@ IsList.containing_all elements in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s not to contain all values of %s"
           (ListVal.to_string value)
           (ListVal.to_string elements) )
    in
    assert_that exec @@ Is.raising expected

let not_is_containing_all__when_some_elements_absent__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () =
      assert_that [1; 2; 3; 4; 5; 6; 7; 8; 9] @@ Satisfies.not @@ IsList.containing_all [4; 12; 8; 20]
    in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_containing_all_Test_list =
  test_list
    [ not_is_containing_all__when_all_elements_present__then_failed;
      not_is_containing_all__when_some_elements_absent__then_passed ]

(* is_containing_any_Test_list *)

let is_containing_any__when_some_elements_present__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that [1; 2; 3; 4; 5; 6; 7; 8; 9] @@ IsList.containing_any [4; 12; 8; 20] in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let is_containing_any__when_all_elements_absent__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [1; 2; 3; 4; 5; 6; 7; 8; 9] and elements = [12; 14; 16; 18] in
    (* when *)
    let exec () = assert_that value @@ IsList.containing_any elements in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s to contain any value of %s"
           (ListVal.to_string value)
           (ListVal.to_string elements) )
    in
    assert_that exec @@ Is.raising expected

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
    let exec () = assert_that value @@ Satisfies.not @@ IsList.containing_any elements in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s not to contain any value of %s"
           (ListVal.to_string value)
           (ListVal.to_string elements) )
    in
    assert_that exec @@ Is.raising expected

let not_is_containing_any__when_all_elements_absent__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () =
      assert_that [1; 2; 3; 4; 5; 6; 7; 8; 9]
      @@ Satisfies.not
      @@ IsList.containing_any [12; 14; 16; 18]
    in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_containing_any_Test_list =
  test_list
    [ not_is_containing_any__when_some_elements_present__then_failed;
      not_is_containing_any__when_all_elements_absent__then_passed ]

(* list_Test *)

let list_Test =
  __MODULE__
  >::: [ is_empty_Test_list;
         not_is_empty_Test_list;
         is_of_length_Test_list;
         not_is_of_length_Test_list;
         is_equal_to_Test_list;
         not_is_equal_to_Test_list;
         is_containing_Test_list;
         not_is_containing_Test_list;
         is_containing_all_Test_list;
         not_is_containing_all_Test_list;
         is_containing_any_Test_list;
         not_is_containing_any_Test_list ]

let _ = run_test_tt_main list_Test
