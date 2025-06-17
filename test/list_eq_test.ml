(* Tests: List assertions. *)
open OUnit2
open OAssert

module Val = struct
  include Values.Tuple2.Of (Values.Int) (Values.Char)

  let equal (x1, y1) (x2, y2) = abs x1 = abs x2 && y1 = y2
end

module ListVal = Values.List.OfEq (Val)
module IsList = Is.List.OfEq (Val)

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
    let value = [(1, 'a'); (2, 'b'); (3, 'c'); (4, 'd')] in
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
    let exec () =
      assert_that [(1, 'a'); (2, 'b'); (3, 'c'); (4, 'd')] @@ Satisfies.not IsList.empty
    in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_empty_Test_list =
  test_list
    [not_is_empty__when_actual_empty__then_failed; not_is_empty__when_actual_not_empty__then_passed]

(* is_of_length_Test_list *)

let is_of_length__when_actual_has_same_length__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that [(1, 'a'); (2, 'b'); (3, 'c'); (4, 'd')] @@ IsList.of_length 4 in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let is_of_length__when_actual_is_shorter__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [(1, 'a'); (2, 'b'); (3, 'c'); (4, 'd')] and length = 10 in
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

let is_of_length__when_actual_is_longer__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [(1, 'a'); (2, 'b'); (3, 'c'); (4, 'd')] and length = 3 in
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
    [ is_of_length__when_actual_has_same_length__then_passed;
      is_of_length__when_actual_is_shorter__then_failed;
      is_of_length__when_actual_is_longer__then_failed ]

(* not_is_of_length_Test_list *)

let not_is_of_length__when_actual_has_same_length__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [(1, 'a'); (2, 'b'); (3, 'c'); (4, 'd')] and length = 4 in
    (* when *)
    let exec () = assert_that value @@ Satisfies.not @@ IsList.of_length length in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf "Expected %s not to have length %d" (ListVal.to_string value) length)
    in
    assert_that exec @@ Is.raising expected

let not_is_of_length__when_actual_is_shorter__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () =
      assert_that [(1, 'a'); (2, 'b'); (3, 'c'); (4, 'd')] @@ Satisfies.not @@ IsList.of_length 10
    in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_of_length__when_actual_is_longer__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () =
      assert_that [(1, 'a'); (2, 'b'); (3, 'c'); (4, 'd')] @@ Satisfies.not @@ IsList.of_length 3
    in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_of_length_Test_list =
  test_list
    [ not_is_of_length__when_actual_has_same_length__then_failed;
      not_is_of_length__when_actual_is_shorter__then_passed;
      not_is_of_length__when_actual_is_longer__then_passed ]

(* is_equal_to_Test_list *)

let is_equal_to__when_same_elements__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [(1, 'a'); (2, 'b'); (3, 'c'); (4, 'd')]
    and value' = [(1, 'a'); (-2, 'b'); (-3, 'c'); (4, 'd')] in
    (* when *)
    let exec () = assert_that value @@ IsList.equal_to value' in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let is_equal_to__when_different_elements__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [(1, 'a'); (2, 'b'); (3, 'c'); (4, 'd')]
    and value' = [(1, 'a'); (-3, 'c'); (-5, 'e'); (7, 'g')] in
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

let is_equal_to__when_list_is_longer__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [(1, 'a'); (2, 'b'); (3, 'c'); (4, 'd')]
    and value' = [(1, 'a'); (-2, 'b'); (-3, 'c')] in
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

let is_equal_to__when_list_is_shorter__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [(1, 'a'); (2, 'b'); (3, 'c'); (4, 'd')]
    and value' = [(1, 'a'); (-2, 'b'); (-3, 'c'); (4, 'd'); (-5, 'e'); (6, 'f'); (7, 'g')] in
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
      is_equal_to__when_list_is_longer__then_failed;
      is_equal_to__when_list_is_shorter__then_failed ]

(* not_is_equal_to_Test_list *)

let not_is_equal_to__when_same_elements__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [(1, 'a'); (2, 'b'); (3, 'c'); (4, 'd')]
    and value' = [(1, 'a'); (-2, 'b'); (-3, 'c'); (4, 'd')] in
    (* when *)
    let exec () = assert_that value @@ Satisfies.not @@ IsList.equal_to value' in
    (* then *)
    let expected =
      Assertion_failed (Printf.sprintf "Expected value different than %s" (ListVal.to_string value'))
    in
    assert_that exec @@ Is.raising expected

let not_is_equal_to__when_different_elements__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () =
      assert_that [(1, 'a'); (2, 'b'); (3, 'c'); (4, 'd')]
      @@ Satisfies.not
      @@ IsList.equal_to [(1, 'a'); (-3, 'c'); (-5, 'e'); (7, 'g')]
    in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_equal_to__when_list_is_longer__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () =
      assert_that [(1, 'a'); (2, 'b'); (3, 'c'); (4, 'd')]
      @@ Satisfies.not
      @@ IsList.equal_to [(1, 'a'); (-2, 'b'); (-3, 'c')]
    in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_equal_to__when_list_is_shorter__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () =
      assert_that [(1, 'a'); (2, 'b'); (3, 'c'); (4, 'd')]
      @@ Satisfies.not
      @@ IsList.equal_to [(1, 'a'); (-2, 'b'); (-3, 'c'); (4, 'd'); (-5, 'e'); (6, 'f'); (7, 'g')]
    in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_equal_to_Test_list =
  test_list
    [ not_is_equal_to__when_same_elements__then_failed;
      not_is_equal_to__when_different_elements__then_passed;
      not_is_equal_to__when_list_is_longer__then_passed;
      not_is_equal_to__when_list_is_shorter__then_passed ]

(* is_containing_Test_list *)

let is_containing__when_element_present__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () =
      assert_that [(1, 'a'); (2, 'b'); (3, 'c'); (4, 'd')] @@ IsList.containing (-2, 'b')
    in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let is_containing__when_element_absent__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [(1, 'a'); (2, 'b'); (3, 'c'); (4, 'd')] and element = (10, 'j') in
    (* when *)
    let exec () = assert_that value @@ IsList.containing element in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s to contain (%d, %c)"
           (ListVal.to_string value)
           (fst element)
           (snd element) )
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
    let value = [(1, 'a'); (2, 'b'); (3, 'c'); (4, 'd')] and element = (-2, 'b') in
    (* when *)
    let exec () = assert_that value @@ Satisfies.not @@ IsList.containing element in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s not to contain (%d, %c)"
           (ListVal.to_string value)
           (fst element)
           (snd element) )
    in
    assert_that exec @@ Is.raising expected

let not_is_containing__when_element_absent__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () =
      assert_that [(1, 'a'); (2, 'b'); (3, 'c'); (4, 'd')]
      @@ Satisfies.not
      @@ IsList.containing (10, 'j')
    in
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
    let exec () =
      assert_that
        [(1, 'a'); (2, 'b'); (3, 'c'); (4, 'd'); (5, 'e'); (6, 'f'); (7, 'g'); (8, 'h'); (9, 'i')]
      @@ IsList.containing_all [(-2, 'b'); (4, 'd'); (6, 'f'); (-8, 'h')]
    in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let is_containing_all__when_some_elements_absent__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value =
      [(1, 'a'); (2, 'b'); (3, 'c'); (4, 'd'); (5, 'e'); (6, 'f'); (7, 'g'); (8, 'h'); (9, 'i')]
    and elements = [(4, 'd'); (12, 'l'); (-8, 'h'); (20, 't')] in
    (* when *)
    let exec () = assert_that value @@ IsList.containing_all elements in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s to contain all values of %s, but %s are missing"
           (ListVal.to_string value)
           (ListVal.to_string elements)
           (ListVal.to_string [(12, 'l'); (20, 't')]) )
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
    let value =
      [(1, 'a'); (2, 'b'); (3, 'c'); (4, 'd'); (5, 'e'); (6, 'f'); (7, 'g'); (8, 'h'); (9, 'i')]
    and elements = [(-2, 'b'); (4, 'd'); (6, 'f'); (-8, 'h')] in
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
      assert_that
        [(1, 'a'); (2, 'b'); (3, 'c'); (4, 'd'); (5, 'e'); (6, 'f'); (7, 'g'); (8, 'h'); (9, 'i')]
      @@ Satisfies.not
      @@ IsList.containing_all [(4, 'd'); (12, 'l'); (-8, 'h'); (20, 't')]
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
    let exec () =
      assert_that
        [(1, 'a'); (2, 'b'); (3, 'c'); (4, 'd'); (5, 'e'); (6, 'f'); (7, 'g'); (8, 'h'); (9, 'i')]
      @@ IsList.containing_any [(4, 'd'); (12, 'l'); (-8, 'h'); (20, 't')]
    in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let is_containing_any__when_all_elements_absent__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value =
      [(1, 'a'); (2, 'b'); (3, 'c'); (4, 'd'); (5, 'e'); (6, 'f'); (7, 'g'); (8, 'h'); (9, 'i')]
    and elements = [(12, 'l'); (14, 'n'); (16, 'p'); (18, 'r')] in
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
    let value =
      [(1, 'a'); (2, 'b'); (3, 'c'); (4, 'd'); (5, 'e'); (6, 'f'); (7, 'g'); (8, 'h'); (9, 'i')]
    and elements = [(4, 'd'); (12, 'l'); (-8, 'h'); (20, 't')] in
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
      assert_that
        [(1, 'a'); (2, 'b'); (3, 'c'); (4, 'd'); (5, 'e'); (6, 'f'); (7, 'g'); (8, 'h'); (9, 'i')]
      @@ Satisfies.not
      @@ IsList.containing_any [(12, 'l'); (14, 'n'); (16, 'p'); (18, 'r')]
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
