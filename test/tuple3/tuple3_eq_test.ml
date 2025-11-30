(* Tests: Tuple (of 3 elements) assertions. *)
open OUnit2
open OAssert

module StringIgnoreCase = struct
  include Values.String

  let equal s1 s2 = String.uppercase_ascii s1 = String.uppercase_ascii s2
end

module IntOption = Values.Option.OfEq (Values.Int)
module BoolList = Values.List.OfEq (Values.Bool)
module IsTuple = Is.Tuple3.OfEq (StringIgnoreCase) (IntOption) (BoolList)

(* is_equal_to_Test_list *)

let is_equal_to__when_all_elements_same__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () =
      assert_that ("qwerty", Some 123, [true; false])
      @@ IsTuple.equal_to ("qwERTy", Some 123, [true; false])
    in
    (* then *)
    assert_that action Is.raising_nothing

let is_equal_to__when_first_element_different__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" and second = Some 123 and third = [true; false] and first' = "asdf" in
    (* when *)
    let action () = assert_that (first, second, third) @@ IsTuple.equal_to (first', second, third) in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected (%S, %s, %s), but was (%S, %s, %s)"
           first'
           (IntOption.to_string second)
           (BoolList.to_string third)
           first
           (IntOption.to_string second)
           (BoolList.to_string third) )
    in
    assert_that action @@ Is.raising expected

let is_equal_to__when_second_element_different__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" and second = Some 123 and third = [true; false] in
    (* when *)
    let action () = assert_that ("qwerty", second, third) @@ IsTuple.equal_to (first, None, third) in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected (%S, None, %s), but was (%S, %s, %s)"
           first
           (BoolList.to_string third)
           first
           (IntOption.to_string second)
           (BoolList.to_string third) )
    in
    assert_that action @@ Is.raising expected

let is_equal_to__when_third_element_different__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" and second = Some 123 and third = [true; false] in
    (* when *)
    let action () = assert_that ("qwerty", second, third) @@ IsTuple.equal_to (first, second, []) in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected (%S, %s, []), but was (%S, %s, %s)"
           first
           (IntOption.to_string second)
           first
           (IntOption.to_string second)
           (BoolList.to_string third) )
    in
    assert_that action @@ Is.raising expected

let is_equal_to_Test_list =
  test_list
    [ is_equal_to__when_all_elements_same__then_passed;
      is_equal_to__when_first_element_different__then_failed;
      is_equal_to__when_second_element_different__then_failed;
      is_equal_to__when_third_element_different__then_failed ]

(* not_is_equal_to_Test_list *)

let not_is_equal_to__when_all_elements_same__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let triple = ("qwerty", Some 123, [true; false])
    and triple' = ("qwERTy", Some 123, [true; false]) in
    (* when *)
    let action () = assert_that triple @@ Satisfies.not @@ IsTuple.equal_to triple' in
    (* then *)
    let f, s, t = triple' in
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected value different than (%S, %s, %s)"
           f
           (IntOption.to_string s)
           (BoolList.to_string t) )
    in
    assert_that action @@ Is.raising expected

let not_is_equal_to__when_first_element_different__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let second = Some 123 and third = [true; false] in
    (* when *)
    let action () =
      assert_that ("qwerty", second, third)
      @@ Satisfies.not
      @@ IsTuple.equal_to ("asdf", second, third)
    in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_equal_to__when_second_element_different__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" and third = [true; false] in
    (* when *)
    let action () =
      assert_that (first, Some 123, third) @@ Satisfies.not @@ IsTuple.equal_to (first, None, third)
    in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_equal_to__when_third_element_different__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" and second = Some 123 in
    (* when *)
    let action () =
      assert_that (first, second, [true; false])
      @@ Satisfies.not
      @@ IsTuple.equal_to (first, second, [])
    in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_equal_to_Test_list =
  test_list
    [ not_is_equal_to__when_all_elements_same__then_failed;
      not_is_equal_to__when_first_element_different__then_passed;
      not_is_equal_to__when_second_element_different__then_passed;
      not_is_equal_to__when_third_element_different__then_passed ]

(* is_first_Test_list *)

let is_first__when_first_element_same__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that ("qwerty", Some 123, [true; false]) @@ IsTuple.first "qwERTy" in
    (* then *)
    assert_that action Is.raising_nothing

let is_first__when_first_element_different__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" and second = Some 123 and third = [true; false] and first' = "asdf" in
    (* when *)
    let action () = assert_that (first, second, third) @@ IsTuple.first first' in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected (%S, %s, %s) to have first element %S"
           first
           (IntOption.to_string second)
           (BoolList.to_string third)
           first' )
    in
    assert_that action @@ Is.raising expected

let is_first_Test_list =
  test_list
    [ is_first__when_first_element_same__then_passed;
      is_first__when_first_element_different__then_failed ]

(* not_is_first_Test_list *)

let not_is_first__when_first_element_same__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" and second = Some 123 and third = [true; false] and first' = "qwERTy" in
    (* when *)
    let action () = assert_that (first, second, third) @@ Satisfies.not @@ IsTuple.first first' in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected (%S, %s, %s) not to have first element %S"
           first
           (IntOption.to_string second)
           (BoolList.to_string third)
           first' )
    in
    assert_that action @@ Is.raising expected

let not_is_first__when_first_element_different__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () =
      assert_that ("qwerty", Some 123, [true; false]) @@ Satisfies.not @@ IsTuple.first "asdf"
    in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_first_Test_list =
  test_list
    [ not_is_first__when_first_element_same__then_failed;
      not_is_first__when_first_element_different__then_passed ]

(* is_second_Test_list *)

let is_second__when_second_element_same__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let second = Some 123 in
    (* when *)
    let action () = assert_that ("qwerty", second, [true; false]) @@ IsTuple.second second in
    (* then *)
    assert_that action Is.raising_nothing

let is_second__when_second_element_different__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" and second = Some 123 and third = [true; false] in
    (* when *)
    let action () = assert_that (first, second, third) @@ IsTuple.second None in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected (%S, %s, %s) to have second element None"
           first
           (IntOption.to_string second)
           (BoolList.to_string third) )
    in
    assert_that action @@ Is.raising expected

let is_second_Test_list =
  test_list
    [ is_second__when_second_element_same__then_passed;
      is_second__when_second_element_different__then_failed ]

(* not_is_second_Test_list *)

let not_is_second__when_second_element_same__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" and second = Some 123 and third = [true; false] in
    (* when *)
    let action () = assert_that (first, second, third) @@ Satisfies.not @@ IsTuple.second second in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected (%S, %s, %s) not to have second element %s"
           first
           (IntOption.to_string second)
           (BoolList.to_string third)
           (IntOption.to_string second) )
    in
    assert_that action @@ Is.raising expected

let not_is_second__when_second_element_different__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () =
      assert_that ("qwerty", Some 123, [true; false]) @@ Satisfies.not @@ IsTuple.second None
    in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_second_Test_list =
  test_list
    [ not_is_second__when_second_element_same__then_failed;
      not_is_second__when_second_element_different__then_passed ]

(* is_third_Test_list *)

let is_third__when_third_element_same__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let third = [true; false] in
    (* when *)
    let action () = assert_that ("qwerty", Some 123, third) @@ IsTuple.third third in
    (* then *)
    assert_that action Is.raising_nothing

let is_third__when_third_element_different__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" and second = Some 123 and third = [true; false] in
    (* when *)
    let action () = assert_that (first, second, third) @@ IsTuple.third [] in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected (%S, %s, %s) to have third element []"
           first
           (IntOption.to_string second)
           (BoolList.to_string third) )
    in
    assert_that action @@ Is.raising expected

let is_third_Test_list =
  test_list
    [ is_third__when_third_element_same__then_passed;
      is_third__when_third_element_different__then_failed ]

(* not_is_third_Test_list *)

let not_is_third__when_third_element_same__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" and second = Some 123 and third = [true; false] in
    (* when *)
    let action () = assert_that (first, second, third) @@ Satisfies.not @@ IsTuple.third third in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected (%S, %s, %s) not to have third element %s"
           first
           (IntOption.to_string second)
           (BoolList.to_string third)
           (BoolList.to_string third) )
    in
    assert_that action @@ Is.raising expected

let not_is_third__when_third_element_different__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () =
      assert_that ("qwerty", Some 123, [true; false]) @@ Satisfies.not @@ IsTuple.third []
    in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_third_Test_list =
  test_list
    [ not_is_third__when_third_element_same__then_failed;
      not_is_third__when_third_element_different__then_passed ]

(* tuple3_Test *)

let tuple3_Test =
  __MODULE__
  >::: [ is_equal_to_Test_list;
         not_is_equal_to_Test_list;
         is_first_Test_list;
         not_is_first_Test_list;
         is_second_Test_list;
         not_is_second_Test_list;
         is_third_Test_list;
         not_is_third_Test_list ]

let _ = run_test_tt_main tuple3_Test
