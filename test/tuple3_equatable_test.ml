(* Tests: Tuple (of 3 elements) assertions. *)
open OUnit2
open OAssert

module StringIgnoreCase = struct
  type t = string

  let to_string s = s

  let equal s1 s2 = String.uppercase_ascii s1 = String.uppercase_ascii s2
end

module IntOption = struct
  type t = int option

  let to_string i =
    match i with
    | Some i' -> "Some " ^ string_of_int i'
    | None -> "None"

  let equal = Option.equal ( = )
end

module BoolList = struct
  type t = bool list

  let to_string bs = Printf.sprintf "[%s]" @@ String.concat "; " @@ List.map string_of_bool bs

  let equal = List.equal ( = )
end

module IsTuple = Is.Tuple3.OfEquatable (StringIgnoreCase) (IntOption) (BoolList)

(* is_equal_to_Test_list *)

let is_equal_to__when_all_elements_same__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () =
      assert_that ("qwerty", Some 123, [true; false])
      @@ IsTuple.equal_to ("qwERTy", Some 123, [true; false])
    in
    (* then *)
    assert_that action @@ Is.raising_nothing

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
           "Expected (%s, %s, %s), but was (%s, %s, %s)"
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
           "Expected (%s, None, %s), but was (%s, %s, %s)"
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
           "Expected (%s, %s, []), but was (%s, %s, %s)"
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
           "Expected value different than (%s, %s, %s)"
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
    assert_that action @@ Is.raising_nothing

let not_is_equal_to__when_second_element_different__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" and third = [true; false] in
    (* when *)
    let action () =
      assert_that (first, Some 123, third) @@ Satisfies.not @@ IsTuple.equal_to (first, None, third)
    in
    (* then *)
    assert_that action @@ Is.raising_nothing

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
    assert_that action @@ Is.raising_nothing

let not_is_equal_to_Test_list =
  test_list
    [ not_is_equal_to__when_all_elements_same__then_failed;
      not_is_equal_to__when_first_element_different__then_passed;
      not_is_equal_to__when_second_element_different__then_passed;
      not_is_equal_to__when_third_element_different__then_passed ]

(* is_with_first_Test_list *)

let is_with_first__when_first_element_same__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that ("qwerty", Some 123, [true; false]) @@ IsTuple.with_first "qwERTy" in
    (* then *)
    assert_that action @@ Is.raising_nothing

let is_with_first__when_first_element_different__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" and second = Some 123 and third = [true; false] and first' = "asdf" in
    (* when *)
    let action () = assert_that (first, second, third) @@ IsTuple.with_first first' in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected (%s, %s, %s) to have first element %s"
           first
           (IntOption.to_string second)
           (BoolList.to_string third)
           first' )
    in
    assert_that action @@ Is.raising expected

let is_with_first_Test_list =
  test_list
    [ is_with_first__when_first_element_same__then_passed;
      is_with_first__when_first_element_different__then_failed ]

(* not_is_with_first_Test_list *)

let not_is_with_first__when_first_element_same__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" and second = Some 123 and third = [true; false] and first' = "qwERTy" in
    (* when *)
    let action () =
      assert_that (first, second, third) @@ Satisfies.not @@ IsTuple.with_first first'
    in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected (%s, %s, %s) not to have first element %s"
           first
           (IntOption.to_string second)
           (BoolList.to_string third)
           first' )
    in
    assert_that action @@ Is.raising expected

let not_is_with_first__when_first_element_different__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () =
      assert_that ("qwerty", Some 123, [true; false]) @@ Satisfies.not @@ IsTuple.with_first "asdf"
    in
    (* then *)
    assert_that action @@ Is.raising_nothing

let not_is_with_first_Test_list =
  test_list
    [ not_is_with_first__when_first_element_same__then_failed;
      not_is_with_first__when_first_element_different__then_passed ]

(* is_with_second_Test_list *)

let is_with_second__when_second_element_same__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let second = Some 123 in
    (* when *)
    let action () = assert_that ("qwerty", second, [true; false]) @@ IsTuple.with_second second in
    (* then *)
    assert_that action @@ Is.raising_nothing

let is_with_second__when_second_element_different__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" and second = Some 123 and third = [true; false] in
    (* when *)
    let action () = assert_that (first, second, third) @@ IsTuple.with_second None in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected (%s, %s, %s) to have second element None"
           first
           (IntOption.to_string second)
           (BoolList.to_string third) )
    in
    assert_that action @@ Is.raising expected

let is_with_second_Test_list =
  test_list
    [ is_with_second__when_second_element_same__then_passed;
      is_with_second__when_second_element_different__then_failed ]

(* not_is_with_second_Test_list *)

let not_is_with_second__when_second_element_same__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" and second = Some 123 and third = [true; false] in
    (* when *)
    let action () =
      assert_that (first, second, third) @@ Satisfies.not @@ IsTuple.with_second second
    in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected (%s, %s, %s) not to have second element %s"
           first
           (IntOption.to_string second)
           (BoolList.to_string third)
           (IntOption.to_string second) )
    in
    assert_that action @@ Is.raising expected

let not_is_with_second__when_second_element_different__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () =
      assert_that ("qwerty", Some 123, [true; false]) @@ Satisfies.not @@ IsTuple.with_second None
    in
    (* then *)
    assert_that action @@ Is.raising_nothing

let not_is_with_second_Test_list =
  test_list
    [ not_is_with_second__when_second_element_same__then_failed;
      not_is_with_second__when_second_element_different__then_passed ]

(* is_with_third_Test_list *)

let is_with_third__when_third_element_same__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let third = [true; false] in
    (* when *)
    let action () = assert_that ("qwerty", Some 123, third) @@ IsTuple.with_third third in
    (* then *)
    assert_that action @@ Is.raising_nothing

let is_with_third__when_third_element_different__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" and second = Some 123 and third = [true; false] in
    (* when *)
    let action () = assert_that (first, second, third) @@ IsTuple.with_third [] in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected (%s, %s, %s) to have third element []"
           first
           (IntOption.to_string second)
           (BoolList.to_string third) )
    in
    assert_that action @@ Is.raising expected

let is_with_third_Test_list =
  test_list
    [ is_with_third__when_third_element_same__then_passed;
      is_with_third__when_third_element_different__then_failed ]

(* not_is_with_third_Test_list *)

let not_is_with_third__when_third_element_same__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" and second = Some 123 and third = [true; false] in
    (* when *)
    let action () = assert_that (first, second, third) @@ Satisfies.not @@ IsTuple.with_third third in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected (%s, %s, %s) not to have third element %s"
           first
           (IntOption.to_string second)
           (BoolList.to_string third)
           (BoolList.to_string third) )
    in
    assert_that action @@ Is.raising expected

let not_is_with_third__when_third_element_different__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () =
      assert_that ("qwerty", Some 123, [true; false]) @@ Satisfies.not @@ IsTuple.with_third []
    in
    (* then *)
    assert_that action @@ Is.raising_nothing

let not_is_with_third_Test_list =
  test_list
    [ not_is_with_third__when_third_element_same__then_failed;
      not_is_with_third__when_third_element_different__then_passed ]

(* tuple3_Test *)

let tuple3_Test =
  __MODULE__
  >::: [ is_equal_to_Test_list;
         not_is_equal_to_Test_list;
         is_with_first_Test_list;
         not_is_with_first_Test_list;
         is_with_second_Test_list;
         not_is_with_second_Test_list;
         is_with_third_Test_list;
         not_is_with_third_Test_list ]

let _ = run_test_tt_main tuple3_Test
