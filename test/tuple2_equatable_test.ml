(* Tests: Tuple (of 2 elements) assertions. *)
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

module IsTuple = Is.Tuple2.OfEquatable (StringIgnoreCase) (IntOption)

(* is_equal_to_Test_list *)

let is_equal_to__when_all_elements_same__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that ("qwerty", Some 123) @@ IsTuple.equal_to ("qwERTy", Some 123) in
    (* then *)
    assert_that action @@ Is.raising_nothing

let is_equal_to__when_first_element_different__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" and second = Some 123 and first' = "asdf" in
    (* when *)
    let action () = assert_that (first, second) @@ IsTuple.equal_to (first', second) in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected (%s, %s), but was (%s, %s)"
           first'
           (IntOption.to_string second)
           first
           (IntOption.to_string second) )
    in
    assert_that action @@ Is.raising expected

let is_equal_to__when_second_element_different__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" and second = Some 123 in
    (* when *)
    let action () = assert_that (first, second) @@ IsTuple.equal_to (first, None) in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected (%s, None), but was (%s, %s)"
           first
           first
           (IntOption.to_string second) )
    in
    assert_that action @@ Is.raising expected

let is_equal_to_Test_list =
  test_list
    [ is_equal_to__when_all_elements_same__then_passed;
      is_equal_to__when_first_element_different__then_failed;
      is_equal_to__when_second_element_different__then_failed ]

(* not_is_equal_to_Test_list *)

let not_is_equal_to__when_all_elements_same__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let pair = ("qwerty", Some 123) and pair' = ("qwERTy", Some 123) in
    (* when *)
    let action () = assert_that pair @@ Satisfies.not @@ IsTuple.equal_to pair' in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected value different than (%s, %s)"
           (fst pair')
           (IntOption.to_string @@ snd pair') )
    in
    assert_that action @@ Is.raising expected

let not_is_equal_to__when_first_element_different__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let second = Some 123 in
    (* when *)
    let action () =
      assert_that ("qwerty", second) @@ Satisfies.not @@ IsTuple.equal_to ("asdf", second)
    in
    (* then *)
    assert_that action @@ Is.raising_nothing

let not_is_equal_to__when_second_element_different__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" in
    (* when *)
    let action () =
      assert_that (first, Some 123) @@ Satisfies.not @@ IsTuple.equal_to (first, None)
    in
    (* then *)
    assert_that action @@ Is.raising_nothing

let not_is_equal_to_Test_list =
  test_list
    [ not_is_equal_to__when_all_elements_same__then_failed;
      not_is_equal_to__when_first_element_different__then_passed;
      not_is_equal_to__when_second_element_different__then_passed ]

(* is_with_first_Test_list *)

let is_with_first__when_first_element_same__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that ("qwerty", Some 123) @@ IsTuple.with_first "qwERTy" in
    (* then *)
    assert_that action @@ Is.raising_nothing

let is_with_first__when_first_element_different__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" and second = Some 123 and first' = "asdf" in
    (* when *)
    let action () = assert_that (first, second) @@ IsTuple.with_first first' in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected (%s, %s) to have first element %s"
           first
           (IntOption.to_string second)
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
    let first = "qwerty" and second = Some 123 and first' = "qwERTy" in
    (* when *)
    let action () = assert_that (first, second) @@ Satisfies.not @@ IsTuple.with_first first' in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected (%s, %s) not to have first element %s"
           first
           (IntOption.to_string second)
           first' )
    in
    assert_that action @@ Is.raising expected

let not_is_with_first__when_first_element_different__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that ("qwerty", Some 123) @@ Satisfies.not @@ IsTuple.with_first "asdf" in
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
    let action () = assert_that ("qwerty", second) @@ IsTuple.with_second second in
    (* then *)
    assert_that action @@ Is.raising_nothing

let is_with_second__when_second_element_different__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" and second = Some 123 in
    (* when *)
    let action () = assert_that (first, second) @@ IsTuple.with_second None in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected (%s, %s) to have second element None"
           first
           (IntOption.to_string second) )
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
    let first = "qwerty" and second = Some 123 in
    (* when *)
    let action () = assert_that (first, second) @@ Satisfies.not @@ IsTuple.with_second second in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected (%s, %s) not to have second element %s"
           first
           (IntOption.to_string second)
           (IntOption.to_string second) )
    in
    assert_that action @@ Is.raising expected

let not_is_with_second__when_second_element_different__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that ("qwerty", Some 123) @@ Satisfies.not @@ IsTuple.with_second None in
    (* then *)
    assert_that action @@ Is.raising_nothing

let not_is_with_second_Test_list =
  test_list
    [ not_is_with_second__when_second_element_same__then_failed;
      not_is_with_second__when_second_element_different__then_passed ]

(* tuple2_Test *)

let tuple2_Test =
  __MODULE__
  >::: [ is_equal_to_Test_list;
         not_is_equal_to_Test_list;
         is_with_first_Test_list;
         not_is_with_first_Test_list;
         is_with_second_Test_list;
         not_is_with_second_Test_list ]

let _ = run_test_tt_main tuple2_Test
