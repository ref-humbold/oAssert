(* Tests: Tuple (of 2 elements) member assertions. *)
open OUnit2
open OAssert
module IsTuple = Is.Tuple2.Of (Values.String) (Values.Int)

(* is_first_Test_list *)

let is_first__when_first_element_same__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" in
    (* when *)
    let action () = assert_that (first, 123) @@ IsTuple.first first in
    (* then *)
    assert_that action Is.raising_nothing

let is_first__when_first_element_different__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" and second = 123 and first' = "asdf" in
    (* when *)
    let action () = assert_that (first, second) @@ IsTuple.first first' in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf "Expected (%S, %d) to have first element equal to %S" first second first')
    in
    assert_that action @@ Is.raising expected

let first_Test_list =
  test_list
    [ is_first__when_first_element_same__then_passed;
      is_first__when_first_element_different__then_failed ]

(* not_is_first_Test_list *)

let not_is_first__when_first_element_same__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" and second = 123 in
    (* when *)
    let action () = assert_that (first, second) @@ Satisfies.not @@ IsTuple.first first in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf "Expected (%S, %d) not to have first element equal to %S" first second first)
    in
    assert_that action @@ Is.raising expected

let not_is_first__when_first_element_different__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that ("qwerty", 123) @@ Satisfies.not @@ IsTuple.first "asdf" in
    (* then *)
    assert_that action Is.raising_nothing

let not_first_Test_list =
  test_list
    [ not_is_first__when_first_element_same__then_failed;
      not_is_first__when_first_element_different__then_passed ]

(* is_second_Test_list *)

let is_second__when_second_element_same__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let second = 123 in
    (* when *)
    let action () = assert_that ("qwerty", second) @@ IsTuple.second second in
    (* then *)
    assert_that action Is.raising_nothing

let is_second__when_second_element_different__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" and second = 123 and second' = 8765 in
    (* when *)
    let action () = assert_that (first, second) @@ IsTuple.second second' in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf "Expected (%S, %d) to have second element equal to %d" first second second')
    in
    assert_that action @@ Is.raising expected

let second_Test_list =
  test_list
    [ is_second__when_second_element_same__then_passed;
      is_second__when_second_element_different__then_failed ]

(* not_is_second_Test_list *)

let not_is_second__when_second_element_same__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" and second = 123 in
    (* when *)
    let action () = assert_that (first, second) @@ Satisfies.not @@ IsTuple.second second in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected (%S, %d) not to have second element equal to %d"
           first
           second
           second )
    in
    assert_that action @@ Is.raising expected

let not_is_second__when_second_element_different__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that ("qwerty", 123) @@ Satisfies.not @@ IsTuple.second 8765 in
    (* then *)
    assert_that action Is.raising_nothing

let not_second_Test_list =
  test_list
    [ not_is_second__when_second_element_same__then_failed;
      not_is_second__when_second_element_different__then_passed ]

let tuple2_test_members =
  __MODULE__ >::: [first_Test_list; not_first_Test_list; second_Test_list; not_second_Test_list]
