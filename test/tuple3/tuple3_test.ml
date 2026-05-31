(* Tests: Tuple (of 3 elements) assertions. *)
open OUnit2
open OAssert
module IsTuple = Is.Tuple3.Of (Values.String) (Values.Int) (Values.Bool)

(* is_equal_to_Test_list *)

let is_equal_to__when_all_elements_same__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let triple = ("qwerty", 123, true) in
    (* when *)
    let action () = assert_that triple @@ IsTuple.equal_to triple in
    (* then *)
    assert_that action Is.raising_nothing

let is_equal_to__when_first_element_different__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" and second = 123 and third = true and first' = "asdf" in
    (* when *)
    let action () = assert_that (first, second, third) @@ IsTuple.equal_to (first', second, third) in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected (%S, %d, %B) to be equal to (%S, %d, %B)"
           first
           second
           third
           first'
           second
           third )
    in
    assert_that action @@ Is.raising expected

let is_equal_to__when_second_element_different__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" and second = 123 and third = true and second' = 8765 in
    (* when *)
    let action () = assert_that (first, second, third) @@ IsTuple.equal_to (first, second', third) in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected (%S, %d, %B) to be equal to (%S, %d, %B)"
           first
           second
           third
           first
           second'
           third )
    in
    assert_that action @@ Is.raising expected

let is_equal_to__when_third_element_different__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" and second = 123 and third = true and third' = false in
    (* when *)
    let action () = assert_that (first, second, third) @@ IsTuple.equal_to (first, second, third') in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected (%S, %d, %B) to be equal to (%S, %d, %B)"
           first
           second
           third
           first
           second
           third' )
    in
    assert_that action @@ Is.raising expected

let is_equal_to_Test_list =
  test_list
    [ is_equal_to__when_all_elements_same__then_passed;
      is_equal_to__when_first_element_different__then_failed;
      is_equal_to__when_second_element_different__then_failed;
      is_equal_to__when_third_element_different__then_failed ]

(* not_is_equal_to_Test_list *)

let not_is_equal_to__when_both_elements_same__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let pair = ("qwerty", 123, true) in
    (* when *)
    let action () = assert_that pair @@ Satisfies.not @@ IsTuple.equal_to pair in
    (* then *)
    let f, s, t = pair in
    let expected =
      Assertion_failed
        (Printf.sprintf "Expected (%S, %d, %B) not to be equal to (%S, %d, %B)" f s t f s t)
    in
    assert_that action @@ Is.raising expected

let not_is_equal_to__when_first_element_different__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let second = 123 and third = true in
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
    let first = "qwerty" and third = true in
    (* when *)
    let action () =
      assert_that (first, 123, third) @@ Satisfies.not @@ IsTuple.equal_to (first, 8765, third)
    in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_equal_to__when_third_element_different__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" and second = 123 in
    (* when *)
    let action () =
      assert_that (first, second, true) @@ Satisfies.not @@ IsTuple.equal_to (first, second, false)
    in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_equal_to_Test_list =
  test_list
    [ not_is_equal_to__when_both_elements_same__then_failed;
      not_is_equal_to__when_first_element_different__then_passed;
      not_is_equal_to__when_second_element_different__then_passed;
      not_is_equal_to__when_third_element_different__then_passed ]

(* is_first_Test_list *)

let is_first__when_first_element_same__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" in
    (* when *)
    let action () = assert_that (first, 123, true) @@ IsTuple.first first in
    (* then *)
    assert_that action Is.raising_nothing

let is_first__when_first_element_different__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" and second = 123 and third = true and first' = "asdf" in
    (* when *)
    let action () = assert_that (first, second, third) @@ IsTuple.first first' in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected (%S, %d, %B) to have first element equal to %S"
           first
           second
           third
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
    let first = "qwerty" and second = 123 and third = true in
    (* when *)
    let action () = assert_that (first, second, third) @@ Satisfies.not @@ IsTuple.first first in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected (%S, %d, %B) not to have first element equal to %S"
           first
           second
           third
           first )
    in
    assert_that action @@ Is.raising expected

let not_is_first__when_first_element_different__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that ("qwerty", 123, true) @@ Satisfies.not @@ IsTuple.first "asdf" in
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
    let second = 123 in
    (* when *)
    let action () = assert_that ("qwerty", second, true) @@ IsTuple.second second in
    (* then *)
    assert_that action Is.raising_nothing

let is_second__when_second_element_different__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" and second = 123 and third = true and second' = 8765 in
    (* when *)
    let action () = assert_that (first, second, third) @@ IsTuple.second second' in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected (%S, %d, %B) to have second element equal to %d"
           first
           second
           third
           second' )
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
    let first = "qwerty" and second = 123 and third = true in
    (* when *)
    let action () = assert_that (first, second, third) @@ Satisfies.not @@ IsTuple.second second in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected (%S, %d, %B) not to have second element equal to %d"
           first
           second
           third
           second )
    in
    assert_that action @@ Is.raising expected

let not_is_second__when_second_element_different__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that ("qwerty", 123, true) @@ Satisfies.not @@ IsTuple.second 8765 in
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
    let third = true in
    (* when *)
    let action () = assert_that ("qwerty", 123, third) @@ IsTuple.third third in
    (* then *)
    assert_that action Is.raising_nothing

let is_third__when_third_element_different__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" and second = 123 and third = true and third' = false in
    (* when *)
    let action () = assert_that (first, second, third) @@ IsTuple.third third' in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected (%S, %d, %B) to have third element equal to %B"
           first
           second
           third
           third' )
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
    let first = "qwerty" and second = 123 and third = true in
    (* when *)
    let action () = assert_that (first, second, third) @@ Satisfies.not @@ IsTuple.third third in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected (%S, %d, %B) not to have third element equal to %B"
           first
           second
           third
           third )
    in
    assert_that action @@ Is.raising expected

let not_is_third__when_third_element_different__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that ("qwerty", 123, true) @@ Satisfies.not @@ IsTuple.third false in
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
