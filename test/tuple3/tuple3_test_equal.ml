(* Tests: Tuple (of 3 elements) equality assertions. *)
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

let equal_to_Test_list =
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

let not_equal_to_Test_list =
  test_list
    [ not_is_equal_to__when_both_elements_same__then_failed;
      not_is_equal_to__when_first_element_different__then_passed;
      not_is_equal_to__when_second_element_different__then_passed;
      not_is_equal_to__when_third_element_different__then_passed ]

let tuple3_test_equal = __MODULE__ >::: [equal_to_Test_list; not_equal_to_Test_list]
