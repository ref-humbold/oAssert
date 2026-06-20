(* Tests: Tuple (of 2 elements) equality assertions. *)
open OUnit2
open OAssert
module IsTuple = Is.Tuple2.Of (Values.String) (Values.Int)

(* is_equal_to_Test_list *)

let is_equal_to__when_all_elements_same__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let pair = ("qwerty", 123) in
    (* when *)
    let action () = assert_that pair @@ IsTuple.equal_to pair in
    (* then *)
    assert_that action Is.raising_nothing

let is_equal_to__when_first_element_different__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" and second = 123 and first' = "asdf" in
    (* when *)
    let action () = assert_that (first, second) @@ IsTuple.equal_to (first', second) in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf "Expected (%S, %d) to be equal to (%S, %d)" first second first' second)
    in
    assert_that action @@ Is.raising expected

let is_equal_to__when_second_element_different__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" and second = 123 and second' = 8765 in
    (* when *)
    let action () = assert_that (first, second) @@ IsTuple.equal_to (first, second') in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf "Expected (%S, %d) to be equal to (%S, %d)" first second first second')
    in
    assert_that action @@ Is.raising expected

let equal_to_Test_list =
  test_list
    [ is_equal_to__when_all_elements_same__then_passed;
      is_equal_to__when_first_element_different__then_failed;
      is_equal_to__when_second_element_different__then_failed ]

(* not_is_equal_to_Test_list *)

let not_is_equal_to__when_all_elements_same__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let pair = ("qwerty", 123) in
    (* when *)
    let action () = assert_that pair @@ Satisfies.not @@ IsTuple.equal_to pair in
    (* then *)
    let f, s = pair in
    let expected =
      Assertion_failed (Printf.sprintf "Expected (%S, %d) not to be equal to (%S, %d)" f s f s)
    in
    assert_that action @@ Is.raising expected

let not_is_equal_to__when_first_element_different__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let second = 123 in
    (* when *)
    let action () =
      assert_that ("qwerty", second) @@ Satisfies.not @@ IsTuple.equal_to ("asdf", second)
    in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_equal_to__when_second_element_different__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = "qwerty" in
    (* when *)
    let action () = assert_that (first, 123) @@ Satisfies.not @@ IsTuple.equal_to (first, 8765) in
    (* then *)
    assert_that action Is.raising_nothing

let not_equal_to_Test_list =
  test_list
    [ not_is_equal_to__when_all_elements_same__then_failed;
      not_is_equal_to__when_first_element_different__then_passed;
      not_is_equal_to__when_second_element_different__then_passed ]

let tuple2_test_equal = __MODULE__ >::: [equal_to_Test_list; not_equal_to_Test_list]
