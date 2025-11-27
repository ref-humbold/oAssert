(* Tests: List length assertions. *)
open OUnit2
open OAssert

module Val = struct
  include Values.Tuple2.Of (Values.Int) (Values.Char)

  let equal (x1, y1) (x2, y2) = abs x1 = abs x2 && y1 = y2
end

module ListVal = Values.List.OfEq (Val)
module IsList = Is.List.OfEq (Val)

(* is_length_equal_to_Test_list *)

let is_length_equal_to__when_actual_has_same_length__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () =
      assert_that [(1, 'a'); (2, 'b'); (3, 'c'); (4, 'd')] @@ IsList.Length.equal_to 4
    in
    (* then *)
    assert_that action Is.raising_nothing

let is_length_equal_to__when_actual_is_shorter__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [(1, 'a'); (2, 'b'); (3, 'c'); (4, 'd')] and length = 10 in
    (* when *)
    let action () = assert_that value @@ IsList.Length.equal_to length in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s to have length %d, but was %d"
           (ListVal.to_string value)
           length
           (List.length value) )
    in
    assert_that action @@ Is.raising expected

let is_length_equal_to__when_actual_is_longer__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [(1, 'a'); (2, 'b'); (3, 'c'); (4, 'd')] and length = 3 in
    (* when *)
    let action () = assert_that value @@ IsList.Length.equal_to length in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s to have length %d, but was %d"
           (ListVal.to_string value)
           length
           (List.length value) )
    in
    assert_that action @@ Is.raising expected

let is_length_equal_to_Test_list =
  test_list
    [ is_length_equal_to__when_actual_has_same_length__then_passed;
      is_length_equal_to__when_actual_is_shorter__then_failed;
      is_length_equal_to__when_actual_is_longer__then_failed ]

(* not_is_length_equal_to_Test_list *)

let not_is_length_equal_to__when_actual_has_same_length__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = [(1, 'a'); (2, 'b'); (3, 'c'); (4, 'd')] and length = 4 in
    (* when *)
    let action () = assert_that value @@ Satisfies.not @@ IsList.Length.equal_to length in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf "Expected %s not to have length %d" (ListVal.to_string value) length)
    in
    assert_that action @@ Is.raising expected

let not_is_length_equal_to__when_actual_is_shorter__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () =
      assert_that [(1, 'a'); (2, 'b'); (3, 'c'); (4, 'd')]
      @@ Satisfies.not @@ IsList.Length.equal_to 10
    in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_length_equal_to__when_actual_is_longer__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () =
      assert_that [(1, 'a'); (2, 'b'); (3, 'c'); (4, 'd')]
      @@ Satisfies.not @@ IsList.Length.equal_to 3
    in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_length_equal_to_Test_list =
  test_list
    [ not_is_length_equal_to__when_actual_has_same_length__then_failed;
      not_is_length_equal_to__when_actual_is_shorter__then_passed;
      not_is_length_equal_to__when_actual_is_longer__then_passed ]

(* list_length_Test *)

let list_length_Test =
  __MODULE__ >::: [is_length_equal_to_Test_list; not_is_length_equal_to_Test_list]

let _ = run_test_tt_main list_length_Test
