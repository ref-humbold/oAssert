(* Tests: String length assertions. *)
open OUnit2
open OAssert

(* is_length_equal_to_Test_list *)

let is_length_equal_to__when_actual_has_specified_length__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that "qwerty" @@ Is.String.Length.equal_to 6 in
    (* then *)
    assert_that action Is.raising_nothing

let is_length_equal_to__when_actual_shorter__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = "qwer" and length = 6 in
    (* when *)
    let action () = assert_that value @@ Is.String.Length.equal_to length in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s to have length %d, but was %d"
           value
           length
           (String.length value) )
    in
    assert_that action @@ Is.raising expected

let is_length_equal_to__when_actual_longer__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = "qwertyuiop" and length = 6 in
    (* when *)
    let action () = assert_that value @@ Is.String.Length.equal_to length in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s to have length %d, but was %d"
           value
           length
           (String.length value) )
    in
    assert_that action @@ Is.raising expected

let is_length_equal_to_Test_list =
  test_list
    [ is_length_equal_to__when_actual_has_specified_length__then_passed;
      is_length_equal_to__when_actual_shorter__then_failed;
      is_length_equal_to__when_actual_longer__then_failed ]

(* not_is_length_equal_to_Test_list *)

let not_is_length_equal_to__when_actual_has_specified_length__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = "qwerty" and length = 6 in
    (* when *)
    let action () = assert_that value @@ Satisfies.not @@ Is.String.Length.equal_to length in
    (* then *)
    let expected =
      Assertion_failed (Printf.sprintf "Expected %s not to have length %d" value length)
    in
    assert_that action @@ Is.raising expected

let not_is_length_equal_to__when_actual_shorter__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that "qwer" @@ Satisfies.not @@ Is.String.Length.equal_to 6 in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_length_equal_to__when_actual_longer__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that "qwertyuiop" @@ Satisfies.not @@ Is.String.Length.equal_to 6 in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_length_equal_to_Test_list =
  test_list
    [ not_is_length_equal_to__when_actual_has_specified_length__then_failed;
      not_is_length_equal_to__when_actual_shorter__then_passed;
      not_is_length_equal_to__when_actual_longer__then_passed ]

(* string_length_Test *)

let string_length_Test =
  __MODULE__ >::: [is_length_equal_to_Test_list; not_is_length_equal_to_Test_list]

let _ = run_test_tt_main string_length_Test
