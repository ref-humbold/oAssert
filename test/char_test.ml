(* Tests: Char assertions. *)
open OUnit2
open OAssert

(* is_uppercase_Test_list *)

let is_uppercase__when_letter_upper_case__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that 'Q' Is.Char.uppercase in
    (* then *)
    assert_that action Is.raising_nothing

let is_uppercase__when_letter_lower_case__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = 'q' in
    (* when *)
    let action () = assert_that value Is.Char.uppercase in
    (* then *)
    let expected =
      Assertion_failed (Printf.sprintf "Expected %c to be an uppercase character" value)
    in
    assert_that action @@ Is.raising expected

let is_uppercase__when_not_letter__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that '$' Is.Char.uppercase in
    (* then *)
    assert_that action Is.raising_nothing

let is_uppercase_Test_list =
  test_list
    [ is_uppercase__when_letter_upper_case__then_passed;
      is_uppercase__when_letter_lower_case__then_failed;
      is_uppercase__when_not_letter__then_passed ]

(* not_is_uppercase_Test_list *)

let not_is_uppercase__when_letter_upper_case__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = 'Q' in
    (* when *)
    let action () = assert_that value @@ Satisfies.not Is.Char.uppercase in
    (* then *)
    let expected =
      Assertion_failed (Printf.sprintf "Expected %c not to be an uppercase character" value)
    in
    assert_that action @@ Is.raising expected

let not_is_uppercase__when_letter_lower_case__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that 'q' @@ Satisfies.not Is.Char.uppercase in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_uppercase__when_not_letter__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that '$' @@ Satisfies.not Is.Char.uppercase in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_uppercase_Test_list =
  test_list
    [ not_is_uppercase__when_letter_upper_case__then_failed;
      not_is_uppercase__when_letter_lower_case__then_passed;
      not_is_uppercase__when_not_letter__then_passed ]

(* is_lowercase_Test_list *)

let is_lowercase__when_letter_lower_case__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that 'q' Is.Char.lowercase in
    (* then *)
    assert_that action Is.raising_nothing

let is_lowercase__when_letter_upper_case__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = 'Q' in
    (* when *)
    let action () = assert_that value Is.Char.lowercase in
    (* then *)
    let expected =
      Assertion_failed (Printf.sprintf "Expected %c to be a lowercase character" value)
    in
    assert_that action @@ Is.raising expected

let is_lowercase__when_not_letter__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that '$' Is.Char.lowercase in
    (* then *)
    assert_that action Is.raising_nothing

let is_lowercase_Test_list =
  test_list
    [ is_lowercase__when_letter_lower_case__then_failed;
      is_lowercase__when_letter_upper_case__then_passed;
      is_lowercase__when_not_letter__then_passed ]

(* not_is_lowercase_Test_list *)

let not_is_lowercase__when_letter_lower_case__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = 'q' in
    (* when *)
    let action () = assert_that value @@ Satisfies.not Is.Char.lowercase in
    (* then *)
    let expected =
      Assertion_failed (Printf.sprintf "Expected %c not to be a lowercase character" value)
    in
    assert_that action @@ Is.raising expected

let not_is_lowercase__when_letter_upper_case__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that 'Q' @@ Satisfies.not Is.Char.lowercase in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_lowercase__when_not_letter__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that '$' @@ Satisfies.not Is.Char.lowercase in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_lowercase_Test_list =
  test_list
    [ not_is_lowercase__when_letter_lower_case__then_failed;
      not_is_lowercase__when_letter_upper_case__then_passed;
      not_is_lowercase__when_not_letter__then_passed ]

(* char_Test *)

let char_Test =
  __MODULE__
  >::: [ is_uppercase_Test_list;
         not_is_uppercase_Test_list;
         is_lowercase_Test_list;
         not_is_lowercase_Test_list ]

let _ = run_test_tt_main char_Test
