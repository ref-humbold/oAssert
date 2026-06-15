(* Tests: Char assertions. *)
open OUnit2
open OAssert
open Char_params

(* is_whitespace_Test_list *)

let is_whitespace__when_whitespace_character__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %C" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param Is.Char.whitespace in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_whitespace

let is_whitespace__when_non_whitespace_character__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %C" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param Is.Char.whitespace in
      (* then *)
      let expected =
        Assertion_failed (Printf.sprintf "Expected %C to be a whitespace character" param)
      in
      assert_that action @@ Is.raising expected
  in
  test_list
  @@ List.map
    with_param
    (params_uppercase_letters @ params_lowercase_letters @ params_symbols @ params_digits)

let is_whitespace_Test_list =
  test_list
    [ is_whitespace__when_whitespace_character__then_passed;
      is_whitespace__when_non_whitespace_character__then_failed ]

(* not_is_whitespace_Test_list *)

let not_is_whitespace__when_whitespace_character__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %C" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Satisfies.not Is.Char.whitespace in
      (* then *)
      let expected =
        Assertion_failed (Printf.sprintf "Expected %C not to be a whitespace character" param)
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_whitespace

let not_is_whitespace__when_non_whitespace_character__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %C" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Satisfies.not Is.Char.whitespace in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list
  @@ List.map
    with_param
    (params_uppercase_letters @ params_lowercase_letters @ params_symbols @ params_digits)

let not_is_whitespace_Test_list =
  test_list
    [ not_is_whitespace__when_whitespace_character__then_failed;
      not_is_whitespace__when_non_whitespace_character__then_passed ]

(* is_digit_Test_list *)

let is_digit__when_digit_character__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %C" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param Is.Char.digit in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_digits

let is_digit__when_non_digit_character__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %C" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param Is.Char.digit in
      (* then *)
      let expected = Assertion_failed (Printf.sprintf "Expected %C to be a digit" param) in
      assert_that action @@ Is.raising expected
  in
  test_list
  @@ List.map
    with_param
    (params_uppercase_letters @ params_lowercase_letters @ params_symbols @ params_whitespace)

let is_digit_Test_list =
  test_list
    [is_digit__when_digit_character__then_passed; is_digit__when_non_digit_character__then_failed]

(* not_is_digit_Test_list *)

let not_is_digit__when_digit_character__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %C" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Satisfies.not Is.Char.digit in
      (* then *)
      let expected = Assertion_failed (Printf.sprintf "Expected %C not to be a digit" param) in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_digits

let not_is_digit__when_non_digit_character__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %C" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Satisfies.not Is.Char.digit in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list
  @@ List.map
    with_param
    (params_uppercase_letters @ params_lowercase_letters @ params_symbols @ params_whitespace)

let not_is_digit_Test_list =
  test_list
    [ not_is_digit__when_digit_character__then_failed;
      not_is_digit__when_non_digit_character__then_passed ]

let char_test_lettertype =
  test_list
    [ is_whitespace_Test_list;
      not_is_whitespace_Test_list;
      is_digit_Test_list;
      not_is_digit_Test_list ]
