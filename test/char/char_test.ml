(* Tests: Char assertions. *)
open OUnit2
open OAssert
open Char_params

(* is_uppercase_Test_list *)

let is_uppercase__when_letter_upper_case__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %C" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param Is.Char.uppercase in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_uppercase_letters

let is_uppercase__when_letter_lower_case__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %C" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param Is.Char.uppercase in
      (* then *)
      let expected =
        Assertion_failed (Printf.sprintf "Expected %C to be an uppercase character" param)
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_lowercase_letters

let is_uppercase__when_not_letter__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %C" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param Is.Char.uppercase in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param (params_not_letters @ params_whitespace)

let is_uppercase_Test_list =
  test_list
    [ is_uppercase__when_letter_upper_case__then_passed;
      is_uppercase__when_letter_lower_case__then_failed;
      is_uppercase__when_not_letter__then_passed ]

(* not_is_uppercase_Test_list *)

let not_is_uppercase__when_negated__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %C" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Satisfies.not Is.Char.uppercase in
      (* then *)
      let expected = Failure "This assertion cannot be negated" in
      assert_that action @@ Is.raising expected
  in
  test_list
  @@ List.map
    with_param
    (params_uppercase_letters @ params_lowercase_letters @ params_not_letters @ params_whitespace)

let not_is_uppercase_Test_list = test_list [not_is_uppercase__when_negated__then_failed]

(* is_lowercase_Test_list *)

let is_lowercase__when_letter_lower_case__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %C" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param Is.Char.lowercase in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_lowercase_letters

let is_lowercase__when_letter_upper_case__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %C" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param Is.Char.lowercase in
      (* then *)
      let expected =
        Assertion_failed (Printf.sprintf "Expected %C to be a lowercase character" param)
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_uppercase_letters

let is_lowercase__when_not_letter__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %C" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param Is.Char.lowercase in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param (params_not_letters @ params_whitespace)

let is_lowercase_Test_list =
  test_list
    [ is_lowercase__when_letter_lower_case__then_failed;
      is_lowercase__when_letter_upper_case__then_passed;
      is_lowercase__when_not_letter__then_passed ]

(* not_is_lowercase_Test_list *)

let not_is_lowercase__when_negated__then_failure param =
  let label = Printf.sprintf "%s %C" __FUNCTION__ param in
  label >:: fun _ ->
    (* when *)
    let action () = assert_that param @@ Satisfies.not Is.Char.lowercase in
    (* then *)
    let expected = Failure "This assertion cannot be negated" in
    assert_that action @@ Is.raising expected

let not_is_lowercase_Test_list =
  test_list
  @@ List.map
    (fun p -> not_is_lowercase__when_negated__then_failure p)
    (params_lowercase_letters @ params_uppercase_letters @ params_not_letters @ params_whitespace)

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
  @@ List.map with_param (params_uppercase_letters @ params_lowercase_letters @ params_not_letters)

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
  @@ List.map with_param (params_uppercase_letters @ params_lowercase_letters @ params_not_letters)

let not_is_whitespace_Test_list =
  test_list
    [ not_is_whitespace__when_whitespace_character__then_failed;
      not_is_whitespace__when_non_whitespace_character__then_passed ]

(* char_Test *)

let char_Test =
  __MODULE__
  >::: [ is_uppercase_Test_list;
         not_is_uppercase_Test_list;
         is_lowercase_Test_list;
         not_is_lowercase_Test_list;
         is_whitespace_Test_list;
         not_is_whitespace_Test_list ]

let _ = run_test_tt_main char_Test
