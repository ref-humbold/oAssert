(* Tests: Option assertions. *)
open OUnit2
open OAssert
module IsStringOption = Is.Option.Of (Values.String)

module IsIntOption = Is.Option.OfEquatable (struct
    type t = int

    let to_string = string_of_int

    let equal x y = abs x = abs y
  end)

(* is_none_Test_list *)

let is_none__when_actual_is_none_of_string__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that None IsStringOption.none in
    (* then *)
    assert_that action Is.raising_nothing

let is_none__when_actual_is_none_of_int__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that None IsIntOption.none in
    (* then *)
    assert_that action Is.raising_nothing

let is_none__when_actual_is_some_of_string__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = "qwerty\nasdf" in
    (* when *)
    let action () = assert_that (Some value) IsStringOption.none in
    (* then *)
    let expected = Assertion_failed (Printf.sprintf "Expected None, but was Some %s" value) in
    assert_that action @@ Is.raising expected

let is_none__when_actual_is_some_of_int__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = 10 in
    (* when *)
    let action () = assert_that (Some value) IsIntOption.none in
    (* then *)
    let expected = Assertion_failed (Printf.sprintf "Expected None, but was Some %d" value) in
    assert_that action @@ Is.raising expected

let is_none_Test_list =
  test_list
    [ is_none__when_actual_is_none_of_string__then_passed;
      is_none__when_actual_is_none_of_int__then_passed;
      is_none__when_actual_is_some_of_string__then_failed;
      is_none__when_actual_is_some_of_int__then_failed ]

(* not_is_none_Test_list *)

let not_is_none__when_actual_is_none_of_string__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that None @@ Satisfies.not IsStringOption.none in
    (* then *)
    assert_that action @@ Is.raising @@ Assertion_failed "Expected value different than None"

let not_is_none__when_actual_is_none_of_int__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that None @@ Satisfies.not IsIntOption.none in
    (* then *)
    assert_that action @@ Is.raising @@ Assertion_failed "Expected value different than None"

let not_is_none__when_actual_is_some_of_string__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that (Some "qwerty\nasdf") @@ Satisfies.not IsStringOption.none in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_none__when_actual_is_some_of_int__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that (Some 10) @@ Satisfies.not IsIntOption.none in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_none_Test_list =
  test_list
    [ not_is_none__when_actual_is_none_of_string__then_failed;
      not_is_none__when_actual_is_none_of_int__then_failed;
      not_is_none__when_actual_is_some_of_string__then_passed;
      not_is_none__when_actual_is_some_of_int__then_passed ]

(* is_some_Test_list *)

let is_some__when_actual_is_some_of_string__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = "qwerty\nasdf" in
    (* when *)
    let action () = assert_that (Some value) @@ IsStringOption.some value in
    (* then *)
    assert_that action Is.raising_nothing

let is_some__when_actual_is_some_of_int__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = 10 in
    (* when *)
    let action () = assert_that (Some value) @@ IsIntOption.some (-value) in
    (* then *)
    assert_that action Is.raising_nothing

let is_some__when_actual_is_some_of_other_string__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = "qwerty\nasdf" and other = "zxcvb\t'qwerty'\tasdfg" in
    (* when *)
    let action () = assert_that (Some other) @@ IsStringOption.some value in
    (* then *)
    let expected =
      Assertion_failed (Printf.sprintf "Expected Some %s, but was Some %s" value other)
    in
    assert_that action @@ Is.raising expected

let is_some__when_actual_is_some_of_other_int__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = -10 and other = 33 in
    (* when *)
    let action () = assert_that (Some other) @@ IsIntOption.some value in
    (* then *)
    let expected =
      Assertion_failed (Printf.sprintf "Expected Some %d, but was Some %d" value other)
    in
    assert_that action @@ Is.raising expected

let is_some__when_actual_is_none_of_string__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = "qwerty\nasdf" in
    (* when *)
    let action () = assert_that None @@ IsStringOption.some value in
    (* then *)
    let expected = Assertion_failed (Printf.sprintf "Expected Some %s, but was None" value) in
    assert_that action @@ Is.raising expected

let is_some__when_actual_is_none_of_int__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = -10 in
    (* when *)
    let action () = assert_that None @@ IsIntOption.some value in
    (* then *)
    let expected = Assertion_failed (Printf.sprintf "Expected Some %d, but was None" value) in
    assert_that action @@ Is.raising expected

let is_some_Test_list =
  test_list
    [ is_some__when_actual_is_some_of_string__then_passed;
      is_some__when_actual_is_some_of_int__then_passed;
      is_some__when_actual_is_some_of_other_string__then_failed;
      is_some__when_actual_is_some_of_other_int__then_failed;
      is_some__when_actual_is_none_of_string__then_failed;
      is_some__when_actual_is_none_of_int__then_failed ]

(* not_is_some_Test_list *)

let not_is_some__when_actual_is_some_of_string__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = "qwerty\nasdf" in
    (* when *)
    let action () = assert_that (Some value) @@ Satisfies.not @@ IsStringOption.some value in
    (* then *)
    let expected = Assertion_failed (Printf.sprintf "Expected value different than Some %s" value) in
    assert_that action @@ Is.raising expected

let not_is_some__when_actual_is_some_of_int__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = 10 in
    (* when *)
    let action () = assert_that (Some value) @@ Satisfies.not @@ IsIntOption.some (-value) in
    (* then *)
    let expected =
      Assertion_failed (Printf.sprintf "Expected value different than Some %d" (-value))
    in
    assert_that action @@ Is.raising expected

let not_is_some__when_actual_is_some_of_other_string__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () =
      assert_that (Some "zxcvb\t'qwerty'\tasdfg")
      @@ Satisfies.not
      @@ IsStringOption.some "qwerty\nasdf"
    in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_some__when_actual_is_some_of_other_int__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that (Some 33) @@ Satisfies.not @@ IsIntOption.some (-10) in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_some__when_actual_is_none_of_string__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that None @@ Satisfies.not @@ IsStringOption.some "qwerty\nasdf" in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_some__when_actual_is_none_of_int__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = -10 in
    (* when *)
    let action () = assert_that None @@ Satisfies.not @@ IsIntOption.some value in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_some_Test_list =
  test_list
    [ not_is_some__when_actual_is_some_of_string__then_failed;
      not_is_some__when_actual_is_some_of_int__then_failed;
      not_is_some__when_actual_is_some_of_other_string__then_passed;
      not_is_some__when_actual_is_some_of_other_int__then_passed;
      not_is_some__when_actual_is_none_of_string__then_passed;
      not_is_some__when_actual_is_none_of_int__then_passed ]

(* option_Test *)

let option_Test =
  __MODULE__
  >::: [is_none_Test_list; not_is_none_Test_list; is_some_Test_list; not_is_some_Test_list]

let _ = run_test_tt_main option_Test
