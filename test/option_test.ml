(* Tests: Option assertions. *)
open OUnit2
open OAssert
module IsOption = Is.Option.Of (Values.String)

(* is_none_Test_list *)

let is_none__when_actual_is_none__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that None IsOption.none in
    (* then *)
    assert_that action Is.raising_nothing

let is_none__when_actual_is_some__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = "qwerty\nasdf" in
    (* when *)
    let action () = assert_that (Some value) IsOption.none in
    (* then *)
    let expected = Assertion_failed (Printf.sprintf "Expected None, but was Some %s" value) in
    assert_that action @@ Is.raising expected

let is_none_Test_list =
  test_list [is_none__when_actual_is_none__then_passed; is_none__when_actual_is_some__then_failed]

(* not_is_none_Test_list *)

let not_is_none__when_actual_is_none__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that None @@ Satisfies.not IsOption.none in
    (* then *)
    assert_that action @@ Is.raising @@ Assertion_failed "Expected value different than None"

let not_is_none__when_actual_is_some__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that (Some "qwerty\nasdf") @@ Satisfies.not IsOption.none in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_none_Test_list =
  test_list
    [not_is_none__when_actual_is_none__then_failed; not_is_none__when_actual_is_some__then_passed]

(* is_some_Test_list *)

let is_some__when_actual_is_some__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = "qwerty\nasdf" in
    (* when *)
    let action () = assert_that (Some value) @@ IsOption.some value in
    (* then *)
    assert_that action Is.raising_nothing

let is_some__when_actual_is_some_of_other__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = "qwerty\nasdf" and other = "zxcvb\t'qwerty'\tasdfg" in
    (* when *)
    let action () = assert_that (Some other) @@ IsOption.some value in
    (* then *)
    let expected =
      Assertion_failed (Printf.sprintf "Expected Some %s, but was Some %s" value other)
    in
    assert_that action @@ Is.raising expected

let is_some__when_actual_is_none__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = "qwerty\nasdf" in
    (* when *)
    let action () = assert_that None @@ IsOption.some value in
    (* then *)
    let expected = Assertion_failed (Printf.sprintf "Expected Some %s, but was None" value) in
    assert_that action @@ Is.raising expected

let is_some_Test_list =
  test_list
    [ is_some__when_actual_is_some__then_passed;
      is_some__when_actual_is_some_of_other__then_failed;
      is_some__when_actual_is_none__then_failed ]

(* not_is_some_Test_list *)

let not_is_some__when_actual_is_some__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = "qwerty\nasdf" in
    (* when *)
    let action () = assert_that (Some value) @@ Satisfies.not @@ IsOption.some value in
    (* then *)
    let expected = Assertion_failed (Printf.sprintf "Expected value different than Some %s" value) in
    assert_that action @@ Is.raising expected

let not_is_some__when_actual_is_some_of_other__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () =
      assert_that (Some "zxcvb\t'qwerty'\tasdfg") @@ Satisfies.not @@ IsOption.some "qwerty\nasdf"
    in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_some__when_actual_is_none__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that None @@ Satisfies.not @@ IsOption.some "qwerty\nasdf" in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_some_Test_list =
  test_list
    [ not_is_some__when_actual_is_some__then_failed;
      not_is_some__when_actual_is_some_of_other__then_passed;
      not_is_some__when_actual_is_none__then_passed ]

(* option_Test *)

let option_Test =
  __MODULE__
  >::: [is_none_Test_list; not_is_none_Test_list; is_some_Test_list; not_is_some_Test_list]

let _ = run_test_tt_main option_Test
