(* Tests: Option equatable assertions. *)
open OUnit2
open OAssert

module IsOption = Is.Option.OfEquatable (struct
    type t = int * string

    let to_string (x, y) = Printf.sprintf "(%d, %s)" x y

    let equal (x1, y1) (x2, y2) = x1 = x2 && String.lowercase_ascii y1 = String.lowercase_ascii y2
  end)

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
    let value = (10, "qwerty") in
    (* when *)
    let action () = assert_that (Some value) IsOption.none in
    (* then *)
    let expected =
      Assertion_failed (Printf.sprintf "Expected None, but was Some (%d, %s)" (fst value) (snd value))
    in
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
    let action () = assert_that (Some (10, "qwerty")) @@ Satisfies.not IsOption.none in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_none_Test_list =
  test_list
    [not_is_none__when_actual_is_none__then_failed; not_is_none__when_actual_is_some__then_passed]

(* is_some_Test_list *)

let is_some__when_actual_is_some__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let first = 10 and second = "qwERTy" in
    (* when *)
    let action () =
      assert_that (Some (first, second)) @@ IsOption.some (first, String.capitalize_ascii second)
    in
    (* then *)
    assert_that action Is.raising_nothing

let is_some__when_actual_is_some_of_other__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = (-10, "qwerty") and other = (33, "qweRTY") in
    (* when *)
    let action () = assert_that (Some other) @@ IsOption.some value in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected Some (%d, %s), but was Some (%d, %s)"
           (fst value)
           (snd value)
           (fst other)
           (snd other) )
    in
    assert_that action @@ Is.raising expected

let is_some__when_actual_is_none__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = (-10, "qwerty") in
    (* when *)
    let action () = assert_that None @@ IsOption.some value in
    (* then *)
    let expected =
      Assertion_failed (Printf.sprintf "Expected Some (%d, %s), but was None" (fst value) (snd value))
    in
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
    let first = 10 and second = "qwerty" in
    (* when *)
    let action () =
      assert_that (Some (first, String.capitalize_ascii second))
      @@ Satisfies.not
      @@ IsOption.some (first, second)
    in
    (* then *)
    let expected =
      Assertion_failed (Printf.sprintf "Expected value different than Some (%d, %s)" first second)
    in
    assert_that action @@ Is.raising expected

let not_is_some__when_actual_is_some_of_other__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () =
      assert_that (Some (33, "qwerty")) @@ Satisfies.not @@ IsOption.some (-10, "asdf")
    in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_some__when_actual_is_none__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that None @@ Satisfies.not @@ IsOption.some (10, "qwerty") in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_some_Test_list =
  test_list
    [ not_is_some__when_actual_is_some__then_failed;
      not_is_some__when_actual_is_some_of_other__then_passed;
      not_is_some__when_actual_is_none__then_passed ]

(* option_equatable_Test *)

let option_equatable_Test =
  __MODULE__
  >::: [is_none_Test_list; not_is_none_Test_list; is_some_Test_list; not_is_some_Test_list]

let _ = run_test_tt_main option_equatable_Test
