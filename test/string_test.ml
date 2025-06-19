(* Tests: String assertions. *)
open OUnit2
open OAssert

(* is_empty_Test_list *)

let is_empty__when_actual_empty__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that "" Is.String.empty in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let is_empty__when_actual_not_empty__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = "qwerty" in
    (* when *)
    let exec () = assert_that value Is.String.empty in
    (* then *)
    let expected = Assertion_failed (Printf.sprintf "Expected empty string, but was %s" value) in
    assert_that exec @@ Is.raising expected

let is_empty_Test_list =
  test_list [is_empty__when_actual_empty__then_passed; is_empty__when_actual_not_empty__then_failed]

(* not_is_empty_Test_list *)

let not_is_empty__when_actual_empty__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that "" @@ Satisfies.not Is.String.empty in
    (* then *)
    let expected = Assertion_failed "Expected value different than empty string" in
    assert_that exec @@ Is.raising expected

let not_is_empty__when_actual_not_empty__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that "qwerty" @@ Satisfies.not Is.String.empty in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_empty_Test_list =
  test_list
    [not_is_empty__when_actual_empty__then_failed; not_is_empty__when_actual_not_empty__then_passed]

(* is_of_length_Test_list *)

let is_of_length__when_actual_has_specified_length__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that "qwerty" @@ Is.String.of_length 6 in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let is_of_length__when_actual_shorter__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = "qwer" and length = 6 in
    (* when *)
    let exec () = assert_that value @@ Is.String.of_length length in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s to have length %d, but was %d"
           value
           length
           (String.length value) )
    in
    assert_that exec @@ Is.raising expected

let is_of_length__when_actual_longer__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = "qwertyuiop" and length = 6 in
    (* when *)
    let exec () = assert_that value @@ Is.String.of_length length in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s to have length %d, but was %d"
           value
           length
           (String.length value) )
    in
    assert_that exec @@ Is.raising expected

let is_of_length_Test_list =
  test_list
    [ is_of_length__when_actual_has_specified_length__then_passed;
      is_of_length__when_actual_shorter__then_failed;
      is_of_length__when_actual_longer__then_failed ]

(* not_of_length_Test_list *)

let not_is_of_length__when_actual_has_specified_length__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = "qwerty" and length = 6 in
    (* when *)
    let exec () = assert_that value @@ Satisfies.not @@ Is.String.of_length length in
    (* then *)
    let expected =
      Assertion_failed (Printf.sprintf "Expected %s not to have length %d" value length)
    in
    assert_that exec @@ Is.raising expected

let not_is_of_length__when_actual_shorter__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that "qwer" @@ Satisfies.not @@ Is.String.of_length 6 in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_of_length__when_actual_longer__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that "qwertyuiop" @@ Satisfies.not @@ Is.String.of_length 6 in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_of_length_Test_list =
  test_list
    [ not_is_of_length__when_actual_has_specified_length__then_failed;
      not_is_of_length__when_actual_shorter__then_passed;
      not_is_of_length__when_actual_longer__then_passed ]

(* is_equal_to_Test_list *)

let is_equal_to__when_same_text__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = "qwertyuiop" in
    (* when *)
    let exec () = assert_that value @@ Is.String.equal_to value in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let is_equal_to__when_different_case__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = "qwertyuiop" in
    let value' = Stdlib.String.uppercase_ascii value in
    (* when *)
    let exec () = assert_that value @@ Is.String.equal_to value' in
    (* then *)
    let expected = Assertion_failed (Printf.sprintf "Expected %s, but was %s" value' value) in
    assert_that exec @@ Is.raising expected

let is_equal_to__when_different_text__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = "qwertyuiop" and value' = "zxcvbnm" in
    (* when *)
    let exec () = assert_that value @@ Is.String.equal_to value' in
    (* then *)
    let expected = Assertion_failed (Printf.sprintf "Expected %s, but was %s" value' value) in
    assert_that exec @@ Is.raising expected

let is_equal_to__when_actual_shorter__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = "qwert" and value' = "qwertyuiop" in
    (* when *)
    let exec () = assert_that value @@ Is.String.equal_to value' in
    (* then *)
    let expected = Assertion_failed (Printf.sprintf "Expected %s, but was %s" value' value) in
    assert_that exec @@ Is.raising expected

let is_equal_to__when_actual_longer__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = "qwertyuiop" and value' = "qwert" in
    (* when *)
    let exec () = assert_that value @@ Is.String.equal_to value' in
    (* then *)
    let expected = Assertion_failed (Printf.sprintf "Expected %s, but was %s" value' value) in
    assert_that exec @@ Is.raising expected

let is_equal_to_Test_list =
  test_list
    [ is_equal_to__when_same_text__then_passed;
      is_equal_to__when_different_case__then_failed;
      is_equal_to__when_different_text__then_failed;
      is_equal_to__when_actual_shorter__then_failed;
      is_equal_to__when_actual_longer__then_failed ]

(* not_is_equal_to_Test_list *)

let not_is_equal_to__when_same_text__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = "qwertyuiop" in
    (* when *)
    let exec () = assert_that value @@ Satisfies.not @@ Is.String.equal_to value in
    (* then *)
    let expected = Assertion_failed (Printf.sprintf "Expected value different than %s" value) in
    assert_that exec @@ Is.raising expected

let not_is_equal_to__when_different_case__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = "qwertyuiop" in
    let value' = Stdlib.String.uppercase_ascii value in
    (* when *)
    let exec () = assert_that value @@ Satisfies.not @@ Is.String.equal_to value' in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_equal_to__when_different_text__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that "qwertyuiop" @@ Satisfies.not @@ Is.String.equal_to "zxcvbnm" in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_equal_to__when_actual_shorter__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that "qwert" @@ Satisfies.not @@ Is.String.equal_to "qwertyuiop" in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_equal_to__when_actual_longer__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that "qwertyuiop" @@ Satisfies.not @@ Is.String.equal_to "qwert" in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_equal_to_Test_list =
  test_list
    [ not_is_equal_to__when_same_text__then_failed;
      not_is_equal_to__when_different_case__then_passed;
      not_is_equal_to__when_different_text__then_passed;
      not_is_equal_to__when_actual_shorter__then_passed;
      not_is_equal_to__when_actual_longer__then_passed ]

(* is_uppercase_Test_list *)

let is_uppercase__when_all_upper_case__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that "(QWERTYUIOP\tASDF@ZXCVB)" @@ Is.String.uppercase in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let is_uppercase__when_all_lower_case__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = "(qwertyuiop\tasdf@zxcvb)" in
    (* when *)
    let exec () = assert_that value @@ Is.String.uppercase in
    (* then *)
    let expected = Assertion_failed (Printf.sprintf "Expected %s to be all uppercase" value) in
    assert_that exec @@ Is.raising expected

let is_uppercase__when_mixed_case__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = "(QWERTYuiop\tASDF@zxcvb)" in
    (* when *)
    let exec () = assert_that value @@ Is.String.uppercase in
    (* then *)
    let expected = Assertion_failed (Printf.sprintf "Expected %s to be all uppercase" value) in
    assert_that exec @@ Is.raising expected

let is_uppercase_Test_list =
  test_list
    [ is_uppercase__when_all_upper_case__then_passed;
      is_uppercase__when_all_lower_case__then_failed;
      is_uppercase__when_mixed_case__then_failed ]

(* not_is_uppercase_Test_list *)

let not_is_uppercase__when_all_upper_case__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = "(QWERTYUIOP\tASDF@ZXCVB)" in
    (* when *)
    let exec () = assert_that value @@ Satisfies.not @@ Is.String.uppercase in
    (* then *)
    let expected = Assertion_failed (Printf.sprintf "Expected %s not to be all uppercase" value) in
    assert_that exec @@ Is.raising expected

let not_is_uppercase__when_all_lower_case__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that "(qwertyuiop\tasdf@zxcvb)" @@ Satisfies.not @@ Is.String.uppercase in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_uppercase__when_mixed_case__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that "(QWERTYuiop\tASDF@zxcvb)" @@ Satisfies.not @@ Is.String.uppercase in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_uppercase_Test_list =
  test_list
    [ not_is_uppercase__when_all_upper_case__then_failed;
      not_is_uppercase__when_all_lower_case__then_passed;
      not_is_uppercase__when_mixed_case__then_passed ]

(* is_lowercase_Test_list *)

let is_lowercase__when_all_lower_case__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that "(qwertyuiop\tasdf@zxcvb)" @@ Is.String.lowercase in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let is_lowercase__when_all_upper_case__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = "(QWERTYUIOP\tASDF@ZXCVB)" in
    (* when *)
    let exec () = assert_that value @@ Is.String.lowercase in
    (* then *)
    let expected = Assertion_failed (Printf.sprintf "Expected %s to be all lowercase" value) in
    assert_that exec @@ Is.raising expected

let is_lowercase__when_mixed_case__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = "(QWERTYuiop\tASDF@zxcvb)" in
    (* when *)
    let exec () = assert_that value @@ Is.String.lowercase in
    (* then *)
    let expected = Assertion_failed (Printf.sprintf "Expected %s to be all lowercase" value) in
    assert_that exec @@ Is.raising expected

let is_lowercase_Test_list =
  test_list
    [ is_lowercase__when_all_lower_case__then_failed;
      is_lowercase__when_all_upper_case__then_passed;
      is_lowercase__when_mixed_case__then_failed ]

(* not_is_lowercase_Test_list *)

let not_is_lowercase__when_all_lower_case__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = "(qwertyuiop\tasdf@zxcvb)" in
    (* when *)
    let exec () = assert_that value @@ Satisfies.not @@ Is.String.lowercase in
    (* then *)
    let expected = Assertion_failed (Printf.sprintf "Expected %s not to be all lowercase" value) in
    assert_that exec @@ Is.raising expected

let not_is_lowercase__when_all_upper_case__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that "(QWERTYUIOP\tASDF@ZXCVB)" @@ Satisfies.not @@ Is.String.lowercase in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_lowercase__when_mixed_case__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = assert_that "(QWERTYuiop\tASDF@zxcvb)" @@ Satisfies.not @@ Is.String.lowercase in
    (* then *)
    assert_that exec @@ Is.raising_nothing

let not_is_lowercase_Test_list =
  test_list
    [ not_is_lowercase__when_all_lower_case__then_failed;
      not_is_lowercase__when_all_upper_case__then_passed;
      not_is_lowercase__when_mixed_case__then_passed ]

(* string_Test *)

let string_Test =
  __MODULE__
  >::: [ is_empty_Test_list;
         not_is_empty_Test_list;
         is_of_length_Test_list;
         not_is_of_length_Test_list;
         is_equal_to_Test_list;
         not_is_equal_to_Test_list;
         is_uppercase_Test_list;
         not_is_uppercase_Test_list;
         is_lowercase_Test_list;
         not_is_lowercase_Test_list ]

let _ = run_test_tt_main string_Test
