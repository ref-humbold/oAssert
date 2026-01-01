(* Tests: Int assertions. *)
open OUnit2
open OAssert

let positive_params =
  [ 1.1;
    2.2;
    3.3;
    5.4;
    8.5;
    13.6;
    21.7;
    34.8;
    55.9;
    89.0;
    144.11;
    233.12;
    377.13;
    610.14;
    987.15;
    1597.16;
    2584.17;
    4181.18;
    6765.19;
    10946.0;
    17711.21;
    28657.22;
    46368.23;
    75025.24;
    121393.25;
    196418.26;
    317811.27;
    514229.28;
    832040.29;
    1000000.0 ]

let negative_params = List.map (fun x -> -.x) positive_params

let pair_params =
  let rec pairing lst =
    match lst with
    | x :: y :: lst' -> (x, y) :: pairing lst'
    | _ -> []
  in
  pairing positive_params

(* is_nan_Test_list *)

let is_nan__when_actual_is_nan__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that Float.nan Is.Float.nan in
    (* then *)
    assert_that action Is.raising_nothing

let is_nan__when_actual_different_than_nan__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %F" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param Is.Float.nan in
      (* then *)
      let expected = Assertion_failed (Printf.sprintf "Expected NaN, but was %F" param) in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param positive_params

let is_nan_Test_list =
  test_list
    [is_nan__when_actual_is_nan__then_passed; is_nan__when_actual_different_than_nan__then_failed]

(* not_is_nan_Test_list *)

let not_is_nan__when_actual_is_nan__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that Float.nan @@ Satisfies.not Is.Float.nan in
    (* then *)
    let expected = Assertion_failed "Expected value other than NaN" in
    assert_that action @@ Is.raising expected

let not_is_nan__when_actual_different_than_nan__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %F" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Satisfies.not Is.Float.nan in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param positive_params

let not_is_nan_Test_list =
  test_list
    [ not_is_nan__when_actual_is_nan__then_failed;
      not_is_nan__when_actual_different_than_nan__then_passed ]

(* is_zero_Test_list *)

let is_zero__when_actual_is_zero__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that 0.0 Is.Float.zero in
    (* then *)
    assert_that action Is.raising_nothing

let is_zero__when_actual_different_than_zero__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %F" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param Is.Float.zero in
      (* then *)
      let expected = Assertion_failed (Printf.sprintf "Expected zero, but was %F" param) in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param (positive_params @ negative_params)

let is_zero_Test_list =
  test_list
    [ is_zero__when_actual_is_zero__then_passed;
      is_zero__when_actual_different_than_zero__then_failed ]

(* not_is_zero_Test_list *)

let not_is_zero__when_actual_is_zero__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that 0.0 @@ Satisfies.not Is.Float.zero in
    (* then *)
    let expected = Assertion_failed "Expected value other than zero" in
    assert_that action @@ Is.raising expected

let not_is_zero__when_actual_different_than_zero__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %F" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Satisfies.not Is.Float.zero in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param (positive_params @ negative_params)

let not_is_zero_Test_list =
  test_list
    [ not_is_zero__when_actual_is_zero__then_failed;
      not_is_zero__when_actual_different_than_zero__then_passed ]

(* is_positive_Test_list *)

let is_positive__when_actual_greater_than_zero__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %F" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param Is.Float.positive in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param positive_params

let is_positive__when_actual_less_than_zero__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %F" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param Is.Float.positive in
      (* then *)
      let expected = Assertion_failed (Printf.sprintf "Expected %F to be positive" param) in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param negative_params

let is_positive__when_actual_equal_to_zero__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = 0.0 in
    (* when *)
    let action () = assert_that value Is.Float.positive in
    (* then *)
    let expected = Assertion_failed (Printf.sprintf "Expected %F to be positive" value) in
    assert_that action @@ Is.raising expected

let is_positive_Test_list =
  test_list
    [ is_positive__when_actual_greater_than_zero__then_passed;
      is_positive__when_actual_less_than_zero__then_failed;
      is_positive__when_actual_equal_to_zero__then_failed ]

(* not_is_positive_Test_list *)

let not_is_positive__when_actual_greater_than_zero__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %F" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Satisfies.not Is.Float.positive in
      (* then *)
      let expected = Assertion_failed (Printf.sprintf "Expected %F not to be positive" param) in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param positive_params

let not_is_positive__when_actual_less_than_zero__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %F" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Satisfies.not Is.Float.positive in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param negative_params

let not_is_positive__when_actual_equal_to_zero__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    (* when *)
    let action () = assert_that 0.0 @@ Satisfies.not Is.Float.positive in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_positive_Test_list =
  test_list
    [ not_is_positive__when_actual_greater_than_zero__then_failed;
      not_is_positive__when_actual_less_than_zero__then_passed;
      not_is_positive__when_actual_equal_to_zero__then_passed ]

(* is_negative_Test_list *)

let is_negative__when_actual_less_than_zero__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %F" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param Is.Float.negative in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param negative_params

let is_negative__when_actual_greater_than_zero__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %F" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param Is.Float.negative in
      (* then *)
      let expected = Assertion_failed (Printf.sprintf "Expected %F to be negative" param) in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param positive_params

let is_negative__when_actual_equal_to_zero__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = 0.0 in
    (* when *)
    let action () = assert_that value Is.Float.negative in
    (* then *)
    let expected = Assertion_failed (Printf.sprintf "Expected %F to be negative" value) in
    assert_that action @@ Is.raising expected

let is_negative_Test_list =
  test_list
    [ is_negative__when_actual_less_than_zero__then_passed;
      is_negative__when_actual_greater_than_zero__then_failed;
      is_negative__when_actual_equal_to_zero__then_failed ]

(* not_is_negative_Test_list *)

let not_is_negative__when_actual_less_than_zero__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %F" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Satisfies.not Is.Float.negative in
      (* then *)
      let expected = Assertion_failed (Printf.sprintf "Expected %F not to be negative" param) in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param negative_params

let not_is_negative__when_actual_greater_than_zero__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %F" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Satisfies.not Is.Float.negative in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param positive_params

let not_is_negative__when_actual_equal_to_zero__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    (* when *)
    let action () = assert_that 0.0 @@ Satisfies.not Is.Float.negative in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_negative_Test_list =
  test_list
    [ not_is_negative__when_actual_less_than_zero__then_failed;
      not_is_negative__when_actual_greater_than_zero__then_passed;
      not_is_negative__when_actual_equal_to_zero__then_passed ]

(* is_equal_to_Test_list *)

let is_equal_to__when_actual_is_same__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %F" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Is.Float.equal_to param in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param positive_params

let is_equal_to__when_actual_different__then_failed =
  let with_params (param1, param2) =
    let label = Printf.sprintf "%s %F %F" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param1 @@ Is.Float.equal_to param2 in
      (* then *)
      let expected =
        Assertion_failed (Printf.sprintf "Expected %F to be equal to %F" param1 param2)
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_params (pair_params @ List.map (fun (x, y) -> (y, x)) pair_params)

let is_equal_to_Test_list =
  test_list
    [is_equal_to__when_actual_is_same__then_passed; is_equal_to__when_actual_different__then_failed]

(* not_is_equal_to_Test_list *)

let not_is_equal_to__when_actual_is_same__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %F" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Satisfies.not @@ Is.Float.equal_to param in
      (* then *)
      let expected =
        Assertion_failed (Printf.sprintf "Expected %F not to be equal to %F" param param)
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param positive_params

let not_is_equal_to__when_actual_is_different__then_passed =
  let with_params (param1, param2) =
    let label = Printf.sprintf "%s %F %F" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param1 @@ Satisfies.not @@ Is.Float.equal_to param2 in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_params (pair_params @ List.map (fun (x, y) -> (y, x)) pair_params)

let not_is_equal_to_Test_list =
  test_list
    [ not_is_equal_to__when_actual_is_same__then_failed;
      not_is_equal_to__when_actual_is_different__then_passed ]

(* is_close_to_Test_list *)

let is_close_to__when_actual_is_same__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = 12.6 in
    (* when *)
    let action () = assert_that value @@ Is.Float.close_to value ~diff:(Difference 1e-3) in
    (* then *)
    assert_that action Is.raising_nothing

let is_close_to__when_actual_is_in_difference__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that 12.600095 @@ Is.Float.close_to 12.6 ~diff:(Difference 1e-3) in
    (* then *)
    assert_that action Is.raising_nothing

let is_close_to__when_actual_different__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value1 = 12.6 and value2 = 345.77 and diff = 1e-3 in
    (* when *)
    let action () = assert_that value2 @@ Is.Float.close_to value1 ~diff:(Difference diff) in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %F to be close to %F with difference %F, but difference was %F"
           value2
           value1
           diff
           (abs_float (value2 -. value1)) )
    in
    assert_that action @@ Is.raising expected

let is_close_to_Test_list =
  test_list
    [ is_close_to__when_actual_is_same__then_passed;
      is_close_to__when_actual_is_in_difference__then_passed;
      is_close_to__when_actual_different__then_failed ]

(* not_is_close_to_Test_list *)

let not_is_close_to__when_actual_is_same__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value = 12.6 and diff = 1e-3 in
    (* when *)
    let action () =
      assert_that value @@ Satisfies.not @@ Is.Float.close_to value ~diff:(Difference diff)
    in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf "Expected %F not to be close to %F with difference %F" value value diff)
    in
    assert_that action @@ Is.raising expected

let not_is_close_to__when_actual_is_in_difference__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let value1 = 12.6 and value2 = 12.600095 and diff = 1e-3 in
    (* when *)
    let action () =
      assert_that value2 @@ Satisfies.not @@ Is.Float.close_to value1 ~diff:(Difference diff)
    in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf "Expected %F not to be close to %F with difference %F" value2 value1 diff)
    in
    assert_that action @@ Is.raising expected

let not_is_close_to__when_actual_is_different__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () =
      assert_that 12.6 @@ Satisfies.not @@ Is.Float.close_to 345.77 ~diff:(Difference 1e-3)
    in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_close_to_Test_list =
  test_list
    [ not_is_close_to__when_actual_is_same__then_failed;
      not_is_close_to__when_actual_is_in_difference__then_failed;
      not_is_close_to__when_actual_is_different__then_passed ]

(* is_greater_than_Test_list *)

let is_greater_than__when_actual_greater__then_passed =
  let with_params (param1, param2) =
    let label = Printf.sprintf "%s %F %F" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param2 @@ Is.Float.greater_than param1 in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_params pair_params

let is_greater_than__when_actual_equal__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %F" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Is.Float.greater_than param in
      (* then *)
      let expected =
        Assertion_failed (Printf.sprintf "Expected %F to be greater than %F" param param)
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param positive_params

let is_greater_than__when_actual_less__then_failed =
  let with_params (param1, param2) =
    let label = Printf.sprintf "%s %F %F" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param1 @@ Is.Float.greater_than param2 in
      (* then *)
      let expected =
        Assertion_failed (Printf.sprintf "Expected %F to be greater than %F" param1 param2)
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_params pair_params

let is_greater_than_Test_list =
  test_list
    [ is_greater_than__when_actual_greater__then_passed;
      is_greater_than__when_actual_equal__then_failed;
      is_greater_than__when_actual_less__then_failed ]

(* not_is_greater_than_Test_list *)

let not_is_greater_than__when_actual_greater__then_failed =
  let with_params (param1, param2) =
    let label = Printf.sprintf "%s %F %F" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param2 @@ Satisfies.not @@ Is.Float.greater_than param1 in
      (* then *)
      let expected =
        Assertion_failed (Printf.sprintf "Expected %F not to be greater than %F" param2 param1)
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_params pair_params

let not_is_greater_than__when_actual_equal__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %F" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Satisfies.not @@ Is.Float.greater_than param in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param positive_params

let not_is_greater_than__when_actual_less__then_passed =
  let with_params (param1, param2) =
    let label = Printf.sprintf "%s %F %F" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param1 @@ Satisfies.not @@ Is.Float.greater_than param2 in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_params pair_params

let not_is_greater_than_Test_list =
  test_list
    [ not_is_greater_than__when_actual_greater__then_failed;
      not_is_greater_than__when_actual_equal__then_passed;
      not_is_greater_than__when_actual_less__then_passed ]

(* is_greater_than_or_equal_to_Test_list *)

let is_greater_than_or_equal_to__when_actual_greater__then_passed =
  let with_params (param1, param2) =
    let label = Printf.sprintf "%s %F %F" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param2 @@ Is.Float.greater_than_or_equal_to param1 in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_params pair_params

let is_greater_than_or_equal_to__when_actual_equal__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %F" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Is.Float.greater_than_or_equal_to param in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param positive_params

let is_greater_than_or_equal_to__when_actual_less__then_failed =
  let with_params (param1, param2) =
    let label = Printf.sprintf "%s %F %F" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param1 @@ Is.Float.greater_than_or_equal_to param2 in
      (* then *)
      let expected =
        Assertion_failed
          (Printf.sprintf "Expected %F to be greater than or equal to %F" param1 param2)
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_params pair_params

let is_greater_than_or_equal_to_Test_list =
  test_list
    [ is_greater_than_or_equal_to__when_actual_greater__then_passed;
      is_greater_than_or_equal_to__when_actual_equal__then_passed;
      is_greater_than_or_equal_to__when_actual_less__then_failed ]

(* not_is_greater_than_or_equal_to_Test_list *)

let not_is_greater_than_or_equal_to__when_actual_greater__then_failed =
  let with_params (param1, param2) =
    let label = Printf.sprintf "%s %F %F" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () =
        assert_that param2 @@ Satisfies.not @@ Is.Float.greater_than_or_equal_to param1
      in
      (* then *)
      let expected =
        Assertion_failed
          (Printf.sprintf "Expected %F not to be greater than or equal to %F" param2 param1)
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_params pair_params

let not_is_greater_than_or_equal_to__when_actual_equal__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %F" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Satisfies.not @@ Is.Float.greater_than_or_equal_to param in
      (* then *)
      let expected =
        Assertion_failed
          (Printf.sprintf "Expected %F not to be greater than or equal to %F" param param)
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param positive_params

let not_is_greater_than_or_equal_to__when_actual_less__then_passed =
  let with_params (param1, param2) =
    let label = Printf.sprintf "%s %F %F" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () =
        assert_that param1 @@ Satisfies.not @@ Is.Float.greater_than_or_equal_to param2
      in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_params pair_params

let not_is_greater_than_or_equal_to_Test_list =
  test_list
    [ not_is_greater_than_or_equal_to__when_actual_greater__then_failed;
      not_is_greater_than_or_equal_to__when_actual_equal__then_failed;
      not_is_greater_than_or_equal_to__when_actual_less__then_passed ]

(* is_less_than_Test_list *)

let is_less_than__when_actual_less__then_passed =
  let with_params (param1, param2) =
    let label = Printf.sprintf "%s %F %F" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param1 @@ Is.Float.less_than param2 in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_params pair_params

let is_less_than__when_actual_equal__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %F" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Is.Float.less_than param in
      (* then *)
      let expected = Assertion_failed (Printf.sprintf "Expected %F to be less than %F" param param) in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param positive_params

let is_less_than__when_actual_greater__then_failed =
  let with_params (param1, param2) =
    let label = Printf.sprintf "%s %F %F" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param2 @@ Is.Float.less_than param1 in
      (* then *)
      let expected =
        Assertion_failed (Printf.sprintf "Expected %F to be less than %F" param2 param1)
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_params pair_params

let is_less_than_Test_list =
  test_list
    [ is_less_than__when_actual_less__then_passed;
      is_less_than__when_actual_equal__then_failed;
      is_less_than__when_actual_greater__then_failed ]

(* not_is_less_than_Test_list *)

let not_is_less_than__when_actual_less__then_failed =
  let with_params (param1, param2) =
    let label = Printf.sprintf "%s %F %F" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param1 @@ Satisfies.not @@ Is.Float.less_than param2 in
      (* then *)
      let expected =
        Assertion_failed (Printf.sprintf "Expected %F not to be less than %F" param1 param2)
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_params pair_params

let not_is_less_than__when_actual_equal__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %F" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Satisfies.not @@ Is.Float.less_than param in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param positive_params

let not_is_less_than__when_actual_greater__then_passed =
  let with_params (param1, param2) =
    let label = Printf.sprintf "%s %F %F" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param2 @@ Satisfies.not @@ Is.Float.less_than param1 in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_params pair_params

let not_is_less_than_Test_list =
  test_list
    [ not_is_less_than__when_actual_less__then_failed;
      not_is_less_than__when_actual_equal__then_passed;
      not_is_less_than__when_actual_greater__then_passed ]

(* is_less_than_or_equal_to_Test_list *)

let is_less_than_or_equal_to__when_actual_less__then_passed =
  let with_params (param1, param2) =
    let label = Printf.sprintf "%s %F %F" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param1 @@ Is.Float.less_than_or_equal_to param2 in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_params pair_params

let is_less_than_or_equal_to__when_actual_equal__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %F" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Is.Float.less_than_or_equal_to param in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param positive_params

let is_less_than_or_equal_to__when_actual_less__then_failed =
  let with_params (param1, param2) =
    let label = Printf.sprintf "%s %F %F" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param2 @@ Is.Float.less_than_or_equal_to param1 in
      (* then *)
      let expected =
        Assertion_failed (Printf.sprintf "Expected %F to be less than or equal to %F" param2 param1)
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_params pair_params

let is_less_than_or_equal_to_Test_list =
  test_list
    [ is_less_than_or_equal_to__when_actual_less__then_passed;
      is_less_than_or_equal_to__when_actual_equal__then_passed;
      is_less_than_or_equal_to__when_actual_less__then_failed ]

(* not_is_less_than_or_equal_to_Test_list *)

let not_is_less_than_or_equal_to__when_actual_less__then_failed =
  let with_params (param1, param2) =
    let label = Printf.sprintf "%s %F %F" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param1 @@ Satisfies.not @@ Is.Float.less_than_or_equal_to param2 in
      (* then *)
      let expected =
        Assertion_failed
          (Printf.sprintf "Expected %F not to be less than or equal to %F" param1 param2)
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_params pair_params

let not_is_less_than_or_equal_to__when_actual_equal__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %F" __FUNCTION__ param in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Satisfies.not @@ Is.Float.less_than_or_equal_to param in
      (* then *)
      let expected =
        Assertion_failed (Printf.sprintf "Expected %F not to be less than or equal to %F" param param)
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param positive_params

let not_is_less_than_or_equal_to__when_actual_greater__then_passed =
  let with_params (param1, param2) =
    let label = Printf.sprintf "%s %F %F" __FUNCTION__ param1 param2 in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param2 @@ Satisfies.not @@ Is.Float.less_than_or_equal_to param1 in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_params pair_params

let not_is_less_than_or_equal_to_Test_list =
  test_list
    [ not_is_less_than_or_equal_to__when_actual_less__then_failed;
      not_is_less_than_or_equal_to__when_actual_equal__then_failed;
      not_is_less_than_or_equal_to__when_actual_greater__then_passed ]

(* float_Test *)

let float_Test =
  __MODULE__
  >::: [ is_nan_Test_list;
         not_is_nan_Test_list;
         is_zero_Test_list;
         not_is_zero_Test_list;
         is_positive_Test_list;
         not_is_positive_Test_list;
         is_negative_Test_list;
         not_is_negative_Test_list;
         is_equal_to_Test_list;
         not_is_equal_to_Test_list;
         is_close_to_Test_list;
         not_is_close_to_Test_list;
         not_is_equal_to_Test_list;
         is_greater_than_Test_list;
         not_is_greater_than_Test_list;
         is_greater_than_or_equal_to_Test_list;
         not_is_greater_than_or_equal_to_Test_list;
         is_less_than_Test_list;
         not_is_less_than_Test_list;
         is_less_than_or_equal_to_Test_list;
         not_is_less_than_or_equal_to_Test_list ]

let _ = run_test_tt_main float_Test
