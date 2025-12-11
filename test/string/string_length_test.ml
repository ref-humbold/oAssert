(* Tests: String length assertions. *)
open OUnit2
open OAssert

let generate_params max_len start =
  let rec generate n i acc =
    if i >= n
    then acc
    else
      let new_string = if List.is_empty acc then String.make i '|' else "|" ^ List.hd acc in
      generate n (i + 1) (new_string :: acc)
  in
  generate max_len start []

let shorter_params = generate_params 25 0

let longer_params = generate_params 100 75

let all_params = shorter_params @ longer_params

let non_empty_params = List.filter (fun p -> p <> "") all_params

(* is_length_zero_Test_list *)

let is_length_zero__when_actual_is_empty__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that "" @@ Is.String.Length.zero in
    (* then *)
    assert_that action Is.raising_nothing

let is_length_zero__when_actual_is_not_empty__then_failed param =
  let label = Printf.sprintf "%s [param = %d]" __FUNCTION__ (String.length param) in
  label >:: fun _ ->
    (* when *)
    let action () = assert_that param @@ Is.String.Length.zero in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf "Expected %S to have length 0, but was %d" param (String.length param))
    in
    assert_that action @@ Is.raising expected

let is_length_zero_Test_list =
  test_list
  @@ is_length_zero__when_actual_is_empty__then_passed
     :: List.map (fun p -> is_length_zero__when_actual_is_not_empty__then_failed p) non_empty_params

(* not_is_length_zero_Test_list *)

let not_is_length_zero__when_actual_is_empty__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that "" @@ Satisfies.not @@ Is.String.Length.zero in
    (* then *)
    let expected = Assertion_failed "Expected \"\" not to have length 0" in
    assert_that action @@ Is.raising expected

let not_is_length_zero__when_actual_is_not_empty__then_passed param =
  let label = Printf.sprintf "%s [param = %d]" __FUNCTION__ (String.length param) in
  label >:: fun _ ->
    (* when *)
    let action () = assert_that param @@ Satisfies.not @@ Is.String.Length.zero in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_length_zero_Test_list =
  test_list
  @@ not_is_length_zero__when_actual_is_empty__then_failed
     :: List.map
       (fun p -> not_is_length_zero__when_actual_is_not_empty__then_passed p)
       non_empty_params

(* is_length_equal_to_Test_list *)

let is_length_equal_to__when_actual_has_specified_length__then_passed param =
  let label = Printf.sprintf "%s [param = %d]" __FUNCTION__ (String.length param) in
  label >:: fun _ ->
    (* when *)
    let action () = assert_that param @@ Is.String.Length.equal_to (String.length param) in
    (* then *)
    assert_that action Is.raising_nothing

let is_length_equal_to__when_actual_shorter__then_failed param =
  let label = Printf.sprintf "%s [param = %d]" __FUNCTION__ (String.length param) in
  label >:: fun _ ->
    (* given *)
    let length = String.length param + 10 in
    (* when *)
    let action () = assert_that param @@ Is.String.Length.equal_to length in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %S to have length %d, but was %d"
           param
           length
           (String.length param) )
    in
    assert_that action @@ Is.raising expected

let is_length_equal_to__when_actual_longer__then_failed param =
  let label = Printf.sprintf "%s [param = %d]" __FUNCTION__ (String.length param) in
  label >:: fun _ ->
    (* given *)
    let length = String.length param - 10 in
    (* when *)
    let action () = assert_that param @@ Is.String.Length.equal_to length in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %S to have length %d, but was %d"
           param
           length
           (String.length param) )
    in
    assert_that action @@ Is.raising expected

let is_length_equal_to_Test_list =
  test_list
  @@ List.concat
    [ List.map
        (fun p -> is_length_equal_to__when_actual_has_specified_length__then_passed p)
        all_params;
      List.map (fun p -> is_length_equal_to__when_actual_shorter__then_failed p) shorter_params;
      List.map (fun p -> is_length_equal_to__when_actual_longer__then_failed p) longer_params ]

(* not_is_length_equal_to_Test_list *)

let not_is_length_equal_to__when_actual_has_specified_length__then_failed param =
  let label = Printf.sprintf "%s [param = %d]" __FUNCTION__ (String.length param) in
  label >:: fun _ ->
    (* given *)
    let length = String.length param in
    (* when *)
    let action () = assert_that param @@ Satisfies.not @@ Is.String.Length.equal_to length in
    (* then *)
    let expected =
      Assertion_failed (Printf.sprintf "Expected %S not to have length %d" param length)
    in
    assert_that action @@ Is.raising expected

let not_is_length_equal_to__when_actual_shorter__then_passed param =
  let label = Printf.sprintf "%s [param = %d]" __FUNCTION__ (String.length param) in
  label >:: fun _ ->
    (* given *)
    let length = String.length param + 10 in
    (* when *)
    let action () = assert_that param @@ Satisfies.not @@ Is.String.Length.equal_to length in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_length_equal_to__when_actual_longer__then_passed param =
  let label = Printf.sprintf "%s [param = %d]" __FUNCTION__ (String.length param) in
  label >:: fun _ ->
    (* given *)
    let length = String.length param - 10 in
    (* when *)
    let action () = assert_that param @@ Satisfies.not @@ Is.String.Length.equal_to length in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_length_equal_to_Test_list =
  test_list
  @@ List.concat
    [ List.map
        (fun p -> not_is_length_equal_to__when_actual_has_specified_length__then_failed p)
        all_params;
      List.map
        (fun p -> not_is_length_equal_to__when_actual_shorter__then_passed p)
        shorter_params;
      List.map (fun p -> not_is_length_equal_to__when_actual_longer__then_passed p) longer_params
    ]

(* is_length_greater_than_Test_list *)

let is_length_greater_than__when_actual_longer__then_passed param =
  let label = Printf.sprintf "%s [param = %d]" __FUNCTION__ (String.length param) in
  label >:: fun _ ->
    (* when *)
    let action () = assert_that param @@ Is.String.Length.greater_than 50 in
    (* then *)
    assert_that action Is.raising_nothing

let is_length_greater_than__when_actual_shorter__then_failed param =
  let label = Printf.sprintf "%s [param = %d]" __FUNCTION__ (String.length param) in
  label >:: fun _ ->
    (* given *)
    let length = 50 in
    (* when *)
    let action () = assert_that param @@ Is.String.Length.greater_than length in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %S to have length greater than %d, but was %d"
           param
           length
           (String.length param) )
    in
    assert_that action @@ Is.raising expected

let is_length_greater_than__when_actual_equal__then_failed param =
  let label = Printf.sprintf "%s [param = %d]" __FUNCTION__ (String.length param) in
  label >:: fun _ ->
    (* given *)
    let length = String.length param in
    (* when *)
    let action () = assert_that param @@ Is.String.Length.greater_than length in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %S to have length greater than %d, but was %d"
           param
           length
           (String.length param) )
    in
    assert_that action @@ Is.raising expected

let is_length_greater_than_Test_list =
  test_list
  @@ List.concat
    [ List.map (fun p -> is_length_greater_than__when_actual_longer__then_passed p) longer_params;
      List.map
        (fun p -> is_length_greater_than__when_actual_shorter__then_failed p)
        shorter_params;
      List.map (fun p -> is_length_greater_than__when_actual_equal__then_failed p) all_params ]

(* not_is_length_greater_than_Test_list *)

let not_is_length_greater_than__when_actual_longer__then_failed param =
  let label = Printf.sprintf "%s [param = %d]" __FUNCTION__ (String.length param) in
  label >:: fun _ ->
    (* given *)
    let length = 50 in
    (* when *)
    let action () = assert_that param @@ Satisfies.not @@ Is.String.Length.greater_than length in
    (* then *)
    let expected =
      Assertion_failed (Printf.sprintf "Expected %S not to have length greater than %d" param length)
    in
    assert_that action @@ Is.raising expected

let not_is_length_greater_than__when_actual_shorter__then_passed param =
  let label = Printf.sprintf "%s [param = %d]" __FUNCTION__ (String.length param) in
  label >:: fun _ ->
    (* when *)
    let action () = assert_that param @@ Satisfies.not @@ Is.String.Length.greater_than 50 in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_length_greater_than__when_actual_equal__then_passed param =
  let label = Printf.sprintf "%s [param = %d]" __FUNCTION__ (String.length param) in
  label >:: fun _ ->
    (* when *)
    let action () =
      assert_that param @@ Satisfies.not @@ Is.String.Length.greater_than (String.length param)
    in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_length_greater_than_Test_list =
  test_list
  @@ List.concat
    [ List.map
        (fun p -> not_is_length_greater_than__when_actual_longer__then_failed p)
        longer_params;
      List.map
        (fun p -> not_is_length_greater_than__when_actual_shorter__then_passed p)
        shorter_params;
      List.map (fun p -> not_is_length_greater_than__when_actual_equal__then_passed p) all_params
    ]

(* is_length_less_than_Test_list *)

let is_length_less_than__when_actual_shorter__then_passed param =
  let label = Printf.sprintf "%s [param = %d]" __FUNCTION__ (String.length param) in
  label >:: fun _ ->
    (* when *)
    let action () = assert_that param @@ Is.String.Length.less_than 50 in
    (* then *)
    assert_that action Is.raising_nothing

let is_length_less_than__when_actual_longer__then_failed param =
  let label = Printf.sprintf "%s [param = %d]" __FUNCTION__ (String.length param) in
  label >:: fun _ ->
    (* given *)
    let length = 50 in
    (* when *)
    let action () = assert_that param @@ Is.String.Length.less_than length in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %S to have length less than %d, but was %d"
           param
           length
           (String.length param) )
    in
    assert_that action @@ Is.raising expected

let is_length_less_than__when_actual_equal__then_failed param =
  let label = Printf.sprintf "%s [param = %d]" __FUNCTION__ (String.length param) in
  label >:: fun _ ->
    (* given *)
    let length = String.length param in
    (* when *)
    let action () = assert_that param @@ Is.String.Length.greater_than length in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %S to have length greater than %d, but was %d"
           param
           length
           (String.length param) )
    in
    assert_that action @@ Is.raising expected

let is_length_less_than_Test_list =
  test_list
  @@ List.concat
    [ List.map (fun p -> is_length_less_than__when_actual_shorter__then_passed p) shorter_params;
      List.map (fun p -> is_length_less_than__when_actual_longer__then_failed p) longer_params;
      List.map (fun p -> is_length_less_than__when_actual_equal__then_failed p) all_params ]

(* not_is_length_less_than_Test_list *)

let not_is_length_less_than__when_actual_shorter__then_failed param =
  let label = Printf.sprintf "%s [param = %d]" __FUNCTION__ (String.length param) in
  label >:: fun _ ->
    (* given *)
    let length = 50 in
    (* when *)
    let action () = assert_that param @@ Satisfies.not @@ Is.String.Length.less_than length in
    (* then *)
    let expected =
      Assertion_failed (Printf.sprintf "Expected %S not to have length less than %d" param length)
    in
    assert_that action @@ Is.raising expected

let not_is_length_less_than__when_actual_longer__then_passed param =
  let label = Printf.sprintf "%s [param = %d]" __FUNCTION__ (String.length param) in
  label >:: fun _ ->
    (* when *)
    let action () = assert_that param @@ Satisfies.not @@ Is.String.Length.less_than 50 in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_length_less_than__when_actual_equal__then_passed param =
  let label = Printf.sprintf "%s [param = %d]" __FUNCTION__ (String.length param) in
  label >:: fun _ ->
    (* when *)
    let action () =
      assert_that param @@ Satisfies.not @@ Is.String.Length.less_than (String.length param)
    in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_length_less_than_Test_list =
  test_list
  @@ List.concat
    [ List.map
        (fun p -> not_is_length_less_than__when_actual_shorter__then_failed p)
        shorter_params;
      List.map
        (fun p -> not_is_length_less_than__when_actual_longer__then_passed p)
        longer_params;
      List.map (fun p -> not_is_length_less_than__when_actual_equal__then_passed p) all_params ]

(* string_length_Test *)

let string_length_Test =
  __MODULE__
  >::: [ is_length_zero_Test_list;
         not_is_length_zero_Test_list;
         is_length_equal_to_Test_list;
         not_is_length_equal_to_Test_list;
         is_length_greater_than_Test_list;
         not_is_length_greater_than_Test_list;
         is_length_less_than_Test_list;
         not_is_length_less_than_Test_list ]

let _ = run_test_tt_main string_length_Test
