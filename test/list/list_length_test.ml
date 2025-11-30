(* Tests: List length assertions. *)
open OUnit2
open OAssert
module ListVal = Values.List.Of (Values.Int)
module IsList = Is.List.Of (Values.Int)

let generate_params max_len start =
  let rec generate n i acc =
    if i >= n
    then acc
    else
      let new_list = if List.is_empty acc then List.init i (fun x -> x) else i :: List.hd acc in
      generate n (i + 1) (new_list :: acc)
  in
  generate max_len start []

let shorter_params = generate_params 25 0

let longer_params = generate_params 100 75

let all_params = shorter_params @ longer_params

let non_empty_params = List.filter (fun p -> not @@ List.is_empty p) all_params

(* is_length_zero_Test_list *)

let is_length_zero__when_actual_is_empty__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that [] @@ IsList.Length.zero in
    (* then *)
    assert_that action Is.raising_nothing

let is_length_zero__when_actual_is_not_empty__then_failed param =
  let label = Printf.sprintf "%s [param = %d]" __FUNCTION__ (List.length param) in
  label >:: fun _ ->
    (* when *)
    let action () = assert_that param @@ IsList.Length.zero in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s to have length 0, but was %d"
           (ListVal.to_string param)
           (List.length param) )
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
    let action () = assert_that [] @@ Satisfies.not @@ IsList.Length.zero in
    (* then *)
    let expected = Assertion_failed "Expected [] not to have length 0" in
    assert_that action @@ Is.raising expected

let not_is_length_zero__when_actual_is_not_empty__then_passed param =
  let label = Printf.sprintf "%s [param = %d]" __FUNCTION__ (List.length param) in
  label >:: fun _ ->
    (* when *)
    let action () = assert_that param @@ Satisfies.not @@ IsList.Length.zero in
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
  let label = Printf.sprintf "%s [param = %d]" __FUNCTION__ (List.length param) in
  label >:: fun _ ->
    (* when *)
    let action () = assert_that param @@ IsList.Length.equal_to (List.length param) in
    (* then *)
    assert_that action Is.raising_nothing

let is_length_equal_to__when_actual_shorter__then_failed param =
  let label = Printf.sprintf "%s [param = %d]" __FUNCTION__ (List.length param) in
  label >:: fun _ ->
    (* given *)
    let length = List.length param + 10 in
    (* when *)
    let action () = assert_that param @@ IsList.Length.equal_to length in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s to have length %d, but was %d"
           (ListVal.to_string param)
           length
           (List.length param) )
    in
    assert_that action @@ Is.raising expected

let is_length_equal_to__when_actual_longer__then_failed param =
  let label = Printf.sprintf "%s [param = %d]" __FUNCTION__ (List.length param) in
  label >:: fun _ ->
    (* given *)
    let length = List.length param - 10 in
    (* when *)
    let action () = assert_that param @@ IsList.Length.equal_to length in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected %s to have length %d, but was %d"
           (ListVal.to_string param)
           length
           (List.length param) )
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
  let label = Printf.sprintf "%s [param = %d]" __FUNCTION__ (List.length param) in
  label >:: fun _ ->
    (* given *)
    let value = param and length = List.length param in
    (* when *)
    let action () = assert_that value @@ Satisfies.not @@ IsList.Length.equal_to length in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf "Expected %s not to have length %d" (ListVal.to_string value) length)
    in
    assert_that action @@ Is.raising expected

let not_is_length_equal_to__when_actual_shorter__then_passed param =
  let label = Printf.sprintf "%s [param = %d]" __FUNCTION__ (List.length param) in
  label >:: fun _ ->
    (* given *)
    let length = List.length param + 10 in
    (* when *)
    let action () = assert_that param @@ Satisfies.not @@ IsList.Length.equal_to length in
    (* then *)
    assert_that action Is.raising_nothing

let not_is_length_equal_to__when_actual_longer__then_passed param =
  let label = Printf.sprintf "%s [param = %d]" __FUNCTION__ (List.length param) in
  label >:: fun _ ->
    (* given *)
    let length = List.length param - 10 in
    (* when *)
    let action () = assert_that param @@ Satisfies.not @@ IsList.Length.equal_to length in
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

(* list_length_Test *)
let list_length_Test =
  __MODULE__
  >::: [ is_length_zero_Test_list;
         not_is_length_zero_Test_list;
         is_length_equal_to_Test_list;
         not_is_length_equal_to_Test_list ]

let _ = run_test_tt_main list_length_Test
