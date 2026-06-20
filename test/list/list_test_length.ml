(* Tests: List assertions - length. *)
open OUnit2
open OAssert
open List_params
module ListVal = Values.List.Of (Values.Int)
module IsList = Is.List.Of (Values.Int)

(* is_length_zero_Test_list *)

let is_length_zero__when_actual_is_empty__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that [] @@ IsList.Length.zero in
    (* then *)
    assert_that action Is.raising_nothing

let is_length_zero__when_actual_is_not_empty__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %d" __FUNCTION__ (List.length param) in
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
  in
  test_list @@ List.map with_param params_non_empty_lists

let is_length_zero_Test_list =
  test_list
    [ is_length_zero__when_actual_is_empty__then_passed;
      is_length_zero__when_actual_is_not_empty__then_failed ]

(* not_is_length_zero_Test_list *)

let not_is_length_zero__when_actual_is_empty__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that [] @@ Satisfies.not @@ IsList.Length.zero in
    (* then *)
    let expected = Assertion_failed "Expected [] not to have length 0" in
    assert_that action @@ Is.raising expected

let not_is_length_zero__when_actual_is_not_empty__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %d" __FUNCTION__ (List.length param) in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Satisfies.not @@ IsList.Length.zero in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_non_empty_lists

let not_is_length_zero_Test_list =
  test_list
    [ not_is_length_zero__when_actual_is_empty__then_failed;
      not_is_length_zero__when_actual_is_not_empty__then_passed ]

(* is_length_equal_to_Test_list *)

let is_length_equal_to__when_actual_has_specified_length__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %d" __FUNCTION__ (List.length param) in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ IsList.Length.equal_to (List.length param) in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_all_lists

let is_length_equal_to__when_actual_shorter__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %d" __FUNCTION__ (List.length param) in
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
  in
  test_list @@ List.map with_param params_short_lists

let is_length_equal_to__when_actual_longer__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %d" __FUNCTION__ (List.length param) in
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
  in
  test_list @@ List.map with_param params_long_lists

let is_length_equal_to_Test_list =
  test_list
  @@ [ is_length_equal_to__when_actual_has_specified_length__then_passed;
       is_length_equal_to__when_actual_shorter__then_failed;
       is_length_equal_to__when_actual_longer__then_failed ]

(* not_is_length_equal_to_Test_list *)

let not_is_length_equal_to__when_actual_has_specified_length__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %d" __FUNCTION__ (List.length param) in
    label >:: fun _ ->
      (* given *)
      let length = List.length param in
      (* when *)
      let action () = assert_that param @@ Satisfies.not @@ IsList.Length.equal_to length in
      (* then *)
      let expected =
        Assertion_failed
          (Printf.sprintf "Expected %s not to have length %d" (ListVal.to_string param) length)
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_all_lists

let not_is_length_equal_to__when_actual_shorter__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %d" __FUNCTION__ (List.length param) in
    label >:: fun _ ->
      (* given *)
      let length = List.length param + 10 in
      (* when *)
      let action () = assert_that param @@ Satisfies.not @@ IsList.Length.equal_to length in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_short_lists

let not_is_length_equal_to__when_actual_longer__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %d" __FUNCTION__ (List.length param) in
    label >:: fun _ ->
      (* given *)
      let length = List.length param - 10 in
      (* when *)
      let action () = assert_that param @@ Satisfies.not @@ IsList.Length.equal_to length in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_long_lists

let not_is_length_equal_to_Test_list =
  test_list
    [ not_is_length_equal_to__when_actual_has_specified_length__then_failed;
      not_is_length_equal_to__when_actual_shorter__then_passed;
      not_is_length_equal_to__when_actual_longer__then_passed ]

(* is_length_greater_than_Test_list *)

let is_length_greater_than__when_actual_longer__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %d" __FUNCTION__ (List.length param) in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ IsList.Length.greater_than 50 in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_long_lists

let is_length_greater_than__when_actual_shorter__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %d" __FUNCTION__ (List.length param) in
    label >:: fun _ ->
      (* given *)
      let length = 50 in
      (* when *)
      let action () = assert_that param @@ IsList.Length.greater_than length in
      (* then *)
      let expected =
        Assertion_failed
          (Printf.sprintf
             "Expected %s to have length greater than %d, but was %d"
             (ListVal.to_string param)
             length
             (List.length param) )
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_short_lists

let is_length_greater_than__when_actual_equal__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %d" __FUNCTION__ (List.length param) in
    label >:: fun _ ->
      (* given *)
      let length = List.length param in
      (* when *)
      let action () = assert_that param @@ IsList.Length.greater_than length in
      (* then *)
      let expected =
        Assertion_failed
          (Printf.sprintf
             "Expected %s to have length greater than %d, but was %d"
             (ListVal.to_string param)
             length
             (List.length param) )
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_all_lists

let is_length_greater_than_Test_list =
  test_list
    [ is_length_greater_than__when_actual_longer__then_passed;
      is_length_greater_than__when_actual_shorter__then_failed;
      is_length_greater_than__when_actual_equal__then_failed ]

(* not_is_length_greater_than_Test_list *)

let not_is_length_greater_than__when_actual_longer__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %d" __FUNCTION__ (List.length param) in
    label >:: fun _ ->
      (* given *)
      let length = 50 in
      (* when *)
      let action () = assert_that param @@ Satisfies.not @@ IsList.Length.greater_than length in
      (* then *)
      let expected =
        Assertion_failed
          (Printf.sprintf
             "Expected %s not to have length greater than %d"
             (ListVal.to_string param)
             length )
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_long_lists

let not_is_length_greater_than__when_actual_shorter__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %d" __FUNCTION__ (List.length param) in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Satisfies.not @@ IsList.Length.greater_than 50 in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_short_lists

let not_is_length_greater_than__when_actual_equal__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %d" __FUNCTION__ (List.length param) in
    label >:: fun _ ->
      (* when *)
      let action () =
        assert_that param @@ Satisfies.not @@ IsList.Length.greater_than (List.length param)
      in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_all_lists

let not_is_length_greater_than_Test_list =
  test_list
    [ not_is_length_greater_than__when_actual_longer__then_failed;
      not_is_length_greater_than__when_actual_shorter__then_passed;
      not_is_length_greater_than__when_actual_equal__then_passed ]

(* is_length_less_than_Test_list *)

let is_length_less_than__when_actual_shorter__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %d" __FUNCTION__ (List.length param) in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ IsList.Length.less_than 50 in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_short_lists

let is_length_less_than__when_actual_longer__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %d" __FUNCTION__ (List.length param) in
    label >:: fun _ ->
      (* given *)
      let length = 50 in
      (* when *)
      let action () = assert_that param @@ IsList.Length.less_than length in
      (* then *)
      let expected =
        Assertion_failed
          (Printf.sprintf
             "Expected %s to have length less than %d, but was %d"
             (ListVal.to_string param)
             length
             (List.length param) )
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_long_lists

let is_length_less_than__when_actual_equal__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %d" __FUNCTION__ (List.length param) in
    label >:: fun _ ->
      (* given *)
      let length = List.length param in
      (* when *)
      let action () = assert_that param @@ IsList.Length.less_than length in
      (* then *)
      let expected =
        Assertion_failed
          (Printf.sprintf
             "Expected %s to have length less than %d, but was %d"
             (ListVal.to_string param)
             length
             (List.length param) )
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_all_lists

let is_length_less_than_Test_list =
  test_list
    [ is_length_less_than__when_actual_shorter__then_passed;
      is_length_less_than__when_actual_longer__then_failed;
      is_length_less_than__when_actual_equal__then_failed ]

(* not_is_length_less_than_Test_list *)

let not_is_length_less_than__when_actual_shorter__then_failed =
  let with_param param =
    let label = Printf.sprintf "%s %d" __FUNCTION__ (List.length param) in
    label >:: fun _ ->
      (* given *)
      let length = 50 in
      (* when *)
      let action () = assert_that param @@ Satisfies.not @@ IsList.Length.less_than length in
      (* then *)
      let expected =
        Assertion_failed
          (Printf.sprintf
             "Expected %s not to have length less than %d"
             (ListVal.to_string param)
             length )
      in
      assert_that action @@ Is.raising expected
  in
  test_list @@ List.map with_param params_short_lists

let not_is_length_less_than__when_actual_longer__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %d" __FUNCTION__ (List.length param) in
    label >:: fun _ ->
      (* when *)
      let action () = assert_that param @@ Satisfies.not @@ IsList.Length.less_than 50 in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_long_lists

let not_is_length_less_than__when_actual_equal__then_passed =
  let with_param param =
    let label = Printf.sprintf "%s %d" __FUNCTION__ (List.length param) in
    label >:: fun _ ->
      (* when *)
      let action () =
        assert_that param @@ Satisfies.not @@ IsList.Length.less_than (List.length param)
      in
      (* then *)
      assert_that action Is.raising_nothing
  in
  test_list @@ List.map with_param params_all_lists

let not_is_length_less_than_Test_list =
  test_list
    [ not_is_length_less_than__when_actual_shorter__then_failed;
      not_is_length_less_than__when_actual_longer__then_passed;
      not_is_length_less_than__when_actual_equal__then_passed ]

(* list_test_length *)
let list_test_length =
  __MODULE__
  >::: [ is_length_zero_Test_list;
         not_is_length_zero_Test_list;
         is_length_equal_to_Test_list;
         not_is_length_equal_to_Test_list;
         is_length_greater_than_Test_list;
         not_is_length_greater_than_Test_list;
         is_length_less_than_Test_list;
         not_is_length_less_than_Test_list ]
