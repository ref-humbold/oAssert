(* Tests: Exceptions assertions. *)
open OUnit2
open OAssert

(* is_raising_nothing_Test_list *)

let is_raising_nothing__when_raised_no_exception__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that (fun () -> ()) Is.raising_nothing in
    (* then *)
    try action () with
    | Assertion_failed _ as af -> assert_failure @@ Printexc.to_string af

let is_raising_nothing__when_raised_exception__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let ex = Not_found in
    (* when *)
    let action () =
      assert_that (fun () ->
                    let _ = raise ex in
                    None )
      @@ Is.raising_nothing
    in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected action not to raise any exception, but %s was raised"
           (Printexc.to_string ex) )
    in
    assert_raises expected action

let is_raising_nothing_Test_list =
  test_list
    [ is_raising_nothing__when_raised_no_exception__then_passed;
      is_raising_nothing__when_raised_exception__then_failed ]

(* not_is_raising_nothing_Test_list *)

let not_is_raising_nothing__when_raised_no_exception__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () = assert_that (fun () -> ()) @@ Satisfies.not Is.raising_nothing in
    (* then *)
    let expected = Assertion_failed "Expected action to raise an exception, but nothing was raised" in
    assert_raises expected action

let not_is_raising_nothing__when_raised_exception__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let action () =
      assert_that (fun () ->
                    let _ = failwith "Exception" in
                    None )
      @@ Satisfies.not @@ Is.raising_nothing
    in
    (* then *)
    try action () with
    | Assertion_failed _ as af -> assert_failure @@ Printexc.to_string af

let not_is_raising_nothing_Test_list =
  test_list
    [ not_is_raising_nothing__when_raised_no_exception__then_failed;
      not_is_raising_nothing__when_raised_exception__then_passed ]

(* is_raising_Test_list *)

let is_raising__when_raised_specified_exception__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let ex = Not_found in
    (* when *)
    let action () =
      assert_that (fun () ->
                    let _ = raise ex in
                    None )
      @@ Is.raising ex
    in
    (* then *)
    try action () with
    | Assertion_failed _ as af -> assert_failure @@ Printexc.to_string af

let is_raising__when_raised_no_exception__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let ex = Not_found in
    (* when *)
    let action () = assert_that (fun () -> ()) @@ Is.raising ex in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected action to raise %s, but nothing was raised"
           (Printexc.to_string ex) )
    in
    assert_raises expected action

let is_raising__when_raised_different_exception__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let ex = Not_found and ex' = Failure "Failure" in
    (* when *)
    let action () =
      assert_that (fun () ->
                    let _ = raise ex' in
                    None )
      @@ Is.raising ex
    in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf
           "Expected action to raise %s, but %s was raised"
           (Printexc.to_string ex)
           (Printexc.to_string ex') )
    in
    assert_raises expected action

let is_raising_Test_list =
  test_list
    [ is_raising__when_raised_specified_exception__then_passed;
      is_raising__when_raised_no_exception__then_failed;
      is_raising__when_raised_different_exception__then_failed ]

(* not_is_raising_Test_list *)

let not_is_raising__when_raised_specified_exception__then_failed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let ex = Not_found in
    (* when *)
    let action () =
      assert_that (fun () ->
                    let _ = raise ex in
                    None )
      @@ Satisfies.not @@ Is.raising ex
    in
    (* then *)
    let expected =
      Assertion_failed
        (Printf.sprintf "Expected action not to raise %s, but it was raised" (Printexc.to_string ex))
    in
    assert_raises expected action

let not_is_raising__when_raised_no_exception__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let ex = Not_found in
    (* when *)
    let action () = assert_that (fun () -> ()) @@ Satisfies.not @@ Is.raising ex in
    (* then *)
    try action () with
    | Assertion_failed _ as af -> assert_failure @@ Printexc.to_string af

let not_is_raising__when_raised_different_exception__then_passed =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let ex = Not_found and ex' = Failure "Failure" in
    (* when *)
    let action () =
      assert_that (fun () ->
                    let _ = raise ex' in
                    None )
      @@ Satisfies.not @@ Is.raising ex
    in
    (* then *)
    try action () with
    | Assertion_failed _ as af -> assert_failure @@ Printexc.to_string af

let not_is_raising_Test_list =
  test_list
    [ not_is_raising__when_raised_specified_exception__then_failed;
      not_is_raising__when_raised_no_exception__then_passed;
      not_is_raising__when_raised_different_exception__then_passed ]

(* exception_Test *)

let exceptions_Test =
  __MODULE__
  >::: [ is_raising_nothing_Test_list;
         not_is_raising_nothing_Test_list;
         is_raising_Test_list;
         not_is_raising_Test_list ]

let _ = run_test_tt_main exceptions_Test
