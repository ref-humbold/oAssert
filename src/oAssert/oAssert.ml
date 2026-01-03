open Internals
include Common
module Is = Assert.Assertions
module Satisfies = Assert.Satisfies
module Values = Values

type 'a assertion = 'a Internals.assertion

exception Assertion_failed of string
(** Exception raised when assertion fails. *)

(** [assertion_failed msg] raises [Assertion_failed] exception with message [msg]. *)
let assertion_failed msg = raise @@ Assertion_failed msg

(** [!!! msg] is [assertion_failed msg]. *)
let ( !!! ) = assertion_failed

(** [assert_that actual assertion] applies assertion function [assertion] on [actual] value. *)
let assert_that actual (Assertion f) =
  match f actual with
  | Passed | NegatablePassed _ -> ()
  | Failed (negated, msg) -> assertion_failed @@ build_message msg negated
  | NegatableFailed msg -> assertion_failed @@ build_message msg false

(** [actual <?> assertion] is [assert_that actual assertion]. *)
let ( <?> ) = assert_that

(** [!: assertion] is [Satisfies.not assertion]. *)
let ( !: ) = Satisfies.not
