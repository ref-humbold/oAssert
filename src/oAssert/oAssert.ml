open Internals
include Common

module Is = struct
  include Assertions.Generic
  include Assertions.Bool
  include Assertions.Exceptions
  include Assertions.Typed
end

module Satisfies = Assertions.Satisfies
module Values = Values

exception Assertion_failed of string
(** Exception raised when assertion fails. *)

(** [assertion_failed msg] raises [Assertion_failed] exception with message [msg]. *)
let assertion_failed msg = raise @@ Assertion_failed msg

(** [!!! msg] is [assertion_failed msg]. *)
let ( !!! ) = assertion_failed

(** [assert_that actual assertion] applies assertion function [assertion] on [actual]. *)
let assert_that actual (Assertion f) =
  match f actual with
  | Passed | NegatablePassed _ -> ()
  | Failed (negated, msg) -> assertion_failed @@ build_message msg negated
  | NegatableFailed msg -> assertion_failed @@ build_message msg false

(** [actual <?> assertion] is [assert_that actual assertion]. *)
let ( <?> ) = assert_that

(** [!: assertion] is [Satisfies.not assertion]. *)
let ( !: ) = Satisfies.not
