open Internals
include Constants

module Is = struct
  include Assertions.Bool
  include Assertions.Exceptions
  include Assertions.Generic
  include Assertions.Typed
end

module Satisfies = Satisfies
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
  | PassAlways | Result (Passed, _) -> ()
  | Result (Failed, failure_message) -> assertion_failed @@ build_message failure_message

(** [actual <?> assertion] is [assert_that actual assertion]. *)
let ( <?> ) = assert_that

(** [!: assertion] is [Satisfies.not assertion]. *)
let ( !: ) = Satisfies.not
