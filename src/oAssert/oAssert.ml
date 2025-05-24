open Internals

module Is = struct
  include Assertions.Common
  include Assertions.Generic
  include Assertions.Specific
end

module Satisfies = Assertions.Satisfies
module Type = Type

exception Assertion_failed of string

type 'a assertion = 'a Internals.assertion

let assertion_failed ~msg = raise @@ Assertion_failed msg

let assert_that actual (Assertion f) =
  let {status; failure_message} = f actual in
  match status with
  | Passed -> ()
  | Failed -> assertion_failed ~msg:(build_message failure_message)
