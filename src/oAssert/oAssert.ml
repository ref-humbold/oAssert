open Internals

module Is = struct
  include Assertions.General
  include Assertions.Specific
end

module Type = Type

exception Assertion_failed of string

type 'a assertion = 'a Internals.assertion

let assert_fail ~msg = raise @@ Assertion_failed msg

let assert_that actual (Assertion f) =
  let {status; failure_message} = f actual in
  match status with
  | Passed -> ()
  | Failed -> assert_fail ~msg:(build_message failure_message)
