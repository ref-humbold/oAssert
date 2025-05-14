open Internals.Common

module Is = struct
  include Assertions.General
  include Assertions.Typed
end

let fail ?(msg = "") = raise @@ Assertion_failed msg

let assert_that actual (Assertion f) =
  let {is_success; message} = f actual in
  if not is_success then fail ~msg:(build_message message)
