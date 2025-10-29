open Internals

let not (Assertion f) =
  let deny msg =
    match msg with
    | Negated m -> m
    | (Equality _ | Condition _ | ConditionResult _ | Raising _ | RaisingNothing _) as m ->
      Negated m
  in
  Assertion
    (fun actual ->
       match f actual with
       | PassAlways -> PassAlways
       | Result (Passed, failure_message) -> Result (Failed, deny failure_message)
       | Result (Failed, failure_message) -> Result (Passed, deny failure_message) )
