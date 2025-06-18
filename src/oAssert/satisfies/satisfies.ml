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
       let {status; failure_message} = f actual in
       build_assertion (status = Failed) (deny failure_message) )
