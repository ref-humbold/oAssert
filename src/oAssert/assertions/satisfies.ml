open Internals

let not (Assertion f) =
  let deny msg =
    match msg with
    | Condition r -> Condition {r with negated = not r.negated}
    | Equality r -> Equality {r with negated = not r.negated}
  in
  Assertion
    (fun actual ->
       let {status; failure_message} = f actual in
       build_assertion (status = Failed) (deny failure_message) )
