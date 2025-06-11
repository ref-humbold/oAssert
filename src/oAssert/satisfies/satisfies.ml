open Internals

let not (Assertion f) =
  let deny msg =
    match msg with
    | Equality r -> Equality {r with negated = not r.negated}
    | Condition r -> Condition {r with negated = not r.negated}
    | Comparison r -> Comparison {r with negated = not r.negated}
    | Raising {expected = Expecting ex; actual} -> Raising {expected = OtherThan ex; actual}
    | Raising {expected = OtherThan ex; actual} -> Raising {expected = Expecting ex; actual}
    | Raising {expected = Nothing; actual} -> Raising {expected = Anything; actual}
    | Raising {expected = Anything; actual} -> Raising {expected = Nothing; actual}
  in
  Assertion
    (fun actual ->
       let {status; failure_message} = f actual in
       build_assertion (status = Failed) (deny failure_message) )
