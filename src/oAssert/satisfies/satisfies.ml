open Internals

let not (Assertion f) =
  Assertion
    (fun actual ->
       match f actual with
       | NegatableFailed _ -> Passed
       | NegatablePassed msg -> Failed (true, msg)
       | Passed | Failed _ -> failwith "This assertion cannot be negated" )
