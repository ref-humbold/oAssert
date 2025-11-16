open Types

let build_message msg negated =
  let negated_str negated = format_of_string @@ if negated then " not " else " " in
  let exn_message negated raise_str caught_str =
    Printf.sprintf
      ("Expected action" ^^ negated_str negated ^^ "to raise %s, but %s was raised")
      raise_str
      caught_str
  in
  match msg with
  | Equality {expected_str; actual_str} ->
    if negated
    then Printf.sprintf "Expected value different than %s" expected_str
    else Printf.sprintf "Expected %s, but was %s" expected_str actual_str
  | Condition {actual_str; description} ->
    Printf.sprintf ("Expected %s" ^^ negated_str negated ^^ "to %s") actual_str description
  | ConditionResult {actual_str; description; result_str} ->
    if negated
    then Printf.sprintf "Expected %s not to %s" actual_str description
    else Printf.sprintf "Expected %s to %s, but %s" actual_str description result_str
  | Raising {expected; actual} ->
    if negated
    then exn_message true (Printexc.to_string expected) "it"
    else (
      match actual with
      | Some ex -> exn_message false (Printexc.to_string expected) (Printexc.to_string ex)
      | None -> exn_message false (Printexc.to_string expected) "nothing" )
  | RaisingNothing {actual} ->
    if negated
    then exn_message false "an exception" "nothing"
    else exn_message true "any exception" (Printexc.to_string @@ Option.get actual)

let build_assertion ?no_negate condition message =
  match (condition, no_negate) with
  | true, Some () -> Passed
  | false, Some () -> Failed (false, message)
  | true, None -> NegatablePassed message
  | false, None -> NegatableFailed message
