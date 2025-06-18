type assertion_message =
  | Negated of assertion_message
  | Equality of {expected_str : string; actual_str : string}
  | Condition of {actual_str : string; description : string}
  | ConditionResult of {actual_str : string; description : string; result_str : string}
  | Raising of {expected : exn; actual : exn option}
  | RaisingNothing of {actual : exn option}

type assertion_status = Passed | Failed

type assertion_result = {status : assertion_status; failure_message : assertion_message}

type 'a assertion = Assertion of ('a -> assertion_result)

let build_message msg =
  let negated_str n = format_of_string @@ if n then " not " else " " in
  let exn_message n raise_str caught_str =
    Printf.sprintf
      ("Expected action" ^^ negated_str n ^^ "to raise %s, but %s was raised")
      raise_str
      caught_str
  in
  let rec build msg' negated =
    match msg' with
    | Negated m -> build m @@ not negated
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
  in
  build msg false

let get_raised_exception action =
  try
    ignore (action ()) ;
    None
  with
  | ex -> Some ex

let build_assertion cond message =
  if cond
  then {status = Passed; failure_message = message}
  else {status = Failed; failure_message = message}
