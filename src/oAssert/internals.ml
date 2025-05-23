type assertion_message =
  | Equality of {expected_str : string; actual_str : string; negated : bool}
  | Condition of {actual_str : string; description : string; negated : bool}

type assertion_status = Passed | Failed

type assertion_result = {status : assertion_status; failure_message : assertion_message}

type 'a assertion = Assertion of ('a -> assertion_result)

let build_message msg =
  let negated_str n = if n then format_of_string " not " else format_of_string " " in
  match msg with
  | Equality {expected_str; actual_str; negated} ->
    if negated
    then Printf.sprintf "Expected value different than %s" expected_str
    else Printf.sprintf "Expected %s, but was %s" expected_str actual_str
  | Condition {actual_str; description; negated} ->
    Printf.sprintf ("Expected %s" ^^ negated_str negated ^^ "to %s") actual_str description

let build_assertion cond message =
  if cond
  then {status = Passed; failure_message = message}
  else {status = Failed; failure_message = message}
