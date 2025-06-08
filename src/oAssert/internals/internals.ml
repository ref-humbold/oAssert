type expected_exception = Expecting of exn | OtherThan of exn | Nothing | Anything

type assertion_message =
  | Equality of {expected_str : string; actual_str : string; negated : bool}
  | Condition of {actual_str : string; description : string; negated : bool}
  | Raising of {expected : expected_exception; actual : exn option}

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
  | Raising {expected; actual} ->
    let raised_message neg raise_str caught_str =
      Printf.sprintf
        ("Expected action" ^^ negated_str neg ^^ "to raise %s, but %s was raised")
        raise_str
        caught_str
    in
    ( match expected with
      | Expecting ex ->
        ( match actual with
          | Some ex' -> raised_message false (Printexc.to_string ex) (Printexc.to_string ex')
          | None -> raised_message false (Printexc.to_string ex) "nothing" )
      | OtherThan ex -> raised_message true (Printexc.to_string ex) "it"
      | Nothing ->
        ( match actual with
          | Some ex -> raised_message true "any exception" (Printexc.to_string ex)
          | None -> failwith "OAssert.Internals.build_message: This should not happen" )
      | Anything -> raised_message false "an exception" "nothing" )

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
