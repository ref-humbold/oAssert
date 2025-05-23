open Internals

let true_ =
  Assertion
    (fun actual ->
       build_assertion
         actual
         (Equality
            {expected_str = string_of_bool true; actual_str = string_of_bool actual; negated = false}
         ) )

let false_ =
  Assertion
    (fun actual ->
       build_assertion
         (not actual)
         (Equality
            {expected_str = string_of_bool false; actual_str = string_of_bool actual; negated = false}
         ) )

let raising exception_ =
  Assertion
    (fun action ->
       let raised = get_raised_exception action in
       let equal =
         match raised with
         | Some ex -> exception_ = ex
         | None -> false
       in
       build_assertion equal (Raising {expected = Expecting exception_; actual = raised}) )

let raising_nothing =
  Assertion
    (fun action ->
       let raised = get_raised_exception action in
       build_assertion (Option.is_none raised) (Raising {expected = Nothing; actual = raised}) )
