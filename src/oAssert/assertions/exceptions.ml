open Internals

let raising exception_ =
  Assertion
    (fun action ->
       let raised = get_raised_exception action in
       let equal =
         match raised with
         | Some ex -> exception_ = ex
         | None -> false
       in
       build_assertion equal (Raising {expected = exception_; actual = raised}) )

let raising_nothing =
  Assertion
    (fun action ->
       let raised = get_raised_exception action in
       build_assertion (Option.is_none raised) (RaisingNothing {actual = raised}) )
