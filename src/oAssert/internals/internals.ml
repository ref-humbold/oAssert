include Assertion
include Builders

let get_raised_exception action =
  try
    ignore (action ()) ;
    None
  with
  | ex -> Some ex
