include Message

exception Assertion_failed of string

type assertion_result = {is_success : bool; message : assertion_message}

type 'a assertion = Assertion of ('a -> assertion_result)
