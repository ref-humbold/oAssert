open Types

module Of (T : VALUE) : VALUE with type t = T.t list = struct
  type t = T.t list

  let to_string lst =
    Printf.sprintf "[%s]" @@ Stdlib.String.concat "; " @@ Stdlib.List.map T.to_string lst
end

module OfEq (T : EQ_VALUE) : EQ_VALUE with type t = T.t list = struct
  include Of (T)

  let equal = Stdlib.List.equal T.equal
end
