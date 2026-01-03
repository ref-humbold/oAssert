open Sigs

module Of (T : VALUE) : VALUE with type t = T.t list = struct
  type t = T.t list

  let to_string lst =
    Printf.sprintf "[%s]" @@ Stdlib.String.concat "; " @@ Stdlib.List.map T.to_string lst

  let equal = Stdlib.List.equal T.equal
end
