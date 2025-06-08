open Types

module Of (T : VALUE) : VALUE with type t = T.t option = struct
  type t = T.t option

  let to_string opt =
    match opt with
    | Some x -> Printf.sprintf "Some %s" @@ T.to_string x
    | None -> "None"
end

module OfEq (T : EQ_VALUE) : EQ_VALUE with type t = T.t option = struct
  include Of (T)

  let equal = Stdlib.Option.equal T.equal
end
