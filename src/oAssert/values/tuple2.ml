open Types

module Of (F : VALUE) (S : VALUE) : VALUE with type t = F.t * S.t = struct
  type t = F.t * S.t

  let to_string (x, y) = Printf.sprintf "(%s, %s)" (F.to_string x) (S.to_string y)
end

module OfEq (F : EQ_VALUE) (S : EQ_VALUE) : EQ_VALUE with type t = F.t * S.t = struct
  include Of (F) (S)

  let equal (x1, y1) (x2, y2) = F.equal x1 x2 && S.equal y1 y2
end
