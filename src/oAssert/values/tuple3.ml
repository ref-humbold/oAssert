open Types

module Of (F : VALUE) (S : VALUE) (T : VALUE) : VALUE with type t = F.t * S.t * T.t = struct
  type t = F.t * S.t * T.t

  let to_string (x, y, z) =
    Printf.sprintf "(%s, %s, %s)" (F.to_string x) (S.to_string y) (T.to_string z)
end

module OfEq (F : EQ_VALUE) (S : EQ_VALUE) (T : EQ_VALUE) : EQ_VALUE with type t = F.t * S.t * T.t =
struct
  include Of (F) (S) (T)

  let equal (x1, y1, z1) (x2, y2, z2) = F.equal x1 x2 && S.equal y1 y2 && T.equal z1 z2
end
