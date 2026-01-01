open Types

module Of (F : VALUE) (S : VALUE) : VALUE with type t = F.t * S.t = struct
  type t = F.t * S.t

  let to_string (x, y) = Printf.sprintf "(%s, %s)" (F.to_string x) (S.to_string y)

  let equal (x1, y1) (x2, y2) = F.equal x1 x2 && S.equal y1 y2
end
