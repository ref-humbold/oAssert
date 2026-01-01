module type VALUE = sig
  type t

  val to_string : t -> string

  val equal : t -> t -> bool
end

module type COMPARE_VALUE = sig
  include VALUE

  val compare : t -> t -> int
end
