(** Interface for solving a cube *)
module type Solver = sig
  open Types

  type t

  (** [get_moves cube] is the list of rotations to get [cube] to the
      solve state Raises [InvalidCubeExn] if the cube cannot be solved *)
  val get_moves : t -> (face * rotation) list

  (** [get_hint cube] gets the next best move according to the cost
      function. This is just a hint and repeated calls to this function
      is not guaranteed to solve the cube. *)
  val get_hint : t -> face * rotation
end
