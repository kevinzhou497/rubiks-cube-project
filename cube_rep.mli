(** [cube_rep] is the internal representation of the rubiks cube *)
module type CubeRep = sig
  open Types

  (** [t] is the representation type for the cube representation*)
  type t

  (** [query pos cube_rep] is the tile coloe in [cube_rep] at [pos]
      Raises and exception if [pos] is out of the bounds of [dims]*)
  val query : tile_pos -> t -> color

  (** [set pos color cube_rep] is [cube_rep] but with the tile as [pos]
      set to [color]. Raises an exception of [pos] is out of the bounds
      of [dims] *)
  val set : tile_pos -> color -> t -> t

  (** [dims cube_rep] is the [(x, y)] dimensions of [cube_rep]*)
  val dims : t -> int * int

  (** [make ()] is the default initialized cube_rep This is the "solved"
      cube state before any scrambling is done *)
  val make : unit -> t

  (** [rotate face rotation rep] is [rep] with the rotation of type
      [rotation] on [face]*)
  val rotate : face -> rotation -> t -> t

  (** [cost rep] is the A* heuristic function cost of the representation
      based on the ML model Raises [InvalidCubeExn] if the cube is
      unsolveable *)
  val cost : t -> float
end
