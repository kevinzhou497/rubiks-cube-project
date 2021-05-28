(** A 3D Cube *)

type t

(** [make ()] creates a cube *)
val make : unit -> t

(** [render cube] renders [cube] onto the screen *)
val render : t -> t

(** [free cube] cleans up the resources of [cube] *)
val free : t -> unit
