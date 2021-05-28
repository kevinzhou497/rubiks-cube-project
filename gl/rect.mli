(** A 2D rectangle in OpenGL *)
type t

(** [make ()] creates a rectangle *)
val make : unit -> t

(** [free rect] frees the resources used by [rect] *)
val free : t -> unit

(** [render rect] draws rect on the screen *)
val render : t -> t

(** [make_instanced pos_data rect] enabled instanced rendering for
    [rect] using [pos_data] for a position offset of each rectangle *)
val make_instanced :
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  t ->
  t

(** [render_instanced num rect] renders [rect] [num] amount of times *)
val render_instanced : int -> t -> t
