(** OpenGL 2D Texture *)
type t

(** [load file] is the texture stored in [file] *)
val load : string -> t

(** [bind target tex] binds [tex] to [target] *)
val bind : int -> t -> t

(** [free tex] frees [tex] *)
val free : t -> unit
