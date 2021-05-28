(** Graphics Frambebuffer *)

type t

(** [make width height] is a new multisampled fbo of dimensions
    [width * height] *)
val make : int -> int -> t

(** [free fbo] cleans up [fbo] *)
val free : t -> unit

(** [bind_for_writing fbo] is [fbo] bound for writing. Future draw calls
    will be performed on this fbo *)
val bind_for_writing : t -> t

(** [bind_for_reading tex fbo] is [fbo] bound for reading on texture
    [tex] *)
val bind_for_reading : int -> t -> t

(** [resize width height fbo] is a [fbo] resized to [w by h] *)
val resize : int -> int -> t -> t
