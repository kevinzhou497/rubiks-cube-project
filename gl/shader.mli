(** An OpenGL shader *)
type t

(** [load vertexSource fragmentSource] is the shader stored in the file
    [vertexSource] and [fragmentSource] *)
val load : string -> string -> t option

(** [free t] cleans up resources for the shader *)
val free : t -> unit

(** [use shader] enables [shader] *)
val use : t -> t

(** [set_mat4 uniform flts shader] sets the shader uniform [uniform] to
    the flattened 4x4 matrix denoted by [flts] in [shader]. Requires
    [shader] to be in use and [flts] to have length 16 *)
val set_mat4 : string -> float array -> t -> t

(** [set_vec3f uniform vec shader] sets the shader uniform [uniform] to
    the float vector [vec] in [shader]. Requires [shader] to be in use *)
val set_vec3f : string -> float * float * float -> t -> t

(** [set_i uniform val shader] is [shader] with the integer uniform
    [uniform] set to [val]*)
val set_i : string -> int -> t -> t

(** [set_b uniform val shader] is [shader] with boolean uniform
    [uniform] set to [val] *)
val set_b : string -> bool -> t -> t
