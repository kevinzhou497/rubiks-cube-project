(** Scrollable list of rendered text messages *)
open Types

type t

type msg =
  | SolveMsg of face * rotation
  | InvalidCube
  | SolveFailed

(** makes a log *)
val make : Shader.t -> t

(** [offset_y off log] adds to the origin y-value of the log by [off] *)
val offset_y : float -> t -> t

(** [append_msg log] appends the msg [msg] to [log] *)
val append_msg : msg -> t -> t

(** [pop_msg msg_log] is [log] without the most recent message *)
val pop_msg : t -> t

(** [clr_msgs log] is [log] without any messages *)
val clr_msgs : t -> t

(** [render (scr_w, scr_h) log] renders [log] for a screen with width
    [scr_w] and [scr_h] *)
val render : int * int -> t -> t

(** frees a message log *)
val free : t -> unit
