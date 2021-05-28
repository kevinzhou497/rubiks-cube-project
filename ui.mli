(** The input commands the user can enter *)
open Types

module type UI = sig
  (** rep_type is the rep type of the Cube *)
  type rep_type

  type t

  (** [display rep] displays [rep] onto the ui *)
  val display : rep_type -> t -> t

  (** [wait_for_usr_cmd ()] waits for a user command and gets that
      command *)
  val wait_for_usr_cmd : t -> t * command

  (** [display_help_msg ()] displays some kind of user help message
      about proper usage of the ui *)
  val display_help_msg : t -> t

  (** [inform_usr msg] displays [msg] to the user *)
  val inform_usr : string -> t -> t

  (** makes a new ui *)
  val make_ui : unit -> t

  (** [push_rotations rots ui] displays the rotations [rots] on the ui *)
  val push_rotations : (face * rotation) list -> t -> t
end
