(** [Commander] is the type that manipulates the internal represenation
    from commands *)
module type Commander = sig
  open Types

  (** [t] is the cube representation *)
  type t

  (** [u] is the type of the Commander *)
  type u

  (** [command rep cmd] is [rep] after [command] has been performed. If
      the command cannot be performed returns Illegal otherwise Legal.
      If The commander does not handle the specified command, returns a
      legal move with no state changed*)
  val command : u -> t -> command -> t move * u

  (** [make ()] is a new Commander *)
  val make : unit -> u
end
