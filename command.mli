(** Converts strings to commands *)
open Types

(** [cmd_of_str str] parses [str] into a command *)
val cmd_of_str : string -> command
