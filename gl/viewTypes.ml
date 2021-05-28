type axis =
  | XAxis
  | YAxis

(** [view_command] is a command to control the view of the renderer *)
type view_command =
  | Nop
  | RotateView of float * axis
  | ResetView
  | ShowHelp
  | ShowColorSelect
  | ShowWait
