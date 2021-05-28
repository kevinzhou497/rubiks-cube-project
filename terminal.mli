(** 2D text based UI *)
module MakeTerminal (Rep : Cube_rep.CubeRep) :
  Ui.UI with type rep_type = Rep.t
