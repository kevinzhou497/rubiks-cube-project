(** The UI for the 3D windowed application *)
module MakeRenderer (Rep : Cube_rep.CubeRep) :
  Ui.UI with type rep_type = Rep.t
