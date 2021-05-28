(** A Controller for a cube representation *)
module MakeCommander (Rep : Cube_rep.CubeRep) :
  Controller.Commander with type t = Rep.t
