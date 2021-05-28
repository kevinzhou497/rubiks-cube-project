(** Performs A* on a cube representation *)
open CubeSolver

module MakeSolver (Rep : Cube_rep.CubeRep) : Solver with type t = Rep.t
