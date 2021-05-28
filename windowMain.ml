(** Insertion point for window UI *)
open Main

module Program =
  MakeRubiksCubeSolver (ArrayCube) (Renderer.MakeRenderer (ArrayCube))

let _ = Program.start ()
