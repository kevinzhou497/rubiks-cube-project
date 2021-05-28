(** Insertion point for terminal UI *)
open Main

module Program =
  MakeRubiksCubeSolver (ArrayCube) (Terminal.MakeTerminal (ArrayCube))

let _ = Program.start ()
