(** Wires together modules into a complete program *)

(** A Rubik's Cube Solver Program *)
module type RubiksCubeSolver = sig
  (** [start ()] starts the Program *)
  val start : unit -> unit
end

module MakeRubiksCubeSolver
    (Rep : Cube_rep.CubeRep)
    (Display : Ui.UI with type rep_type = Rep.t) : RubiksCubeSolver =
struct
  open Action
  open Types
  open CubeSearcher
  module Controller = MakeCommander (Rep)
  module Solver = MakeSolver (Rep)

  let init_state = Rep.make ()

  (** [illegal_mv_msg ()] informs the user that an illegal move was
      made\n *)
  let illegal_mv_msg ui =
    ui
    |> Display.inform_usr "Illegal move\n"
    |> Display.inform_usr
         "Ensure that you have the correct amount of parameters for \
          the command\n"
    |> Display.inform_usr
         "or the parameters are within the correct range\n"

  (** [state_from_move ui mv] gets the state associated with the move
      [mv] Displays an error message if [mv] is Illegal *)
  let state_from_move ui = function
    | Legal st, control -> (ui, st, control)
    | Illegal st, control -> (illegal_mv_msg ui, st, control)

  (** [solve_rots_to_ui ui cmd st] is [ui] updated with the solve
      command [cmd] to solve [st] *)
  let solve_rots_to_ui display cmd st =
    let moves =
      match cmd with
      | Solve -> Solver.get_moves st
      | SolveStep -> [ Solver.get_hint st ]
      | _ -> []
    in
    Display.push_rotations moves display

  (** [main ui_state] is the main "loop" of the program. It displays
      [state], wait for user input and handles that input accordingly,
      calling itself recursively to advance the state of the program *)
  let rec main ui_state =
    let ui, state, control = ui_state in
    match Display.wait_for_usr_cmd (Display.display state ui) with
    | _, Quit -> ()
    | display, Invalid msg ->
        main (Display.inform_usr (msg ^ "\n") display, state, control)
    | display, Help ->
        main (Display.display_help_msg display, state, control)
    | ((_, Set _) as c)
    | ((_, SetFace _) as c)
    | ((_, Rotate _) as c)
    | ((_, Pass) as c)
    | ((_, Reset) as c)
    | ((_, Scramble) as c)
    | ((_, Undo) as c) ->
        dispatch_command c state control
    | ((_, Solve) as c) | ((_, SolveStep) as c) ->
        solve_cube c state control

  (** [dispatch_command display_cmd_pair state controller] dispatches a
      command to [state] and calls [main] *)
  and dispatch_command (display, cmd) st control =
    Controller.command control st cmd |> state_from_move display |> main

  (** [solve_cube display_cmd_pair state controller] solves the cube and
      calls [main] *)
  and solve_cube (display, cmd) st control =
    let ui =
      try solve_rots_to_ui display cmd st with
      | InvalidCubeExn -> Display.inform_usr "invalid" display
      | Not_found -> Display.inform_usr "failed" display
    in
    main (ui, st, control)

  let start () =
    let ui =
      Display.inform_usr
        "3110 Rubiks Cube Solver\nType \"help\" for information\n"
        (Display.make_ui ())
    in
    main (ui, init_state, Controller.make ())
end
