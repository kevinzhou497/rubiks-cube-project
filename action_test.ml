(** Creates a test suite for the controller for the representation Rep *)
module MakeControllerTest (Rep : Cube_rep.CubeRep) = struct
  open OUnit2
  open Command
  open Types
  open Action
  open CubeSearcher
  open CubeUtils
  module Controller = MakeCommander (Rep)
  module Solver = MakeSolver (Rep)
  module Cloner = MakeCopier (Rep)
  module Cmp = MakeComparator (Rep)

  (** [print_str x] is [x] to allow printing a string value in OUnit
      tests *)
  let print_str x = x

  (** [state_tst_helper name state cmd modifications] creates a test
      called [name] which ensures that [cmd] manipulates [state] to
      produce [(modifications state)] *)
  let state_tst_helper name state cmd modifications =
    name >:: fun _ ->
    match
      Controller.command (Controller.make ()) (Cloner.cpy state) cmd
    with
    | Legal x, _ ->
        assert_equal
          (Cmp.str_eq x (modifications (Cloner.cpy state)))
          "" ~printer:print_str
    | Illegal _, _ -> failwith "Illegal move"

  (** [apply_cmds st cmds] applies all of the commands, from left to
      right in [cmds] to [st]. If any command fails, an error is raised *)
  let rec apply_cmds st = function
    | h :: t -> (
        match Controller.command (Controller.make ()) st h with
        | Legal x, _ -> apply_cmds x t
        | Illegal _, _ -> failwith "Application of illegal command")
    | _ -> st

  (** [cmd_eq_helper name init state cmds_a cmds_b] is a test that
      asserts that the state after applying [cmds_a] is the same state
      after applying [cmds_b] *)
  let cmd_eq_helper name init cmds_a cmds_b =
    let a = apply_cmds (Cloner.cpy init) cmds_a in
    let b = apply_cmds (Cloner.cpy init) cmds_b in
    name >:: fun _ ->
    assert_equal (Cmp.str_eq a b) "" ~printer:print_str

  (** [solve_tst_helper name state] is an ounit test to ensure that
      Solving [state] gets back moves which transform it to the default
      cube *)
  let solve_tst_helper name state =
    let rots = List.map (fun x -> Rotate x) (Solver.get_moves state) in
    let maybe_solved = apply_cmds state rots in
    name >:: fun _ ->
    assert_equal
      (Cmp.str_eq maybe_solved (Rep.make ()))
      "" ~printer:print_str

  let set_test =
    let init = Rep.make () in
    let set_tile = { x = 2; y = 1; face = D } in
    [
      state_tst_helper "set single" init
        (Set ({ x = 0; y = 0; face = U }, Green))
        (fun st -> Rep.set { x = 0; y = 0; face = U } Green st);
      state_tst_helper "set multi" init
        (SetFace
           [
             ({ x = 0; y = 0; face = U }, Green);
             ({ x = 1; y = 0; face = U }, Blue);
             ({ x = 2; y = 0; face = U }, Yellow);
             ({ x = 0; y = 1; face = U }, Green);
             ({ x = 1; y = 1; face = U }, Yellow);
             ({ x = 2; y = 1; face = U }, White);
             ({ x = 1; y = 2; face = U }, Red);
             ({ x = 1; y = 2; face = U }, Orange);
             ({ x = 2; y = 2; face = U }, Blue);
           ])
        (fun st ->
          st
          |> Rep.set { x = 0; y = 0; face = U } Green
          |> Rep.set { x = 1; y = 0; face = U } Blue
          |> Rep.set { x = 2; y = 0; face = U } Yellow
          |> Rep.set { x = 0; y = 1; face = U } Green
          |> Rep.set { x = 1; y = 1; face = U } Yellow
          |> Rep.set { x = 2; y = 1; face = U } White
          |> Rep.set { x = 1; y = 2; face = U } Red
          |> Rep.set { x = 1; y = 2; face = U } Orange
          |> Rep.set { x = 2; y = 2; face = U } Blue);
      cmd_eq_helper "set overrides" init
        [
          Set (set_tile, Green);
          Set (set_tile, White);
          Set (set_tile, Red);
        ]
        [ Set (set_tile, Red) ];
    ]

  let rotate_test st =
    [
      cmd_eq_helper "clockwise undoes counter" st
        [ Rotate (D, Counter); Rotate (D, Clockwise) ]
        [];
      cmd_eq_helper "clockwise-counter" st
        [
          Rotate (B, Clockwise);
          Rotate (B, Clockwise);
          Rotate (B, Clockwise);
        ]
        [ Rotate (B, Counter) ];
      cmd_eq_helper "dbl clockwise" st
        [ Rotate (L, Clockwise); Rotate (L, Clockwise) ]
        [ Rotate (L, Counter); Rotate (L, Counter) ];
      cmd_eq_helper "identical transforms" st
        [
          Rotate (F, Clockwise);
          Rotate (B, Counter);
          Rotate (F, Clockwise);
          Rotate (F, Counter);
        ]
        [
          Rotate (B, Counter);
          Rotate (F, Counter);
          Rotate (F, Clockwise);
          Rotate (F, Clockwise);
        ];
      cmd_eq_helper "identical transforms 2" st
        [
          Rotate (R, Clockwise);
          Rotate (L, Counter);
          Rotate (R, Clockwise);
        ]
        [
          Rotate (L, Counter);
          Rotate (R, Clockwise);
          Rotate (R, Clockwise);
        ];
    ]

  let rotate_tests =
    let init = Rep.make () in
    List.flatten
      [
        rotate_test init;
        rotate_test (apply_cmds init [ Scramble ]);
        rotate_test (apply_cmds init [ Scramble; Scramble; Scramble ]);
        rotate_test (apply_cmds init [ Scramble; Scramble; Scramble ]);
        rotate_test
          (apply_cmds init [ Scramble; Scramble; Scramble; Scramble ]);
      ]

  let solve_tests () =
    let solved = Rep.make () in
    [
      solve_tst_helper "Single rotation"
        (apply_cmds (Cloner.cpy solved) [ Rotate (F, Clockwise) ]);
      solve_tst_helper "Two rotations"
        (apply_cmds (Cloner.cpy solved)
           [ Rotate (L, Counter); Rotate (B, Clockwise) ]);
      solve_tst_helper "Three rotations"
        (apply_cmds (Cloner.cpy solved)
           [
             Rotate (F, Clockwise);
             Rotate (L, Counter);
             Rotate (D, Clockwise);
           ]);
      solve_tst_helper "More complex solve"
        (apply_cmds (Cloner.cpy solved)
           [
             Rotate (L, Counter);
             Rotate (D, Clockwise);
             Rotate (U, Counter);
             Rotate (R, Clockwise);
             Rotate (L, Clockwise);
             Rotate (F, Counter);
           ]);
    ]

  let controller_suite =
    "controller test suite"
    >::: List.flatten [ set_test; rotate_tests; solve_tests () ]
end
