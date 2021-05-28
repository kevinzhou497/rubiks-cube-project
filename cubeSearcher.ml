open CubeSolver

module MakeSolver (Rep : Cube_rep.CubeRep) :
  Solver with type t = Rep.t = struct
  type t = Rep.t

  open Astar
  open Types
  open CubeUtils
  module Clone = MakeCopier (Rep)
  module Cmp = MakeComparator (Rep)
  module Printer = MakePrinter (Rep)

  let rotations =
    [
      (L, Clockwise);
      (L, Counter);
      (R, Clockwise);
      (R, Counter);
      (U, Clockwise);
      (U, Counter);
      (D, Clockwise);
      (D, Counter);
      (F, Clockwise);
      (F, Counter);
      (B, Clockwise);
      (B, Counter);
    ]

  let get_next_states cube prev_states =
    (* Maybe neet to check that old moves aren't revisited, A* seems to
       be doing this already? *)
    (*List.filter_map (fun m -> let st = Rep.rotate (fst m) (snd m)
      (Clone.cpy cube) in match List.find_opt (fun x -> Cmp.eq st x)
      prev_states with | Some _ -> None | None -> Some st) rotations *)
    List.map
      (fun (face, rot) -> Rep.rotate face rot (Clone.cpy cube))
      rotations

  let solve cube =
    let open Astar in
    let goal = Rep.make () in
    let cost cube goal = cube |> Rep.cost |> Float.abs in
    (* this is the cost function h(n) *)
    let problem = { cost; goal; get_next_states; eq = ( = ) } in
    let x = search problem cube None in
    print_endline "Done";
    x

  (* [find_helper cube1 cube2 move] does the rotation to see if cube1
     with the move will equal cube2 and is used in [find_move cube1
     cube2] to carry out its functionality*)
  let find_helper cube1 cube2 move =
    Rep.rotate (fst move) (snd move) (Clone.cpy cube1) = cube2

  (* [find_move cube1 cube2] takes in two cubes and finds the move that
     would make cube1 into cube2*)
  let find_move cube1 cube2 =
    List.find (find_helper cube1 cube2) rotations

  (* [cubes_to_moves cubes move_list] gets the list of moves [move_list
     @ cubes] that bring [List.hd cubes] to the last element in [cubes]*)
  let rec cubes_to_moves cubes move_list =
    match cubes with
    | x :: (y :: xs as tl) ->
        cubes_to_moves tl (move_list @ [ find_move x y ])
    | _ -> move_list

  let rec step_str = function
    | h :: t ->
        "Step:\n\n"
        ^ Printer.to_string abbrev_of_color "  " h
        ^ "\n\n" ^ step_str t
    | _ -> ""

  (* [get_moves cube] takes in a cube and gets the moves that are needed
     to solve the cube from its current state *)
  let get_moves cube =
    let c = Clone.cpy cube in
    let states = List.rev (solve c) in
    (* print_string (step_str states); *)
    cubes_to_moves states []

  let get_hint cube =
    let _, (face, rot) =
      List.fold_left
        (fun (cost, mv) (face, rotation) ->
          let cost', mv' =
            ( cube |> Clone.cpy
              |> Rep.rotate face rotation
              |> Rep.cost |> Float.abs,
              (face, rotation) )
          in
          if cost' < cost then (cost', mv') else (cost, mv))
        (Float.max_float, (F, Clockwise))
        rotations
    in
    (face, rot)
end
