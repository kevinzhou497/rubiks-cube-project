open Types

type t = color array array

type side =
  | N
  | S
  | E
  | W

let width = 3

let height = 3

let dims rep = (width, height)

let add (a, b) (a1, b1) = (a + a1, b + b1)

(** [coords_face face] gets the coordinates of [face] *)
let coords_face = function
  | U -> (3, 0)
  | L -> (0, 3)
  | F -> (3, 3)
  | R -> (6, 3)
  | B -> (9, 3)
  | D -> (3, 6)
  | EmptyFace -> raise InvalidCubeExn

(** [color_face face] gets the default color of [face] *)
let color_face = function
  | U -> White
  | L -> Orange
  | F -> Green
  | R -> Red
  | B -> Blue
  | D -> Yellow
  | EmptyFace -> Black

(** [color_num color] gets the numeric index of [color] *)
let color_num = function
  | White -> 6
  | Orange -> 5
  | Green -> 4
  | Red -> 3
  | Blue -> 2
  | Yellow -> 1
  | Black -> raise InvalidCubeExn

(** [sort_color a b] sorts the colors a and b based on their numeric
    index by returning 1 if [a > b] else -1. Raises [InvalidCubeExn] if
    the colors are equal. *)
let sort_color a b =
  let x = color_num a in
  let y = color_num b in
  if x = y then raise InvalidCubeExn else if x > y then 1 else -1

(** [num_face face] gets the numeric index of [face] *)
let num_face = function
  | U -> 0
  | L -> 36
  | F -> 27
  | R -> 45
  | B -> 18
  | D -> 9
  | EmptyFace -> -1

(** [paint c rep] paints [rep] [c] *)
let paint c rep =
  let ul = coords_face c in
  let _ =
    for y = snd ul to snd ul + 2 do
      for x = fst ul to fst ul + 2 do
        rep.(y).(x) <- color_face c
      done
    done
  in
  rep

let make () =
  Array.make_matrix 9 12 Black
  |> paint U |> paint L |> paint F |> paint R |> paint B |> paint D

(** [coords_edge edge] is the coordinates of [edge] *)
let coords_edge = function
  | N -> [ (0, 0); (1, 0); (2, 0) ]
  | E -> [ (2, 0); (2, 1); (2, 2) ]
  | S -> [ (2, 2); (1, 2); (0, 2) ]
  | W -> [ (0, 2); (0, 1); (0, 0) ]

(** [get_side_colors rep] gets the side colors of [rep] *)
let get_side_colors (s, f) rep =
  let ul = coords_face f in
  let ls = coords_edge s in
  ls |> List.map (add ul) |> List.map (fun (x, y) -> rep.(y).(x))

(** [set_side_colors rep colors] sets the side color of [rep] *)
let set_side_colors (s, f) rep colors =
  let ul = coords_face f in
  let ls = coords_edge s in
  ls
  |> List.map (add ul)
  |> List.combine colors
  |> List.map (fun (c, (x, y)) -> rep.(y).(x) <- c)

let query pos rep =
  let _ = if pos.x > 2 || pos.y > 2 then failwith "3 by 3" else () in
  let x = pos.x + (coords_face pos.face |> fst) in
  let y = pos.y + (coords_face pos.face |> snd) in
  rep.(y).(x)

let set pos c rep =
  let _ = if pos.x > 2 || pos.y > 2 then failwith "3 by 3" else () in
  let x = pos.x + (coords_face pos.face |> fst) in
  let y = pos.y + (coords_face pos.face |> snd) in
  let _ = rep.(y).(x) <- c in
  rep

(** [splot list n] splits the elements of [list] From :
    https://ocaml.org/learn/tutorials/99problems.html *)
let split list n =
  let rec aux i acc = function
    | [] -> (List.rev acc, [])
    | h :: t as l ->
        if i = 0 then (List.rev acc, l) else aux (i - 1) (h :: acc) t
  in
  aux n [] list

(** [rotate_ls list n] rotates the element s in a list From :
    https://ocaml.org/learn/tutorials/99problems.html *)
let rotate_ls list n =
  let len = List.length list in
  (* Compute a rotation value between 0 and len - 1 *)
  let n = if len = 0 then 0 else ((n mod len) + len) mod len in
  if n = 0 then list
  else
    let a, b = split list n in
    b @ a

(**Implements 90 degree face rotation clockwise*)
let rotate_face face rep =
  let ul = coords_face face in
  let ls =
    [ (0, 0); (1, 0); (2, 0); (2, 1); (2, 2); (1, 2); (0, 2); (0, 1) ]
  in
  let cols =
    List.map
      (fun (a, b) ->
        let s, t = add (a, b) ul in
        rep.(t).(s))
      ls
  in
  let cols = rotate_ls cols 6 |> List.combine ls in
  let rec repaint = function
    | [] -> ()
    | ((a, b), c) :: t ->
        let x, y = add ul (a, b) in
        let _ = rep.(y).(x) <- c in
        repaint t
  in
  let _ = repaint cols in
  rep

(** [rotate_edges face rep] rotates the edges corresponding with a
    clockwise rotation of [face] *)
let rotate_edges f rep =
  let edges =
    match f with
    | U -> [ (N, B); (N, R); (N, F); (N, L) ]
    | L -> [ (W, U); (W, F); (W, D); (E, B) ]
    | F -> [ (S, U); (W, R); (N, D); (E, L) ]
    | R -> [ (E, U); (W, B); (E, D); (E, F) ]
    | B -> [ (N, U); (W, L); (S, D); (E, R) ]
    | D -> [ (S, F); (S, R); (S, B); (S, L) ]
    | EmptyFace -> raise InvalidCubeExn
  in
  let edge_colors =
    List.map (fun (a, b) -> get_side_colors (a, b) rep) edges
  in
  let comb = rotate_ls edge_colors 7 |> List.combine edges in
  let _ =
    List.map (fun ((s, f), ls) -> set_side_colors (s, f) rep ls) comb
  in
  rep

(** [rotate_cw face rep] rotates [face] clockwise *)
let rotate_cw f rep = rotate_face f rep |> rotate_edges f

(** [rtate face rotation rep] rotates [face] of [rep] in the direction
    of [rotation] *)
let rotate f rot rep =
  match rot with
  | Clockwise -> rotate_cw f rep
  | Counter -> rotate_cw f rep |> rotate_cw f |> rotate_cw f
  | Double -> rotate_cw f rep |> rotate_cw f

let corners =
  [
    [ (3, 3); (3, 2); (2, 3) ];
    [ (5, 3); (5, 2); (6, 3) ];
    [ (3, 5); (2, 5); (3, 6) ];
    [ (5, 5); (5, 6); (6, 5) ];
    [ (9, 3); (8, 3); (5, 0) ];
    [ (11, 3); (0, 3); (3, 0) ];
    [ (9, 5); (8, 5); (5, 8) ];
    [ (11, 5); (0, 5); (3, 8) ];
  ]

let edges =
  [
    [ (4, 3); (4, 2) ];
    [ (5, 4); (6, 4) ];
    [ (4, 5); (4, 6) ];
    [ (3, 4); (2, 4) ];
    [ (10, 3); (4, 0) ];
    [ (10, 5); (4, 8) ];
    [ (9, 4); (8, 4) ];
    [ (11, 4); (0, 4) ];
    [ (1, 3); (3, 1) ];
    [ (1, 5); (3, 7) ];
    [ (7, 3); (5, 1) ];
    [ (7, 5); (5, 7) ];
  ]

(** [get_sticker_corner (x, y) rep] gets the sticker number for a corner
    [(x, y)] *)
let get_sticker_corner (x, y) rep =
  let colors =
    corners
    |> List.find (fun ls -> List.mem (x, y) ls)
    |> List.map (fun (x, y) -> rep.(y).(x))
    |> List.sort sort_color
  in
  let color = rep.(y).(x) in
  match colors with
  | [ Green; Orange; White ] -> (
      match color with
      | Green -> 29
      | White -> 6
      | Orange -> 36
      | _ -> raise InvalidCubeExn)
  | [ Red; Green; White ] -> (
      match color with
      | Green -> 35
      | White -> 8
      | Red -> 51
      | _ -> raise InvalidCubeExn)
  | [ Yellow; Green; Orange ] -> (
      match color with
      | Green -> 27
      | Yellow -> 17
      | Orange -> 38
      | _ -> raise InvalidCubeExn)
  | [ Yellow; Red; Green ] -> (
      match color with
      | Green -> 33
      | Yellow -> 15
      | Red -> 53
      | _ -> raise InvalidCubeExn)
  | [ Blue; Red; White ] -> (
      match color with
      | Blue -> 20
      | White -> 2
      | Red -> 45
      | _ -> raise InvalidCubeExn)
  | [ Blue; Orange; White ] -> (
      match color with
      | Blue -> 26
      | White -> 0
      | Orange -> 42
      | _ -> raise InvalidCubeExn)
  | [ Yellow; Blue; Red ] -> (
      match color with
      | Blue -> 18
      | Yellow -> 9
      | Red -> 47
      | _ -> raise InvalidCubeExn)
  | [ Yellow; Blue; Orange ] -> (
      match color with
      | Blue -> 24
      | Yellow -> 11
      | Orange -> 44
      | _ -> raise InvalidCubeExn)
  | _ -> raise InvalidCubeExn

(** [get_sticker_edge (x, y) rep] gets the sticker number for an edge
    [(x, y)] *)
let get_sticker_edge (x, y) rep =
  let colors =
    edges
    |> List.find (fun ls -> List.mem (x, y) ls)
    |> List.map (fun (x, y) -> rep.(y).(x))
    |> List.sort sort_color
  in
  let color = rep.(y).(x) in
  match colors with
  | [ Red; White ] -> (
      match color with
      | White -> 5
      | Red -> 48
      | _ -> raise InvalidCubeExn)
  | [ Blue; White ] -> (
      match color with
      | White -> 1
      | Blue -> 23
      | _ -> raise InvalidCubeExn)
  | [ Orange; White ] -> (
      match color with
      | White -> 3
      | Orange -> 39
      | _ -> raise InvalidCubeExn)
  | [ Green; White ] -> (
      match color with
      | White -> 7
      | Green -> 32
      | _ -> raise InvalidCubeExn)
  | [ Yellow; Red ] -> (
      match color with
      | Yellow -> 12
      | Red -> 50
      | _ -> raise InvalidCubeExn)
  | [ Yellow; Blue ] -> (
      match color with
      | Yellow -> 10
      | Blue -> 21
      | _ -> raise InvalidCubeExn)
  | [ Yellow; Orange ] -> (
      match color with
      | Yellow -> 14
      | Orange -> 41
      | _ -> raise InvalidCubeExn)
  | [ Yellow; Green ] -> (
      match color with
      | Yellow -> 16
      | Green -> 30
      | _ -> raise InvalidCubeExn)
  | [ Red; Green ] -> (
      match color with
      | Red -> 52
      | Green -> 34
      | _ -> raise InvalidCubeExn)
  | [ Green; Orange ] -> (
      match color with
      | Green -> 28
      | Orange -> 37
      | _ -> raise InvalidCubeExn)
  | [ Blue; Orange ] -> (
      match color with
      | Blue -> 25
      | Orange -> 43
      | _ -> raise InvalidCubeExn)
  | [ Blue; Red ] -> (
      match color with
      | Blue -> 19
      | Red -> 46
      | _ -> raise InvalidCubeExn)
  | _ -> raise InvalidCubeExn

(** [get_sticker_center (x, y) rep] gets the sticker number for a center
    [(x, y)] *)
let get_sticker_center (x, y) rep =
  let color = rep.(y).(x) in
  match color with
  | White -> 4
  | Yellow -> 13
  | Green -> 31
  | Orange -> 40
  | Red -> 49
  | Blue -> 22
  | _ -> raise InvalidCubeExn

(** [get_sticker (x, y) rep] gets the sticker number for [(x, y)] in
    [rep] *)
let get_sticker (x, y) rep =
  if List.mem (x, y) (List.flatten corners) then
    get_sticker_corner (x, y) rep
  else if List.mem (x, y) (List.flatten edges) then
    get_sticker_edge (x, y) rep
  else get_sticker_center (x, y) rep

(** [nums rep] is the sticker numbers for each face *)
let nums rep =
  let ls = function
    | U ->
        [
          (0, 0);
          (1, 0);
          (2, 0);
          (0, 1);
          (1, 1);
          (2, 1);
          (0, 2);
          (1, 2);
          (2, 2);
        ]
    | F ->
        [
          (0, 2);
          (0, 1);
          (0, 0);
          (1, 2);
          (1, 1);
          (1, 0);
          (2, 2);
          (2, 1);
          (2, 0);
        ]
    | D ->
        [
          (2, 2);
          (1, 2);
          (0, 2);
          (2, 1);
          (1, 1);
          (0, 1);
          (2, 0);
          (1, 0);
          (0, 0);
        ]
    | R ->
        [
          (2, 0);
          (2, 1);
          (2, 2);
          (1, 0);
          (1, 1);
          (1, 2);
          (0, 0);
          (0, 1);
          (0, 2);
        ]
    | L ->
        [
          (2, 0);
          (2, 1);
          (2, 2);
          (1, 0);
          (1, 1);
          (1, 2);
          (0, 0);
          (0, 1);
          (0, 2);
        ]
    | B ->
        [
          (0, 2);
          (0, 1);
          (0, 0);
          (1, 2);
          (1, 1);
          (1, 0);
          (2, 2);
          (2, 1);
          (2, 0);
        ]
    | EmptyFace -> failwith "EmptyFace"
  in
  [ U; D; B; F; L; R ]
  |> List.map (fun a -> ls a |> List.map (add (coords_face a)))
  |> List.flatten
  |> List.map (fun (x, y) -> get_sticker (x, y) rep)

open Torch

let model = Module.load "Ruby.pt"

(** [state_to_ml state] converts [state] to a numerical representation
    to be used in the ML heuristic *)
let state_to_ml state =
  let a =
    nums state |> List.map (fun i -> i / 9) |> List.map float_of_int
  in
  Tensor.float_vec a

(** [cost state] is the cost of [state] using the ML heuristic *)
let cost state =
  let input = state_to_ml state in
  let out = Module.forward model [ input ] in
  Tensor.float_value out
