module MakeCommander (Rep : Cube_rep.CubeRep) :
  Controller.Commander with type t = Rep.t = struct
  open Types
  open CubeUtils
  module Copier = MakeCopier (Rep)

  type t = Rep.t

  type u = Rep.t list

  (** [is_valid_tile tile rep] is whether or not [tile] is valid for
      [rep]. A valid tile is one whose x and y positions are within the
      bounds of [rep] and whose face is not EmptyFace *)
  let is_valid_tile tile rep =
    let sz = Rep.dims rep in
    tile.x >= 0
    && tile.x < fst sz
    && tile.y >= 0
    && tile.y < snd sz
    && tile.face <> EmptyFace

  (** [is_valid_color_lst lst rep] is whether or not [lst] is valid for
      [rep]. A valid color list is of length [(fst dims) * (snd dims)]
      and only contains valid tiles *)
  let is_valid_color_lst lst rep =
    let sz = Rep.dims rep in
    List.length lst = fst sz * snd sz

  (** [set_multi_helper rep lst] is [rep] with all tiles as keys in
      [lst] changed to their colors which are the values in [lst]*)
  let rec set_multi_helper rep = function
    | h :: t ->
        if is_valid_tile (fst h) rep then
          set_multi_helper (Rep.set (fst h) (snd h) rep) t
        else failwith "Invalid Tile"
    | _ -> rep

  (** [set_multi rep lst] is [rep] with all the tiles specified in [lst]
      set to their new value in [lst] *)
  let set_multi rep lst =
    try Legal (set_multi_helper rep lst) with _ -> Illegal rep

  (** [set_face face color rep] sets the entire [face] of [rep] to
      [color] *)
  let set_face face color rep =
    let w, h = Rep.dims rep in
    let rec set_i i r =
      if i < w * h then
        let x = i mod w in
        let y = i / w in
        set_i (i + 1) (Rep.set { x; y; face } color r)
      else r
    in
    set_i 0 rep

  (** [reset_cube rep] sets the cube [rep] to its default state *)
  let reset_cube rep =
    rep |> set_face F Green |> set_face B Blue |> set_face D Yellow
    |> set_face L Orange |> set_face R Red |> set_face U White

  (** [rand_face ()] is a random cube face *)
  let rand_face () =
    match Random.int 6 with
    | 0 -> F
    | 1 -> B
    | 2 -> R
    | 3 -> L
    | 4 -> D
    | 5 -> U
    | _ -> failwith "Random.int postcondition violated"

  (** [rand_roation ()] is a random rotation *)
  let rand_rotation () =
    match Random.int 2 with
    | 0 -> Clockwise
    | 1 -> Counter
    | _ -> failwith "Random.int postcondition violated"

  (** [rand_rotate i count rep] rotates [rep] with some random series of
      rotations [count - i] times *)
  let rec rand_rotate i count rep =
    if i < count then
      rep
      |> Rep.rotate (rand_face ()) (rand_rotation ())
      |> rand_rotate (i + 1) count
    else rep

  (** [scramble rep] scrambles [rep] by performing 1 to 50 random
      rotations *)
  let scramble rep =
    let rotations = Random.int 50 + 1 in
    rand_rotate 0 rotations rep

  let make () = []

  (** [undo_mv rep mv_stack] is a move and move-stack pair where the
      move-stack is popped if it is not empty. If the move stack is
      empty [rep] is returned *)
  let undo_mv rep = function
    | h :: t -> (Legal h, t)
    | _ -> (Legal rep, [])

  let command mv_stk rep = function
    | Set tile ->
        if is_valid_tile (fst tile) rep then
          ( Legal (Rep.set (fst tile) (snd tile) rep),
            Copier.cpy rep :: mv_stk )
        else (Illegal rep, mv_stk)
    | SetFace lst ->
        if is_valid_color_lst lst rep then
          (set_multi rep lst, Copier.cpy rep :: mv_stk)
        else (Illegal rep, mv_stk)
    | Rotate (face, rotation) ->
        (Legal (Rep.rotate face rotation rep), Copier.cpy rep :: mv_stk)
    | Reset -> (Legal (reset_cube rep), Copier.cpy rep :: mv_stk)
    | Scramble -> (Legal (scramble rep), Copier.cpy rep :: mv_stk)
    | Undo -> undo_mv rep mv_stk
    | _ -> (Legal rep, mv_stk)
end
