(** The screen for selecting colors in the 3D ui *)
open Tgl3

open Util
open UiUtil
open Types

type t = {
  (* shader is not owned by ColorSlect *)
  shader : Shader.t;
  rect : Rect.t;
  offs :
    (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t;
  size : int;
}

let color_num = 54

(** [gen_offsets spacing sz] generates the instanced offsets for the
    rectangles in the color select screen based on [spacing] and [sz] *)
let gen_offsets spacing sz =
  let ba = bigarray_create Bigarray.float32 (color_num * 2) in
  let rec add_face init_x init_y i init_idx =
    if i < sz * sz then begin
      let x = (float_of_int (i mod sz) *. spacing) +. init_x in
      let y_idx = sz - (i / sz) in
      let y = (float_of_int y_idx *. spacing) +. init_y in
      ba.{(i * 2) + init_idx} <- x;
      ba.{(i * 2) + 1 + init_idx} <- y;
      add_face init_x init_y (i + 1) init_idx
    end
    else ()
  in
  let left = ~-.0.9 in
  let middle = ~-.0.3 in
  let face_space = 0.05 in
  (* Left, Front, Right, Back, Up, Down *)
  add_face left middle 0 0;
  add_face ((3. *. spacing) +. left +. face_space) middle 0 (9 * 2);
  add_face
    ((6. *. spacing) +. left +. (2. *. face_space))
    middle 0 (18 * 2);
  add_face
    ((9. *. spacing) +. left +. (3. *. face_space))
    middle 0 (27 * 2);
  add_face
    ((3. *. spacing) +. left +. face_space)
    (middle +. 0.5) 0 (36 * 2);
  add_face
    ((3. *. spacing) +. left +. face_space)
    (middle -. 0.5) 0 (45 * 2);
  ba

let make shader sz =
  let offsets = gen_offsets 0.15 sz in
  let rect = Rect.make_instanced offsets (Rect.make ()) in
  { shader; rect; offs = offsets; size = sz }

let free cs = Rect.free cs.rect

(** index of the cube face *)
let idx_of_face = function
  | L -> 0
  | F -> 1
  | R -> 2
  | B -> 3
  | U -> 4
  | D -> 5
  | EmptyFace -> failwith "EmptyFace"

(** face of a cube face index *)
let face_of_idx = function
  | 0 -> L
  | 1 -> F
  | 2 -> R
  | 3 -> B
  | 4 -> U
  | 5 -> D
  | _ -> failwith "Not a face"

let update_colors rep cs (dims, query) =
  set_colors rep cs.shader idx_of_face dims query

let model = Matrix.scale 0.05

let rect_width = 0.05

let rect_height = 0.05

(** [select cursor offsets] is the position (x, y) of the rectangle that
    overlaps [cursor] or [None] if none of them overlap *)
let select (cur_x, cur_y) ba =
  let rec colliding_rect idx =
    if idx < color_num then
      let x = ba.{idx * 2} in
      let y = ba.{(idx * 2) + 1} in
      if
        x -. cur_x <= rect_width
        && cur_x -. x <= rect_width
        && cur_y -. y <= rect_height
        && y -. cur_y <= rect_height
      then Some (x, y, idx)
      else colliding_rect (idx + 1)
    else None
  in
  colliding_rect 0

(** [render cs] shows the color select menu [cs]. Temporally coupled
    with [highlight] and must be called after it *)
let render cs =
  cs.shader |> Shader.use
  |> Shader.set_b "instanced" true
  |> Shader.set_mat4 "model" model
  |> Shader.set_b "select" false
  |> ignore;
  Rect.render_instanced 54 cs.rect |> ignore;
  cs.shader |> Shader.set_b "instanced" false |> ignore

(** [highlight cs cursor] highlights a specific block in cs depending on
    the (x, y) pair [cursor] in NDC. Temporally linked with render, must
    be called before render *)
let highlight cs cursor =
  match select cursor cs.offs with
  | None -> ()
  | Some (x, y, _) ->
      let mat =
        Matrix.scale 0.06 |> Matrix.mul (Matrix.translate x y 0.)
      in
      cs.shader |> Shader.use
      |> Shader.set_b "select" true
      |> Shader.set_mat4 "model" mat
      |> ignore;
      Rect.render cs.rect |> ignore

(** [tile_pos_from_cursor cs cursor] is the x, y and face of the
    currently highlighted tile or None *)
let tile_pos_from_cursor cs cursor =
  match select cursor cs.offs with
  | None -> None
  | Some (_, _, idx) ->
      let fc = face_of_idx (idx / 9) in
      let x = idx mod 9 mod cs.size in
      let y = idx mod 9 / cs.size in
      Some { x; y; face = fc }
