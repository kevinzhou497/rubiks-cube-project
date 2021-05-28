open Types

(** [t] is the Rubiks cube [({x, y, face}, color)...] where each element
    represents the tile [{x, y, face}] with color [color] There can only
    be one [color] for each tile and the size can only be [width] *
    [height] - 1 *)
type t = (tile_pos * color) list

let width = 3

let height = 3

let query = List.assoc

let set v c rep = (v, c) :: List.remove_assoc v rep

let dims rep = (width, height)

(** [make_face face color] creates a rubiks cube face where every tile
    is [color] *)
let make_face face color =
  let rec add_ith_tile i lst =
    if i < width * height then
      ({ face; x = i mod width; y = i / width }, color)
      :: add_ith_tile (i + 1) lst
    else lst
  in
  add_ith_tile 0 []

let make () =
  List.flatten
    [
      make_face U White;
      make_face L Orange;
      make_face F Green;
      make_face R Red;
      make_face B Blue;
      make_face D Yellow;
    ]

let rotate fc rot rep = failwith "Rotate unimplemented for list cube"

let cost _ = failwith "Heurstic unimplemented"
