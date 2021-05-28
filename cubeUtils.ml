(** Utility functions for general cube representations *)

(** Copy performs a deep copy on a cube rep *)
module type Copy = sig
  type t

  (** [cpy rep] is a copy of [rep] that works with mutable and immutable
      representations *)
  val cpy : t -> t
end

(** Tests for equality between two cube reps *)
module type Equality = sig
  type t

  open Types

  (** [eq cube1 cube2] is true if [cube1] has the same sticker colors as
      [cube2] *)
  val eq : t -> t -> bool

  (** [str_eq f c1 c2] is empty if [c1 = c2] otherwise it is a string
      representation of [c1] followed by [c2]. Color to string
      conversions are done by converting the color to the first letter
      of its name*)
  val str_eq : t -> t -> string
end

(** Converts a cube rep to a string *)
module type Printer = sig
  type t

  open Types

  (** [to_string str_of_color space_str rep] is the string
      representation of [rep] using [str_of_color] to convert colors to
      strings and [space_str] to represent spaces between tiles *)
  val to_string : (color -> string) -> string -> t -> string
end

module MakeCopier (Rep : Cube_rep.CubeRep) : Copy with type t = Rep.t =
struct
  type t = Rep.t

  (** [deep_cpy_face face src dst] copies sets [face] of [dst] according
      to the colors of [src] *)
  let deep_cpy_face face src dst =
    let w, h = Rep.dims dst in
    let rec cp_face i sz dst =
      if i < sz then
        let x = i mod w in
        let y = i / w in
        let dst' =
          Rep.set { x; y; face } (Rep.query { x; y; face } src) dst
        in
        cp_face (i + 1) sz dst'
      else dst
    in
    cp_face 0 (w * h) dst

  let cpy rep =
    Rep.make () |> deep_cpy_face F rep |> deep_cpy_face B rep
    |> deep_cpy_face L rep |> deep_cpy_face R rep |> deep_cpy_face D rep
    |> deep_cpy_face U rep
end

module MakePrinter (Rep : Cube_rep.CubeRep) :
  Printer with type t = Rep.t = struct
  type t = Rep.t

  open Types

  (** [print_tile_or_space rep cur_pos] displays the tile of [cur_pos]
      or a space if [cur_pos] is in the EmtpyFace*)
  let print_tile_or_space rep conf cur_pos =
    let str_of_col, space = conf in
    match cur_pos.face with
    | EmptyFace -> space
    | _ ->
        let cur_col = Rep.query cur_pos rep in
        str_of_col cur_col ^ " "

  (** [print_row rep cur_pos max_x] displays an x row of tiles starting
      with [cur_pos] and ending at [max_x] *)
  let rec print_row rep conf cur_pos max_x =
    if cur_pos.x < max_x then
      print_tile_or_space rep conf cur_pos
      ^ print_row rep conf { cur_pos with x = cur_pos.x + 1 } max_x
    else ""

  (** [print_tile_line rep y face_lst max_x] displays the [y]th row of
      each face in [face_lst] in that order on a single line. *)
  let rec print_tile_line rep conf y face_lst max_x =
    match face_lst with
    | h :: t ->
        print_row rep conf { x = 0; y; face = h } max_x
        ^ print_tile_line rep conf y t max_x
    | _ -> ""

  (** [print_faces_helper rep face_lst max_y max_x] the faces in
      [face_lst] starting with [(0, 0)] and ending with [(max_x, max_y)]
      in order of how they are listed in [face_lst] *)
  let print_faces_helper rep conf face_lst max_y max_x =
    let rec print_lines cur_y =
      if cur_y < max_y then
        print_tile_line rep conf cur_y face_lst max_x
        ^ "\n\n"
        ^ print_lines (cur_y + 1)
      else ""
    in
    print_lines 0

  (** [print_faces rep face_lst] displays the faces in [face_lst] of the
      cube [rep] in order from left to right*)
  let print_faces rep conf face_lst =
    let size = Rep.dims rep in
    print_faces_helper rep conf face_lst (snd size) (fst size)

  let to_string color_conv space rep =
    let config = (color_conv, space) in
    print_faces rep config [ EmptyFace; U ]
    ^ "\n"
    ^ print_faces rep config [ L; F; R; B ]
    ^ "\n"
    ^ print_faces rep config [ EmptyFace; D ]
end

module MakeComparator (Rep : Cube_rep.CubeRep) :
  Equality with type t = Rep.t = struct
  open Types

  type t = Rep.t

  module Printer = MakePrinter (Rep)

  (** [is_face_equal actual exp face] is true if [exp] and [actual] are
      equal for [face]*)
  let is_face_equal actual exp face =
    let sz = Rep.dims actual in
    let sz2 = Rep.dims exp in
    let rec check_ith_tile i =
      let x = i mod fst sz in
      let y = i / fst sz in
      let v = { x; y; face } in
      if Rep.query v actual = Rep.query v exp then
        if i + 1 < fst sz * snd sz then check_ith_tile (i + 1) else true
      else false
    in
    if sz = sz2 then check_ith_tile 0 else false

  (** [eq actual exp] is true if [actual] and [exp] represent the same
      cube *)
  let eq actual exp =
    let faces = [ F; U; L; R; B; D ] in
    let rec are_faces_eq = function
      | h :: t -> is_face_equal actual exp h && are_faces_eq t
      | [] -> true
    in
    are_faces_eq faces

  let str_eq a b =
    if eq a b then ""
    else
      "\n\n"
      ^ Printer.to_string abbrev_of_color "  " a
      ^ "\n\n\n\n"
      ^ Printer.to_string abbrev_of_color "  " b
end
