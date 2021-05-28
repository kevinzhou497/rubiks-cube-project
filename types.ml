(** Types used to communicate between UI and solver *)

(** [color] is the color of the cube tile *)
type color =
  | White
  | Orange
  | Yellow
  | Red
  | Blue
  | Green
  | Black
      (** Black is not actually a Rubiks cube color, denotes empty *)

(** [face] is the face of the cube *)
type face =
  | U (* Up/White*)
  | L (* Left/Orange *)
  | F (* Front/Green *)
  | R (* Right/Red *)
  | B (* Back/Blue *)
  | D (* Down/Yellow *)
  | EmptyFace

(** a move *)
type 'a move =
  | Legal of 'a
  | Illegal of 'a

(** representation of a tile on the cube *)
type tile_pos = {
  x : int;
  y : int;
  face : face;
}

type rotation =
  | Clockwise
  | Counter
  | Double

type command =
  | Quit
  | Set of (tile_pos * color)
  | SetFace of (tile_pos * color) list
  | Solve
  | SolveStep
  | Rotate of (face * rotation)
  | Invalid of string
  | Help
  | Reset
  | Pass
  | Scramble
  | Undo

(* cmd to do nothing*)

(** [string_of_face face] is a string representation of [face] *)
let string_of_face = function
  | U -> "U"
  | F -> "F"
  | B -> "B"
  | D -> "D"
  | L -> "L"
  | R -> "R"
  | EmptyFace -> "Empty"

(** [face_of_string str] is [str] converted to a face *)
let face_of_string = function
  | "U" -> U
  | "F" -> F
  | "B" -> B
  | "D" -> D
  | "L" -> L
  | "R" -> R
  | _ -> failwith "Unknown Face"

(** [string_of_color color] is the full color name of [color] *)
let string_of_color = function
  | White -> "White"
  | Orange -> "Orange"
  | Yellow -> "Yellow"
  | Red -> "Red"
  | Blue -> "Blue"
  | Green -> "Green"
  | Black -> "Black"

(** [abbrev_of_color color] is the abbreviated string color name of
    [color] *)
let abbrev_of_color = function
  | White -> "W"
  | Orange -> "O"
  | Yellow -> "Y"
  | Red -> "R"
  | Blue -> "B"
  | Green -> "G"
  | Black -> "Bl"

(** [color_of_string str] is the color variant from the string [str]
    [str] can be the full color name or the first letter *)
let color_of_string = function
  | "White" | "W" -> White
  | "Orange" | "O" -> Orange
  | "Yellow" | "Y" -> Yellow
  | "Red" | "R" -> Red
  | "Blue" | "B" -> Blue
  | "Green" | "G" -> Green
  | "Black" -> Black
  | _ -> failwith "Unknown Color"

(** [str_of_rotation rot] is the symbol appended to the face string to
    denote a specific rotation Ex. A 180 degree rotation is denoted as 2*)
let str_of_rotation = function
  | Clockwise -> ""
  | Counter -> "'"
  | Double -> "2"

(** [rotation_of_str str] is the rotation variant representation of the
    character symbol appended to a face character to denoating a
    rotation. Ex. ' Denotes a counterclockwise rotation*)
let rotation_of_str = function
  | "'" -> Counter
  | "2" -> Double
  | "" -> Clockwise
  | _ -> failwith "Unknown rotation"

exception InvalidCubeExn
