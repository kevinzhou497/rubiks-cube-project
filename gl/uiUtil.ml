(** Utility functions for the UI *)
open Types

(** [color_to_shader_index color] is the integral represenation of
    [color] as used in the shader uniform array *)
let color_to_shader_index = function
  | White -> 0
  | Yellow -> 1
  | Red -> 2
  | Orange -> 3
  | Green -> 4
  | Blue -> 5
  | Black -> failwith "Invalid color"

(** [set_face w h i fc rep] sets the colors of [fc] in [rep] starting
    from index [i] to [h * w + w]*)
let rec set_face (shader, idx_of_face, dims, query) i fc rep =
  let w, h = dims rep in
  if i < w * h then (
    let x = i mod w in
    let y = i / w in
    let c = color_to_shader_index (query { x; y; face = fc } rep) in
    let fc_idx = idx_of_face fc in
    shader |> Shader.use
    |> Shader.set_i
         ("tileColors[" ^ string_of_int ((fc_idx * 9) + i) ^ "]")
         c
    |> ignore;
    set_face (shader, idx_of_face, dims, query) (i + 1) fc rep)
  else rep

(** [set_colors rep sh idx_of_fc] sets the colors of the shader [sh] to
    the colors specified by [rep]. Uses the function [idx_of_fc] to
    convert faces to indexes in the 2D color uniform array in the shader *)
let set_colors rep sh idx_of_fc dims query =
  let sh = (sh, idx_of_fc, dims, query) in
  rep |> set_face sh 0 F |> set_face sh 0 B |> set_face sh 0 U
  |> set_face sh 0 D |> set_face sh 0 L |> set_face sh 0 R |> ignore
