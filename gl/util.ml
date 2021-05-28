(** Utility functions for OpenGL bindings *)
open Tgl3

(************* Taken from tgls documentation *************************
  Copyright (c) 2013 Daniel C. Bünzli Released under an ISC license
  Permission to use, copy, or modify with attribution and without
  warranty

  *********************************************************************)
let bigarray_create k len = Bigarray.(Array1.create k c_layout len)

(** [get_int f] gets an integer from a function that uses an output
    pointer. Compatability for C bindings which use output parameters *)
let get_int f =
  let a = bigarray_create Bigarray.int32 1 in
  f a;
  Int32.to_int a.{0}

(** [get_string len f] gets a string of length [len] from a function [f]
    which returns its value in an output pointer. Compatability for C
    functions *)
let get_string len f =
  let a = bigarray_create Bigarray.char len in
  f a;
  Gl.string_of_bigarray a

let set_int f i =
  let a = bigarray_create Bigarray.int32 1 in
  a.{0} <- Int32.of_int i;
  f a

(***************** End (the rest of this file is ours) ***********)

(** [parse_line ic] reads the next nonempty, noncomment line from the
    file [ic]. Returns empty string and closes [ic] if there is no more
    left to read*)
let rec parse_line ic =
  try
    let line = input_line ic |> String.trim in
    if
      String.length line < 2
      || (String.length line >= 2 && line.[0] = '/' && line.[1] = '/')
    then parse_line ic
    else line
  with
  | End_of_file ->
      close_in_noerr ic;
      ""
  | e ->
      close_in_noerr ic;
      raise e

(** [fill_bigarray ba idx lst] fills [ba] starting from index [idx] with
    data in [lst] after trimming and converting to float*)
let rec fill_bigarray ba idx = function
  | h :: t when String.length (String.trim h) = 0 ->
      fill_bigarray ba idx t
  | h :: t ->
      ba.{idx} <- h |> String.trim |> float_of_string;
      fill_bigarray ba (idx + 1) t
  | [] -> idx

(** [parse_data ba f idx] parses the file [f] and reads the data into
    [ba] starting from index [idx] *)
let rec parse_data ba f idx =
  let ln = parse_line f in
  match ln with
  | "" -> idx
  | x ->
      String.split_on_char ',' x
      |> fill_bigarray ba idx |> parse_data ba f

(** [load_bigarray file] loads the vertex data from [file] into a
    bigarray *)
let load_bigarray file =
  let ic = open_in file in
  let sz = int_of_string (parse_line ic) in
  let data = bigarray_create Bigarray.float32 sz in
  let read = parse_data data ic 0 in
  if read <> sz then
    let _ =
      print_endline
        ("Read: " ^ string_of_int read ^ " but expected: "
       ^ string_of_int sz)
    in
    data
  else data
