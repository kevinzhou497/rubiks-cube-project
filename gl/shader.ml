open Tgl3
open Util

(** [read_all_of_file file] reads the contents of an entire txt file
    [file] *)
let read_all_of_file file =
  let ic = open_in file in
  let rec read_all cur_data =
    try
      let line = input_line ic in
      read_all (cur_data ^ "\n" ^ line)
    with
    | End_of_file ->
        close_in_noerr ic;
        cur_data
    | e ->
        close_in_noerr ic;
        raise e
  in
  read_all ""

type t = int

(** [compile_shader src typ] is the compiled code of [src] for the
    specified type [typ] or None Effect: Displays a compilation error if
    fails*)
let compile_shader src typ file =
  let shader = Gl.create_shader typ in
  Gl.shader_source shader src;
  Gl.compile_shader shader;
  if get_int (Gl.get_shaderiv shader Gl.compile_status) = Gl.false_ then (
    let len = get_int (Gl.get_shaderiv shader Gl.info_log_length) in
    let err = get_string len (Gl.get_shader_info_log shader len None) in
    Gl.delete_shader shader;
    print_endline
      ("\x1b[31mShader \"" ^ file ^ "\" failed to compile:\x1b[0m");
    print_endline err;
    None)
  else Some shader

(** [link_program vert frag] is the linked shader program from compiled
    shaders [vert] and [frag] *)
let link_program vert frag =
  let program = Gl.create_program () in
  Gl.attach_shader program vert;
  Gl.attach_shader program frag;
  Gl.link_program program;
  Gl.delete_shader vert;
  Gl.delete_shader frag;
  if get_int (Gl.get_programiv program Gl.link_status) = Gl.false_ then (
    let len = get_int (Gl.get_programiv program Gl.info_log_length) in
    let err =
      get_string len (Gl.get_program_info_log program len None)
    in
    print_endline "\x1b[31mShader failed to link:\x1b[0m";
    print_endline err;
    Gl.delete_program program;
    None)
  else Some program

(** [load vert frag] loads a shader program from the files [vert] and
    [frag] *)
let load vert frag =
  let vertCode = read_all_of_file vert in
  let fragCode = read_all_of_file frag in
  let vert = compile_shader vertCode Gl.vertex_shader vert in
  let frag = compile_shader fragCode Gl.fragment_shader frag in
  match (vert, frag) with
  | Some v, Some f -> link_program v f
  | _ -> None

let free shader = Gl.delete_program shader

let use shader =
  Gl.use_program shader;
  shader

let set_vec3f uniform vec shader =
  let x, y, z = vec in
  let loc = Gl.get_uniform_location shader uniform in
  Gl.uniform3f loc x y z;
  shader

(** [fill_bigarray ba i lst] is [ba] set with the values of [lst] from
    index [i] to [List.length lst] Requires size of [ba] >
    [i + List.length lst] *)
let rec fill_bigarray ba i = function
  | h :: t ->
      ba.{i} <- h;
      fill_bigarray ba (i + 1) t
  | _ -> ()

(** [fill_bigarray_array] is [ba] set with the values of [arry] from
    index [i] to [Array.length arry] *)
let rec fill_bigarray_array ba arry i =
  if i < Array.length arry then begin
    ba.{i} <- arry.(i);
    fill_bigarray_array ba arry (i + 1)
  end
  else ()

let set_mat4 uniform mat shader =
  let loc = Gl.get_uniform_location shader uniform in
  let array = bigarray_create Bigarray.float32 16 in
  fill_bigarray_array array mat 0;
  Gl.uniform_matrix4fv loc 1 false array;
  shader

let set_i uniform v shader =
  let loc = Gl.get_uniform_location shader uniform in
  Gl.uniform1i loc v;
  shader

let set_b uniform v shader =
  let i = if v then 1 else 0 in
  set_i uniform i shader
