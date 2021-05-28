open Tgl3
open Util

type t = {
  vao : int;
  vbo : int;
  ibo : int;
}

let vertices = load_bigarray "gl/res/cubeData.txt"

(** [offset_array size spacing] is a 3D matrix of size
    [size * size * size] where each vector (x, y, z) is spaced [spacing]
    from the last *)
let offset_array size spacing =
  let offs =
    bigarray_create Bigarray.float32 (3 * size * size * size)
  in
  let rec gen_single idx depth offset =
    if idx < 3 * size * size then begin
      let cube_idx = idx / 3 in
      let x = float_of_int (cube_idx mod size) in
      let y = float_of_int depth in
      let z = float_of_int (cube_idx / size) in
      offs.{offset + idx} <- x *. spacing;
      offs.{offset + idx + 1} <- y *. spacing;
      offs.{offset + idx + 2} <- z *. spacing;
      gen_single (idx + 3) depth offset
    end
    else ()
  in
  let rec gen_slice idx =
    if idx < size then
      let _ = gen_single 0 idx (idx * 3 * size * size) in
      gen_slice (idx + 1)
    else ()
  in
  gen_slice 0;
  offs

let make () =
  let vao = get_int (Gl.gen_vertex_arrays 1) in
  let vbo = get_int (Gl.gen_buffers 1) in
  let ibo = get_int (Gl.gen_buffers 1) in
  let instance_offsets = offset_array 3 1.0 in
  Gl.bind_vertex_array vao;
  Gl.bind_buffer Gl.array_buffer vbo;

  Gl.buffer_data Gl.array_buffer
    (Gl.bigarray_byte_size vertices)
    (Some vertices) Gl.static_draw;
  Gl.vertex_attrib_pointer 0 3 Gl.float false (8 * 4) (`Offset 0);
  Gl.enable_vertex_attrib_array 0;
  Gl.vertex_attrib_pointer 1 3 Gl.float false (8 * 4) (`Offset (3 * 4));
  Gl.enable_vertex_attrib_array 1;
  Gl.vertex_attrib_pointer 2 2 Gl.float false (8 * 4) (`Offset (6 * 4));
  Gl.enable_vertex_attrib_array 2;

  Gl.bind_buffer Gl.array_buffer ibo;
  Gl.buffer_data Gl.array_buffer
    (Gl.bigarray_byte_size instance_offsets)
    (Some instance_offsets) Gl.static_draw;
  Gl.vertex_attrib_pointer 3 3 Gl.float false (3 * 4) (`Offset 0);
  Gl.vertex_attrib_divisor 3 1;
  (* Update each instance *)
  Gl.enable_vertex_attrib_array 3;
  { vao; vbo; ibo }

let free c =
  set_int (Gl.delete_buffers 1) c.vbo;
  set_int (Gl.delete_buffers 1) c.ibo;
  set_int (Gl.delete_vertex_arrays 1) c.vao

let render c =
  Gl.bind_vertex_array c.vao;
  Gl.draw_arrays_instanced Gl.triangles 0 36 27;
  c
