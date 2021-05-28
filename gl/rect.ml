open Util
open Tgl3

type t = {
  ebo : int;
  vbo : int;
  vao : int;
  ibo : int option;
}

let vertices =
  let vts = bigarray_create Bigarray.float32 8 in
  (* Top Right *)
  vts.{0} <- 1.;
  vts.{1} <- 1.;
  (* Bottom Right *)
  vts.{2} <- 1.;
  vts.{3} <- 0.;
  (* Bottom Left *)
  vts.{4} <- 0.;
  vts.{5} <- 0.;
  (* Top Left *)
  vts.{6} <- 0.;
  vts.{7} <- 1.;
  vts

let indices =
  let idx = bigarray_create Bigarray.int8_unsigned 6 in
  idx.{0} <- 0;
  idx.{1} <- 1;
  idx.{2} <- 3;
  (* Second Triangle *)
  idx.{3} <- 1;
  idx.{4} <- 2;
  idx.{5} <- 3;
  idx

let make () =
  let vao = get_int (Gl.gen_vertex_arrays 1) in
  let vbo = get_int (Gl.gen_buffers 1) in
  let ebo = get_int (Gl.gen_buffers 1) in
  Gl.bind_vertex_array vao;
  Gl.bind_buffer Gl.array_buffer vbo;
  Gl.buffer_data Gl.array_buffer
    (Gl.bigarray_byte_size vertices)
    (Some vertices) Gl.static_draw;
  Gl.vertex_attrib_pointer 0 2 Gl.float false (2 * 4) (`Offset 0);
  Gl.enable_vertex_attrib_array 0;
  Gl.bind_buffer Gl.element_array_buffer ebo;
  Gl.buffer_data Gl.element_array_buffer
    (Gl.bigarray_byte_size indices)
    (Some indices) Gl.static_draw;
  { ebo; vbo; vao; ibo = None }

let free r =
  set_int (Gl.delete_buffers 1) r.vbo;
  set_int (Gl.delete_buffers 1) r.ebo;
  set_int (Gl.delete_vertex_arrays 1) r.vao;
  match r.ibo with
  | None -> ()
  | Some x -> set_int (Gl.delete_buffers 1) x

let render r =
  Gl.bind_vertex_array r.vao;
  Gl.draw_elements Gl.triangles 6 Gl.unsigned_byte (`Offset 0);
  r

let make_instanced ba r =
  let ibo = get_int (Gl.gen_buffers 1) in
  Gl.bind_vertex_array r.vao;
  Gl.bind_buffer Gl.array_buffer ibo;
  Gl.buffer_data Gl.array_buffer
    (Gl.bigarray_byte_size ba)
    (Some ba) Gl.static_draw;
  Gl.vertex_attrib_pointer 1 2 Gl.float false (2 * 4) (`Offset 0);
  Gl.enable_vertex_attrib_array 1;
  Gl.vertex_attrib_divisor 1 1;
  { r with ibo = Some ibo }

let render_instanced count r =
  Gl.bind_vertex_array r.vao;
  Gl.draw_elements_instanced Gl.triangles 6 Gl.unsigned_byte (`Offset 0)
    count;
  r
