open Tgl3
open Util

type t = int

(** [load_if_done img] takes the image [img] and loads it in VRAM by
    creating an OpenGL texture. [img] is freed *)
let load_if_done img =
  let tex = get_int (Gl.gen_textures 1) in
  let width = Stb_image.width img in
  let height = Stb_image.height img in
  Stb_image.vflip img;
  Gl.bind_texture Gl.texture_2d tex;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_min_filter
    Gl.linear_mipmap_linear;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_mag_filter
    Gl.linear_mipmap_linear;
  Gl.tex_image2d Gl.texture_2d 0 Gl.rgba width height 0 Gl.rgba
    Gl.unsigned_byte
    (`Data (Stb_image.data img));
  Gl.generate_mipmap Gl.texture_2d;
  Stb_image.free_unmanaged img;
  Gl.bind_texture Gl.texture_2d 0;
  tex

let load file =
  match Stb_image.load_unmanaged file with
  | Ok img -> load_if_done img
  | Error _ -> failwith "Failed to load image"

let bind target tex =
  Gl.active_texture (Gl.texture0 + target);
  Gl.enable Gl.texture_2d;
  Gl.bind_texture Gl.texture_2d tex;
  tex

let free tex = set_int (Gl.delete_textures 1) tex
