open Tgl3
open Util

type t = {
  ms_fbo : int;
  ms_tex : int;
  w_fbo : int;
  w_tex : int;
  width : int;
  height : int;
  ms_rbo : int;
}

let make width height =
  let ms_fbo = get_int (Gl.gen_framebuffers 1) in
  let ms_tex = get_int (Gl.gen_textures 1) in
  let ms_rbo = get_int (Gl.gen_renderbuffers 1) in
  Gl.active_texture Gl.texture10;
  Gl.bind_framebuffer Gl.framebuffer ms_fbo;
  Gl.bind_texture Gl.texture_2d_multisample ms_tex;
  Gl.tex_image2d_multisample Gl.texture_2d_multisample 4 Gl.rgba width
    height true;
  Gl.framebuffer_texture2d Gl.framebuffer Gl.color_attachment0
    Gl.texture_2d_multisample ms_tex 0;
  Gl.bind_renderbuffer Gl.renderbuffer ms_rbo;
  Gl.renderbuffer_storage_multisample Gl.renderbuffer 4
    Gl.depth24_stencil8 width height;
  Gl.framebuffer_renderbuffer Gl.framebuffer Gl.depth_stencil_attachment
    Gl.renderbuffer ms_rbo;
  Gl.bind_renderbuffer Gl.renderbuffer 0;
  Gl.enable Gl.depth_test;
  Gl.depth_func Gl.less;
  let _ =
    if
      Gl.check_framebuffer_status Gl.framebuffer
      <> Gl.framebuffer_complete
    then failwith "Framebuffer incomplete"
    else ()
  in
  let w_fbo = get_int (Gl.gen_framebuffers 1) in
  Gl.bind_framebuffer Gl.framebuffer w_fbo;
  let w_tex = get_int (Gl.gen_textures 1) in
  Gl.bind_texture Gl.texture_2d w_tex;
  Gl.tex_image2d Gl.texture_2d 0 Gl.rgba width height 0 Gl.rgba
    Gl.unsigned_byte (`Offset 0);
  Gl.tex_parameteri Gl.texture_2d Gl.texture_min_filter Gl.linear;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_mag_filter Gl.linear;
  Gl.framebuffer_texture2d Gl.framebuffer Gl.color_attachment0
    Gl.texture_2d w_tex 0;
  let _ =
    if
      Gl.check_framebuffer_status Gl.framebuffer
      <> Gl.framebuffer_complete
    then failwith "Write framebuffer incomplete"
    else ()
  in
  Gl.bind_framebuffer Gl.framebuffer 0;
  { ms_fbo; ms_tex; w_fbo; w_tex; width; height; ms_rbo }

let resize width height fbo =
  Gl.bind_texture Gl.texture_2d_multisample fbo.ms_tex;
  Gl.tex_image2d_multisample Gl.texture_2d_multisample 4 Gl.rgba width
    height true;
  Gl.bind_texture Gl.texture_2d fbo.w_tex;
  Gl.tex_image2d Gl.texture_2d 0 Gl.rgba width height 0 Gl.rgba
    Gl.unsigned_byte (`Offset 0);
  Gl.bind_renderbuffer Gl.renderbuffer fbo.ms_rbo;
  Gl.renderbuffer_storage_multisample Gl.renderbuffer 4
    Gl.depth24_stencil8 width height;
  { fbo with width; height }

let free fbo =
  set_int (Gl.delete_textures 1) fbo.w_tex;
  set_int (Gl.delete_textures 1) fbo.ms_tex;
  set_int (Gl.delete_framebuffers 1) fbo.ms_fbo;
  set_int (Gl.delete_framebuffers 1) fbo.w_fbo;
  set_int (Gl.delete_renderbuffers 1) fbo.ms_rbo

let bind_for_writing fbo =
  Gl.bind_framebuffer Gl.framebuffer fbo.ms_fbo;
  fbo

let bind_for_reading tex fbo =
  Gl.bind_framebuffer Gl.read_framebuffer fbo.ms_fbo;
  Gl.bind_framebuffer Gl.draw_framebuffer fbo.w_fbo;
  Gl.blit_framebuffer 0 0 fbo.width fbo.height 0 0 fbo.width fbo.height
    Gl.color_buffer_bit Gl.nearest;
  Gl.bind_framebuffer Gl.framebuffer 0;
  Gl.active_texture (Gl.texture0 + tex);
  Gl.enable Gl.texture_2d;
  Gl.bind_texture Gl.texture_2d fbo.w_fbo;
  fbo
