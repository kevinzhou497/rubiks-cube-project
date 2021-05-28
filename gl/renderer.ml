module MakeRenderer (Rep : Cube_rep.CubeRep) :
  Ui.UI with type rep_type = Rep.t = struct
  open Types
  open Cube_rep
  open Tgl3
  open ViewTypes

  type rep_type = Rep.t

  type animation = {
    (* The function to perform the animation. Should update last_frame *)
    action : animation -> animation;
    start_time : float;
    duration : float;
  }

  type t = {
    wind : Window.t;
    shader : Shader.t;
    cube : Cube.t;
    last_draw : float;
    cube_mat : float array;
    (* List of animations that are queued up. Requires that each
       animation have a corresponding animation command in
       [animating_cmds] *)
    anims : animation list;
    (* List of commands that have animations currently queued up *)
    animating_cmds : command list;
    (* list of commands that have animations that finished. Requires
       that all done commands have come off the animating_cmds list *)
    done_cmds : command list;
    screen_shader : Shader.t;
    screen : Rect.t;
    screen_fbo : Framebuffer.t ref;
    help_img : Texture.t;
    show_help : bool;
    show_select : bool;
    col_sel : ColorSelect.t;
    col_sel_img : Texture.t;
    msgs : MsgLog.t ref;
    wait_img : Texture.t;
    show_wait : bool;
  }

  let width = 1280

  let height = 720

  let aspect_ratio = float_of_int width /. float_of_int height

  let fov = 35.

  let f_ner = 0.01

  let f_far = 20.

  let perspective = Matrix.perspective fov aspect_ratio f_ner f_far

  let eye_pos = [| 0.; 0.; ~-.10. |]

  (* position, center, up*)
  let view = Matrix.lookat eye_pos [| 0.; 0.; 0. |] [| 0.; 1.; 0. |]

  let model = Matrix.translate ~-.1.25 ~-.1.25 ~-.1.25

  (** [prepare_for_quit ui] is [ui] after all resources have been
      cleaned up *)
  let prepare_for_quit ui =
    Shader.free ui.shader;
    Shader.free ui.screen_shader;
    Cube.free ui.cube;
    Rect.free ui.screen;
    Shader.free ui.screen_shader;
    Framebuffer.free !(ui.screen_fbo);
    Window.free ui.wind;
    ColorSelect.free ui.col_sel;
    Texture.free ui.help_img;
    Texture.free ui.col_sel_img;
    MsgLog.free !(ui.msgs);
    Texture.free ui.wait_img;
    ui

  (** [idx_of_face face] is the integral representation of [face]
      according to the order of specification in the shader *)
  let idx_of_face = function
    | U -> 0
    | D -> 1
    | R -> 2
    | L -> 3
    | F -> 4
    | B -> 5
    | EmptyFace -> failwith "Empty face"

  (** [angle_of_rotation rot] is the radian angle of [rot] *)
  let angle_of_rotation = function
    | Clockwise -> (Float.pi /. 2.) +. 0.03
    | Counter -> (~-.Float.pi /. 2.) -. 0.03
    | Double -> Float.pi +. 0.06

  let face_rotation_func rotateMat xOff yOff zOff rads =
    Matrix.mul
      (Matrix.mul (Matrix.translate xOff yOff zOff) (rotateMat rads))
      (Matrix.translate ~-.xOff ~-.yOff ~-.zOff)

  (** [rotate_anim_data face rotation] is the
      [(angle, shader_matrix_index, matrix_function)] for the given
      [rotation] of [face] *)
  let rotate_anim_data face rotation =
    let angle = angle_of_rotation rotation in
    let angle =
      match face with
      | F | R | U -> angle
      | D | B | L -> ~-.angle
      | EmptyFace -> failwith "Empty face encountered in rotate"
    in
    let mat_idx = idx_of_face face in
    let func =
      match face with
      | U | D -> face_rotation_func Matrix.rotateY 1. 0. 1.
      | L | R -> face_rotation_func Matrix.rotateX 0. 1. 1.
      | F | B -> face_rotation_func Matrix.rotateZ 1. 1. 0.
      | EmptyFace -> failwith "Empty face"
    in
    (angle, mat_idx, func)

  (** [reset_shader_matrices i shader] is [shader] after setting the
      [i]th rotation matirx to the identity matrix *)
  let rec reset_shader_matrices i shader =
    if i < 6 then
      shader
      |> Shader.set_mat4
           ("sliceMatrices[" ^ string_of_int i ^ "]")
           Matrix.idt
      |> reset_shader_matrices (i + 1)
    else shader

  (** [calc_rot_anim_angle anim angle] time adjusts the rotation through
      [angle] based on elapsed time of [anim] *)
  let calc_rot_anim_angle anim angle =
    angle
    *. min
         ((Window.time_sec () -. anim.start_time) /. anim.duration)
         1.0

  (** [set_shader_rot_mat idx mat shader] sets rotation matrix [idx] of
      [shader] to [mat] *)
  let set_shader_rot_mat idx mat shader =
    shader |> Shader.use
    |> Shader.set_mat4 ("sliceMatrices[" ^ string_of_int idx ^ "]") mat
    |> ignore

  (** [make_anim_action_func face rotation] makes the onAnimation
      callback for a rotation of type [rotation] on [face] *)
  let make_anim_action_func ui face rotation =
    let angle, mat_idx, func = rotate_anim_data face rotation in
    fun anim ->
      let rads = calc_rot_anim_angle anim angle in
      set_shader_rot_mat mat_idx (func rads) ui.shader;
      anim

  let make_rot_anim action_func =
    { action = action_func; start_time = ~-.1.; duration = 0.8 }

  let append_anim anims anim = anims @ [ anim ]

  (** enable_wait ui en enables or disables the wait screen depending on
      [en] for [ui] *)
  let enable_wait ui en =
    let _ =
      ui.screen_shader |> Shader.use |> Shader.set_b "showWait" en
    in
    { ui with show_wait = en }

  (** [push_rotate_animation ui face rotation] is [ui] after adding an
      animation to perform [rotation] on [face] *)
  let push_rotate_animation ui face rotation =
    let do_action = make_anim_action_func ui face rotation in
    {
      ui with
      anims = append_anim ui.anims (make_rot_anim do_action);
      animating_cmds = ui.animating_cmds @ [ Rotate (face, rotation) ];
    }

  (** [push_anim_to_done ui anims_tl] is [ui] with [ui.anims = anims_tl]
      and the animating commands popped with the head moving to
      done_cmds *)
  let push_anim_to_done ui anims_tl =
    {
      ui with
      anims = anims_tl;
      done_cmds = ui.done_cmds @ [ List.hd ui.animating_cmds ];
      animating_cmds = List.tl ui.animating_cmds;
    }

  let push_rotations rots ui =
    let rec push_rots_helper ui = function
      | (f, r) :: t -> push_rots_helper (push_rotate_animation ui f r) t
      | _ -> ui
    in
    push_rots_helper (enable_wait ui false) rots

  (** [anim_not_started a] is true if [a] hasn't started *)
  let anim_not_started a = a.start_time < 0.

  (** true when [a] has completed *)
  let anim_finished a = Window.time_sec () -. a.start_time >= a.duration

  let start_anim a = { a with start_time = Window.time_sec () }

  (** [animate ui] is [ui] after performing the next animation in the
      animation queue of [ui] *)
  let animate ui =
    match ui.anims with
    | anim :: t ->
        if anim_not_started anim then
          { ui with anims = start_anim anim :: t }
        else if anim_finished anim then push_anim_to_done ui t
        else { ui with anims = anim.action anim :: t }
    | [] -> ui

  (** [push_log_msg ui cmd] pushes a log msg corresponding to [cmd] if
      there is one. Otherwise does nothing *)
  let push_log_msg ui cmd =
    match cmd with
    | Rotate (f, rot) ->
        ui.msgs := MsgLog.append_msg (SolveMsg (f, rot)) !(ui.msgs)
    | _ -> ()

  (** [pop_done_anum_q ui] is [(ui, cmd)] where [cmd] is the head of the
      finished animations q. Cmd is popped from ui. Otherwise,
      [(ui, Pass)] is returned *)
  let pop_done_anim_q ui =
    match ui.done_cmds with
    | h :: t ->
        push_log_msg ui h;
        ( {
            ui with
            done_cmds = t;
            shader = reset_shader_matrices 0 (ui.shader |> Shader.use);
          },
          h )
    | [] -> (ui, Pass)

  let wait_for_usr_cmd ui =
    let cmd, wnd =
      if ui.show_select then Window.get_color_command ui.wind ui.col_sel
      else Window.get_command ui.wind
    in
    let new_ui = { ui with wind = wnd } in
    match cmd with
    | Quit -> (prepare_for_quit new_ui, cmd)
    | Rotate (face, rotation) ->
        (push_rotate_animation new_ui face rotation, Pass)
    | Pass -> pop_done_anim_q new_ui
    | (Scramble as c) | (Reset as c) ->
        new_ui.msgs := MsgLog.clr_msgs !(new_ui.msgs);
        (new_ui, c)
    | Undo ->
        new_ui.msgs := MsgLog.pop_msg !(new_ui.msgs);
        (new_ui, Undo)
    | _ -> (new_ui, cmd)

  (** [display_help_msg ui] shows the help screen on [ui] *)
  let display_help_msg ui =
    let help_visible = Bool.not ui.show_help in
    ui.screen_shader |> Shader.use
    |> Shader.set_b "showHelp" help_visible
    |> ignore;
    { ui with show_help = help_visible }

  (** [rotate_view ui rads f] rotates ui by [rads] by calling [f rads] *)
  let rotate_view ui rads f =
    let time_fac = (Window.time_sec () -. ui.last_draw) /. 0.008 in
    let angle = rads *. time_fac in
    let modl = Matrix.mul (f angle) ui.cube_mat in
    { ui with cube_mat = modl }

  (** [do_view_cmd ui cmd] manipulates [ui] according to [cmd]. Controls
      the view of the cube in the ui *)
  let do_view_cmd ui cmd =
    match cmd with
    | Nop -> ui
    | RotateView (rads, XAxis) -> rotate_view ui rads Matrix.rotateX
    | RotateView (rads, YAxis) -> rotate_view ui rads Matrix.rotateY
    | ResetView -> { ui with cube_mat = model }
    | ShowHelp -> display_help_msg ui
    | ShowColorSelect ->
        { ui with show_select = Bool.not ui.show_select }
    | ShowWait -> enable_wait ui true

  let clear () =
    Gl.clear_color 0.429 0.708 0.902 1.;
    Gl.clear Gl.color_buffer_bit;
    Gl.clear Gl.depth_buffer_bit

  (** renders the 3D scene onto a framebuffer *)
  let render_scene rep ui =
    ui.shader |> Shader.use
    |> Shader.set_mat4 "model" ui.cube_mat
    |> ignore;
    Framebuffer.bind_for_writing !(ui.screen_fbo) |> ignore;
    clear ();
    Cube.render ui.cube |> ignore

  (** [setup_scr_shader ui] sets up the screen shader and sets all of
      the uniforms to default values *)
  let setup_scr_shader ui =
    ui.screen_shader |> Shader.use |> Shader.set_i "tex" 0
    |> Shader.set_i "help" 1 |> Shader.set_i "wait" 3
    |> Shader.set_mat4 "model" Matrix.idt
    |> Shader.set_b "instanced" false
    |> ignore

  (** [setup_scr_textures ui] binds the correct textures for [ui] *)
  let setup_scr_textures ui =
    if ui.show_wait then Texture.bind 3 ui.wait_img |> ignore
    else if ui.show_help && ui.show_select then
      Texture.bind 1 ui.col_sel_img |> ignore
    else if ui.show_help then Texture.bind 1 ui.help_img |> ignore
    else ();
    let show_select_menu = ui.show_select && ui.show_help = false in
    if show_select_menu then
      ui.screen_shader |> Shader.set_b "backdrop" true |> ignore
    else ui.screen_shader |> Shader.set_b "backdrop" false |> ignore;
    show_select_menu

  (** [blit_screen ui] blits the framebuffer onto the screen and renders
      the 2D UI *)
  let blit_screen ui =
    setup_scr_shader ui;
    Framebuffer.bind_for_reading 0 !(ui.screen_fbo) |> ignore;
    Gl.disable Gl.depth_test;
    let show_select_menu = setup_scr_textures ui in
    Rect.render ui.screen |> ignore;
    if show_select_menu then
      let _ =
        ColorSelect.highlight ui.col_sel (Window.get_cursor_pos ui.wind)
      in
      ColorSelect.render ui.col_sel
    else ();
    MsgLog.render (Window.fb_size ui.wind) !(ui.msgs) |> ignore;
    Gl.enable Gl.depth_test

  let display rep ui =
    let cmd, wnd = Window.poll_events ui.wind in
    clear ();
    let viewed_ui = do_view_cmd ui cmd in
    let new_ui = animate { viewed_ui with wind = wnd } in
    UiUtil.set_colors rep ui.shader idx_of_face Rep.dims Rep.query;
    ColorSelect.update_colors rep ui.col_sel (Rep.dims, Rep.query);
    render_scene rep new_ui;
    blit_screen new_ui;
    Window.buffer_frame new_ui.wind;
    { new_ui with last_draw = Window.time_sec () }

  let inform_usr msg ui =
    print_endline msg;
    let ui_msg =
      match String.lowercase_ascii msg with
      | "invalid" -> Some MsgLog.InvalidCube
      | "failed" -> Some MsgLog.SolveFailed
      | _ -> None
    in
    match ui_msg with
    | Some x ->
        ui.msgs := MsgLog.append_msg x !(ui.msgs);
        enable_wait ui false
    | None -> ui

  let load_shader vert frag =
    match Shader.load vert frag with
    | Some x -> x
    | None -> failwith "Unable to compile shaders"

  (** [resize_src w h ui] is [ui] set up to display on a screen of [w] x
      [h] *)
  let resize_scr w h ui =
    Gl.viewport 0 0 w h;
    let aspect = float_of_int w /. float_of_int h in
    let perp = Matrix.perspective fov aspect f_ner f_far in
    ui.shader |> Shader.use
    |> Shader.set_mat4 "projection" perp
    |> ignore;
    ui.screen_fbo := Framebuffer.resize w h !(ui.screen_fbo)

  (** [reset_shader shader] sets the basics of [shader] *)
  let reset_shader wind shader =
    let w, h = Window.fb_size wind in
    let aspect = float_of_int w /. float_of_int h in
    let perp = Matrix.perspective fov aspect f_ner f_far in
    shader |> Shader.use
    |> Shader.set_mat4 "view" view
    |> Shader.set_mat4 "projection" perp
    |> Shader.set_vec3f "eye" (eye_pos.(0), eye_pos.(1), eye_pos.(2))
    |> reset_shader_matrices 0

  let init_gl wind =
    let w, h = Window.fb_size wind in
    Gl.viewport 0 0 w h;
    Gl.enable Gl.depth_test;
    Gl.enable Gl.multisample;
    Gl.depth_func Gl.less;
    Gl.enable Gl.texture_2d

  let new_ui wind shader =
    let w, h = Window.fb_size wind in
    let ss = load_shader "gl/screenVert.glsl" "gl/screenFrag.glsl" in
    let fbo = Framebuffer.make w h in
    let help = Texture.load "gl/res/RubiksCubeKeyMaps.png" in
    let cs = Texture.load "gl/res/colorSelectHelp.png" in
    let wait_img = Texture.load "gl/res/wait.png" in
    let msgs = MsgLog.make ss in
    {
      wind;
      shader;
      cube = Cube.make ();
      last_draw = Window.time_sec ();
      cube_mat = model;
      anims = [];
      animating_cmds = [];
      done_cmds = [];
      screen_shader = ss;
      screen = Rect.make ();
      screen_fbo = ref fbo;
      help_img = help;
      show_help = true;
      show_select = false;
      col_sel = ColorSelect.make ss 3;
      col_sel_img = cs;
      msgs = ref msgs;
      wait_img;
      show_wait = false;
    }

  (** [scroll_msgs ui y] scrolls the log messages by [y] *)
  let scroll_msgs ui y =
    let scroll_fac = 0.04 in
    ui.msgs := MsgLog.offset_y (y *. scroll_fac) !(ui.msgs)

  let make_ui () =
    let wind = Window.make width height "Rubik's Cube" in
    init_gl wind;
    let shader =
      load_shader "gl/vertex.glsl" "gl/frag.glsl" |> reset_shader wind
    in
    let ui = new_ui wind shader in
    Window.on_wind_resize (fun w h -> resize_scr w h ui) wind;
    Window.on_mouse_scroll (fun _ y -> scroll_msgs ui y) wind;
    ui
end
