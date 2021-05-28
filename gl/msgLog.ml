open Types
open Tgl3

type msg =
  | SolveMsg of face * rotation
  | InvalidCube
  | SolveFailed

type t = {
  rect : Rect.t;
  textures : (char * Texture.t) list;
  y_pos : float;
  (* shader is not owned by MsgLog *)
  shader : Shader.t;
  msgs : msg list;
}

let line_height = 0.079

let char_width = 0.03

let x_init = ~-.0.95

let y_init = 0.95

let scale_fac = 0.06

let load_textures () =
  [
    ('B', Texture.load "gl/res/b.png");
    ('D', Texture.load "gl/res/d.png");
    ('L', Texture.load "gl/res/el.png");
    ('F', Texture.load "gl/res/f.png");
    ('U', Texture.load "gl/res/u.png");
    ('R', Texture.load "gl/res/r.png");
    ('\'', Texture.load "gl/res/tick.png");
    ('I', Texture.load "gl/res/i.png");
    ('V', Texture.load "gl/res/v.png");
    ('N', Texture.load "gl/res/n.png");
    ('C', Texture.load "gl/res/c.png");
    ('A', Texture.load "gl/res/a.png");
    ('E', Texture.load "gl/res/e.png");
    ('2', Texture.load "gl/res/2.png");
    ('O', Texture.load "gl/res/o.png");
    ('S', Texture.load "gl/res/s.png");
  ]

let free_textures texts =
  List.fold_right (fun (_, tex) _ -> Texture.free tex) texts ()

let make shader =
  {
    rect = Rect.make ();
    textures = load_textures ();
    y_pos = y_init;
    shader;
    msgs = [];
  }

let append_msg msg log = { log with msgs = msg :: log.msgs }

let pop_msg log =
  match log.msgs with h :: t -> { log with msgs = t } | _ -> log

let offset_y off log =
  let temp_y = log.y_pos +. off in
  let log_height = float_of_int (List.length log.msgs) *. line_height in
  let scr_height = y_init +. 1. in
  let excess = max (log_height -. scr_height) 0. in
  let max_off = y_init +. excess in
  let min_off = y_init in
  let ny =
    if temp_y >= max_off then max_off
    else if temp_y <= min_off then min_off
    else temp_y
  in
  { log with y_pos = ny }

let free log =
  Rect.free log.rect;
  free_textures log.textures

(** [render_char c xoff line log] renders the character [c] with a
    horizontal offset of [xoff] characters on line [line] *)
let render_char c xoff line log (s_w, s_h) =
  let trans =
    Matrix.translate
      (x_init +. (xoff *. char_width))
      (log.y_pos -. (line *. line_height))
      0.
  in
  let scale = Matrix.scale_nu (scale_fac *. s_h /. s_w) scale_fac 1. in
  let model = Matrix.mul trans scale in
  log.shader |> Shader.set_mat4 "model" model |> ignore;
  let tex = List.assoc c log.textures in
  Texture.bind 2 tex |> ignore;
  Rect.render log.rect |> ignore

(** [render_solve_mv face rot line log] renders the indications that the
    next step is to rotate [face] [rotation] on line [line] *)
let render_solve_mv dims face rot line log =
  let _ =
    match face with
    | F -> render_char 'F' 0. line log dims
    | B -> render_char 'B' 0. line log dims
    | U -> render_char 'U' 0. line log dims
    | R -> render_char 'R' 0. line log dims
    | L -> render_char 'L' 0. line log dims
    | D -> render_char 'D' 0. line log dims
    | EmptyFace -> failwith "Can't render text of empty"
  in
  match rot with
  | Clockwise -> ()
  | Double -> render_char '2' 1. line log dims
  | Counter -> render_char '\'' 1. line log dims

(** [render_str str dims line log start_idx] renders a string [str] with
    character dimenions [dims] on line number [line] for the msg log
    [log] with the horizontal start index [start_idx] *)
let rec render_str str dims line log start_idx =
  if start_idx < String.length str then (
    match str.[start_idx] with
    | ' ' -> render_str str dims line log (start_idx + 1)
    | x ->
        render_char x (float_of_int start_idx) line log dims;
        render_str str dims line log (start_idx + 1))
  else ()

(** renders the invalid cube message *)
let render_invalid_msg dims line log =
  render_str "INVALID CUBE" dims line log 0

(** [render_msg dims msg line log] renders [msg] on line number [line] *)
let render_msg scr_dims msg line log =
  match msg with
  | SolveMsg (f, rot) -> render_solve_mv scr_dims f rot line log
  | InvalidCube -> render_invalid_msg scr_dims line log
  | SolveFailed -> render_str "SOLVE FAILED" scr_dims line log 0

(** [render_msgs dims logs] renders all of the messages in [log] using
    aspect ration based on the character screen dimensions [scr_dims] *)
let render_msgs scr_dims log =
  List.fold_right
    (fun msg acc ->
      render_msg scr_dims msg acc log;
      acc +. 1.)
    log.msgs 0.
  |> ignore

let render (s_w, s_h) log =
  log.shader |> Shader.use
  |> Shader.set_b "text" true
  |> Shader.set_b "instanced" false
  |> Shader.set_i "charTex" 2
  |> ignore;
  render_msgs (float_of_int s_w, float_of_int s_h) log;
  log.shader |> Shader.set_b "text" false |> ignore;
  log

let clr_msgs log = { log with msgs = [] }
