open Types
open ViewTypes

type t = {
  wind : GLFW.window;
  last_press : float;
  width : int ref;
  height : int ref;
}

let make width height name =
  GLFW.init ();
  at_exit GLFW.terminate;
  GLFW.windowHint GLFW.ContextVersionMajor 3;
  GLFW.windowHint GLFW.ContextVersionMinor 3;
  GLFW.windowHint GLFW.OpenGLProfile GLFW.CoreProfile;
  let _ =
    if Sys.file_exists "USING_MAC_OS" then
      GLFW.windowHint GLFW.OpenGLForwardCompat true
    else ()
  in
  GLFW.windowHint GLFW.Samples (Some 4);
  let wind = GLFW.createWindow width height name () in
  GLFW.makeContextCurrent (Some wind);
  { wind; last_press = 0.; width = ref width; height = ref height }

let time_sec = GLFW.getTime

let buffer_frame wind = GLFW.swapBuffers wind.wind

let key_cmd_view_events wind =
  if time_sec () -. wind.last_press >= 0.3 then
    let new_wind = { wind with last_press = time_sec () } in
    if GLFW.getKey wind.wind GLFW.Kp0 || GLFW.getKey wind.wind GLFW.Num0
    then (ResetView, new_wind)
    else if GLFW.getKey wind.wind GLFW.Space then (ShowHelp, new_wind)
    else if GLFW.getKey wind.wind GLFW.Tab then
      (ShowColorSelect, new_wind)
    else (Nop, wind)
  else (Nop, wind)

let poll_events wind =
  let _ = GLFW.pollEvents () in
  if GLFW.getKey wind.wind GLFW.Left then
    (RotateView (~-.0.3, YAxis), wind)
  else if GLFW.getKey wind.wind GLFW.Right then
    (RotateView (0.3, YAxis), wind)
  else if GLFW.getKey wind.wind GLFW.Up then
    (RotateView (~-.0.3, XAxis), wind)
  else if GLFW.getKey wind.wind GLFW.Down then
    (RotateView (0.3, XAxis), wind)
  else if
    GLFW.getKey wind.wind GLFW.Enter
    && GLFW.getKey wind.wind GLFW.LeftShift
  then (ShowWait, wind)
  else key_cmd_view_events wind

let get_console_command () =
  print_string "> ";
  let input = read_line () in
  Command.cmd_of_str input

let get_command wind =
  if GLFW.windowShouldClose wind.wind then (Quit, wind)
  else if time_sec () -. wind.last_press >= 0.15 then
    let new_wind = { wind with last_press = time_sec () } in
    let rot =
      if GLFW.getKey wind.wind GLFW.LeftShift then Counter
      else Clockwise
    in
    if GLFW.getKey wind.wind GLFW.F then (Rotate (F, rot), new_wind)
    else if GLFW.getKey wind.wind GLFW.B then (Rotate (B, rot), new_wind)
    else if GLFW.getKey wind.wind GLFW.U then (Rotate (U, rot), new_wind)
    else if GLFW.getKey wind.wind GLFW.D then (Rotate (D, rot), new_wind)
    else if GLFW.getKey wind.wind GLFW.L then (Rotate (L, rot), new_wind)
    else if GLFW.getKey wind.wind GLFW.R then (Rotate (R, rot), new_wind)
    else if GLFW.getKey wind.wind GLFW.X then (Scramble, new_wind)
    else if GLFW.getKey wind.wind GLFW.Backspace then
      let cmd =
        if GLFW.getKey wind.wind GLFW.LeftShift then Reset else Undo
      in
      (cmd, new_wind)
    else if GLFW.getKey wind.wind GLFW.Slash then
      (get_console_command (), new_wind)
    else if GLFW.getKey wind.wind GLFW.Enter then
      let cmd =
        if GLFW.getKey wind.wind GLFW.LeftShift then Solve
        else SolveStep
      in
      (cmd, new_wind)
    else (Pass, wind)
  else (Pass, wind)

let free wind = GLFW.destroyWindow wind.wind

let on_wind_resize cb wind =
  ignore
    (GLFW.setFramebufferSizeCallback wind.wind
       (Some
          (fun _ w h ->
            wind.width := w;
            wind.height := h;
            cb w h)));
  ()

let get_cursor_pos wind =
  let x, y = GLFW.getCursorPos wind.wind in
  let nx = x /. float_of_int !(wind.width) in
  let ny =
    (float_of_int !(wind.height) -. y) /. float_of_int !(wind.height)
  in
  let to_ndc v = (v *. 2.) -. 1. in
  (to_ndc nx, to_ndc ny)

(** [set_color color cs win] uses the ColorSelect [cs] to convert the
    key press into a command which is returned *)
let set_color color cs win =
  match ColorSelect.tile_pos_from_cursor cs (get_cursor_pos win) with
  | None -> (Pass, win)
  | Some tile_pos -> (Set (tile_pos, color), win)

let get_color_command wind cs =
  if GLFW.windowShouldClose wind.wind then (Quit, wind)
  else if time_sec () -. wind.last_press >= 0.3 then
    let new_wind = { wind with last_press = time_sec () } in
    if GLFW.getKey wind.wind GLFW.W then set_color White cs new_wind
    else if GLFW.getKey wind.wind GLFW.B then set_color Blue cs new_wind
    else if GLFW.getKey wind.wind GLFW.G then
      set_color Green cs new_wind
    else if GLFW.getKey wind.wind GLFW.Y then
      set_color Yellow cs new_wind
    else if GLFW.getKey wind.wind GLFW.O then
      set_color Orange cs new_wind
    else if GLFW.getKey wind.wind GLFW.R then set_color Red cs new_wind
    else (Pass, wind)
  else (Pass, wind)

let fb_size wind = GLFW.getFramebufferSize wind.wind

let on_mouse_scroll cb wind =
  GLFW.setScrollCallback ~window:wind.wind
    ~f:(Some (fun _ x y -> cb x y))
  |> ignore
