(** GLFW window for 3D display *)
open ViewTypes

type t

(** [make width height name] is a window of width and height called
    [name] *)
val make : int -> int -> string -> t

(** [buffer_frame window] buffers the last drawn frame from the back
    buffer onto [window] *)
val buffer_frame : t -> unit

(** [poll_events window] polls events and gets the last view command if
    any. *)
val poll_events : t -> view_command * t

(** [get_command window] gets a user command to control the application *)
val get_command : t -> Types.command * t

(** [get_color_command window] gets a user command that is enabled only
    during the color select menu *)
val get_color_command : t -> ColorSelect.t -> Types.command * t

(** [on_wind_resize callback wind] binds [callback] to be called
    everytime the window is resized. Passes the new width and height *)
val on_wind_resize : (int -> int -> unit) -> t -> unit

(** [free window] destroys window *)
val free : t -> unit

(** [get_cursor_pos wind] is the current cursor position for [wind] in
    NDC *)
val get_cursor_pos : t -> float * float

(** [time_sec ()] is the elapsed time in seconds since the first call to
    [Window.make] *)
val time_sec : unit -> float

(** [fb_size wind] is the current framebuffer size of the window *)
val fb_size : t -> int * int

(** [get_delta_scroll cb wind] registers the callback [cb] to be called
    whenever the user scrolls *)
val on_mouse_scroll : (float -> float -> unit) -> t -> unit
