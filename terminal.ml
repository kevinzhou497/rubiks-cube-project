module MakeTerminal (Rep : Cube_rep.CubeRep) :
  Ui.UI with type rep_type = Rep.t = struct
  open Types
  open Cube_rep
  open CubeUtils

  type t = unit

  type rep_type = Rep.t

  module Printer = MakePrinter (Rep)

  (** [str_of_color col fg] is the ansi string code of color [col].
      Returns a foreground color if [fg], otherwise returns a background
      color. *)
  let str_of_color col fg =
    let start_num = if fg then "3" else "4" in
    let end_num =
      match col with
      | White -> "8;2;255;255;255"
      | Orange -> "8;2;255;165;0"
      | Blue -> "4"
      | Yellow -> "8;2;255;255;0"
      | Green -> "2"
      | Red -> "1"
      | Black -> "0"
    in
    start_num ^ end_num

  (* 38;2;r;g;b is the foreground color with value rgb 48;2;r;g;b is the
     background color rgb*)

  (** controls the width of each space *)
  let block_str = "   "

  (** [print_colored_txt txt color] displays [txt] in [color] *)
  let get_colored_txt txt color =
    let color_fg_str = str_of_color color true in
    Printf.sprintf "\x1b[%sm%s" color_fg_str txt

  (** [reset_color] turns off fb and bg colors *)
  let reset_color = "\x1b[0m"

  (** [print_bg_colored_txt txt fg_color bg_color] displays [txt] with
      foreground and background color [fg_color] and [bg_color]
      respectively.*)
  let get_bg_colored_txt txt fg_color bg_color =
    let color_bg_str = str_of_color bg_color false in
    Printf.sprintf "\x1b[%sm" color_bg_str
    ^ get_colored_txt txt fg_color

  let print_space () = print_string block_str

  (** [print_block col] prints a tile of foreground and background color
      [col] *)
  let get_block col = get_bg_colored_txt "  " col col ^ reset_color

  let display rep ui =
    print_string (Printer.to_string get_block block_str rep)

  let wait_for_usr_cmd ui =
    print_string "> ";
    let input = read_line () in
    (ui, Command.cmd_of_str input)

  (** [print_in_color txt color] prints [txt] in [color] and resets the
      color back to default *)
  let print_in_color txt color =
    print_string (get_colored_txt txt color ^ reset_color)

  let display_notation () =
    print_endline "Notation:";
    print_endline
      "Faces: U(up), L(left), F(front), R(right), B(back), D(Down)";
    print_endline "  U";
    print_endline "L F R B";
    print_endline "  D";
    print_endline "Colors:";
    print_endline "White, Yellow, Red, Orange, Blue, Green";
    print_endline "Or you can use just the first letter:";
    print_endline "W, Y, R, O, B, G";
    print_endline "Rotations:";
    print_endline "[Face Letter][Rotation Type]";
    print_endline
      "Where [Rotation Type] is empty for a 90 degree clockwise \
       rotation";
    print_endline "\"'\" for a 90 degree counterclockwise";
    print_endline "\"2\" for a 180 degree turn";
    print_endline "Example: \"U\" - 90 clockwise of Up face";
    print_endline "\"F'\" - 90 counterclockwise of Front face";
    print_endline "\"B2\" - 180 degree of Back face"

  let display_help_msg ui =
    let cmd_color = Yellow in
    print_endline "Please enter a command in the following format";
    print_in_color "[Face] [x] [y]" cmd_color;
    print_string " <- ";
    print_in_color "[color]" cmd_color;
    print_string " - sets position ";
    print_in_color "(x, y)" cmd_color;
    print_string " on the specified face to";
    print_in_color " [color]\n" cmd_color;

    print_in_color "[Face]" cmd_color;
    print_string " <- ";
    print_in_color "[colors]..." cmd_color;
    print_string
      (" - sets the entire face to the colors listed"
     ^ " starting with (0, 0) and going left to right, top to bottom. "
     ^ "Requires ");
    print_in_color "[colors]..." cmd_color;
    print_string " is a flattened square matrix\n";
    print_string "rotate ";
    print_in_color "[Face][Rotation Type]" cmd_color;
    print_endline
      " - makes the specified rotation in Rubik's Cube rotation \
       notation";
    print_string "quit - quits the program\n";
    print_string "solve - solves the cube\n";
    print_endline "step - makes one step towards the solution\n\n";
    display_notation ()

  let inform_usr msg ui = print_string msg

  let push_rotations rot ui =
    failwith "Solve animations not implemented in terminal"

  let make_ui () = ()
end
