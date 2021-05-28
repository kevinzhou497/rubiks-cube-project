open Types

(** [param_lst_of_str str] converts [str] to a list of non-empty, space
    seperated strings *)
let param_lst_of_str str =
  String.split_on_char ' ' str
  |> List.filter_map (fun e ->
         if String.length e > 0 then Some e else None)

(** [try_parse_set face x y color] is the set command representation of
    Set [face] [x] [y] [colo] or Invalid if the parse failed*)
let try_parse_set face x y color =
  try
    Set
      ( {
          x = int_of_string x;
          y = int_of_string y;
          face = face_of_string face;
        },
        color_of_string color )
  with _ -> Invalid "Could not parse tile or color syntax"

(** [is_square i] is true if the integer [i] is a perfect square *)
let is_square i =
  let f = Float.of_int i in
  let sqrt_i = f |> Float.sqrt |> Float.floor |> Float.to_int in
  if sqrt_i * sqrt_i = i then true else false

let sqrt_int i =
  i |> Float.of_int |> Float.sqrt |> Float.floor |> Float.to_int

(** [str_lst_to_vec_color_lst face lst width] converts [lst] of color
    strings to a list of (vec3i, color) on [face] Requires [lst] be a
    flattened matrix starting from (0, 0) with width [width]*)
let str_lst_to_vec_color_lst face lst width =
  List.mapi
    (fun index e ->
      ( { x = index mod width; y = index / width; face },
        color_of_string e ))
    lst

(** [try_parse_color_lst face color_lst width] is [color_lst] of color
    strings as a list of (vec3i, color) pairs on [face] in a SetFace
    command Returns Invalid on error Requires [color_lst] be a flattened
    matrix of width [width] *)
let try_parse_color_lst face color_lst width =
  try SetFace (str_lst_to_vec_color_lst face color_lst width)
  with _ -> Invalid "Could not parse colors"

(** [try_parse_multi_set face color_lst width] is [color_lst] of color
    strings as a list of (vec3i, color) pairs on [face] in a SetFace
    command Returns Invalid on error Requires [color_lst] be a flattened
    matrix of width [width] *)
let try_parse_multi_set face color_lst =
  if is_square (List.length color_lst) then
    let width = sqrt_int (List.length color_lst) in
    try_parse_color_lst (face_of_string face) color_lst width
  else Invalid "Setting a face requires a perfect square sized list"

(** [parse_set param_lst] is the set command for [param_lst] or invalid
    if the parameters do not match set syntax Examples: "U 0 1 <- White"
    Sets White to (0, 1) of face U "F <- Red Blue Orange White Green
    Yellow Blue White Orange" sets the entire face F to the above colors
    in the above pattern (first row, then second, then third w/ 3 colors
    each) So (0, 0) <- Red, (0, 1) <- Blue ... (1, 0) <- White etc
    Requires [param_lst] be a square matrix so that its size is a
    perfect square and has a width of [sqrt(length)] *)
let parse_set = function
  | [ face; x; y; "<-"; color ] -> try_parse_set face x y color
  | face :: "<-" :: colors -> try_parse_multi_set face colors
  | _ -> Invalid "Illegal set syntax"

(** [parse_clockwise str] parses [str] into a clockwise rotation of some
    face Or returns Invalid on error*)
let parse_clockwise str =
  try
    let face = face_of_string str in
    Rotate (face, Clockwise)
  with _ -> Invalid "Illegal clockwise rotation parameter"

(** [parse_nonclockwise str] parses [str] is a counterclockwise or
    double rotation. Returns Invalid on error Requires [str] to be
    trimmed and have length 2 *)
let parse_nonclockwise str =
  try
    let face = face_of_string (String.sub str 0 1) in
    let type_str = String.sub str 1 (String.length str - 1) in
    match type_str with
    | "'" -> Rotate (face, Counter)
    | "2" -> Rotate (face, Double)
    | _ -> Invalid "Illegal rotation specifier"
  with _ -> Invalid "Illegal nonclockwise rotation parameter"

(** [parse_rotate param_lst] parses [param_lst] of a rotate command.
    Returns the specified rotate or Invalid. Requires [param_lst] not
    include the first term "rotate" *)
let parse_rotate = function
  | [ param ] ->
      let p = String.trim param in
      if String.length p = 2 then parse_nonclockwise p
      else if String.length p = 1 then parse_clockwise p
      else Invalid "Illegal rotation parameter"
  | _ -> Invalid "Illegal amount of rotation parameters"

(** [cmd_of_str str] is the command form of [str] *)
let cmd_of_str str =
  let params = param_lst_of_str str in
  match params with
  | q :: _ when String.lowercase_ascii q = "quit" -> Quit
  | s :: _ when String.lowercase_ascii s = "solve" -> Solve
  | ss :: _ when String.lowercase_ascii ss = "step" -> SolveStep
  | rot :: t when String.lowercase_ascii rot = "rotate" ->
      parse_rotate t
  | help :: _ when String.lowercase_ascii help = "help" -> Help
  | undo :: _ when String.lowercase_ascii undo = "undo" -> Undo
  | undo :: _ when String.lowercase_ascii undo = "scramble" -> Scramble
  | undo :: _ when String.lowercase_ascii undo = "reset" -> Reset
  | lst when String.contains str '<' -> parse_set lst
  | _ -> Invalid "Unknown command"
