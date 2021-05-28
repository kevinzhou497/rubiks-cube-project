(** Test suit for interpretting and parsing commands *)
open OUnit2

open Command
open Types

(** [string_of_vec vec] is a string representation of [vec] *)
let string_of_vec vec =
  "(" ^ string_of_face vec.face ^ ", " ^ string_of_int vec.x ^ ", "
  ^ string_of_int vec.y ^ ")"

(** [cmd_printer cmd] is a string representation of [cmd] *)
let cmd_printer = function
  | Quit -> "quit"
  | Set (vec, color) ->
      "set " ^ string_of_vec vec ^ " <- " ^ string_of_color color
  | SetFace lst ->
      string_of_face (fst (List.hd lst)).face
      ^ " <- "
      ^ List.fold_right
          (fun v acc -> string_of_color (snd v) ^ " " ^ acc)
          lst ""
  | Solve -> "solve"
  | Rotate (face, rot) ->
      "rotate " ^ string_of_face face ^ str_of_rotation rot
  | SolveStep -> "step"
  | Invalid msg -> "invalid (" ^ msg ^ ")"
  | Help -> "help"
  | Pass -> "null command"
  | Reset -> "reset"
  | Scramble -> "scramble"
  | Undo -> "undo"

(** [cmd_test_helper name str cmd] adds a OUnit test named [name] and
    asserts that the command that is parsed from [str] equals [cmd] *)
let cmd_test_helper name str cmd =
  name >:: fun _ ->
  assert_equal (cmd_of_str str) cmd ~printer:cmd_printer

(** [cmd_test_invalid name str] adds an OUnit test named [name] which
    ensures that the command encoded in [str] is invalid *)
let cmd_test_invalid name str =
  name >:: fun _ ->
  match cmd_of_str str with
  | Invalid _ -> assert_equal true true
  | x -> assert_equal (Invalid "") x ~printer:cmd_printer

let quit_test =
  [
    cmd_test_helper "simple quit" "quit" Quit;
    cmd_test_helper "space quit" "quit " Quit;
    cmd_test_helper "case quit" "QuIt" Quit;
    cmd_test_invalid "invalid quit" "qquit";
    cmd_test_helper "precede space quit" " quiT" Quit;
  ]

let set_test =
  [
    cmd_test_helper "set" "U 0 1 <- White"
      (Set ({ face = U; x = 0; y = 1 }, White));
    cmd_test_helper "set back" "B 2 2 <- Green"
      (Set ({ face = B; x = 2; y = 2 }, Green));
    cmd_test_helper "set left" "L 0 0 <- Orange"
      (Set ({ face = L; x = 0; y = 0 }, Orange));
    cmd_test_invalid "missing param" "L 1 <- White";
    cmd_test_invalid "unparseable" "F F F <- Blue";
    cmd_test_helper "single color char" "F 1 1 <- Y"
      (Set ({ face = F; x = 1; y = 1 }, Yellow));
  ]

let vec = { x = 0; y = 0; face = U }

let vecf = { x = 0; y = 0; face = F }

let multi_set_test =
  [
    cmd_test_helper "set_multi"
      "U <- White White White Green Green Green Blue Blue Blue"
      (SetFace
         [
           (vec, White);
           ({ vec with x = 1 }, White);
           ({ vec with x = 2 }, White);
           ({ vec with y = 1 }, Green);
           ({ vec with x = 1; y = 1 }, Green);
           ({ vec with y = 1; x = 2 }, Green);
           ({ vec with y = 2 }, Blue);
           ({ vec with y = 2; x = 1 }, Blue);
           ({ vec with y = 2; x = 2 }, Blue);
         ]);
    cmd_test_helper "set_multi_mix_rows"
      "U <- White Orange White Green Blue Green Blue Yellow Red"
      (SetFace
         [
           (vec, White);
           ({ vec with x = 1 }, Orange);
           ({ vec with x = 2 }, White);
           ({ vec with y = 1 }, Green);
           ({ vec with x = 1; y = 1 }, Blue);
           ({ vec with y = 1; x = 2 }, Green);
           ({ vec with y = 2 }, Blue);
           ({ vec with y = 2; x = 1 }, Yellow);
           ({ vec with y = 2; x = 2 }, Red);
         ]);
    cmd_test_helper "set_multi_front"
      "F <- White Red Green Green Orange Yellow Blue White Blue"
      (SetFace
         [
           (vecf, White);
           ({ vecf with x = 1 }, Red);
           ({ vecf with x = 2 }, Green);
           ({ vecf with y = 1 }, Green);
           ({ vecf with x = 1; y = 1 }, Orange);
           ({ vecf with y = 1; x = 2 }, Yellow);
           ({ vecf with y = 2 }, Blue);
           ({ vecf with y = 2; x = 1 }, White);
           ({ vecf with y = 2; x = 2 }, Blue);
         ]);
    cmd_test_helper "set_multi_front_char_only" "F <- W R G G O Y B W B"
      (SetFace
         [
           (vecf, White);
           ({ vecf with x = 1 }, Red);
           ({ vecf with x = 2 }, Green);
           ({ vecf with y = 1 }, Green);
           ({ vecf with x = 1; y = 1 }, Orange);
           ({ vecf with y = 1; x = 2 }, Yellow);
           ({ vecf with y = 2 }, Blue);
           ({ vecf with y = 2; x = 1 }, White);
           ({ vecf with y = 2; x = 2 }, Blue);
         ]);
    cmd_test_invalid "set_multi_missing"
      "B <- White White Green Green Yellow";
  ]

let solve_test =
  [
    cmd_test_helper "solve" "solve" Solve;
    cmd_test_helper "solve case" "SOlve" Solve;
    cmd_test_helper "solve space" "   solve   " Solve;
    cmd_test_helper "solve step" "step" SolveStep;
    cmd_test_invalid "bad solve" "solv";
    cmd_test_helper "step case" "STep" SolveStep;
  ]

let rotate_test =
  [
    cmd_test_helper "rotate clockwise" "rotate F"
      (Rotate (F, Clockwise));
    cmd_test_helper "rotate clockwise U" "rotate U"
      (Rotate (U, Clockwise));
    cmd_test_helper "rotate counter" "rotate R'" (Rotate (R, Counter));
    cmd_test_helper "rotate counter D" "RoTaTe    D'"
      (Rotate (D, Counter));
    cmd_test_invalid "rotate invalid" "rotate";
    cmd_test_helper "rotate space counter B" "  rotate  B' "
      (Rotate (B, Counter));
    cmd_test_helper "rotate double" "rotate L2" (Rotate (L, Double));
    cmd_test_helper "rotate double R" "roTate R2" (Rotate (R, Double));
    cmd_test_invalid "rotate too many args" "rotate R2 F'";
    cmd_test_invalid "rotate too long arg" "rotate R'2";
  ]

let command_suite =
  "command tests"
  >::: List.flatten
         [
           quit_test; set_test; multi_set_test; solve_test; rotate_test;
         ]
