(** Tests functionality of a cube representation *)
open OUnit2

module CubeTest (Rep : Cube_rep.CubeRep) = struct
  open Types

  (** [color_test_helper name cube position expected_color] creates a
      test called [name] which asserts that position [position] on
      [cube] is colored [expected_color] *)
  let color_test_helper name cube position expected_color =
    name >:: fun _ ->
    assert_equal expected_color
      (Rep.query position cube)
      ~printer:string_of_color

  let default_test =
    let init = Rep.make () in
    [
      color_test_helper "front center" init
        { x = 1; y = 1; face = F }
        Green;
      color_test_helper "back TL" init { x = 0; y = 0; face = B } Blue;
      color_test_helper "left BR" init { x = 2; y = 2; face = L } Orange;
      color_test_helper "down (1, 2)" init
        { x = 1; y = 2; face = D }
        Yellow;
      color_test_helper "down face solid color" init
        { x = 0; y = 2; face = D }
        Yellow;
    ]

  let set_test =
    let cube =
      Rep.make ()
      |> Rep.set { x = 1; y = 1; face = F } Blue
      |> Rep.set { x = 2; y = 0; face = D } Green
      |> Rep.set { x = 0; y = 0; face = L } White
      |> Rep.set { x = 1; y = 2; face = U } Orange
    in
    [
      color_test_helper "set front" cube { x = 1; y = 1; face = F } Blue;
      color_test_helper "normal front" cube
        { x = 0; y = 0; face = F }
        Green;
      color_test_helper "set up" cube { x = 1; y = 2; face = U } Orange;
      color_test_helper "normal up" cube
        { x = 2; y = 2; face = U }
        White;
      color_test_helper "set left" cube { x = 0; y = 0; face = L } White;
      color_test_helper "normal left" cube
        { x = 2; y = 0; face = L }
        Orange;
      color_test_helper "set down" cube { x = 2; y = 0; face = D } Green;
      color_test_helper "normal down" cube
        { x = 0; y = 1; face = D }
        Yellow;
    ]

  let cube_rep_suite =
    "representation tests" >::: List.flatten [ default_test; set_test ]
end
