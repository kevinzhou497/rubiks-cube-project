(* Test Plan: We separated out the tests into seperate files for
   organization. In cube_test.ml we test the basic functions of a cube
   representation such as setting and querying tiles in the cube This is
   done by black block testing and ensuring that after we set a tile,
   querying that same tile should result in the color that we just set.
   We also test queries manually by creating a cube manually and
   ensuring that querying the cube correctly gets the color that we
   specified

   In command_test, we test the parsing of each terminal command. This
   is done by black box testing and ensuring that the strings that
   should be recognized according to our usage spec are parsed to the
   commands that we expect them to.

   In action_test, we test the controller of a the cube. By black box
   testing, we ensure that a given command causes the cube to mutate in
   the way we expect it to. Commands tested here are things such as the
   command for sets and queries along with rotations. We also employ
   randomized testing by scrambling the cube randomly, and then
   performing tests. We test rotations based on properties that
   rotations should have. Ie. We create two sequences of rotations that
   should be equal and tests that the cube states are indeed equal. For
   example, we ensure that rotating clockwise then counterclockwise
   should return the state that we started with. This module also tests
   solving by ensuring that a any state that results from a solve
   command is the initial state.

   We tested our graphics manually by repeadetdely trying different
   combinations of controls in the actual UI. Since we could not think
   of a way to automate tests of the UI, we extensively when though and
   tested each feature of the UI in the actual program. In other words
   "play testing".

   We believe that this shows the correctness of our system because we
   have tested, either manually or automatically with OUnit, each
   feature of our system. We also believe that black box testing
   adequately shows correctness because it goes through many situations
   that simulate the use cases that a user would perform. *)
open OUnit2
open Command_test
open Cube_test
open Action_test
module ListCubeTest = CubeTest (ListCube)
module ArrayCubeTest = CubeTest (ArrayCube)
module ArrayCubeControllerTest = MakeControllerTest (ArrayCube)

let _ = run_test_tt_main command_suite

let _ = run_test_tt_main ListCubeTest.cube_rep_suite

(* Does not implement rotations *)
(* let _ = run_test_tt_main ListCubeControllerTest.controller_suite *)

let _ = run_test_tt_main ArrayCubeTest.cube_rep_suite

let _ = run_test_tt_main ArrayCubeControllerTest.controller_suite
