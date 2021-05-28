# rubiks-cube

# Description

3D Rubik's Cube solver in OCaml. You can scramble and solve a cube directly within the application or enter the state of a physical Rubik's Cube. The program allows you to rotate the cube manually, solve the cube step by step or solve the cube completely.

Requires support for OpenGL 3.3 or above.

(This project was worked on by myself and 3 team members from Cornell's CS 3110 course)

# Controls

## Window UI

* Press `F`, `U`, `R`, `L`, `D`, `B` to rotate the Front, Up, Right, Left, Down and Back faces clockwise respectively
* Hold `LShift` with any of the previous keys to perform a counterclockwise rotation
* Press any of the face rotation keys twice to rotate a face twice
* Use the arrow keys to rotate the view of the cube
* Hit `SPACE` to open or close a context-senstitive help menu. This help page will differ depending if you are viewing the cube or the color selector menu
* Press `TAB` to open the color set menu
    * Pressing space will open a help menu for this color set menu
    * Each tile represents a sticker on a face, groups of 3x3 tiles represents a single face
    * The faces are ordered as follows:
    ```
      U
    L F R B
      D
    ```
    * Hover your mouse over a tile to change the color of that tile. Then press the key corresponding to the first letter of the color you wish to set that tile to.
        * `R` - Red
        * `G` - Green
        * `O` - Orange
        * `Y` - Yellow
        * `B` - Blue
        * `W` - White
    * If a set puts the cube in an unsolveable state, a message will appear in the left side message log
* Use the mouse scroll wheel to scroll the message log (shown on the left) if all messages do not fit on the screen
* Press `X` to scramble the cube
* Press `BACKSPACE` to undo a move
* Press `LSHIFT` + `BACKSPACE` to reset the cube
* Press `ENTER` to make one move towards solving the cube
* Press `0` to reset the view of the cube so you are looking directly at it's Front face again

## Terminal Commands
These are the commands for the 2D demo and is no longer "part" of the final program (in the UI) but can be accessed from it


* Press `/` to pause the UI and wait for terminal commands. After pressing `ENTER` in the terminal and sending a command, operation of the 3D UI will resume
* Type `help` to view a help message
* The terminal recognizes the following as colors (either full name or capitalized first letter):
    * White, W, Orange, O, Red, R, Blue, B, Yellow, Y, Green, G
* The capitalized first letter of a face is recognized as faces:
    * F (Front), U (Up), D (Down), L (Left), R (Right), B (Back)
* Colors can be set by the following command format:
    * Each face has 9 positions, (0, 0) to (2, 2) where (0, 0) is the top left
    * `FACE` `X` `Y` <- `COLOR`
    * `F 0 0 <- W` - Sets the top left sticker on the front face to white
    * `B 1 2 <- G`
* An entire face can be set at once with the following format:
    * `FACE` <- `COLOR1` `COLOR2` ...
    * `F <- R O Y G B Y R B G` - Sets the front face colors starting with Red at (0, 0) and Orange at (1, 0) to Blue at (1, 2) and Green at (2, 2)
