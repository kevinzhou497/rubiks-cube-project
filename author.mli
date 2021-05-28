(************************************************************ 
   Copyright (C) 2021 Cornell University.
   Created by Michael Clarkson (mrc26@cornell.edu) and CS 3110 course staff.
   You may not redistribute this assignment, distribute any derivatives,
   or use it for commercial purposes.
 ************************************************************)

(** CS 3110 Spring 2021 MS1

    @author Kevin Zhou (klz23)
    @author Stephen Verderame (sev47)
    @author Alii Taha (amt297)
    @author Roger Kim (rk564)*)

(************************************************************ 

   Academic Integrity Statement

   I, the person named in the @author comment above, have fully reviewed the
   course academic integrity policies.  I have adhered to those policies in
   solving the assignment.

   The policies do permit some limited collaboration among students currently
   enrolled in the course. If I did engage in such collaboration, here is the
   list of other students with whom I collaborated, and a brief summary of that
   collaboration:

   - none

   Here are any deviations from the policies that I want to document, with the
   understanding that being honest about my mistakes is more honorable than
   lying about my intellectual work:

  The Matrix module (matrix.ml and matrix.mli) is part of the Ocaml GLES 3.0 library. We only needed the library's
  matrix module, so instead of depending on another Opam package for just one thing, we decided to take
  the source code directly into our project.
  https://github.com/craff/gles3

  The Bigarray utility functions (in util.ml) come directly from Tgls documentation
  OpenGL is a C library ported to Ocaml, as such it is inherintly imperative and we could not always use
  purely functional features when interacting with it

  We referenced the following github repository in working on the pattern matching implementation of solving
  https://github.com/benbotto/rubiks-cube-cracker

  We used an A* implementation (in Astar.ml) from https://gist.github.com/akabe/ad9a6deaaa639124944f

 ************************************************************)

(** [hours_worked] is the number of hours you worked on this assignment. *)
val hours_worked : int
