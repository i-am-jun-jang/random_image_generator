open Graphics
open Rig
open Art

let () =
  open_graph " 800x800";
  make_random_tree ();
  ignore (wait_next_event [ Button_down ]);
  close_graph ()
