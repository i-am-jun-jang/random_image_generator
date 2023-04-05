open Graphics
open Rig

let width = 500
let height = 500

let () =
  Random.self_init ();
  open_graph (" " ^ string_of_int width ^ "x" ^ string_of_int height);
  Motion_image.animate_pixels max_int width height;
  close_graph ();
  exit 0
