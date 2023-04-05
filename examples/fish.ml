open Graphics

let fish () =
  (* Draw the body of the fish *)
  moveto 150 150;
  lineto 200 200;
  lineto 250 175;
  lineto 200 150;
  lineto 150 150;
  fill_poly [| (150, 150); (200, 200); (250, 175); (200, 150) |];

  (* Draw the tail of the fish *)
  moveto 250 175;
  lineto 275 200;
  lineto 275 150;
  lineto 250 175;
  fill_poly [| (250, 175); (275, 200); (275, 150) |];

  (* Draw the eye of the fish *)
  set_color black;
  fill_circle 185 170 5;

  (* Draw the mouth of the fish *)
  set_color red;
  fill_poly [| (170, 155); (180, 165); (170, 175) |]
;;

open_graph " 480x480";;
fish ();;

ignore (wait_next_event [ Button_down ]);
close_graph ()
