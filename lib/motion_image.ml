open Graphics

let rec draw_pixels n w h =
  if n > 0 then (
    let x = Random.int w in
    let y = Random.int h in
    let color = Random.int 0xffffff in
    let size = Random.int 10 in
    set_color
      (rgb (color land 0xff)
         ((color lsr 8) land 0xff)
         ((color lsr 16) land 0xff));
    fill_rect x y size size;
    draw_pixels (n - 1) w h)

let rec animate_pixels n w h =
  if n > 0 then (
    clear_graph ();
    draw_pixels (w * h / 100) w h;
    synchronize ();
    let evt = wait_next_event [ Poll ] in
    if evt.keypressed then animate_pixels 0 w h else animate_pixels (n - 1) w h)
