(*Library---------------------------------------------------------------------*)
Random.init;;

#load "graphics.cma" ;;

open  Graphics  ;;

open_graph "";;

open_graph " 300 x100";;

let draw (x,y) (x2,y2) =
  moveto x y;
  lineto x2 y2;;

(*2.1.Curves------------------------------------------------------------------*)

(*2.1.1.Mountain--------------------------------------------------------------*)

let rec mountain n (x,y) (x2,y2) =
  if n = 0 then
    draw (x,y) (x2,y2)
  else
    let peak =
        (x+x2)/2
      and
        height =
          (y + y2)/2 + Random.int(abs(x2-x)/5 + 20) in
    begin
      mountain (n-1) (x,y) (peak, height);
      mountain (n-1) (peak, height) (x2,y2)
    end;;

(*2.1.2.Dragon---------------------------------------------------------------*)


let rec dragon  n (x,y) (x2,y2) =
  if n = 0 then
    draw (x,y) (x2,y2)
  else
    let peak =
        (x + x2) / 2 + (y2 - y) / 2
      and
        height =
        (y + y2) / 2 - (x2 - x) / 2 in
    begin
      dragon (n-1) (x,y) (peak, height);
      dragon (n-1) (x2,y2) (peak, height)
    end;;

(*2.2.Surfaces---------------------------------------------------------------*)

(*2.2.1.Serpienski's Sponge--------------------------------------------------*)

let rec draw_squaresponge n  =
  let (x,y) = current_point() in
  if n < 3 then fill_rect x y n n
  else
    begin
      draw_squaresponge (n/3);
      rmoveto 0 (n/3);

      draw_squaresponge (n/3);
      rmoveto 0 (n/3);

      draw_squaresponge (n/3);
      rmoveto (n/3) 0;

      draw_squaresponge (n/3);
      rmoveto (n/3) 0;

      draw_squaresponge (n/3);
      rmoveto 0 (-n/3);

      draw_squaresponge (n/3);
      rmoveto 0 (-n/3);

      draw_squaresponge (n/3);
      rmoveto (-n/3) 0;

      draw_squaresponge (n/3);
      moveto x y;
    end;;

let sponge (x,y) n =
  clear_graph ();
  moveto x y;
  draw_squaresponge n;;


(*2.2.1.Serpienski's Triangle-------------------------------------------------*)


let siertri (x,y) n =
  moveto x y ;
  set_color blue;
  let rec siertri2 (x,y) n i =
    match n with
      n when n < 1 -> ()
       |_ -> begin
           draw (x,y) (x + n,y);
           draw (x + n,y) (x + n/2, y+i);
           draw (x + n/2, y+i) (x,y);
           siertri2 (x + n/2, y) (n/2) (i/2);
           siertri2 (x,y) (n/2) (i/2);
           siertri2 (x + n/4 ,y + i/2) (n/2) (i/2);
         end
  in
  let i = int_of_float(cos(60.) *.(float_of_int n)) in
  siertri2 (x,y) (n) (-i);;




