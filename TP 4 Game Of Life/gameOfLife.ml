#use "list_tools.ml";;
#load "graphics.cma";;
open Graphics;;
open_graph "";;

let open_window size = open_graph (" " ^ string_of_int size ^ "x" ^ string_of_int (size+20));;

(*Color settings*)

let grey = rgb 127 127 127;;

(*Draw Cell*)

let draw_cell (x,y) size color =
  set_color grey;
  draw_rect (x) (y) size size;
  set_color color;
  fill_rect (x+1) (y+1) (size-2) (size-2);;

(*Cell Color*)

let cell_color = function
    0 -> white
   |_ -> black;;

(*Draw board*)

let draw_board board cell_size =
  let rec tmp board (x,y) = match board with
      [] -> ()
    |l::board when l=[] -> tmp board (x+cell_size,0)
    |l::board -> match l with
                   [] -> tmp board (x+cell_size,0)
                  |e::l -> draw_cell (x,y) cell_size (cell_color e);
                           tmp (l::board) (x, y+cell_size)
  in tmp board (0,0);;

(*Matrix*)

let board = [[1;1;1;1;1;1;1;1;1;1];
           [0;0;0;0;0;0;0;0;0;0];
           [1;0;1;0;1;0;1;0;1;0];
           [0;1;0;1;0;1;0;1;0;1];
           [0;0;0;0;0;0;0;0;0;0];
           [1;1;1;1;1;1;1;1;1;1];
           [0;0;0;0;0;0;0;0;0;0];
           [1;0;1;0;1;0;1;0;1;0];
           [0;1;0;1;0;1;0;1;0;1];
           [0;0;0;0;0;0;0;0;0;0]];;

(*Display Board*)

let test_display board cell_size =
  open_window (lenght board * cell_size + 40);
  draw_board board cell_size;;

(*2 The Game - Rules*)

(*2.1 rules 0*)

let rec rules0 cell n = match cell with
    0 -> if n = 3 then 1 else 0
  |1 -> if n = 2 || n = 3 then 1 else 0
  |_ -> failwith "cell content not defined";;

(*2.2 count_neighbors*)

let count_neighbours (x,y) board size =
  if board = [[]] then failwith "board must be empty"
  else
    let tmp (x,y) = if (x < 0 || y < 0 || x>=size || y >= size)
                    then 0
                    else get_cell (x,y) board in
    tmp (x-1 , y-1)
    + tmp (x , y +1)
    + tmp (x+1 , y+1)
    + tmp (x-1 , y)
    + tmp (x-1 , y-1)
    + tmp (x , y-1)
    + tmp (x+1 , y+1);;

(*Life*)

(*1 Seed Life*)

let random = Random.int;;

let seed_life board size nb_cell =
  if board = [] then [[]]
  else let rec tmp board nb_cell =
         match nb_cell with
           0 -> board
         |_ -> let (x,y) = (random size, random size) in
               if (get_cell (x,y) board) = 1 then tmp board nb_cell
               else tmp (put_cell 1 (x,y) board) (nb_cell-1) in
       tmp board nb_cell;;

(*2 New Board*)

let new_board size nb =
  seed_life (init_board (size, size) 0) size nb;;

(*3 Next Generation*)

let next_generation board size =
  let rec tmp1 (x,y) new_board =
    match new_board with
      [] -> []
     |e1 :: new_board -> let rec tmp2 (x,y) e = match e with
                             [] -> []
                            |e2 :: e1 -> (rules0(get_cell (x,y) board) (count_neighbours (x,y) (board) size)) :: tmp2 (x , y+1) e1 in
                         tmp2 (x,y) e1 :: (tmp1 (x+1 , y) new_board) in
  tmp1 (0,0) board;;

(*4 Game*)

let cell_size = 10;;

let rec game board size n = match n with
    0 -> test_display board cell_size
  |_ -> test_display board cell_size;
        game (next_generation board size) size (n-1);;

(*5 New Game*)

let rec new_game size nb_cell n =
  open_window size;
  game (new_board size nb_cell) size n;;










