(*list_tools.ml*)

let rec lenght l =
  match l with
   |[]->0
   |e::l->1+(lenght l);;

let nth x l =
  if x<0 then invalid_arg "x must be positive"
  else let rec tmp (x,l) = match (x,l) with
  (_,[])->failwith "list should not be empty"
    |(x,l) when x>lenght l -> failwith "nth list is too short"
    |(0,e::_)->e
    |(i,_::l)->tmp((i-1),l) in tmp (x,l);;


(*let rec nth n = function
  |[] -> failwith "Empty list"
  |e :: l -> if n = 1 then e else nth (n-1) l;;*)

let rec put_list value n l =
  match l with
  |[] -> []
  |e :: l -> if n = 0 then value :: l
             else e :: put_list value (n-1) l;;

let rec init_list n value =
  match n  with
  |n when n < 0 -> invalid_arg "init_list : n must be a natural"
  |0 -> []
  |_ -> value :: (init_list (n-1) value);;


let rec init_board (l,c) value =
  match (l,c) with
  |(0,l) -> []
  |(c,l) ->  (init_list l value) :: init_board ((c-1) , l) value;;

let rec get_cell (x,y) l = nth y (nth x l);;

let rec put_cell value (x,y) board =
  put_list (put_list value y (nth y board)) x board;;


