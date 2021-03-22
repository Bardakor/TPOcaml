(*1.Toolbox*)

(*1.1. Basic*)


let rec lenght l =
match l with
   |[]->0
   |e::l->1+(lenght l);;

let rec append l1 l2 =
  match l1 with
   | [] -> l2
   |e :: l -> e :: append l l2;;

let rec product l =
  match l with
  |[] -> 0
  |e :: l -> e * product  l;;

let rec nth n = function
  |[] -> failwith "Empty list"
  |e :: l -> if n = 1 then e else nth (n-1) l;;

let search_pos n l1 =
  let rec seak n l1 i =
    match l1 with
      [] -> failwith "Empty list"
    |e :: l ->
      if e = n then i
      else seak n l (i+1) in
  seak n l1 0;;

let assoc k list =
  if k <= 0 then invalid_arg "k not a natural number"
  else
let rec seak = function
    [] -> failwith "not found"
  |(key, value)::l ->  if key = k then value
                     else
                       if k < key then
                         failwith "not round"
                       else seak list in
seak list;;

(*1.2. Build - Modify*)

let rec init_list n value =
  match n  with
  |n when n < 0 -> invalid_arg "init_list : n must be a natural"
  |0 -> []
  |_ -> value :: (init_list (n-1) value);;

let rec put_list value n l =
  match l with
  |[] -> []
  |e :: l -> if n = 0 then value :: l
             else e :: put_list value (n-1) l;;

(*1.3 ’a list list*)

let rec init_board (l,c) value =
  match (l,c) with
  |(0,l) -> []
  |(c,l) ->  (init_list l value) :: init_board ((c-1) , l) value;;

let rec get_cell (x,y) l = nth y (nth x l);;

let rec put_cell value (x,y) board =
  put_list (put_list value y (nth y board)) x board;;






