#use "list_tools.ml";;

(*Exercise 1*)


let is_prime x =
  let divisors x =
    let rec divisors_aux x y z=
      if y = 1 then z
      else
        if (x mod y) = 0 then divisors_aux x (y-1) (z+1)
        else divisors_aux x (y-1) z in
    divisors_aux x x 1 in
  divisors x = 2;;


let decompose x =
  if x <= 0 then invalid_arg "decompose : parameter <= 1"
  else
    let rec decompose_aux  x y z =
      if is_prime x then (append z [x])
      else
        if (x mod y = 0) then decompose_aux (x / y) y (append z [y])
        else decompose_aux x (y+1) z in
    decompose_aux x 2 [];;


let rec shared l1 l2 = match (l1, l2) with
  | ([], _) | (_, []) -> []
  | (e1::l1, e2::l2) -> if e1 = e2 then
                          e1::(shared l1 l2)
                        else if e1 > e2 then
                          shared (e1::l1) l2
                        else
                          shared l1 (e2::l2);;

let rec gcd x y =
  if x >= y then
    if y = 0 then
      x
    else
      gcd y (x mod y)
  else
    if gcd y x <= 2 then invalid_arg "gcd : parameters  < 2"
    else
      gcd y x;;

