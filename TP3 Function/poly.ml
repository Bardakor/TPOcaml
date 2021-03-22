(*Exercise 2 : Representing polynomials with lists*)

(*Library*)
#use "pgcd.ml";;

let rec add_poly l1 l2 =
  match (l1,l2) with
    ([],[]) -> []
   |([],_) -> l2
   |(_,[]) -> l1
  |(((e1,e2) :: l3),((e3,e4)::l4)) -> if e2 = e4 then
                                        if e1 + e3 = 0 then
                                          add_poly l3 l4
                                        else append [(e1 + e3 , e2)] (add_poly l3 l4)
                                      else
                                        if e2 > e4 then append [(e1 , e2)] (add_poly l3 l2)
                                        else
                                          append [(e3,e4)] (add_poly l1 l4);;
let rec times l1 (x, y) =
  match (l1, (x,y)) with
    ([],_) -> []
  |(_,(0,_)) -> []
  |(z,t)::l2,(x,y) -> append [x * z, y * t] (times l2 (z,t));;


let rec product_poly (l1, l2) =
  match (l1, l2) with
  | [], [] -> []
  | _::_, [] -> []
  |_, (z, t)::l2 -> add_poly (times l1 (z, t)) (product_poly (l1, l2))
;;

