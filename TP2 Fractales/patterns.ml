(*1.1.1-----------------------------------------------------------------------*)

let rec build_line a str  =
  if a<0 then
    failwith "build_line : number of line must be positive number"
  else
    if a=0 then ""
    else str^(build_line ( a-1) str);;

(*1.1.2-----------------------------------------------------------------------*)

let square a str =
  let rec square_aux b str a =
    if a < (-1) then failwith "square : number of line must be a positive number"
    else
      if b = 0 then ""
      else build_line a str ^ "\n" ^ square_aux (b-1) str a
  in
  print_string(square_aux  a str a);;

(*1.1.3-----------------------------------------------------------------------*)

let square2 a (str1,str2) =
  if a < (-1) then failwith "square2 : number of line must pe a positive number"
  else let rec square2_aux b =
         match b with
           |b when a = b -> ""
           |_ -> if b mod 2 = 0 then
               build_line a (str1^str2)^"\n"^square2_aux(b+1)
             else
               build_line a (str2^str1)^"\n"^square2_aux(b+1)

       in print_string(square2_aux 0);;

(*1.1.4-----------------------------------------------------------------------*)

let triangle a str =
let rec triangle_aux a b str =
  if a < (-1) then failwith "triangle : number of line must be a positive number"
  else
    if a = 0 then ""
    else build_line b str ^ "\n" ^ triangle_aux (a-1) (b+1) str
in
print_string(triangle_aux a 1 str );;

(*1.2.1-----------------------------------------------------------------------*)

let pyramid n (str1,str2) =
  if n <= 0 then failwith "pyramid : number of line must be a positive number"
  else let m = n in
    let rec pyramid_aux n n2 m =
         if m = 0 then ""
         else build_line n str1 ^ build_line n2 str2
           ^ build_line n2 str2 ^ build_line n str1 ^ "\n"
           ^ pyramid_aux (n-1) (n2+1) (m-1)
    in
    print_string(pyramid_aux (n-1) 1 (m));;

