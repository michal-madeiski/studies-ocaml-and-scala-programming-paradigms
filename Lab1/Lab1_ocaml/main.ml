(*zad1*)
let zad1(a, b, c)  =
  if a <= 0.0 || b <= 0.0 || c <= 0.0 then raise (Failure "Invalid value of a or b or c!")
  else if abs_float(b-.c) < a && a < b+.c then
  let p = (a+.b+.c)/.2.0 in sqrt(p*.(p-.a)*.(p-.b)*.(p-.c))
  else raise(Failure "Invalid value of a or b or c!");;
zad1(2.0, 3.0, 4.0);;
(*zad1*)

(*zad2*)
let zad2(n, x) =
  if n < 0.0 then raise (Failure "Invalid value of n!")
  else let  rec rec_zad2(pow, mult, i, s, silnia) =
    if i>n then s
    else let add = (mult*.pow)/.silnia in
     rec_zad2(pow*.x, mult*.(-1.0), i+.1.0, s+.add, silnia*.(i+.1.0))
   in  rec_zad2(x, -1.0, 1.0, 0.0, 1.0);;
zad2(5.0, 3.0);;
(*zad2*)

(*zad3*)
let zad3(r) =
  if r < 0.0 then raise (Failure "Invalid value of n!")
  else let  rec rec_zad3(i, n, s) =
    if s>r then n
    else let add = 1.0/.i in
     rec_zad3(i+.1.0, n+1,  s+.add)
   in  rec_zad3(1.0, 0, 0.0);;
zad3(2.3);;
(*zad3*)

(*zad4*)
let zad4(list) =
  if list = [] then raise (Failure "List cannot be empty!")
  else let rec rec_zad4(list,  min) =
         if list = [] then min
         else if List.hd list < min then rec_zad4(List.tl list, List.hd list)
         else rec_zad4(List.tl list, min)
       in rec_zad4(list, max_float);;
zad4([2.0; 3.0; 1.1; 4.0; 10.0]);;
(*zad4*)

(*zad5*)
let zad5(list) =
  if list = [] then raise (Failure "List cannot be empty!")
  else let listSmallerThanZero = [] in 
       let listBiggerThanZero = [] in
       let rec rec_zad5(list, lS, lG)=
         if list = [] then (lS, lG)
         else if List.hd list < 0 then rec_zad5(List.tl list, lS@[List.hd list], lG)
         else rec_zad5(List.tl list, lS, lG@[List.hd list])
       in rec_zad5(list, listSmallerThanZero, listBiggerThanZero);;
zad5([-1; 0; -2; 3; 4]);;
(*zad5*)

