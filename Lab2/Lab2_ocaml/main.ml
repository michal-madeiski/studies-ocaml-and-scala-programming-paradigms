(*zad1*)
let func1(a, b)=
  if a+b>10 then (a, b)
  else (1, 1);;
func1(6, 6);;
func1(2, 2);;
func1(2.0, 2.0);;

let func2(a, b)=
  if a+.b>0.0 then true
  else false;;
func2(1.0, 1.0);;
func2(-0.5, -0.5);;
func2(1, 1);;

let func3(list, a) =
  if a>0 then [List.hd list]
  else List.tl list;;
func3([2; 1; 1], 1);;
func3([2; 1; 1], 0);;
(*zad1*)

(*zad3*)
let zad3(eps)=
  if eps <= 0.0 then raise (Failure "error")
  else let rec rec_zad3(el, mult)=
         let x = sqrt(0.5 +. 0.5*.el)in
         let p = mult in
         let next = mult*.x in
         if abs_float(next -.p) < eps then  2.0/.mult
         else rec_zad3(x, mult*.x)
       in rec_zad3(sqrt(0.5), sqrt(0.5));;
zad3(0.0000000000000002);;
(*zad3*)
