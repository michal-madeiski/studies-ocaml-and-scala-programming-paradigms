type 'a llist = LNil | LCons of 'a * 'a llist Lazy.t;;

let rec lfrom k = LCons(k, lazy (lfrom(k + 1)));;

let rec ltake(n, lxs) =
  match (n, lxs) with
  |(0, _) -> []
  |(_, LNil) -> []
  |(n, LCons(x, lazy xf)) -> x::ltake(n - 1, xf);;

(*zad1*)
let replicateByFunc llist fn =
  let rec copyEl n pos ll =
    match ll with
    |LNil -> LNil
    |LCons(v, _) when n > 0 -> LCons(v, lazy (copyEl (n-1) pos ll))
    |LCons(v, lazy ltl) -> copyEl (fn(pos+1)) (pos+1) ltl in
  copyEl (fn 1) 1 llist;;

ltake(20, replicateByFunc (lfrom 1) (fun x -> 2*x - 5));;
(*zad1*)

(*zad2*)
type slowo = int*int;;

let slownik1 = [(2, 2); (4, 1); (6, 3)];;

let rec dodajSlowo x slownik =
  match slownik with
  |(k, v)::tl when x > k -> (k, v)::dodajSlowo x tl
  |(k, v)::tl when x < k -> (x, 1)::(k, v)::tl
  |(k, v)::tl when x = k -> (k, v + 1)::tl
  |_ -> [(x, 1)];;

let slownik2 = dodajSlowo 1 (dodajSlowo 4 (dodajSlowo 5 (dodajSlowo 7 slownik1)));;
let slownik3 = dodajSlowo 1 slownik2;;
(*zad2*)
