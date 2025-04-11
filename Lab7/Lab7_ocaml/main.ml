(*zad3*)
type 'a drzewo = Lisc of 'a | Wezel of 'a drzewo * 'a drzewo;;

let t1 = Lisc 1;;
let t2 = Wezel(Lisc 1, Wezel(Lisc 1, Lisc 2));;
let t3 = Wezel(Lisc 3, Wezel(Lisc 4, Lisc 5));;
let t4 = Wezel(Wezel(Lisc 1, Lisc 2), Wezel(Wezel(Lisc 3, Wezel(Lisc 4, Lisc 5)), Lisc 6));;

let rec isSubTree t1 t2 =
  match (t1, t2) with
  |(Lisc l1, Lisc l2) -> l1 = l2
  |(Wezel(r1, l1), Wezel(r2, l2)) -> (isSubTree r1 r2 && isSubTree l1 l2)
                                     || isSubTree t1 r2
                                     || isSubTree t1 l2
  |(_, Wezel(r2, l2)) -> isSubTree t1 r2 || isSubTree t1 l2
  |(_, Lisc _) -> false;;
isSubTree t1 t2;;
isSubTree t2 t1;;
isSubTree t3 t4;;
isSubTree t4 t3;;
(*zad3*)
 
  
