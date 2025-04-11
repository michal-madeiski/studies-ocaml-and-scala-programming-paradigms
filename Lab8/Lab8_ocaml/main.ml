(*zad3*)
type 'a tree = Empty | Node of 'a * 'a tree * 'a tree;;

let t1 = Node(1, Node(2, Node(4, Empty, Empty), Node(5, Empty, Empty)), Node(3, Node(2, Node(4, Empty, Empty), Node(5, Empty, Empty)), Empty));;
let t2 = Node(2, Node(4, Empty, Empty), Node(5, Empty, Empty))
let t3 = Node(6, Empty, Empty);;

let rec isEqual t1 t2 =
  t1 = t2;;
isEqual t2 t1;;
isEqual t3 t1;;


let rec isSubTree t1 t2 =
  if isEqual t1 t2 then true
  else match t2 with
       |Node(_, l, r) -> (isSubTree t1 l) || (isSubTree t1 r)
       |_ -> false;;
isSubTree t2 t1;;
isSubTree t3 t1;;

let countSubTree t1 t2 =
  let rec rec_countSubTree tree =
    match tree with
    |Node(v, l, r) -> if isEqual t1 tree then 1 else rec_countSubTree l + rec_countSubTree r
    |_ -> 0 in
  rec_countSubTree t2;;
countSubTree t2 t1;;
countSubTree t3 t1;;
(*zad3*)
