import scala.annotation.tailrec

//zad2
type Samochod = (String, String, Int)
type Samochody = List[Samochod]

def brandAndCount(list: Samochody): List[(String, Int)]=
  if list == Nil then Nil
  else {
    val listCopy = list
    @tailrec
    def containsTuple2(l: List[(String, Int)], t: (String, Int)): Boolean =
        if l == Nil then false
        else (l, l.head._1 == t._1 && l.head._2 == t._2) match
            case (_, true) => true
            case (hd::tl, false) => containsTuple2(l.tail, t)
            case (hd::Nil, false) => false
    @tailrec
    def countForOne(s: Samochod, l: List[Samochod], n: Int): Int =
      if l == Nil then n
      else l match
        case hd::tl if s._1 == l.head._1 => countForOne(s, l.tail, n + 1)
        case hd::tl if s._1 != l.head._1 => countForOne(s, l.tail, n)
        case hd::Nil => countForOne(s, Nil, n)

    list.foldLeft(List.empty[(String, Int)])((listAcc, el) => if !containsTuple2(listAcc, (el._1, countForOne(el, list, 0))) then listAcc:+(el._1, countForOne(el, list, 0)) else listAcc)
  }

val test: Samochody = List(("Opel", "astra", 1999), ("Renault", "megane", 2004), ("Opel", "corsa", 2009), ("Nissan", "micra", 2004), ("Opel", "corsa", 2009), ("Nissan", "micra", 2003))
brandAndCount(test)
//zad2

//zad3
sealed trait Tree[+A]
case class Node2[+A](l: Tree[A], r: Tree[A]) extends Tree[A]
case class NodeR[+A](r: Tree[A]) extends Tree[A]
case class NodeL[+A](l: Tree[A]) extends Tree[A]
case class Leaf[+A](v: A) extends Tree[A]

sealed trait TreeData
case class P[+B](data: (String, B)) extends TreeData
case class S(data: String) extends TreeData

def postfixToHeteroList[A](tree: Tree[A]): List[TreeData] =
  tree match
    case Leaf(x) => List(P(("Dana", x)))
    case Node2(left, right) =>
      val leftList = postfixToHeteroList(left)
      val rightList = postfixToHeteroList(right)
      (left, right) match
        case (Leaf(_), Leaf(_)) => leftList ++ rightList ++ List(S("Wezel (element, element"))
        case (Leaf(_), _) => leftList ++ rightList ++ List(S("Wezel (element, prawo)"))
        case (_, Leaf(_)) => leftList ++ rightList ++ List(S("Wezel (lewo, element)"))
        case (_, _) => leftList ++ rightList ++ List(S("Wezel (lewo, prawo)"))
    case NodeR(right) =>
      val rightList = postfixToHeteroList(right)
      right match
        case Leaf(_) => rightList ++ List(S("Wezel (element)"))
        case _ => rightList ++ List(S("Wezel (prawo)"))
    case NodeL(left) =>
      val leftList = postfixToHeteroList(left)
      left match
        case Leaf(_) => leftList ++ List(S("Wezel (element)"))
        case _ => leftList ++ List(S("Wezel (lewo)"))
val t1: Tree[Int] = Node2(Node2(Node2(Leaf(2), Leaf(1)), NodeR(Leaf(4))), Leaf(3))
postfixToHeteroList(t1)
//zad3