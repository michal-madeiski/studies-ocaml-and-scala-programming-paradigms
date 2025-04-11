import scala.annotation.tailrec

//zad1
sealed trait BT[+A]
case class Empty[A]() extends BT[A]
case class Node[A](value: A, left: BT[A], right: BT[A]) extends BT[A]

val treeList: BT[List[Int]] = Node(List(1,2,3), Node(List(1,2,3,4), Empty(), Empty()), Node(List(1,2,3,4,5), Empty(), Empty()))

def treeListSum(tree: BT[List[Int]]): BT[Int] =
  tree match
    case Empty() => Empty()
    case Node(list, left, right) => Node(list.foldLeft(0)((acc, el) => acc + el), treeListSum(left), treeListSum(right))

treeListSum(treeList)
//zad1

//zad2
sealed trait Graph[+A]
case class GNode[A](value: A, connections: List[Graph[A]]) extends Graph[A]

val graph: Graph[Int]= GNode(1, List(GNode(2, Nil), GNode(3, List(GNode(6, Nil), GNode(7, Nil), GNode(8, Nil), GNode(9, Nil))), GNode(4, Nil), GNode(5, Nil)))

//z pomijaniem powyzej dwoch polaczen
def treeFromGraph1[A](graph: Graph[A]): BT[A] =
  def gNodeToTree(g: Graph[A]): Node[A] =
    g match
      case GNode(value, connections) => {
        connections match
          case Nil => Node(value, Empty(), Empty())
          case hd::Nil => Node(value, gNodeToTree(hd), Empty())
          case hd::tl => Node(value, gNodeToTree(hd), gNodeToTree(tl.head))
      }
  gNodeToTree(graph)
  
treeFromGraph1(graph)
//z pomijaniem powyzej dwoch polaczen

//bez pomijania powyzej dwoch polaczen
def treeFromGraph2[A](graph: Graph[A]): BT[A] =
  def valuesFromQueue(queue: List[Graph[A]], list: List[A]): List[A] =
    queue match
      case Nil => list
      case hd::Nil => hd match
        case GNode(v, c) => c match
          case Nil => v::list
          case _ => valuesFromQueue(c, v::list)
      case hd::tl => hd match
        case GNode(v, c) => c match
          case Nil => valuesFromQueue(tl, v::list)
          case _ => {
            val l1 = valuesFromQueue(c, v::list)
            valuesFromQueue(tl, l1)
          }
  
  def insertVal(tree: BT[A], value: A, flag: Boolean): BT[A] =
    (flag, tree) match
      case (_, Empty()) => Node(value, Empty(), Empty())
      //dwa wolne miejsca -> wstawianie zgodnie z false=lewo i true=prawo
      case (false, Node(v, Empty(), Empty())) => Node(v, insertVal(Empty(), value, flag), Empty())
      case (true, Node(v, Empty(), Empty())) => Node(v, Empty(), insertVal(Empty(), value, flag))
      
      //jedno wolne miejsce -> wstawianie w to miejsce
      case (_, Node(v, Empty(), r)) => Node(v, insertVal(Empty(), value, flag), r)
      case (_, Node(v, l, Empty())) => Node(v, l, insertVal(Empty(), value, flag))
      
      //brak wolnych miejsc -> szukanie zgodnie z false=lewo i true=prawo
      case (false, Node(v, l, r)) => Node(v, insertVal(l, value, !flag), r)
      case (true, Node(v, l, r)) => Node(v, l, insertVal(r, value, !flag))

  @tailrec
  def attachValuesToTree(tree: BT[A], values: List[A]): BT[A] =
    values match
      case Nil => tree
      case hd::tl => attachValuesToTree(insertVal(tree, hd, false), tl)

  def gNodeToTree(g: Graph[A], queue: List[Graph[A]]): (Node[A], List[Graph[A]]) =
    g match
      case GNode(value, connections) => {
        connections match
          case Nil => (Node(value, Empty(), Empty()), queue)
          case hd::Nil => (Node(value, gNodeToTree(hd, queue)._1, Empty()), queue)
          case first::second::rest => {
            val (leftTree, queue1) = gNodeToTree(first, queue)
            val (rightTree, queue2) = gNodeToTree(second, rest ++ queue1)
            (Node(value, leftTree, rightTree), queue2)
          }
      }
  val temp = gNodeToTree(graph, List.empty)
  val t = temp._1
  val q = temp._2
  val valList = valuesFromQueue(q, List.empty)
  attachValuesToTree(t, valList)
  
treeFromGraph2(graph)
//bez pomijania powyzej dwoch polaczen
//zad2

