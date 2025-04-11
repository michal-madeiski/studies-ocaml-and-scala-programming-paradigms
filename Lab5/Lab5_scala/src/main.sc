import scala.annotation.tailrec

//zad1.1
def minMax(list2D : List[List[Int]]) : List[(Int, Int)] =
  list2D.map(l => {if l == Nil then throw new RuntimeException("List cannot be empty!")})
  @tailrec
  def minMax1List(list : List[Int], min : Int, max : Int) : (Int, Int) =
    list match
      case Nil => (min, max)
      case hd::_ if hd > max => minMax1List(list.tail, min, hd)
      case hd::_ if hd < min => minMax1List(list.tail, hd, max)
      case _ => minMax1List(list.tail, min, max)
  @tailrec
  def minMaxEveryList(list2Dd : List[List[Int]], listAct : List[(Int, Int)]) : List[(Int, Int)] =
    list2Dd match
      case Nil => listAct.reverse
      case hd::tl => minMaxEveryList(list2Dd.tail, minMax1List(list2Dd.head, hd.head, hd.head) :: listAct)
  minMaxEveryList(list2D, Nil)

minMax(List(List(0, 1, 2, 3), List(-3,-2,-1,0), List(1)))
//zad1.1

//zad1.2
def minMax2(list2D : List[List[Int]]) : List[(Int, Int)] =
  def minMax1List2(list : List[Int]) : (Int, Int) =
    list.foldLeft(list.head, list.head)((acc, n) => {
      if n < acc._1 then (n, acc._2)
      else if n > acc._2 then (acc._1, n)
      else acc
    })

  list2D.map(l => {
    l match
      case hd::Nil => (hd, hd)
      case hd::tl => minMax1List2(l)
      case Nil => throw new RuntimeException("List cannot be empty!")
  })
minMax2(List(List(0, 1, 2, 3), List(-3,-2,-1,0), List(1)))
//zad1.2

//zad3
val dx = 0.0000001

def deriv(f: Double => Double) : Double => Double =
  (x: Double) => (f(x + dx) - f(x))/dx

def derivOfDeriv(f: Double => Double, g: Double => Double) : Double => Double =
  (x: Double) => (deriv(g)(deriv(f)((x))))

derivOfDeriv(math.sin, math.cos)(0)
//zad3




