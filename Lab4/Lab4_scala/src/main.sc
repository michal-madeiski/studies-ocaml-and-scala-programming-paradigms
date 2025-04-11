import scala.annotation.tailrec

//zad1
def pow(x : Double, n : Int) : Double =
  if n == 0 && x == 0.0 then throw new Exception("Invalid combination of x and n!")
  else {
    @tailrec
    def rec_pow(result: Double, x: Double, n: Int): Double =
      if n == 0 then result
      else rec_pow(result * x, x, n - 1)
    rec_pow(1.0, x, n)
  }

def nRoot(n : Int, a : Double, eps : Double) : Double =
  if n <= 0 then throw new Exception("Invalid value of n!")
  else if a <= 0 then throw new Exception("Invalid value of a!")
  else {
    @tailrec
    def rec_nRoot(prev : Double, act : Double, start : Double, end : Double) : Double =
      if math.abs(act - prev) < eps then act
      else {
        val mid : Double = 0.5*(end + start)
        if (a - pow(start, n)) * (a - pow(mid, n)) < 0 then rec_nRoot(act, mid, start, mid)
        else rec_nRoot(act, mid, mid, end)
      }
    rec_nRoot(0.0, math.max(a + eps, 1 + eps), 0.0, math.max(a + eps, 1 + eps)) //a + eps dla >= 1; 1 + eps dla < 1
  }
nRoot(3, 4, 0.0000000000001)
//zad1

//zad3
def removeStartEnd(list : List[Int], n : Int) : List[Int] =
  if n*2 > list.length || n < 0 then throw new Exception("Invalid value of n!")
  if list == Nil then Nil
  else if n == 0 then list  
  else {
    @tailrec
    def rec_removeStartEnd(listAct: List[Int], removed: Int): List[Int] =
      removed < 2*n match
        case false => listAct.reverse
        case true if removed < n => rec_removeStartEnd(listAct.tail, removed + 1)
        case true if removed == n => rec_removeStartEnd(listAct.reverse.tail, removed + 1)
        case true if removed > n => rec_removeStartEnd(listAct.tail, removed + 1)
    rec_removeStartEnd(list, 0)
  }
removeStartEnd(List(1,2,3,4,5,6,7,8), 2)
removeStartEnd(List(1,2,3,4,5,6,7,8), 4)
removeStartEnd(List(1,2,3,4,5,6,7,8), 5)
//zad3