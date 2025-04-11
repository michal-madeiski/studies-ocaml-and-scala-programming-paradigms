import scala.annotation.tailrec

//zad1
def eulerConst(eps: Double): Double =
  if eps <= 0 then throw new Exception("Eps must be above zero!")
  else {
    @tailrec
    def rec_eulerConst(n: Double, act: Double, prev: Double): Double =
      if math.abs((act - math.log(n)) - (prev - math.log(n - 1))) < eps then act - math.log(n)
      else rec_eulerConst(n + 1, act + 1/(n + 1), act)
    rec_eulerConst(1, 1, 0)
  }
eulerConst(0.0000000000001)
//zad1

//zad2
def foldFuncs1(list: List[Double => Double]): Double => Double =
  if list == Nil then throw new Exception("List cannot be empty!")
  else {
    @tailrec
    def rec_foldFuncs1(list: List[Double => Double], f: Double => Double): Double => Double =
      list match
        case hd::tl => rec_foldFuncs1(tl, (x: Double) => hd(f(x)))
        case Nil => f
    rec_foldFuncs1(list.tail, (x: Double) => list.head(x))
  }
foldFuncs1(List((x: Double) => x*2, (x: Double) => x*3, (x: Double) => x*4))(1)

def foldFuncs2(list: List[Double => Double]): Double => Double =
  if list == Nil then throw new Exception("List cannot be empty!")
  list.foldLeft((x: Double) => x)((acc, f) => (x: Double) => f(acc(x)))
foldFuncs2(List((x: Double) => x*2, (x: Double) => x*3, (x: Double) => x*4))(1)
//zad2