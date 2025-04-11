import scala.annotation.tailrec

//zad1
def zad1(a:Double, b:Double, c:Double) : Double =
  if a <= 0 || b <= 0 || c <= 0 then throw new RuntimeException("Invalid values!")
  else if math.abs(a-b) < a && a < b + c then {val p:Double = (a+b+c)/2
    math.sqrt(p*(p-a)*(p-b)*(p-c))}
  else throw new RuntimeException("Invalid values!")
zad1(2.0, 3.0, 4.0)
//zad1

//zad2
def zad2(n:Int, x:Double) : Double =
  if n < 0 then throw new RuntimeException("Invalid value of n!")
  else {
    @tailrec
    def rec_zad2(pow:Double, mult:Int, i:Int, s:Double, silnia:Int) : Double =
    if i>n then s
    else {val add:Double = (mult*pow)/silnia
      rec_zad2(pow*x, mult*(-1), i + 1, s + add, silnia*(i+1))}
    rec_zad2(x, -1, 1, 0.0, 1)
  }
zad2(5, 3.0)
//zad2

//zad3
def zad3(R:Double) : Int =
  if R < 0 then throw new RuntimeException("Invalid value of R!")
  else {
    @tailrec
    def rec_zad3(i:Double, n:Int, s:Double) : Int =
  if s > R then n
  else {val add:Double = 1.0/i
    rec_zad3(i+1, n+1, s+add)}
  rec_zad3(1.0, 0, 0.0)
  }
zad3(2.3)
//zad3

//zad4
def zad4(list:List[Double]):Double =
  if list == Nil then throw new RuntimeException("List cannot be empty!")
  else {
    @tailrec
    def rec_zad4(list: List[Double], min: Double):Double =
      if list == Nil then min
      else if list.head < min then rec_zad4(list.tail, list.head)
      else rec_zad4(list.tail, min)
    rec_zad4(list, Float.MaxValue)
  }
zad4(List(2.0, 3.0, 1.1, 4.0, 10.0))
//zad4

//zad5
def zad5(list:List[Int]):List[List[Int]] =
  if list == Nil then throw new RuntimeException("List cannot be empty!")
  else {val listSmallerThanZero:List[Int] = Nil
  val listBiggerThanZero:List[Int] = Nil
  @tailrec
  def rec_zad5(list:List[Int], lS:List[Int], lG:List[Int]):List[List[Int]]=
    if list == Nil then List(lS, lG)
    else if list.head < 0 then rec_zad5(list.tail, lS ++ List(list.head), lG)
    else rec_zad5(list.tail, lS, lG++List(list.head))
  rec_zad5(list, listSmallerThanZero, listBiggerThanZero)
  }
zad5(List(-1, 0, -2, 3, 4))
//zad5