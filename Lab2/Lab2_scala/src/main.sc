import scala.annotation.tailrec

//zad1
def func1(a:(Int, Int)):(Int, Int)=
  if a._1+a._1>10 then (a._1, a._2)
  else (1, 1)
func1(6, 6)

def func2(a:(Float, Float)):Boolean=
  if a._1+a._1>0.0 then true
  else false
func2(1, 1)

def func3[A](a:(List[A], Int)):List[A]=
  if a._2>0 then List(a._1.head)
  else a._1.tail
func3(List(2, 1, 1), 1)
//zad1

//zad2
@tailrec
def zad2(list:List[Int]) : Boolean=
  if list == Nil then throw new Exception("List cannot be empty!")
  else if list.tail == Nil then true
  else if list.head>=list.tail.head then zad2(list.tail)
  else false
zad2(List(2))
zad2(List(2, 1, 0))
zad2(List(2, 2, 2))
zad2(List(1, 2, 0))
zad2(Nil)
//zad2
