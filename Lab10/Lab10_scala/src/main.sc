//zad1
//rekurencja ogonowa
def array2DMinRec(arr2D: Array[Array[Int]]): Array[Int] = {
  val resultBuffer = Array.fill(arr2D.length)(0)

  def arrayMin(arr: Array[Int]): Int = {
    if arr.length < 0 then throw new Exception("Empty array!")
    else {
      @scala.annotation.tailrec
      def helper1(idx: Int, min: Int): Int = {
        if (idx < arr.length)
          if arr(idx) < min then helper1(idx + 1, arr(idx)) else helper1(idx + 1, min)
        else min
      }

      helper1(1, arr(0))
    }
  }

  @scala.annotation.tailrec
  def helper2(idx: Int, arr: Array[Int]): Array[Int] = {
    if (idx < arr.length) {
      arr(idx) = arrayMin(arr2D(idx))
      helper2(idx + 1, arr)
    } else arr
  }

  helper2(0, resultBuffer)
}
//rekurencja ogonowa

//imperatywnie
def array2DMinImp(arr2D: Array[Array[Int]]): Array[Int] = {
  val resultBuffer = Array.fill(arr2D.length)(0)

  def arrayMin(arr: Array[Int]): Int = {
    if arr.length < 0 then throw new Exception("Empty array!")
    else {
      var min = arr(0)

      for (i <- 1 to (arr.length - 1)) {
        if arr(i) < min then min = arr(i)
      }
      min
    }
  }

  for (i <- 0 to (arr2D.length -1)) {
    resultBuffer(i) = arrayMin(arr2D(i))
  }
  resultBuffer
}
//imperatywnie

val arr2D = Array(
  Array(1, 2, 3, 4),
  Array(1, 2, 1, 2),
  Array(100, 0, -1, 2)
)

array2DMinRec(arr2D)
array2DMinImp(arr2D)
//zad1

//zad2
class Rownanie(wsp: Array[Double]) {
  def rozwiaz() = {
    wsp.length match
      case 0 => throw new Exception("Nie podano równania!")
      case 1 if wsp(0) == 0 => throw new Exception("Równanie tożsamościowe!")
      case 1 => throw new Exception("Równanie sprzeczne!")
      case 2 => liniowe(wsp)
      case 3 => kwadratowe(wsp)
      case _ => {
        for (i <- 3 to (wsp.length - 1)) {
          if wsp(i) != 0 then throw new Exception("Równanie ma za duży stopień!")
        }
        kwadratowe(wsp)
      }
  }

  private def liniowe(wsp: Array[Double]) = {
    val resultBuffer = Array.fill(1)(0.0)

    val b: Double = wsp(0)
    val a: Double = wsp(1)

    (a, b) match
      case (0, 0) => throw new Exception("Równanie tożsamościowe!")
      case (0, _) => throw new Exception("Równanie sprzeczne!")
      case (_, _) => resultBuffer(0) = (-b)/a

    resultBuffer
  }

  private def kwadratowe(wsp: Array[Double]) = {
    val resultBuffer = Array.fill(2)(0.0)

    val c: Double = wsp(0)
    val b: Double = wsp(1)
    val a: Double = wsp(2)

    (a, b, c) match
      case (0, 0, 0) => throw new Exception("Równanie tożsamościowe")
      case (0, 0, _) => throw new Exception("Równanie sprzeczne")
      case (0, _, _) => {
        val buffer = new Array[Double](2)
        buffer(0) = c
        buffer(1) = b
        liniowe(buffer)
      }
      case (_, _, _) => {
        val delta = b*b - 4*a*c
        if delta >= 0 then {
          val sqrtOfDelta = Math.sqrt(delta)

          val x1 = (-b - sqrtOfDelta)/(2*a)
          val x2 = (-b + sqrtOfDelta)/(2*a)

          if x1 == x2 then Array.fill(1)(x1)
          else {
            val buffer = new Array[Double](2)
            buffer(0) = x1
            buffer(1) = x2
            buffer
          }
        } else
          throw new Exception("Brak rozwiązań!")
      }
  }
}

val r1 = new Rownanie(Array(1, 2))
r1.rozwiaz()
val r2 = new Rownanie(Array(2, -3, 1))
r2.rozwiaz()
val r3 = new Rownanie(Array(1))
r3.rozwiaz()
val r4 = new Rownanie(Array(0))
r4.rozwiaz()
val r5 = new Rownanie(Array(1, 2, 3, 4))
r5.rozwiaz()
val r6 = new Rownanie(Array(1, 2, 1, 0, 0))
r6.rozwiaz()
//zad2
