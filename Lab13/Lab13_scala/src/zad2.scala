class Buffer(val size:Int):
  private val arrOfSums:Array[Int] = new Array[Int](size)

  def setValueAtIndex(i:Int, v:Int):Unit = this.synchronized {
    arrOfSums(i) = v
  }

  def totalSum():Int = {
    arrOfSums.sum
  }

class NumListSumByThreads(numList:List[Int], numOfThreads:Int, buff:Buffer) {
  if numList.isEmpty then throw new Exception("Lista nie może być pusta!")
  if numOfThreads <= 0 then throw new Exception("Nie podano liczby wątków!")
  if numOfThreads > numList.length then throw new Exception("Podano za dużą liczbę wątków!")

  private val numsForOneThread:Int = numList.length/numOfThreads

  private val threadsArr:Array[Thread] = new Array[Thread](numOfThreads)

  def calculateSum():Int = {
    for (i <- 0 until numOfThreads) {
      val startForThread:Int = i*numsForOneThread
      var endForThread:Int = startForThread+numsForOneThread
      if endForThread+numsForOneThread >= numList.length then endForThread = numList.length

      threadsArr(i) = new Thread(() => {
        var tempSum:Int = 0
        for (j <- startForThread until endForThread) {
          tempSum += numList(j)
        }
        buff.setValueAtIndex(i, tempSum)

        val numsCount:Int = endForThread - startForThread
        println(s"Wątek nr.${i+1} rozpoczyna pracę (oblicza sumę $numsCount liczb)")
      })
    }

    threadsArr.foreach(_.start())
    threadsArr.foreach(_.join())

    buff.totalSum()
  }
}

object zad2 {
  def main(args: Array[String]): Unit = {
    
    val numbers:List[Int] = List.range(1, 11)

    val numOfThreads:Int = 3
    val buff:Buffer = new Buffer(numOfThreads)
    val numListSumByThreads:NumListSumByThreads = new NumListSumByThreads(numbers, numOfThreads, buff)

    val res:Int = numListSumByThreads.calculateSum()
    println(s"Suma liczb z listy: $res")
    
  }
}