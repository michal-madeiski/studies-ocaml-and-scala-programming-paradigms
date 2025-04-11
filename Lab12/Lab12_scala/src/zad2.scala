object Constans:
  val THREAD_SLEEP:Int = 0

import Constans.THREAD_SLEEP

class Buffer():
  private var currentData:Option[Double] = None

  def put(newData:Double):Unit = synchronized {
    while(currentData.isDefined) {
      try {
        wait()
      } catch {
        case e:InterruptedException =>
      }
    }
    println(Thread.currentThread().getName + " puts")
    currentData = Some(newData)
    notifyAll()
  }

  def take():Double = synchronized {
    while(currentData.isEmpty) {
      try {
        wait()
      } catch {
        case e:InterruptedException =>
      }
    }
    println(Thread.currentThread().getName + " takes")
    val res:Double = currentData.get
    currentData = None
    notifyAll()
    res
  }

class Calculator(buff:Buffer) extends Thread:
  private val eps:Double = 0.000001

  private def calculate(n:Int):Double = (2.0*n / (2.0*n - 1.0)) * (2.0*n / (2.0*n + 1.0))

  override def run():Unit =
    var n:Int = 1
    while (true)
      println(Thread.currentThread().getName + " calculates")
      buff.put(calculate(n))
      n += 1
      Thread.sleep(THREAD_SLEEP)

class Approximator(buff:Buffer) extends Thread:
  private var result:Double = 1.0
  private var prevPiAprx:Double = 0.0
  private val eps:Double = 0.000001

  override def run():Unit =
    while(true)
      val valFromBuff:Double = buff.take()
      
      result *= valFromBuff
      val currPiAprx:Double = 2*result
      
      val diff:Double = math.abs(currPiAprx - prevPiAprx)

      println(Thread.currentThread().getName + " approximates " + currPiAprx)

      if (diff < eps) {
        println("\nPRZYBLIŻENIE LICZBY PI: " + currPiAprx.toString)
        System.exit(0)
      }

      prevPiAprx = currPiAprx
      Thread.sleep(THREAD_SLEEP)

object zad2 {
  def main(args:Array[String]):Unit = {
    
    val buff:Buffer = new Buffer()
    val aprx:Approximator = new Approximator(buff)
    val calc:Calculator = new Calculator(buff)

    calc.start()
    aprx.start()
    
  }
}