class Bankomat(val minIloscPieniedzy:Int):
  private var obecnaIloscPieniedzy:Int = 0
  private var czyUzupelniono:Boolean = false
  private var iloscWyplat:Int = 0
  private val maxIloscWyplat:Int = 5

  def wyplac(kwota:Int):Unit = synchronized {
    czyUzupelniono = false
    println("Kwota do wypłaty: " + kwota.toString)

    if obecnaIloscPieniedzy >= kwota then
      obecnaIloscPieniedzy -= kwota
      println("Wypłacono: " + kwota.toString + " | Gotówka w bankomacie: " + obecnaIloscPieniedzy.toString)

      iloscWyplat += 1
      Thread.sleep(500)

      if iloscWyplat >= maxIloscWyplat then
        println("\nZakończono działanie bankomatu.")
        System.exit(0)
    else
      println("Nie wypłacono: za mało gotówki w bankomacie")
      while (!czyUzupelniono) wait()
      wyplac(kwota)
  }

  private def uzupelnij(kwota:Int):Unit = synchronized {
    if obecnaIloscPieniedzy < minIloscPieniedzy then
      obecnaIloscPieniedzy += kwota
      czyUzupelniono = true
      notifyAll()
      println("Uzupełniono: " + kwota.toString + " | Gotówka w bankomacie: " + obecnaIloscPieniedzy.toString)
  }

  private val thread = new Thread(new Runnable {
    override def run():Unit =
      while (true)
        uzupelnij(1000)
        Thread.sleep(5000)
  })
  thread.start()

object zad1 {
  def main(args:Array[String]):Unit = {

    val bankomat:Bankomat = new Bankomat(1000)
    val kolejkaDoBankomatu:List[Int] = List(100, 200, 400, 1000, 400, 500)

    for (kwota <- kolejkaDoBankomatu) {
      bankomat.wyplac(kwota)
    }

  }
}