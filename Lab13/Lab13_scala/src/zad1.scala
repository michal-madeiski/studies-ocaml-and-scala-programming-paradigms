class Piekarnia(val ileChlebowDoZabrania:Int):
  private var czyUzupelniono:Boolean = false
  private val ileChlebowZWorkaMaki:Int = 4
  private val dostawaWorkowMaki:Int = 2
  private var iloscWorkowMaki:Double = 0.0
  private var iloscChlebowObecnie:Int = 0
  private var iloscChlebowRazem:Int = 0
  private val maxIloscChlebowRazem:Int = 30

  def pieczenie():Unit = synchronized {
    czyUzupelniono = false

    if iloscWorkowMaki > 0.0 then
      iloscWorkowMaki -= 1.0 / ileChlebowZWorkaMaki
      iloscChlebowRazem += 1
      iloscChlebowObecnie += 1
      println(s"Chleb został wypieczony | Wypieczone chleby dziś: ${iloscChlebowRazem.toString} | Obecna ilość chlebów w piekarni: ${iloscChlebowObecnie.toString}")
      if iloscChlebowRazem >= maxIloscChlebowRazem then
        println("\nZakończono działanie piekarni.")
        System.exit(0)
    else
      println("Pieczenie wstrzymane: za mało mąki w piekarni")
      while (!czyUzupelniono) wait()
      pieczenie()
  }

  def uzupelnianie():Unit = synchronized {
    iloscWorkowMaki += dostawaWorkowMaki
    czyUzupelniono = true
    notifyAll()
    println("Uzupełniono mąkę")
  }

  def odbieranie():Unit = synchronized {
    if iloscChlebowObecnie == ileChlebowDoZabrania then
      iloscChlebowObecnie -= ileChlebowDoZabrania
      notifyAll()
      println("Zabrano chleby")
  }

class Piekarz(piekarnia:Piekarnia) extends Thread:
  override def run():Unit =
    while (true)
      piekarnia.pieczenie()
      Thread.sleep(1000)

class Dostawca(piekarnia:Piekarnia) extends Thread:
  override def run():Unit =
    while (true)
      piekarnia.uzupelnianie()
      Thread.sleep(10000)

class Odbiorca(piekarnia:Piekarnia) extends Thread:
  override def run():Unit =
    while (true)
      piekarnia.odbieranie()
      Thread.sleep(0)

object zad1 {
  def main(args:Array[String]):Unit = {

    val piekarnia:Piekarnia = new Piekarnia(10)
    val odbiorca:Odbiorca = new Odbiorca(piekarnia)
    val dostawca:Dostawca = new Dostawca(piekarnia)
    val piekarz:Piekarz = new Piekarz(piekarnia)

    piekarz.start()
    odbiorca.start()
    dostawca.start()

  }
}
