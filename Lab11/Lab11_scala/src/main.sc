//zad1
class Zwierzak(gatunek:String, imie:String, rokUr:Int):
  def getImie:String = imie
  override def toString:String = gatunek + " " + imie + " " + rokUr

object Obora:
  var nrOb:Int = 0

class Obora(wlasciciel:String, maxLiczbaBoksow:Int):
  import Obora.nrOb

  private var aktLiczbaBoksow:Int = 0

  private val nr = { nrOb += 1; nrOb }

  private var listaZwierzakow:List[Zwierzak] = List()

  def getNr:Int = nr

  def kwateruj(z:Zwierzak):Boolean =
    if aktLiczbaBoksow == maxLiczbaBoksow then throw new Exception("Nie można zakwaterować - obora jest pełna!")
    else
      listaZwierzakow = z :: listaZwierzakow
      aktLiczbaBoksow += 1
      true

  def wykwateruj(imieZwierzaka:String):Zwierzak =
    if aktLiczbaBoksow == 0 then throw Exception("Nie można wykwaterować - obora jest pusta!")
    else {
      var wykwaterowany:Zwierzak = null

      def wykwateruj2(imieZwierzaka:String, lst:List[Zwierzak]):List[Zwierzak] =
        lst match
          case Nil => Nil
          case hd::tl if hd.getImie == imieZwierzaka => {
            wykwaterowany = hd
            tl
          }
          case hd::tl => hd::wykwateruj2(imieZwierzaka, tl)

      listaZwierzakow = wykwateruj2(imieZwierzaka, listaZwierzakow)

      if wykwaterowany == null then throw Exception("Nie można wykwaterować - nie ma zwierzaka o takim imieniu!")
      else {
        aktLiczbaBoksow -= 1
        wykwaterowany
      }
    }

  def przenies(imieZwierzaka:String, obora2:Obora):Boolean =
    obora2.kwateruj(wykwateruj(imieZwierzaka))

  def wyswietl():Unit =
    for (i <- 1 to aktLiczbaBoksow) {
      println(s"${i}. ${listaZwierzakow(i-1).toString}")
    }

var ob1:Obora = new Obora("Michał", 3)
var ob2:Obora = new Obora("Madeiski", 5)

ob1.kwateruj(new Zwierzak("Krowa", "Ela", 2021))
ob1.kwateruj(new Zwierzak("Świnia", "Peppa", 2022))
ob1.kwateruj(new Zwierzak("Pies", "Maksiu", 2023))
ob1.kwateruj(new Zwierzak("Owca", "Krysia", 2024))
ob2.kwateruj(new Zwierzak("Koza", "Baśka", 2025))

ob1.wyswietl()

ob2.wyswietl()

ob1.przenies("Peppa", ob2)

ob1.wyswietl()

ob2.wyswietl()

ob1.getNr
ob2.getNr
//zad1

//zad2
trait Plywanie { def plywanie:String = "Pływam"}
trait Latanie(stp:Int) {
  def latanie:String =
    stp match
      case 1 => "Słabo latam"
      case 2 => "Dobrze latam"
      case 3 => "Latam znakomicie"}
trait Nurkowanie { def nurkowanie:String = "Nurkuję"}
trait Bieganie(stp:Int) {
  def bieganie:String =
    stp match
      case 1 => "Słabo biegam"
      case 2 => "Dobrze biegam"
      case 3 => "Świetnie biegam"}

object Ptak:
  var nrEw:Int = 0
abstract class Ptak:
  import Ptak.nrEw

  private val nr:Int = { nrEw += 1; nrEw}

  println("Pochodzę od dinozarów!!")

  def dane:String = s"Ptak nr ${nr} - ${this.getClass.getSimpleName()}"

class Pingwin extends Ptak with Plywanie with Nurkowanie
class Golab extends Ptak with Plywanie with Latanie(2) with Bieganie(1)
class Strus extends Ptak with Bieganie(3)
class Sokol extends Ptak with Latanie(3)
class Kura extends Ptak with Latanie(1) with Bieganie(2)

val pi1 = new Pingwin
val go1 = new Golab
val st1 = new Strus
val so1 = new Sokol
val ku1 = new Kura
val pi2 = new Pingwin
val go2 = new Golab
val st2 = new Strus

val ptaki = List(pi1, pi2, go1, go2, st1, st2, so1, ku1)

val latajace:List[Latanie] = List(go1, so1, ku1, go2)
val nurkujace:List[Nurkowanie] = List(pi1, pi2)
val plywajace:List[Plywanie] = List(pi1, go1, pi2, go2)
val biegajace:List[Bieganie] = List(go1, st1, ku1, go2, st2)

latajace.foreach(p => println(p.asInstanceOf[Ptak].dane + "\n   " + p.latanie))
nurkujace.foreach(p => println(p.asInstanceOf[Ptak].dane + "\n   " + p.nurkowanie))
plywajace.foreach(p => println(p.asInstanceOf[Ptak].dane + "\n   " + p.plywanie))
biegajace.foreach(p => println(p.asInstanceOf[Ptak].dane + "\n   " + p.bieganie))
//zad2