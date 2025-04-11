import scala.annotation.tailrec

//zad2 - instrukcje warunkowe
def multiList[A](list:List[A]) : List[A] =
  if list == Nil then Nil
  else {
    @tailrec
    def rec_multiList(listOrigin: List[A], index: Int, newList: List[A], counter: Int, el: A): List[A] =
      if listOrigin.tail == Nil && counter == index then newList.reverse
      else if counter < index then rec_multiList(listOrigin, index, el :: newList, counter + 1, el)
      else rec_multiList(listOrigin.tail, index + 1, newList, 0, listOrigin.tail.head)
    rec_multiList(list, 1, Nil, 0, list.head)
  }
multiList(List(1,2,3))
//zad2 - instrukcje warunkowe

//zad2 - dopasowanie wzorca
def multiList2[A](list:List[A]) : List[A] =
  if list == Nil then Nil
  else {
    @tailrec
    def rec_multiList2(listOrigin:List[A], index:Int, newList:List[A], counter:Int, el:A) : List[A] =
      (listOrigin.tail, counter < index) match {
        case (Nil, _) if counter == index => newList.reverse
        case (_, true) => rec_multiList2(listOrigin, index, el::newList, counter + 1, el)
        case _ => rec_multiList2(listOrigin.tail, index + 1, newList, 0, listOrigin.tail.head)}
    rec_multiList2(list, 1, Nil, 0, list.head)
  }
multiList2(List(1,2,3))
//zad2 - dopasowanie wzorca