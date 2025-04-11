//zad1
def replicateByFunc[A](llist: LazyList[A], fn: Int => Int): LazyList[A] =
  def copyEl(n: Int, pos: Int, ll: LazyList[A]): LazyList[A] =
    ll match
      case LazyList() => LazyList()
      case lhd#::ltl if n > 0 => lhd#::copyEl(n-1, pos, ll)
      case lhd#::ltl => copyEl(fn(pos+1), pos+1, ltl)
  copyEl(fn(1), 1, llist)

replicateByFunc(LazyList.from(1), x => 2*x -5).take(20).toList
//zad1