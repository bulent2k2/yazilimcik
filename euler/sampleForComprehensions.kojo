def pluralS(n: Int): String = if (n == 1) "" else "s"
def pluralBe(n: Int): String = if (n == 1) "is" else "are"
def plural3rd(n: Int): String = if (n == 1) "it" else "they"
val face: Char = '☺'

def sampleForComprehensions(n: Int, k: Int) = {
  require(k == 2 || k == 3) // note we support only 2 for now, because we have hardcoded the for comprehensions to nest only once
  require(0 < k && k <= n)
  require(n < 10)
  require(k <= n)
  def doIt[T](list1: List[T], k: Int)(implicit ord: math.Ordering[T]) = {
    val n = list1.size
    val heading = s"We are selecting from the set ${list1.mkString("{", " ", "}")}. There are $n elements."
    val sep = "=" * heading.size
    println(sep + "\n" + heading + "\n" + sep)
    def instruct1(numBullet: Int, k: Int, set: Set[List[T]], repeat: Boolean, order: Boolean) = {
      def sortLists(xs: List[T], ys: List[T]): Boolean =
        if (xs.isEmpty || ys.isEmpty) xs.isEmpty
        else if (xs.head == ys.head) sortLists(xs.tail, ys.tail)
        else ord.lt(xs.head, ys.head)
      val cmdName = (if (order) "seq" else "col") ++ (if (repeat) "Rep" else "Unq")
      val orderOrChoose = if (order) "ordering" else "choosing"
      val withOrWithout = if (repeat) "with" else "without"
      val elaborate = if (order) "." else " (when we don't care about their order)."
      val i = numBullet
      val num = set.size
      println(f"${i}) $cmdName%6s : Count the ways of $orderOrChoose $k from the set $withOrWithout repetition$elaborate.")
      println(s"  There ${pluralBe(num)} $num. ${plural3rd(num).capitalize} ${pluralBe(num)}: " + set.toList.sortWith(sortLists).map(xs => xs.mkString).mkString("{", " ", "}"))
      println("")
    }
    if (k == 2) {
      val set1 = for (x <- list1; y <- list1; if y != x) yield(List(x, y).sorted) /* colUnq */; instruct1(1, k, set1.toSet, false, false)
      val set2 = for (x <- list1; y <- list1) yield(List(x, y).sorted) /*            colRep */; instruct1(2, k, set2.toSet, true, false)
      val set3 = for (x <- list1; y <- list1; if y != x) yield(List(x, y)) /*        seqUnq */; instruct1(3, k, set3.toSet, false, true)
      val set4 = for (x <- list1; y <- list1) yield(List(x, y)) /*                   seqRep */; instruct1(4, k, set4.toSet, true, true)
    } else {
      val set1 = for (x <- list1; y <- list1; z <- list1; if y != x && z != y) yield(List(x, y, z).sorted) /* colUnq */; instruct1(1, k, set1.toSet, false, false)
      val set2 = for (x <- list1; y <- list1; z <- list1) yield(List(x, y, z).sorted) /*                      colRep */; instruct1(2, k, set2.toSet, true, false)
      val set3 = for (x <- list1; y <- list1; z <- list1; if y != x && z != y) yield(List(x, y, z)) /*        seqUnq */; instruct1(3, k, set3.toSet, false, true)
      val set4 = for (x <- list1; y <- list1; z <- list1) yield(List(x, y, z)) /*                             seqRep */; instruct1(4, k, set4.toSet, true, true)

    }
  }
  doIt((1 to n).toList, k)
  doIt("abcdefghijklmnop".take(n).toList, k)
}
clearOutput
sampleForComprehensions(2, 2)
for(n <- List(3, 5, 9); k <- List(2, 3)) sampleForComprehensions(n, k)
println(s"Enjoy counting. Saymak güzel şey! ${face} Dr. Ben Bülent Başaran ${face}")
