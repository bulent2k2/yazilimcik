// bak: ./hesaplayalim.kojo

val face: Char = '☺'

def sampleForComprehensions(n: Int, k: Int) = {
  require(k == 2 || k == 3) // note we support only 2 for now, because we have hardcoded the for comprehensions to nest only once
  require(0 < k && k <= n)
  require(n < 10)
  require(k <= n)
  def doIt[T](list1: List[T], k: Int)(implicit ord: math.Ordering[T]) = {
    val n = list1.size
    val heading = s"$n ögesi olan ${list1.yazıYap("{", " ", "}")} kümesinden $k öge seçelim."
    val sep = "=" * heading.size
    println(sep + "\n" + heading + "\n" + sep)
    def instruct1(numBullet: Int, k: Int, set: Set[List[T]], repeat: Boolean, order: Boolean) = {
      def sortLists(xs: List[T], ys: List[T]): Boolean =
        if (xs.isEmpty || ys.isEmpty) xs.isEmpty
        else if (xs.head == ys.head) sortLists(xs.tail, ys.tail)
        else ord.lt(xs.head, ys.head)
      val cmdName = (if (order) "SLI" else "SIZ") ++ (if (repeat) "YLİ" else "YİZ")
      val orderOrChoose = if (order) "sıraya dikkat ederek (SıraLI)" else "sırayı önemsemeden (SırasIZ)"
      val withOrWithout = if (repeat) "her seferinde geri koyarak (YinelemeLİ)" else "geri koymadan (YinelemesİZ)"
      val i = numBullet
      val num = set.size
      println(f"${i}) $cmdName%6s : $orderOrChoose ve $withOrWithout $k öge seçelim.")
      println(s"  $num tane bulduk: " + set.toList.sortWith(sortLists).map(xs => xs.mkString).mkString("{", " ", "}"))
      println("")
    }
    if (k == 2) {
      val set1 = for (x <- list1; y <- list1; if y != x) yield(List(x, y).sorted) /* SIZYİZ */; instruct1(1, k, set1.toSet, false, false)
      val set2 = for (x <- list1; y <- list1) yield(List(x, y).sorted) /*            SIZYLİ */; instruct1(2, k, set2.toSet, true, false)
      val set3 = for (x <- list1; y <- list1; if y != x) yield(List(x, y)) /*        SLIYİZ */; instruct1(3, k, set3.toSet, false, true)
      val set4 = for (x <- list1; y <- list1) yield(List(x, y)) /*                   SLIYLİ */; instruct1(4, k, set4.toSet, true, true)
    } else { // k = 3
      val set1 = for (x <- list1; y <- list1; z <- list1; if y != x && z != y && x != z) yield(List(x, y, z).sorted) /* SIZYİZ */; instruct1(1, k, set1.toSet, false, false)
      val set2 = for (x <- list1; y <- list1; z <- list1) yield(List(x, y, z).sorted) /*                                SIZYLİ */; instruct1(2, k, set2.toSet, true, false)
      val set3 = for (x <- list1; y <- list1; z <- list1; if y != x && z != y && x != z) yield(List(x, y, z)) /*        SLIYİZ */; instruct1(3, k, set3.toSet, false, true)
      val set4 = for (x <- list1; y <- list1; z <- list1) yield(List(x, y, z)) /*                                       SLIYLİ */; instruct1(4, k, set4.toSet, true, true)
    }
  }
  //doIt((1 to n).toList, k)
  doIt("abcdefghijklmnop".take(n).toList, k)
}
tümEkranÇıktı()
clearOutput
sampleForComprehensions(2, 2)
for(n <- List(3, 4, 5, 9); k <- List(2, 3)) sampleForComprehensions(n, k)
println(s"Saymak ne güzel! ${face} Dr. Ben Bülent Başaran ${face}")
