// run as: amm ~/src/yaz/yazilimcik/saySade.kojo
// see ./say.kojo for more info

def pow(base: Int, exp: Int): Long = if (exp < 1) 1 else base * pow(base, exp - 1) //> def pow(base: Int, exp: Int): Long
def fac(n: Int): Long = if (n < 2) 1 else n * fac(n - 1) //> def fac(n: Int): Long
def seqWithRep(n: Int, k: Int): Long = pow(n, k) //> def seqWithRep(n: Int, k: Int): Long
def seqUniq(n: Int, k: Int): Long = if (k < 1) 1 else if (k < 2) n else n * seqUniq(n - 1, k - 1) //> def seqUniq(n: Int, k: Int): Long
def colUniq(n: Int, k: Int): Long = seqUniq(n, k) / fac(k) //> def colUniq(n: Int, k: Int): Long
def colWithRep(n: Int, k: Int): Long = colUniq(n + k - 1, k) //> def colWithRep(n: Int, k: Int): Long

def multi(n: Int, a: List[Int]): Long = {
    if (a.sum != n) throw new Exception(s"a:$a does not sum up to n=$n")
    fac(n) / a.map(fac(_)).product
} //> def multi(n: Int, a: List[Int]): Long
def bc2(n: Int, k: Int): Long = multi(n, List(k, n - k)) //> def bc2(n: Int, k: Int): Long

def sortLists(xs: List[Int], ys: List[Int]): Boolean = if (xs.isEmpty || ys.isEmpty) xs.isEmpty else if (xs.head == ys.head) sortLists(xs.tail, ys.tail) else xs.head < ys.head //> def sortLists(xs: List[Int], ys: List[Int]): Boolean
def instruct1(n: Int, k: Int, set: Set[List[Int]], repeat: Boolean, order: Boolean) = {
    val orderOrChoose = if (order) "ordering" else "choosing"
    val withOrWithout = if (repeat) "with" else "without"
    val elaborate = if (order) "." else " (when we don't care about their order)."
    println(s"There are ${set.size} ways of $orderOrChoose $k objects out of $n $withOrWithout repetition$elaborate.")
    println("Here are all those: " + set.toList.sortWith(sortLists).map(xs => xs.mkString).mkString("{", " ", "}"))
    println(s"And, again, there are ${set.size} of them. Count them all if you don't believe me!")
} //> def instruct1(n: Int, k: Int, set: Set[List[Int]], repeat: Boolean, order: Boolean): Unit
def fourWays() {
    var set1 = Set.empty[List[Int]]
    val list1 = (0 to 4).toList
    for (x <- list1; y <- list1; if y != x) set1 += List(x, y).sorted
    instruct1(5, 2, set1, false, false); println(set1.size, colUniq(5, 2))    // 10
    set1 = Set.empty
    for (x <- list1; y <- list1) set1 += List(x, y).sorted
    instruct1(5, 2, set1, true, false);  println(set1.size, colWithRep(5, 2)) // 15
    set1 = Set.empty
    for (x <- list1; y <- list1; if y != x) set1 += List(x, y)
    instruct1(5, 2, set1, false, true);  println(set1.size, seqUniq(5, 2))    // 20
    set1 = Set.empty
    for (x <- list1; y <- list1) set1 += List(x, y)
    instruct1(5, 2, set1, true, true);   println(set1.size, seqWithRep(5, 2)) // 25
} //> def fourWays(): Unit
//fourWays
def dice1() {
    val die = (1 to 6).toList
    var set1 = Set.empty[List[Int]]
    for (d1 <- die; d2 <- die) set1 += List(d1, d2).sorted
    instruct1(6, 2, set1, true, false)
    println(s"colWithRep formula gives: ${colWithRep(6, 2)}")
    val count = set1.filter(xs => xs.sum == 7).size
    println(s"$count out of ${set1.size} sum up to 7")
} //> def dice1(): Unit
//dice1
colWithRep(32, 7) //> val res2696: Long = 12620256
