/* run as: 
 * amm ~/src/yaz/yazilimcik/saySade.kojo
 * see ./say.kojo for more info */
def fac2(n: Int): BigInt = if (n < 2) 1 else n * fac2(n - 1) // risks stack overflow
def fac(n: Int): BigInt = if (n < 2) 1 else (2 to n).map(BigInt(_)).toList.reduce(_ * _)
def pow(b: Int, e: Int): BigInt = if (e < 1) 1 else b * pow(b, e - 1)
// count sequences or collections of size k chosen from a pool of n objects.
// we can allow repetition (like throwing dice, or after each choice, it is as if we put it back into the deck or bag)
// or, keep each choice unique (as if we are dealing from a pocker deck)
def seqRep(n: Int, k: Int): BigInt = pow(n, k)
def seqUnq(n: Int, k: Int): BigInt = if (k < 1) 1 else if (k < 2) n else n * seqUnq(n - 1, k - 1)
def colUnq(n: Int, k: Int): BigInt = seqUnq(n, k) / fac(k)
def colRep(n: Int, k: Int): BigInt = colUnq(n + k - 1, k)
val choose = colUnq _  // choose k from n -- equivalently choosing the other n-k, that's why it is known as binomial
val bc = colUnq _ // binomial coefficients -- the kth coefficient in the expansion of (a+b)^n, for all k=0 to n

def multi(n: Int, k: List[Int]): BigInt = {  // multinomial coefficients, choosing k1, k2, k3 ... km-2, km-1, km where sum(ki) = n
    if (k.sum != n) throw new Exception(s"k:$k does not sum up to n=$n")
    fac(n) / k.map(fac(_)).product
}
def bc2(n: Int, k: Int): BigInt = multi(n, List(k, n - k))
assert(bc2(9, 4) == bc(9, 4))
assert(bc2(10, 2) == bc(10, 2))

def bernTrials(n: Int, k: Int, p: Double) = choose(n, k).toDouble * pow2(p, k) * pow2(1-p, n-k) // bernouilli trials (Switzerland, circa 1700)
def pow2(x: Double, k: Int): Double = math.pow(x, k)
def randomWalk(a: Int, b: Int, p: Double): Double = {  // getting from a to b in roulette betting 1 in each roll, or winning in world series final by getting to (b-a) more wins.
    require(p > 0 & p < 1, "probability to be between 0 and 1")
    require (b > a, "target to be greater than starting point")
    require (a >= 0, "starting point to be positive")
    val s = (1-p)/p
    (pow2(s, a) - 1)/(pow2(s, b) - 1)
}
type Numbers = List[Double]
// Expected value of a game:  ev(G) = sum(pi*ai)
def expectedValue(ps: Numbers, as: Numbers): Double = {
    require(round(ps.sum, 8) == 1, s"One and only one outcome is required. Probabilities=$ps.")
    ps.zip(as).map { case (p, a) => p * a }.sum
}

// Variance is defined as (also known as square of standard-deviation):
//   sigma^2(G) = sum(pi*(ai - ev(G))^2)
def variance(ps: Numbers, as: Numbers): Double = {
    val ev = expectedValue(ps, as)
    ps.zip(as).map {
        case (p, a) => p * math.pow(a - ev, 2)
    }.sum
}
def sigma(ps: Numbers, as: Numbers): Double = math.sqrt(variance(ps, as))
def normalize(ps: Numbers, as: Numbers): Numbers = {
    val ev = expectedValue(ps, as)
    val sig = sigma(ps, as)
    val div = if (round(sig, 8) != 0) sig else as.head // ev is 0 if ai == aj for all i&j
    as.map { a => (a - ev) / div }
}
def unfairCoin(p: Double, a: Double = 1.0, b: Double = 0.0): (Double, Double) = {
    val ev = p * a + (1 - p) * b
    (r(ev), r(p * (a * a - b * b) + 2 * p * ev * (b - a) + b * b + ev * ev - 2 * b * ev))
}
def normalizeUnfairCoin(p: Double, a: Double = 1.0, b: Double = 0.0): (Double, Double) = {
    val (ev, vari) = unfairCoin(p, a, b)
    val as = normalize(List(p, 1 - p), List(a, b))
    (r(as.head), r(as.tail.head))
}
def gaussian(x: Double): Double = math.sqrt(0.5 / math.Pi) * math.exp(-0.5 * x * x)

def pluralS(n: Int): String = if (n == 1) "" else "s"
val face: Char = '???'
/* */
  //clearOutput
  def round(n: Number, digits: Int = 0): Double = {
    val factor = math.pow(10, digits).toDouble
    math.round(n.doubleValue * factor).toLong / factor
  }
  val Random = new java.util.Random
  def randomNormalDouble = Random.nextGaussian()
/* */
def r(d: Double) = round(d, 3)

// See ~/src/euler/sampleForComprehensions.kojo
//def sortLists(xs: List[Int], ys: List[Int]): Boolean = if (xs.isEmpty || ys.isEmpty) xs.isEmpty else if (xs.head == ys.head) sortLists(xs.tail, ys.tail) else xs.head < ys.head
def sortLists[N](xs: List[N], ys: List[N])(implicit arg0: math.Ordering[N]): Boolean = if (xs.isEmpty || ys.isEmpty) xs.isEmpty
else if (xs.head == ys.head) sortLists(xs.tail, ys.tail) else arg0.lt(xs.head, ys.head)
def instruct1(numBullet: Int, n: Int, k: Int, set: Set[List[Int]], repeat: Boolean, order: Boolean) = {
    val cmdName = (if (order) "seq" else "col") ++ (if (repeat) "Rep" else "Unq")
    val orderOrChoose = if (order) "ordering" else "choosing"
    val withOrWithout = if (repeat) "with" else "without"
    val elaborate = if (order) "." else " (when we don't care about their order)."
    val i = numBullet
    println(f"${i}) $cmdName%6s : There are ${set.size} ways of $orderOrChoose $k objects out of $n $withOrWithout repetition$elaborate.")
    println("  They are: " + set.toList.sortWith(sortLists).map(xs => xs.mkString).mkString("{", " ", "}"))
  //println(s"And, again, there are ${set.size} of them. Count them all if you don't believe me!")
    println("")
}
def sampleForExpr(n: Int) = {
  var set1 = Set.empty[List[Int]]
  val list1 = (1 to n).toList
  var i = 1
  for (x <- list1; y <- list1; if y != x) set1 += List(x, y).sorted /* colUnq */; instruct1(i, n, 2, set1, false, false); set1 = Set.empty; i+=1
  for (x <- list1; y <- list1) set1 += List(x, y).sorted /*            colRep */; instruct1(i, n, 2, set1, true, false) ; set1 = Set.empty; i+=1
  for (x <- list1; y <- list1; if y != x) set1 += List(x, y) /*        seqUnq */; instruct1(i, n, 2, set1, false, true) ; set1 = Set.empty; i+=1
  for (x <- list1; y <- list1) set1 += List(x, y) /*                   seqRep */; instruct1(i, n, 2, set1, true, true)  ; set1 = Set.empty; i+=1
}
sampleForExpr(4)

val tableOfBC = for (r <- 0 to 10; c <- 0 to r) yield (r, c, bc(r, c))
val tab2 = tableOfBC.groupBy { case (r, c, bc) => r }
val l1: ((Int, Int, BigInt)) => BigInt = { case (_, _, bc) => bc }
println("Binomial coefficients:\n\t(a+b)^n = c0xa^n + c1xa^(n-1)xb + c2xa^(n-2)xb^2 + ...\n\t          ... + c(n-1)xaxb^(n-1) + cnxb^n\nPascal's Triangle:")
for (r <- 0 to 9) println(s"\tn=$r => ${tab2(r).map(l1).mkString(",")}"); println("")

def sampleTable(n: Int) = {
  println( "=============================")
  println(s"  Given a pool of $n objects ")
  println( "  choose or order k objects  ")
  println( "=============================")
  println("k colUnq colRep seqUnq seqRep")
  println("= ====== ====== ====== ======")
  for (k <- 0 to n) println(f"$k%s ${colUnq(n, k)}%6d ${colRep(n, k)}%6d ${seqUnq(n, k)}%6d ${seqRep(n, k)}%6d")
  println("=============================")
  println("Note that seqUnq(n,k)/fac(k) gives us colUnq(n,k).")
  println("However   seqRep(n,k)/fac(k) is not   colRep(n,k). Instead it gives: " + (for (k <- 0 to n) yield (seqRep(5, k) / fac(k))).mkString(" ") + ".")
  println("")
}
sampleTable(5)

println("=====\t ===========")
println("  x  \t Gaussian(x)")
println("=====\t ===========")
val range=26
for (x <- Range(0, range+1).toList.map(x => x.toDouble/4)) yield println(f"$x%5.2f\t ${gaussian(x)}%11.9f")

println(s"Enjoy counting. Saymak g??zel ??ey! ${face} Dr. Ben B??lent Ba??aran ${face}")
