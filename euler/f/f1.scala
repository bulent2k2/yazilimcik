// my favorite functions (:-)  For doc, see the source listed as from
type Num = BigInt
type Numbers = List[Double]

// from ./e2.kojo
def fib1(n: Int): Num = if (n < 2) n else fib1(n - 1) + fib1(n - 2)
def fib2(n: Int): Array[Num] = {
    val fib = new Array[Num](n+1)
    fib(0) = 0
    fib(1) = 1
    for (i <- 2 to n) fib(i) = fib(i-1) + fib(i-2)
    fib
}

// from ./e33.kojo
def num2Digits(x: Num): List[Num] =
  if (x < 10) List(x) else x % 10 :: num2Digits(x / 10)
def digits2Num(digits: List[Num]): Num = {
  def doIt(ds: List[Num], acc: Num): Num =
    if (ds.isEmpty) 0
    else if (ds.tail.isEmpty) acc + ds.head
    else doIt(ds.tail, acc + 10 * ds.head)
  doIt(digits.reverse, 0)
}

/*
 * from ~/src/yaz/yazilimcik/saySapSade.kojo
 */
def fac(n: Int): BigInt = if (n < 2) 1 else n * fac(n - 1)
def pow(b: Int, e: Int): BigInt = if (e < 1) 1 else b * pow(b, e - 1)
def seqRep(n: Int, k: Int): BigInt = pow(n, k)
def seqUnq(n: Int, k: Int): BigInt = if (k < 1) 1 else if (k < 2) n else n * seqUnq(n - 1, k - 1)
def colUnq(n: Int, k: Int): BigInt = seqUnq(n, k) / fac(k)
def colRep(n: Int, k: Int): BigInt = colUnq(n + k - 1, k)
val choose = colUnq _ 
val bc = colUnq _
def multi(n: Int, k: List[Int]): BigInt = {
    if (k.sum != n) throw new Exception(s"k:$k does not sum up to n=$n")
    fac(n) / k.map(fac(_)).product
}
def bc2(n: Int, k: Int): BigInt = multi(n, List(k, n - k))
assert(bc2(9, 4) == bc(9, 4))
assert(bc2(10, 2) == bc(10, 2))
def bernTrials(n: Int, k: Int, p: Double) = choose(n, k).toDouble * pow2(p, k) * pow2(1-p, n-k)
def pow2(x: Double, k: Int): Double = math.pow(x, k)
def randomWalk(a: Int, b: Int, p: Double): Double = {
    require(p > 0 & p < 1, "probability to be between 0 and 1")
    require (b > a, "target to be greater than starting point")
    require (a >= 0, "starting point to be positive")
    val s = (1-p)/p
    (pow2(s, a) - 1)/(pow2(s, b) - 1)
}
def expectedValue(ps: Numbers, as: Numbers): Double = {
    require(round(ps.sum, 8) == 1, s"One and only one outcome is required. Probabilities=$ps.")
    ps.zip(as).map { case (p, a) => p * a }.sum
}
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
    val div = if (round(sig, 8) != 0) sig else as.head
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
def getOutcomeOfNormalizedIteratedGame(outcome: Double, n: Int, ev: Double, vari: Double): Double =
  (outcome - n * ev) / math.sqrt(n * vari)
def pluralS(n: Int): String = if (n == 1) "" else "s"
def pluralBe(n: Int): String = if (n == 1) "is" else "are"
def plural3rd(n: Int): String = if (n == 1) "it" else "they"
val face: Char = '☺'
/* Comment out if not in Kojo: */
//clearOutput
def round(n: Number, digits: Int = 0): Double = {
  val factor = math.pow(10, digits).toDouble
  math.round(n.doubleValue * factor).toLong / factor
}
val Random = new java.util.Random
def randomNormalDouble = Random.nextGaussian()
/* */
def r(d: Double) = round(d, 3)
def sortLists(xs: List[Char], ys: List[Char])(implicit arg: Ordering[Char]): Boolean = 
    if (xs.isEmpty || ys.isEmpty) xs.isEmpty
    else if (xs.head == ys.head) sortLists(xs.tail, ys.tail) else arg.lt(xs.head, ys.head)
def sampleForExpressions(n: Int) = {
  def instruct1(numBullet: Int, n: Int, k: Int, set: Set[List[Char]], repeat: Boolean, order: Boolean) = {
    val cmdName = (if (order) "seq" else "col") ++ (if (repeat) "Rep" else "Unq")
    val orderOrChoose = if (order) "ordering" else "choosing"
    val withOrWithout = if (repeat) "with" else "without"
    val elaborate = if (order) "." else " (when we don't care about their order)."
    val i = numBullet
    val num = set.size
    println(f"${i}) $cmdName%6s : There ${pluralBe(num)} $num way${pluralS(num)} of $orderOrChoose $k objects out of $n $withOrWithout repetition$elaborate.")
    println(s"  ${plural3rd(num).capitalize} ${pluralBe(num)}: " + set.toList.sortWith(sortLists).map(xs => xs.mkString).mkString("{", " ", "}"))
    println("")
  }
  /* var set1 = Set.empty[List[Int]]
  val list1 = (1 to n).toList */
  var set1 = Set.empty[List[Char]]
  val list1 = "abcdefghijklmnop".take(n)
  var i = 1
  for (x <- list1; y <- list1; if y != x) set1 += List(x, y).sorted /* colUnq */; instruct1(i, n, 2, set1, false, false); set1 = Set.empty; i+=1
  for (x <- list1; y <- list1) set1 += List(x, y).sorted /*            colRep */; instruct1(i, n, 2, set1, true, false) ; set1 = Set.empty; i+=1
  for (x <- list1; y <- list1; if y != x) set1 += List(x, y) /*        seqUnq */; instruct1(i, n, 2, set1, false, true) ; set1 = Set.empty; i+=1
  for (x <- list1; y <- list1) set1 += List(x, y) /*                   seqRep */; instruct1(i, n, 2, set1, true, true)  ; set1 = Set.empty; i+=1
}
println(s"Enjoy counting. Saymak güzel şey! ${face} Dr. Ben Bülent Başaran ${face}")
/*
 * End of from ~/src/yaz/yazilimcik/saySapSade.kojo
 */
sampleForExpressions(3)
