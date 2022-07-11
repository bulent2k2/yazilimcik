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

// from ./e33.kojo (also e4)
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
type N = BigInt
object N { def apply(elem: Int): BigInt = BigInt(elem) }
def faC(n: Int): N = if (n < 2) 1 else n * faC(n - 1) // risks stack overflow
def fac(n: Int): N = if (n < 2) 1 else (2 to n).map(N(_)).toList.reduce(_ * _)
def pow(b: Int, e: Int): N = if (e < 1) 1 else b * pow(b, e - 1)
def seqRep(n: Int, k: Int): N = pow(n, k)
def seqUnq(n: Int, k: Int): N = if (k < 1) 1 else if (k < 2) n else n * seqUnq(n - 1, k - 1)
def colUnq(n: Int, k: Int): N = seqUnq(n, k) / fac(k)
def colRep(n: Int, k: Int): N = colUnq(n + k - 1, k)
val choose = colUnq _ 
val bc = colUnq _
def multi(n: Int, k: List[Int]): N = {
    if (k.sum != n) throw new Exception(s"k:$k does not sum up to n=$n")
    fac(n) / k.map(fac(_)).product
}
def bc2(n: Int, k: Int): N = multi(n, List(k, n - k))
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
/*
 * End of from ~/src/yaz/yazilimcik/saySapSade.kojo
 */

// e3.kojo
def prime(n: Num): Boolean =
  if (n < 2) false
  else if (n == 2) true
  else if (n % 2 == 0) false
  else (for(f <- Range(3, math.sqrt(n.toDouble).toInt + 1, 2) if n % f == 0) yield(f)).isEmpty

def factors(n: Num): List[Num] = {
    val fs = for(f <- Range(2, math.sqrt(n.toDouble).toInt + 1) if n % f == 0) yield(N(f))
    val xs = for(x <- fs) yield((n / x))
    (fs ++ xs).toList.sorted.distinct
}

def primeFactors(n: Num): List[Num] = factors(n).filter(prime(_))

// from ~/Desktop/COPYA/src/scala/course/evren/example/src/main/scala/example/Lists.scala

object Lists {
  // simplest definitions
  def suM(xs: List[Int]): Int = if (xs.isEmpty) 0 else xs.head + suM(xs.tail)
  def maX(xs: List[Int]): Int =
    if (xs.isEmpty) throw new java.util.NoSuchElementException
    else if (xs.tail.isEmpty) xs.head else {
      val candidate = maX(xs.tail)
      if (candidate > xs.head) candidate
      else xs.head
    }
  // better versions
  def sum(xs: List[Int]): Int = {
    @annotation.tailrec
    def loop(ys: List[Int], acc: Int): Int =
      if (ys.isEmpty) acc
      else loop(ys.tail, acc + ys.head)
    if (xs.isEmpty) 0 else loop(xs.tail, xs.head)
  }
  def max(xs: List[Int]): Int = { 
    @annotation.tailrec
    def loop(ys: List[Int], maxSoFar: Int): Int = 
      if (ys.isEmpty) maxSoFar
      else loop(ys.tail, if (maxSoFar > ys.head) maxSoFar else ys.head)
    if (xs.isEmpty) throw new java.util.NoSuchElementException
    else if (xs.tail.isEmpty) xs.head else loop(xs.tail, xs.head)
  }
}

// e4
def isPalindrome(n: Num): Boolean = {
  val ds = num2Digits(n)
  ds == ds.reverse
}

// e5
def product(ns: Seq[Int]) = ns.map(N(_)).reduce(_ * _)
def greatestPowerLE(p: Int, n: Int): Int = { // greatest power of p <= n
  if (p == 1 || p > n) 1
  else if (p == n) n
  else p * greatestPowerLE(p, n/p)
}
def powers(n: Int) = for(p<-(1 to n).filter(prime(_))) yield(greatestPowerLE(p, n))

// e33
def gcdEuclid(a: Int, b: Int): Int = if (b == 0) a else gcdEuclid(b, a%b)

// e34
// stack overflow for n=3940
def factorialR(n: Num): Num = if (n < 2) N(1) else n * factorialR(n - 1)
def factorial(n: Num): Num = if (n < 2) 1 else (2 to n.toInt).map(N(_)).toList.reduce(_ * _)
def numDigits(x: Num): Int = x.toString.size

