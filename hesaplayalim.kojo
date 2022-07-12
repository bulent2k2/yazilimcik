// bak: sayalim.kojo
def çarpımsal0(n: Int): İriSayı = if (n < 2) 1 else n * çarpımsal0(n - 1) // stack overflow risk
def çarpımsal(n: Int): İriSayı = if (n < 2) 1 else (2 to n).map(İriSayı(_)).toList.reduce(_ * _)
def üssü(b: Int, e: Int): İriSayı = if (e < 1) 1 else b * üssü(b, e - 1)
// count sequences or collections of size k chosen from a pool of n objects.
// we can allow repetition (like throwing dice, or after each choice, it is as if we put it back into the deck or bag)
// or, keep each choice unique (as if we are dealing from a pocker deck)
def slıYli(n: Int, k: Int): İriSayı = üssü(n, k)
def slıYiz(n: Int, k: Int): İriSayı = if (k < 1) 1 else if (k < 2) n else n * slıYiz(n - 1, k - 1)
def sızYiz(n: Int, k: Int): İriSayı = slıYiz(n, k) / çarpımsal(k)
def sızYli(n: Int, k: Int): İriSayı = sızYiz(n + k - 1, k)
val choose = sızYiz _  // choose k from n -- equivalently choosing the other n-k, that's why it is known as binomial
val bc = sızYiz _ // binomial coefficients -- the kth coefficient in the expansion of (a+b)^n, for all k=0 to n

def multi(n: Int, k: List[Int]): İriSayı = {  // multinomial coefficients, choosing k1, k2, k3 ... km-2, km-1, km where sum(ki) = n
    if (k.sum != n) throw new Exception(s"k:$k does not sum up to n=$n")
    çarpımsal(n) / k.map(çarpımsal(_)).product
}
def bc2(n: Int, k: Int): İriSayı = multi(n, List(k, n - k))
assert(bc2(9, 4) == bc(9, 4))
assert(bc2(10, 2) == bc(10, 2))

def bernTrials(n: Int, k: Int, p: Double) = choose(n, k).toDouble * pow2(p, k) * pow2(1-p, n-k) // bernouilli trials (Switzerland, circa 1700)
def pow2(x: Double, k: Int): Double = gücü(x, k)
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
        case (p, a) => p * gücü(a - ev, 2)
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
def gaussian(x: Double, mean: Double = 0.0, stdDev: Double = 1.0): Double = eüssü(-0.5 * karesi((x - mean)/stdDev))/(stdDev * karekökü(2.0*piSayısı))

// see ./iterated-games.kojo and ./normal-ders.kojo
def getOutcomeOfNormalizedIteratedGame(outcome: Double, n: Int, ev: Double, vari: Double): Double =
  (outcome - n * ev) / math.sqrt(n * vari)

def pluralS(n: Int): String = if (n == 1) "" else "s"
val face: Char = '☺'
/* */
  clearOutput
  def round(n: Number, digits: Int = 0): Double = {
    val factor = gücü(10, digits).toDouble
    math.round(n.doubleValue * factor).toLong / factor
  }
  val Random = new java.util.Random
  def randomNormalDouble = Random.nextGaussian()
/* */
def r(d: Double) = round(d, 3)
val tableOfBC = for (r <- 0 to 10; c <- 0 to r) yield (r, c, bc(r, c))
val tab2 = tableOfBC.groupBy { case (r, c, bc) => r }
val l1: ((Int, Int, İriSayı)) => İriSayı = { case (_, _, bc) => bc }
println("İkiterimli (binomial) katsayılar:\n\t(a+b)^n = c0xa^n + c1xa^(n-1)xb + c2xa^(n-2)xb^2 + ...\n\t          ... + c(n-1)xaxb^(n-1) + cnxb^n\nPascal'ın üçgeni:")
for (r <- 0 to 9) println(s"\tn=$r => ${tab2(r).map(l1).mkString(",")}"); println("")

def sampleTable(n: Int) = {
  def pp = println( "==============================="); pp
  println(s" $n nesneden k tanesini seçelim ")
  println( "     SıraLI ya da SırasIZ       ")
  println( "  Her seferinde geri koyarak    ")
  println( "       yani YinelemeLİ          ")
  println( "    ya da geri koymayarak       ")
  println( "       yani YinelemesİZ         ")
  pp
  println( " k  slıYli slıYiz sızYiz sızYli")
  println( "=== ====== ====== ====== ======")
  for (k <- 0 to n) {
    val (a, b, c, d) = (slıYli(n, k), slıYiz(n, k), sızYiz(n, k), sızYli(n, k))
    println(f"$k%2s $a%6d $b%6d $c%6d $d%6d")
  }
  pp
  println("Bir de bakın: slıYiz(n,k)/çarpımsal(k) = sızYiz(n,k).")
  println("Ama, slıYli(n,k)/çarpımsal(k) != sızYli(n,k).\nOnun yerine herbir k için değeri şunlar: " + (for (k <- 0 to n) yield (slıYli(5, k) / çarpımsal(k))).mkString(" ") + ".")
  println("")
}
sampleTable(5)

println("=====\t ===============")
println("  x  \t Gauss eğrisi(x)")
println("=====\t ===============")
val range=26
for (x <- Range(0, range+1).toList.map(x => x.toDouble/4)) yield println(f"$x%5.2f\t ${gaussian(x)}%11.9f")
println(s"Saymak ve hesaplamak ne güzel! ${face} Dr. Ben Bülent Başaran ${face}")
