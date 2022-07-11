// 7.1
// Game is a single event with a number of possible outcomes Pi with
// probabilities pi and a pay-off ai for each outcome.

type Numbers = List[Double]

  def round(n: Number, digits: Int = 0): Double = {
    val factor = math.pow(10, digits).toDouble
    math.round(n.doubleValue * factor).toLong / factor
  }

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

// Normalize the game meaning, adjust the payoffs so that:
// 1) expected value is 0,
// 2) variance is 1 (or 0 iff all payoffs are identical)
// Gnorm = (G-ev)/sigma    Except when sigma is 0!
def normalize(ps: Numbers, as: Numbers): Numbers = {
    val ev = expectedValue(ps, as)
    val sig = sigma(ps, as)
    val div = if (round(sig, 8) != 0) sig else as.head // ev is 0 if ai == aj for all i&j
    as.map { a => (a - ev) / div }
}

def r(x: Double): Double = round(x, 3)
def test1() = {
    val ps = List(0.7, 0.2, 0.1)
    val as = List(2.0, 2, 2.0)

    val expVal = expectedValue(ps, as)
    val vari = variance(ps, as)

    val nas = normalize(ps, as)
    println(s"Game has (ev, var)             : (${r(expVal)}, ${r(vari)})")
    println(s"Normalized G  (G - ev)/sig has : (${r(expectedValue(ps, nas))}, ${r(variance(ps, nas))})")
}

// Return expected value and variance for an unfair coin
// p(heads) = p, payoff for heads is a
// p(tails) = 1-p, payoff for heads is b
// e = ev = p * a + (1 - p) * b
// var = p * (a - ev)^2 + (1 - p) * (b - ev)^2
//     = p(aa-bb) + 2pe(b-a) + bb + ee - 2be    (Substitute e and see what we get!)
// if b = 0 =>
//    e = pa
//    var = paa(1-p)
// furthermore if a = 1 =>
//    e = p
//    var = p(1-p)
def unfairCoin(p: Double, a: Double = 1.0, b: Double = 0.0): (Double, Double) = {
    val ev = p * a + (1 - p) * b
    (r(ev), r(p * (a * a - b * b) + 2 * p * ev * (b - a) + b * b + ev * ev - 2 * b * ev))
}

def normalizeUnfairCoin(p: Double, a: Double = 1.0, b: Double = 0.0): (Double, Double) = {
    val (ev, vari) = unfairCoin(p, a, b)
    val as = normalize(List(p, 1 - p), List(a, b))
    (r(as.head), r(as.tail.head))
}
//clearOutput
test1

for (p <- List(0.1, 0.25, 0.3, 0.5); (a, b) <- List((1.0, 0.0), (2.0, 1.0))) {
    val napair = normalizeUnfairCoin(p, a, b)
    val nas = List(napair._1, napair._2)
    val ps = List(p, 1-p)
    val ev = r(expectedValue(ps, nas))
    val vari = r(variance(ps, nas))
    println(s"unfair coin with p=$p a=$a b=$b: ${unfairCoin(p, a, b)}\nnormalized: ${(ev, vari)}")
}

// iterated games: G+G+G = G(3)
// Let G be a flip of a fair game and payoff for H is 1 and for T is 0:
// Show that
//   ev(G(3)) = 3*ev(G)
// and
//   sig^2(G(3)) = 3*sig^2(G)
/* 
 *  ev(G+G+G) = 3 * (P(3 heads) = 1/8) + 
 *              2 * (P(2 heads) = 3/8) + 
 *              1 * (P(1 heads) = 3/8) 
 *            = 3/8 + 6/8 + 3/8 = 12/8 = 3/2
 *            = 3 * ev(G)
 * sig^2(G+G+G) = 1/8 * (3 - 3/2)^2 +
 *                3/8 * (2 - 3/2)^2 +
 *                3/8 * (1 - 3/2)^2 + 
 *                P(no heads = 1/8) * (0 - 3/2)^2
 *              = 1/8 * 9/4 + 3/8 * 1/4 + 3/8 * 1/4 + 1/8 * 9/4
 *              = 9/32 + 3/32 + 3/32 +9/32= 24/32 = 3/4
 *              = 3 * sig^2(G)
 */

// see ~/src/yaz/yazilimcik/iterated-games.kojo
def getOutcomeOfNormalizedIteratedGame(outcome: Double, n: Int, ev: Double, vari: Double): Double =
  (outcome - n * ev) / math.sqrt(n * vari)

def coinTossExamples() = {
  val ev   = 0.5
  val vari = 0.25
  // 52 heads out of 100 coin tosses corresponds to 0.4 on the normal curve
  assert(getOutcomeOfNormalizedIteratedGame(52,  100 , ev, vari) == 0.4, "100 coin flips")
  assert(r(getOutcomeOfNormalizedIteratedGame(520,  1000 , ev, vari)) == 1.265, "1k coin flips")
  assert(getOutcomeOfNormalizedIteratedGame(5200, 10000, ev, vari) == 4.0, "10k coin flips")
}
coinTossExamples
def gaussian(x: Double): Double = math.sqrt(0.5 / math.Pi) * math.exp(-0.5 * x * x)

for (x <- Range(-20, 20).toList.map(x => x.toDouble/4)) yield println(f"$x%3.2f ${gaussian(x)}%3.4f")

