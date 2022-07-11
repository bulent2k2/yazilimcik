// see ~/src/yaz/yazilimcik/saySapSade.kojo and saySade.kojo and say.kojo and many others...
def fac(n: Int): BigInt = if (n < 2) 1 else n * fac(n - 1) //> def fac(n: Int): BigInt
def pow(b: Int, e: Int): BigInt = if (e < 1) 1 else b * pow(b, e - 1) //> def pow(b: Int, e: Int): BigInt
// count sequences or collections of size k chosen from a pool of n objects.
// we can allow repetition (like throwing dice, or after each choice, it is as if we put it back into the deck or bag)
// or, keep each choice unique (as if we are dealing from a pocker deck)
def seqRep(n: Int, k: Int): BigInt = pow(n, k) //> def seqRep(n: Int, k: Int): BigInt
def seqUnq(n: Int, k: Int): BigInt = if (k < 1) 1 else if (k < 2) n else n * seqUnq(n - 1, k - 1) //> def seqUnq(n: Int, k: Int): BigInt
def colUnq(n: Int, k: Int): BigInt = seqUnq(n, k) / fac(k) //> def colUnq(n: Int, k: Int): BigInt
def colRep(n: Int, k: Int): BigInt = colUnq(n + k - 1, k) //> def colRep(n: Int, k: Int): BigInt
val choose = colUnq _ // choose k from n -- equivalently choosing the other n-k, that's why it is known as binomial //> val choose: (Int, Int) => BigInt = $Lambda$2749/0x0000000801467040@1c2a1e0d
val bc = colUnq _ // binomial coefficients -- the kth coefficient in the expansion of (a+b)^n, for all k=0 to n //> val bc: (Int, Int) => BigInt = $Lambda$2750/0x0000000801466840@2834028b

def multi(n: Int, k: List[Int]): BigInt = { // multinomial coefficients, choosing k1, k2, k3 ... km-2, km-1, km where sum(ki) = n
    if (k.sum != n) throw new Exception(s"k:$k does not sum up to n=$n")
    fac(n) / k.map(fac(_)).product
} //> def multi(n: Int, k: List[Int]): BigInt
def bc2(n: Int, k: Int): BigInt = multi(n, List(k, n - k)) //> def bc2(n: Int, k: Int): BigInt
assert(bc2(9, 4) == bc(9, 4))
assert(bc2(10, 2) == bc(10, 2))

def bernTrials(n: Int, k: Int, p: Double) = choose(n, k).toDouble * pow2(p, k) * pow2(1 - p, n - k) // bernouilli trials (Switzerland, circa 1700) //> def bernTrials(n: Int, k: Int, p: Double): Double
def pow2(x: Double, k: Int): Double = math.pow(x, k) //> def pow2(x: Double, k: Int): Double
def randomWalk(a: Int, b: Int, p: Double): Double = { // getting from a to b in roulette betting 1 in each roll, or winning in world series final by getting to (b-a) more wins.
    require(p > 0 & p < 1, "probability to be between 0 and 1")
    require(b > a, "target to be greater than starting point")
    require(a >= 0, "starting point to be positive")
    val s = (1 - p) / p
    (pow2(s, a) - 1) / (pow2(s, b) - 1)
} //> def randomWalk(a: Int, b: Int, p: Double): Double

clearOutput
def r(d: Double) = round(d, 3) //> def r(d: Double): Double

def sortLists[N](xs: List[N], ys: List[N])(implicit arg0: math.Ordering[N]): Boolean = if (xs.isEmpty || ys.isEmpty) xs.isEmpty //> def sortLists[N](xs: List[N], ys: List[N])(implicit arg0: scala.math.Ordering[N]): Boolean
else if (xs.head == ys.head) sortLists(xs.tail, ys.tail) else arg0.lt(xs.head, ys.head)

clearOutput
// eval 3.3
// Say you flip a coin seven times.
// What is the probability the number of heads will be even?
// if we flipped three times:
def e3_3() = {
    val numAll = pow(2, 3)
    val numPick = choose(3, 0) + choose(3, 2)
    numPick.toDouble / numAll.toDouble
} //> def e3_3(): Double
println(e3_3)

// eval 3.4
// In the game of Phigh, you roll three dice and your score is the highest number showing.
// What is the probability of getting a score of " 2 " or less?
// Only 2s or 1s.
// Pick: 2^3
// All ways: 6^3
// 8 / 6*6*6
println(r(pow(2, 3).toDouble / pow(6, 3).toDouble))
8 / 216.9 //> val res520: Double = 0.036883356385431075

// eval 3.7 
// What is the probability of being dealt a full house, aces over kings?
// In other words, what is the probability of being dealt a five-card poker
// hand containing three aces and two kings
def e3_7() = {
    val numAll = choose(52, 5)
    val numDesired = choose(4, 3) * choose(4, 2)
    numDesired.toDouble / numAll.toDouble
} //> def e3_7(): Double
e3_7 // 9.2e-6 //> val res521: Double = 9.23446301597562E-6

// eval 3.8
// In a standard deck of cards, two suits (diamonds and hearts) are red, 
// and the other two (spades and clubs) are black. We'll call a poker hand
// a "nearsighted flush" if all five cards are of the same color. 
// What is the probability of being dealt a nearsighted flush?
def e3_8() = {
    val numAll = choose(52, 5).toDouble
    val numDesired = choose(26, 5).toDouble
    println(numAll/numDesired)
} //> def e3_8(): Unit
e3_8

def e6_1() = {
    bernTrials(10, 2, 1/6.toDouble)
} //> def e6_1(): Double
e6_1 //> val res523: Double = 0.2907100492017224

// Say you roll a fair die  10  times. Which of the following is most likely?
// exactly 0, 1, 2 or 3 sixes
def e6_2() = {
    for (i <- 0 to 3) println(i, r(bernTrials(10, i, 1/6.toDouble)))
} //> def e6_2(): Unit
e6_2

def e7_1() = {
    val ev = 10 * 4/52.0 + (-1) * 48/52.0
    println(ev)
    val vari = (4/52.0) * pow2(10 + 2/13.0, 2) + 48/52.0 * pow2(-1 + 2/13.0, 2)
    println(vari)
} //> def e7_1(): Unit
e7_1

-2/13.0 //> val res526: Double = -0.15384615384615385

type Numbers = List[Double] //> type Numbers
// Expected value of a game:  ev(G) = sum(pi*ai)
def expectedValue(ps: Numbers, as: Numbers): Double = {
    require(round(ps.sum, 8) == 1, s"One and only one outcome is required. Probabilities=$ps.")
    ps.zip(as).map { case (p, a) => p * a }.sum
} //> def expectedValue(ps: Numbers, as: Numbers): Double
// Variance is defined as (also known as square of standard-deviation):
//   sigma^2(G) = sum(pi*(ai - ev(G))^2)
def variance(ps: Numbers, as: Numbers): Double = {
    val ev = expectedValue(ps, as)
    ps.zip(as).map {
        case (p, a) => p * math.pow(a - ev, 2)
    }.sum
} //> def variance(ps: Numbers, as: Numbers): Double

def e7_2() {
    val p = 1/6.0
    val ps = List(p, p, p, p, p, p)
    val as: Numbers = List(1,2,3,4,5,6)
    val ev = expectedValue(ps, as)
    println(ev)
    val vari = variance(ps, as)
    println(vari)
} //> def e7_2(): Unit
e7_2

def e7_4() {
    val p = 1/6.0
    val ps = List(p, 1-p)
    val as = List(1.0, 0)
    val ev = expectedValue(ps, as)
    val vari = variance(ps, as)
    println(ev, vari)
} //> def e7_4(): Unit
e7_4

500/36.0 //> val res529: Double = 13.88888888888889
// 13.8888

// 16.6666

// 3/5 * (2/5)^2 + 2/5 * (3/5)^2
// = a/b
// a= 3 * 4 + 2 * 9 = 12 + 18 = 30
// b= 5 * 25
// 6/25
// G(100) = 600/25

def sigma(ps: Numbers, as: Numbers): Double = math.sqrt(variance(ps, as)) //> def sigma(ps: Numbers, as: Numbers): Double
def normalize(ps: Numbers, as: Numbers): Numbers = {
    val ev = expectedValue(ps, as)
    val sig = sigma(ps, as)
    val div = if (round(sig, 8) != 0) sig else as.head // ev is 0 if ai == aj for all i&j
    as.map { a => (a - ev) / div }
} //> def normalize(ps: Numbers, as: Numbers): Numbers
def unfairCoin(p: Double, a: Double = 1.0, b: Double = 0.0): (Double, Double) = {
    val ev = p * a + (1 - p) * b
    (r(ev), r(p * (a * a - b * b) + 2 * p * ev * (b - a) + b * b + ev * ev - 2 * b * ev))
} //> def unfairCoin(p: Double, a: Double, b: Double): (Double, Double)
def normalizeUnfairCoin(p: Double, a: Double = 1.0, b: Double = 0.0): (Double, Double) = {
    val (ev, vari) = unfairCoin(p, a, b)
    val as = normalize(List(p, 1 - p), List(a, b))
    (r(as.head), r(as.tail.head))
} //> def normalizeUnfairCoin(p: Double, a: Double, b: Double): (Double, Double)
def gaussian(x: Double): Double = math.sqrt(0.5 / math.Pi) * math.exp(-0.5 * x * x) //> def gaussian(x: Double): Double

normalize(List(0.5,0.5), List(1.0, 0.0)) //> val res530: Numbers = List(1.0, -1.0)

0.6 * 0.4 * 0.4 + 0.4 * 0.6 * 0.6 //> val res531: Double = 0.24

10/math.sqrt(24) //> val res532: Double = 2.041241452319315

1 - 0.9793 //> val res533: Double = 0.02070000000000005

def e7_8() = {
    val ps = List(1/6.0, 5/6.0)
    val as = List(1.0, 0.0)
    val ev = expectedValue(ps, as)
    val vari = variance(ps, as)
    val nEv = 100 * ev
    val nVari = 100 * vari
    (25 - nEv) / math.sqrt(nVari)
} //> def e7_8(): Double
println(e7_8) // 2.24
val gaussianArea = 1-0.9875 // 0.0125 //> val gaussianArea: Double = 0.012499999999999956
