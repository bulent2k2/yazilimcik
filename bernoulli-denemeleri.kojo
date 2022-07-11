// run as: amm ~/src/yaz/yazilimcik/saySade.kojo
// see ./say.kojo for more info
def fac(n: Int): BigInt = if (n < 2) 1 else n * fac(n - 1)
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
def pluralS(n: Int): String = if (n == 1) "" else "s"

clearOutput
// Newton is asked: 1 in 6 tickets have a prize. Which is likelier, 1 in 6, at least 2 in 12 or at least 3 in 18?
val p = 1/6.toDouble
// at least one win in six tickets:
def r(d: Double) = round(d, 3)
r(1 - bernTrials(6, 0, p))
r(1 - bernTrials(12, 0, p) - bernTrials(12, 1, p))
r(1 - bernTrials(18, 0, p) - bernTrials(18, 1, p) - bernTrials(18, 2, p)) 
r(bernTrials(18, 3, p))
r(bernTrials(18, 2, p))
for(i <- 1 to 5) println(f"$i ${"prize" + pluralS(i)}%6s ${round(bernTrials(18, i, p), 3)}")

// Lesson 6.1
// what's the probability that we get, say Ace of Hearts, in a five-card draw out of 52?
val numAll = choose(52, 5)
val numOne = choose(51, 4) // number of ways of getting four other cards
val prob1 = 5 / 52.0 // [51 * 50 * 49 * 48 / (2 * 3 * 4)] / [(52 * 51 * 50 * 49 * 48) / (2 * 3 * 4 * 5)]
assert(numOne.toDouble / numAll.toDouble == prob1, "math")
// what's the probability that we get 3 hands with Ace of Hearts out of 10 hands:
val prob2 = bernTrials(10, 3, prob1)
assert(prob2 == choose(10, 3).toDouble * pow2(prob1, 3) * pow2(1 - prob1, 7), "3 out of 10 hands")
val prob3 = (for(i <- 0 to 2) yield(bernTrials(10, 3, prob1))).sum
println(s"a five-card hand with an ace of H: ${r(prob1)} -- almost 10%!") 
println(s"exactly 3 hands out of 10 with an ace of H: ${r(prob2)} -- only ~5%.")
println(s"at least 3 hands out of 10 with an ace of H: ${r(1 - prob3)} -- more than 80%!")
