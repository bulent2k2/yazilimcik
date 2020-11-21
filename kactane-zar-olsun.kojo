// 4.3 Practice Problem 2.a
/* Suppose we modify a dice rolling game in your favor. You pay  $3,000  to play,
 * and now you win and get a  $10,000  payoff if you get either a  5  or a  6 ,
 * but no  1 s; otherwise, you lose.
 *
 * What's the optimal number of dice to roll in this situation?
 */

def fac(n: Int): BigInt              = if (n < 2) 1 else n * fac(n - 1)
def pow(base: Int, exp: Int): BigInt = if (exp < 1) 1 else base * pow(base, exp - 1)

def seqRep(n: Int, k: Int): BigInt   = pow(n, k)
def seqUniq(n: Int, k: Int): BigInt  = if (k < 1) 1 else if (k < 2) n else n * seqUniq(n - 1, k - 1)
def colUniq(n: Int, k: Int): BigInt  = seqUniq(n, k) / fac(k)
def colRep(n: Int, k: Int): BigInt   = colUniq(n + k - 1, k)
val choose = colUniq _
clearOutput

def expected(n: Int) = {
    val numAll = pow(6, n)
    // no 1s minus no (5 and 6)
    val numDesired = pow(5, n) - pow(3, n)
    val probDesired = numDesired.toDouble / numAll.toDouble
    10000 * probDesired - 3000
}

for (i <- 1 to 5) println(i, expected(i).toInt)
