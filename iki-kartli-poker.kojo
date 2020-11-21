// 4.2 Practice Problem 1
/* In a drastically simplified poker game, you're dealt just two cards.
 * If you get a pair—that is, if the two cards are of the same denomination—you receive $50. 
 * If you get a "flush"—two cards of the same suit—the payoff is $10. 
 * What's the expected value of the game?
*/

def pow(base: Int, exp: Int): BigInt = if (exp < 1) 1 else base * pow(base, exp - 1)
def fac(n: Int): BigInt = if (n < 2) 1 else n * fac(n - 1)
def seqRep(n: Int, k: Int): BigInt = pow(n, k)
def seqUniq(n: Int, k: Int): BigInt = if (k < 1) 1 else if (k < 2) n else n * seqUniq(n - 1, k - 1)
def colUniq(n: Int, k: Int): BigInt = seqUniq(n, k) / fac(k)
def colRep(n: Int, k: Int): BigInt = colUniq(n + k - 1, k)
val choose = colUniq _
clearOutput
val numAll   = choose(52, 2).toInt // 1326
val numPair  = choose(4, 2) * choose(13, 1) // 78 
val numFlush = choose(4, 1) * choose(13, 2) // 312 
val pPair    = numPair.toDouble / numAll // 0.06
val pFlush   = numFlush.toDouble / numAll // 0.24
// expected value is 50 * 78 / 1326 + 10 * 312 / 1326
val expected = 50 * pPair + 10 * pFlush // 5.3
println("Expected payoff is " + round(expected, 1))
