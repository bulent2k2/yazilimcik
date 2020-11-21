// 4.3 Practice Problem 1
/* Here are two more variations on the one-card game we talked about at the beginning of the lesson.
 * 
 * In Game 1, you are dealt one card from a standard deck of  52  cards.
 * You win  $10  if you get a face card, and otherwise the payoff is twice
 * the number showing on your card:  $2  for an Ace,  $4  for a  2 , 
 * and so on up to  $20  for a  10 .
 * 
 * In Game 2, you are also dealt one card from a standard deck of  52  cards. 
 * You win  $20  for a face card; if your card is a number card (we count an Ace as a  1 ), 
 * you get  $10  if the number is even and  $5  if it's odd.
 * 
 * Which game is better?
 */
 
def pow(base: Int, exp: Int): BigInt = if (exp < 1) 1 else base * pow(base, exp - 1)
def fac(n: Int): BigInt = if (n < 2) 1 else n * fac(n - 1)
def seqRep(n: Int, k: Int): BigInt = pow(n, k)
def seqUniq(n: Int, k: Int): BigInt = if (k < 1) 1 else if (k < 2) n else n * seqUniq(n - 1, k - 1)
def colUniq(n: Int, k: Int): BigInt = seqUniq(n, k) / fac(k)
def colRep(n: Int, k: Int): BigInt = colUniq(n + k - 1, k)
val choose = colUniq _
clearOutput

// pick one suit instead of four to simplify (13 cards instead of 52)
val numAll          = 13.0
val numFace         = 3
val cards = (1 to 10).toList
val sum1            = 10 * numFace + cards.map(2 * _).sum // 140
val expected1       = sum1 / numAll
val sum2            = 20 * numFace + cards.map(x => if (x % 2 == 0) 10 else 5).sum // 135
val expected2       = sum2 / numAll
sum1 > sum2 // true -- game 1 pays more
for ((i, e) <- List((1, expected1), (2, expected2))) 
  println(s"Expected payoff in game $i is " + round(e, 2))
