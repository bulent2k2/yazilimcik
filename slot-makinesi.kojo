// 4.2 Practice Problem 2
/* Suppose you play a slot machine like the one we described in Lesson 4.2, 
 * but on this one each wheel has seven pictures: one bell and six fruits. 
 * Let's say the payoffs are the same:  $50  for three bells,  $10  for three of the same fruit, 
 * and  $2  for two bells and one fruit. 
 * Now what's the expected value of the game?
 */
 
def pow(base: Int, exp: Int): BigInt = if (exp < 1) 1 else base * pow(base, exp - 1)
def fac(n: Int): BigInt = if (n < 2) 1 else n * fac(n - 1)
def seqRep(n: Int, k: Int): BigInt = pow(n, k)
def seqUniq(n: Int, k: Int): BigInt = if (k < 1) 1 else if (k < 2) n else n * seqUniq(n - 1, k - 1)
def colUniq(n: Int, k: Int): BigInt = seqUniq(n, k) / fac(k)
def colRep(n: Int, k: Int): BigInt = colUniq(n + k - 1, k)
val choose = colUniq _
clearOutput

val numAll          = pow(7, 3) // 343
val num3bells       = 1
val num3ofSameFruit = 6
val num2bells1Fruit = 3 * 6 // choose the slot and the fruit
val sum             = 50 * num3bells + 10 * num3ofSameFruit + 2 * num2bells1Fruit // 146
val expected        = sum / numAll.toDouble // 0.43
println("Expected payoff is " + round(expected, 2))
