// bağımlı olayların olasılık hesabı  (5.3 Practice Problems)

// iki olayın ilişkisi olmasa, şu doğru olurdu:
//   Olasılık(A ve B) = Olasılık(A) * Olasilık(B)
// Ama bağımsız değillerse, şöyle olur:
//   Olasılık(A ve B) = O(B) x O(B olduğunda A)  ya da
//                    = O(A) x O(A olduğunda B)

// In English:
//   P(A and B) = P(A assuming B) * P(B)  or
//              = P(B assuming A) * P(A)

def fac(n: Int): BigInt = if (n < 2) 1 else n * fac(n - 1)
def pow(b: Int, e: Int): BigInt = if (e < 1) 1 else b * pow(b, e - 1)
def seqRep(n: Int, k: Int): BigInt = pow(n, k)
def seqUnq(n: Int, k: Int): BigInt = if (k < 1) 1 else if (k < 2) n else n * seqUnq(n - 1, k - 1)
def colUnq(n: Int, k: Int): BigInt = seqUnq(n, k) / fac(k)
def colRep(n: Int, k: Int): BigInt = colUnq(n + k - 1, k)
val choose = colUnq _
val bc = colUnq _

// Good example, a couple of years ago, Cowboys won only 1 out of their first 5 games.
// Historically, only 1 in 25 of the teams that made it to the playoffs started with the
// dismal record.
// Now, if a commenter says, Cowboys have only 1 in 25 chance of making to the playoffs,
// is he speaking the truth or not? Is the argument correct or not? Why?

// No! Because, he is saying that P(A given B) is P(B given A) where
// A: Making the play offs, B: dismal record in the first five games.
//    P(B given A) = 1/25  (Check the record when a team makes it to play offs)
//    P(A given B) (Dismal in first 5 games)
clearOutput
val numTeams = 32
val numTeamsMakingThePlayoffs = 12
// rough guess (assuming all the teams are equally good!)
val pMakingThePlayoffs = 12 / 32.0
val pWinningAnyGame = 0.5
val pWinningOnlyOneOfFirstFive = choose(5, 1).toDouble / pow(2, 5).toDouble

def prob1() = {
    val pA = pMakingThePlayoffs
    val pB = pWinningOnlyOneOfFirstFive
    val pAgivenB = 1 / 25.0 // 4%
    // pAgivenB * pB = pBgivenA * pA =>
    pAgivenB * pA / pB // 9.6%
}
prob1

// 5.3 Practice Problem 2
/* Many of the humans in Jamestown work in the quarry, so at any given time 40% of them are covered in dirt.
 * Likewise, 90% of the zombies in Jamestown are covered in dirt, though the other 10% have managed to
 * clean themselves up. Overall, 20% of the population of Jamestown are zombies.
 * Suppose you see a figure in the distance. You are not close enough to discern whether
 * they are human or a zombie, but you can see that they are covered in dirt.
 * What are the odds that a zombie is approaching?
 */

val pDirtyGivenHumans = 0.4
val pDirtyGivenZombies = 0.9
val pZombies = 0.2
val pHumans = 1 - pZombies

val pDirtyHumans = pDirtyGivenHumans * pHumans
val pDirtyZombies = pDirtyGivenZombies * pZombies
val pDirty = pDirtyHumans + pDirtyZombies // 0.5
// pZombieGivenDirty * pDirty = pDirtyGivenZombies * pZombies
val pZombieGivenDirty = pDirtyGivenZombies * pZombies / pDirty // 0.36
// println(s"Odds that the dirty figure far away is a zombie is not too high: $pZombieGivenDirty")

// 5.3 Practice Problem 3
/* A patient goes to see a doctor. The doctor performs a test with 99 percent reliability —
 * that is, 99 percent of people who are sick test positive and 99 percent of the healthy people test negative.
 * The doctor knows that only one percent of the people in the country are sick.
 * If the patient tests positive, what are the chances the patient is sick?
 */

val sickPerCent = 1 // out of 100
val pSick = sickPerCent / 100.0
val pWell = 1 - pSick
val pTestPosGivenSick = 0.99
val pTestNegGivenWell = 0.99
val pTestPosGivenWell = 1 - pTestNegGivenWell // 1/100
val pTestPosAndSick = pTestPosGivenSick * pSick // 99/1e4
val pTestPosAndWell = pTestPosGivenWell * pWell // 99/1e4
val pTestPos = pTestPosAndSick + pTestPosAndWell // 2*99 / 1e4
val pSickGivenTestPos = pTestPosAndSick / pTestPos // 1/2  (99 / 1e4)/(2*99 / 1e4)
// println(s"Odds of being sick when the test posits sick is only: ${round(pSickGivenTestPos, 3)}")

// Make it more symbolic
def evalTest(testEffectiveness: Double = 99, sickPerCent: Double = 1) = {
    def r(p: Double) = round(p, 4)
    println(s"\nLet's see the utility of a test with %$testEffectiveness effectiveness when ${r(sickPerCent)} in 100 is sick:")
    // A: a person being sick  V: healthy = not A
    // B: test positing that a person is sick (test gives a positive result)   W: test posits well  = not B
    val pA = sickPerCent / 100.0
    val pV = 1 - pA // p of not A -- a person being well
    val pBgA = testEffectiveness / 100.0 // p of B given A (assuming that A occurred) -- if sick, test will say sick
    val pWgV = pBgA // todo: support asym tests where p(test will say well if well) != p(test will say sick if sick)
    val pBgV = 1 - pWgV // p of B given not A + p of not B given not A = 1
    val pAB = pBgA * pA // p of (A and B) -- they both occur
    val pVB = pBgV * pV // == pVB
    val pB = pAB + pVB // p of B is the same as p of (B and A) plus p of (B and NOT A)
    val pAgB = pAB / pB
    println(s"Odds of being sick, when the test posits sick, is: ${r(pAgB)} (only that!?)")
    val pW = (1 - pB)  // test positing that a person is well
    val pVW = pWgV * pV // p of (not B AND not A) = p of (not B given not A) * p of (not A)
    val pWgA = (1 - pBgA)
    val pAW = pWgA * pA // p of (not B AND A) = p of (not B given A) * p of A
    val pAgW = pAW / pW
    val pVgW = pVW / pW
    println(s"Odds of being well, when the test posits well, is: ${r(pVgW)}")
    val pVgB = pVB / pB  // false positive (test posits sick, but person is well
    assert(pAgB + pVgB == 1.0, "all cases when test posits sick") // true positives + false positives sum up to 1
    assert(pVgW + pAgW == 1.0, "all cases when test posits well") // true negatives + false negatives sum up to 1
    List("P(person is sick)=", r(pA), " Test effectiveness=", r(pBgA), 
    " P(test posits sick)=", r(pB),
    " P(true pos)=", r(pAgB), " P(true neg)=", r(pVgW),
    " P(false pos)=", r(pVgB), " P(false neg)=", r(pAgW)).mkString("")
}
println(evalTest())
println(evalTest(50)) // coin toss!
// Neuroblastoma. Very rare. Fewer than 20,000 US cases per year => 20 / 300k => 0.02 / 300
def numUScasesToPerCent(num: Double): Double = 100 * num / 300e6
println(evalTest(99.99, numUScasesToPerCent(20e3)))

