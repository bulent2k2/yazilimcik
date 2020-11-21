// n doors, k cars, n-k goats
// pick a door. Monty opens another door with goat. Should you switch or not?
// Always, yes!

/* In Monty Hall's generalized game there are  𝑘  cars as prizes behind  𝑛  doors ( 1≤𝑘<𝑛−1 ).
 * The rules are the same as before: You pick a door. Monty opens a door you didn't pick to reveal a goat. 
 * Then, you have the opportunity to switch your pick. 
 * Let  𝐴  denote the event of guessing a door with a car behind it, and 
 * let  𝐵  denote the event of guessing a door with a goat behind it on your initial pick. 
 * Let  𝐿  represent the event of losing the game if your strategy is always to switch doors.
 */ 

// for amm only. from: ~/src/kojo/git/kojo/src/main/scala/net/kogics/kojo/lite/CoreBuiltins.scala
def round(n: Number, digits: Int = 0): Double = {
  val factor = math.pow(10, digits).toDouble
  math.round(n.doubleValue * factor).toLong / factor
}

// p(win no switch)    = k / n
// Event A: the first gate is a win
// Event B: the first gate is a goat
// p(win after switch) = p(win if A)*p(A) + p(win if B)*p(B)
def probWinWoSwitch(n: Int, k: Int) = round(k / n.toDouble, 2)
def probWinWithSwitch(n: Int, k: Int) = {
    val nD    = n.toDouble
    val pA    = k / nD
    val pB    = (n-k) / nD
    val pWifA = (k-1) / (nD-2)
    val pWifB = k / (nD-2)
    round(pWifA * pA + pWifB * pB, 2)
}

println("Probability of winning a car:")
println("doorCnt carCnt w/o Switch w/Switch")
println("======= ====== ========== ========")
val check = (for(n <- 3 to 10; k <- 1 to n-2) yield {
    val p1 = probWinWoSwitch(n,k)
    val p2 = probWinWithSwitch(n,k)
    println(f"$n%7d $k%6d $p1%10.2f $p2%8.2f")
    p2 > p1
}).forall { x => x == true }
if (check) println("Yes, switching always increases the probability of winning a car.")
