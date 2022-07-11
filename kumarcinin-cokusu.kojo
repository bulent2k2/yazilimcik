// Gambler's Ruin or a random walk with absorbing barriers

// Playing roulette, putting your money on black (48% chance to win)
// Put it all in one shot ($1000) or play $1 at a time? And stop if you get to $2000 or lose it all.

// Paradoxically, this one is easier to solve generally, 
// instead of generalizing from a seemingly simpler instance which gets complicated..
// So, generally, let's say we walk in to the casino with:
val a = 1000
// The odds of winning is:
val p = 0.48
// Each bid is $:
val bid = 1
// and the target is 
val b = 2000
// so, what's the odds of winning b.
// Find the general formula, x(a) = f(a, b, p)
// Note: x(0) = 0 -- if starting with 0, can't win.
// x(b) = 1. -- if starting with b, we are done.
//   x(a) = p * x(a+1) + (1-p) * x(a-1)   -- why?? conditional probability? weighed sum?
// x(a) = 1 solves the last equation for all a != 0 and != b. x(a) = (p / (1-p)^a solves it, too.
// In fact, any linear combination of the two solutions also do. 
// Here is what solves it for all a including 0 and b:
//   x(a) = (s^a - 1) / (s^b - 1), where s = q/p
def x(a: Int, b: Int, p: Double): Double = {
    require(p > 0 & p < 1, "probability to be between 0 and 1")
    require (b > a, "target to be greater than starting point")
    require (a >= 0, "starting point to be positive")
    val s = (1-p)/p
    (pow2(s, a) - 1)/(pow2(s, b) - 1)
}
def pow2(x: Double, k: Int): Double = math.pow(x, k)
val face: Char = '☺'

clearOutput
def r(d: Double) = round(d, 3)

val odds = x(1000, 2000, p) // 1.73e-35!
println("Odds of winning with this strategy of betting small and playing long is: " + r(odds) + " -- really!")
println("Much better to bet it all and lose it. Of course, the best strategy is to stay home. Don't go to las vegas!")

x(1000, 1100, p) // 3.3e-4
x(1000, 1010, p) // ~0.45 
x(1000, 1001, p) // ~0.92

/*
 6.2 Practice Problem 1
 0 points possible (ungraded)
 In the World Series, there are two baseball teams, and they play a series of
 games until one or the other team has won a total of four games. 
 So, in particular, the series can last for a maximum of seven games. 
 Suppose though that they made a rule change, and they decided that in the 
 World Series the two teams play until one of the other team has won four
 more games than the other.

 Let's say that we have two teams: Team A and Team B. Team A has a 40% chance of
 winning any given game. Team B has a 60% chance of winning any given game. 
 What are the odds of Team A winning the World Series?
 */

// if we start at 4 and go down to 0, it means team B has won. If we get to 8, it means team A has won.
println(r(x(4, 8, 0.4))) // 17%
println(r(x(4, 8, 0.6))) // 84%

println(s"Enjoy counting. Saymak güzel şey! ${face} Dr. Ben Bülent Başaran ${face}")
