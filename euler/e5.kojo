/* Smallest multiple Problem 5
 2520 is the smallest number that can be divided by
 each of the numbers from 1 to 10 without any remainder.
 What is the smallest positive number that is evenly 
 divisible by all of the numbers from 1 to 20?
 */

//import $exec.f

// find all greatest prime powers less than or equal to n
def powers(n: Int) =
  for (p <- (1 to n).filter(prime(_))) yield(greatestPowerLE(p, n))

def findIt(n: Int) = powers(n).product

def greatestPowerLE(p: Int, n: Int): Int = { // greatest power of p <= n
  if (p == 1 || p > n) 1
  else if (p == n) n
  else p * greatestPowerLE(p, n/p)
}

type Num = Int
// e3
def prime(n: Num): Boolean =
  if (n < 2) false
  else if (n == 2) true
  else if (n % 2 == 0) false
  else (for(f <- Range(3, math.sqrt(n.toDouble).toInt + 1, 2) if n % f == 0) yield(f)).isEmpty

assert(findIt(10) == 2520)
assert(findIt(20) == 232792560, "20")

assert(prime(2520+1) == true)
assert(prime(findIt(20)+1) == true)

println(powers(10).sorted)
println(powers(20).sorted)
println(powers(30).sorted)

def product(ns: Seq[Int]): BigInt = ns.map(BigInt(_)).reduce(_ * _)

assert(product(powers(40)).toString == "5342931457063200")

println("All good!")
