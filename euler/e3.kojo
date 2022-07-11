/* Largest prime factor

Problem 3

The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ? */

type Num = BigInt

def prime(n: Num): Boolean =
  if (n < 2) false
  else if (n == 2) true
  else if (n % 2 == 0) false
  else (for(f <- Range(3, math.sqrt(n.toDouble).toInt + 1, 2) if n % f == 0) yield(f)).isEmpty

clearOutput
// for(i <- 1 to 100; if prime(BigInt(i))) print(s"$i, ")

for(i <- List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97))
    assert(prime(i) == true)
for(i <- List(1, 4, 6, 9, 15)) assert(prime(i) == false)

def factors(n: Num): List[Num] = {
    val fs = for(f <- Range(2, math.sqrt(n.toDouble).toInt + 1) if n % f == 0) yield(BigInt(f))
    val xs = for(x <- fs) yield((n / x))
    (fs ++ xs).toList.sorted.distinct
}

def primeFactors(n: Num): List[Num] = factors(n).filter(prime(_))

assert(factors(83) == Nil)
assert(factors(100) == List(2, 4, 5, 10, 20, 25, 50))
assert(factors(64) == List(2, 4, 8, 16, 32))
assert(primeFactors(BigInt(13195)) == List(5, 7, 13, 29))
assert(primeFactors(BigInt("600851475143")) == List(71, 839, 1471, 6857))
