/* Largest palindrome product

Problem 4

A palindromic number reads the same both ways.

The largest palindrome made from the product of two 2-digit numbers is
9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers. */

type Num = Int

def num2Digits(x: Num): List[Num] =
  if (x < 10) List(x) else x % 10 :: num2Digits(x / 10)

def isPalindrome(n: Num): Boolean = {
  val ds = num2Digits(n)
  ds == ds.reverse
}

for (n <- List(9009, 99, 101, 1)) assert(isPalindrome(n), s"$n is a palindrome.")
for (n <- List(9109, 98, 102, 12)) assert(!isPalindrome(n), s"$n is NOT a palindrome.")

// a million multiplications takes almost no time..
def findIt() = {
  val r = 100 to 999
  (for (a <- r; b <- r; if (isPalindrome(a * b))) yield(a * b)).max
}

assert(906609 == findIt, "906609 is the largest palindrome made from the product of two 3-digit numbers")
println("What is the problem now, if any?")
