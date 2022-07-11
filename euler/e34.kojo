/* Digit factorials Problem 34
 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

 Find the sum of all numbers which are equal to the sum of
 the factorial of their digits.

 Note: As 1! = 1 and 2! = 2 are not sums they are not included. */

type Num = Int
def factorial2(n: Num): Num = if (n < 2) 1 else n * factorial2(n - 1)

def factorial(n: Int) = if (n < 2) 1 else (2 to n).toList.reduce(_ * _)

// from e4
def num2Digits(x: Num): List[Num] =
  if (x < 10) List(x) else x % 10 :: num2Digits(x / 10)

// f9 * numDigits < 10^numDigits

val f9 = factorial(9).toDouble
println("num digits / max sum / min num / enough?")
for (d <- 1 to 10) {
  val max_sum = f9 * d
  val min_num = math.pow(10, d-1)
  val enough = max_sum > min_num
  println(f"$d%10s   $max_sum%7.0f $min_num%7.0e $enough%10s")
}

/*
num digits / max sum / min num / enough?
         1    362880   1e+00       true
         2    725760   1e+01       true
         3   1088640   1e+02       true
         4   1451520   1e+03       true
         5   1814400   1e+04       true
         6   2177280   1e+05       true
         7   2540160   1e+06       true
         8   2903040   1e+07      false
         9   3265920   1e+08      false
        10   3628800   1e+09      false
 */

def doIt = {
  for(n <- 10 to 2540160; if n == num2Digits(n).map(factorial(_)).reduce(_ + _)) println(n)
}

println(145+40585) // 40730




