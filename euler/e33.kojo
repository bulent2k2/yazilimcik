/* Digit cancelling fractions Problem 33
The fraction 49/98 is a curious fraction, as an inexperienced mathematician
in attempting to simplify it may incorrectly believe that 49/98 = 4/8,
which is correct, is obtained by cancelling the 9s.

We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

There are exactly four non-trivial examples of this type of fraction, less 
than one in value, and containing two digits in the numerator and denominator.

If the product of these four fractions is given in its lowest common terms,
find the value of the denominator.
 */
def gcdEuclid(a: Int, b: Int): Int = if (b == 0) a else gcdEuclid(b, a%b)
def simplify(a: Int, b: Int): (Int, Int) = {
  val (a1, a2, b1, b2) = (a / 10, a % 10, b / 10, b % 10)
  val d = List(a1, a2).toSet.intersect(List(b1, b2).toSet).toList
  if (d.isEmpty) (a, a) else (if (a1 == d.head) a2 else a1, if (b1 == d.head) b2 else b1)
}
val product = (for (a <- 10 to 98; b <- a+1 to 99; if a%10 != 0) yield (a, b, simplify(a, b))).
  filter { t => val (a, b, num, den) = (t._1, t._2, t._3._1, t._3._2); a != num && a * den == b * num }.
  map { t => (t._3._1, t._3._2) }.
  reduce { (r1, r2) => (r1._1 * r2._1, r1._2 * r2._2) } // (Int, Int) = (8,800)
val result = product._2 / gcdEuclid(product._1, product._2) //    Int = 100
println(result)
