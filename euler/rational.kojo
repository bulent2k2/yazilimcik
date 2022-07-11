
case class Rational(n: Int, d: Int) extends Number {
    def gcdEuclid(a: Int, b: Int): Int = if (b == 0) a else gcdEuclid(b, a % b)
    val gcd = gcdEuclid(n, d)
    val num = n / gcd
    val den = d / gcd
    override def toString = if (den == 1) num.toString else s"$num/$den"
    def doubleValue = num.toDouble / den
    def floatValue = num.toFloat / den
    def intValue = num / den
    def longValue = num / den
    def *(r2: Rational) = Rational(num * r2.num, den * r2.den)
    def /(r2: Rational) = Rational(num * r2.den, den * r2.num)
    def +(r2: Rational) = Rational(num * r2.den + r2.num * den, den * r2.den)
    def -(r2: Rational) = Rational(num * r2.den - r2.num * den, den * r2.den)
    def *(n: Int) = Rational(num * n, den)
    def /(n: Int) = Rational(num, den * n)
    def +(n: Int) = Rational(num + n * den, den)
    def -(n: Int) = Rational(num - n * den, den)
    def ==(r2: Rational) = if (den == r2.den) num == r2.num else {
        val gcd1 = gcdEuclid(num, r2.num)
        val gcd2 = gcdEuclid(den, r2.den)
        (num/gcd1) * (r2.den/gcd2) == (r2.num/gcd1) * (den/gcd2)
    }
    def !=(r2: Rational) = !(this == r2)
    def <(r2: Rational) = if (den == r2.den) num < r2.num else {
        val gcd1 = gcdEuclid(num, r2.num)
        val gcd2 = gcdEuclid(den, r2.den)
        (num/gcd1) * (r2.den/gcd2) < (r2.num/gcd1) * (den/gcd2)
    }
    def >(r2: Rational) = !(this < r2)
    def <=(r2: Rational) = this < r2 || this == r2
    def >=(r2: Rational) = this > r2 || this == r2
}

object Rational {
    val min = -2_147_483_648
    val max = 2_147_483_647
    def apply(n: Int): Rational = Rational(n, 1)
    def apply(x: Float): Rational = apply(x.toDouble)
    def apply(x: Double): Rational = {
        if (x < min) Rational(min)
        else if (x <= max) {
            val f = 1_000_000_000
            Rational((x * f).toInt, f)
        }
        else Rational(max)
    }
    def apply(n: Long): Rational = {
        if (n < min) Rational(min)
        else if (n <= max) Rational(n.toInt)
        else Rational(max)
    }
}
val ns: List[Number] = List(
    1,
    0xd,
    0xabcdef,
    1.2,
    0.5e-15,
    1.8e12,
    BigInt("1234567890987654321"),
    Rational(1, 3)
)
ns.foreach(println)
ns.map {
    n => math.pow(n.doubleValue, 2)
}.foreach(println _)

val r1 = Rational(2, 6)
val r2 = Rational(3, 9)
println(r1 * r2)
println(r1 + r2)
println(r1 * 3); println(Rational(3) * r1)
println(r1 / 5)
println(r1 / (r1 * 25))
println(Rational(1, 3) + 1)
println(Rational(14, 3).doubleValue)
println(Rational(0.25))
println(Rational(0.1e-20))
println(Rational(0.00000005))
assert(Rational(123456789000L) == Rational(2147483647), "compare")
val n = 100000 * 200000; println(n)
assert(Rational(100000/2222) == Rational(200000/4444), "comp2")
assert(Rational(3, 5) == Rational(15, 25), "comp3")
assert(Rational(2, 3) != Rational(3, 4), "comp4")
assert(Rational(5, 6) < Rational(7, 8), "comp5")
assert(Rational(3, 6) < Rational(4, 6), "comp6")
