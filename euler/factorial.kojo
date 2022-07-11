type N = BigInt
object N { def apply(elem: Int): BigInt = BigInt(elem) }
// stack overflow for n=3940
def factoriaL(n: N): N = if (n < 2) N(1) else n * factoriaL(n - 1)
def factorial(n: N): N = if (n < 2) 1 else (2 to n.toInt).map(N(_)).toList.reduce(_ * _)
def numDigits(x: N): Int = x.toString.size

val n = 3900 // factorial of 10k has 35k digits!
val f: N = factorial(n)
println(f)
println(s"Has ${numDigits(f)} digits!")
if (n < 3940) {
    println("second impl running")
    assert(factoriaL(n) == factorial(n), s"$n")
}
