def pow(base: Int, exp: Int): BigInt = if (exp < 1) 1 else base * pow(base, exp - 1)
def fac(n: Int): BigInt = if (n < 2) 1 else n * fac(n - 1)
def seqRep(n: Int, k: Int): BigInt = pow(n, k)
def seqUniq(n: Int, k: Int): BigInt = if (k < 1) 1 else if (k < 2) n else n * seqUniq(n - 1, k - 1)
def colUniq(n: Int, k: Int): BigInt = seqUniq(n, k) / fac(k)
def colRep(n: Int, k: Int): BigInt = colUniq(n + k - 1, k)
val choose = colUniq _

// 50 at a party, how many choices to get distinct birthdays
println("number of party goers vs probability that two share a birthday:")
for (n <- List(50, 40, 30, 25, 23, 22, 20, 10)) {
    val numDistinct = seqUniq(365, n).toDouble
    val numTotal = pow(365, n).toDouble
    println(n, round(1 - numDistinct / numTotal, 3))
}
