def pow(base: Int, exp: Int): BigInt = if (exp < 1) 1 else base * pow(base, exp - 1)
def fac(n: Int): BigInt = if (n < 2) 1 else n * fac(n - 1)
def seqRep(n: Int, k: Int): BigInt = pow(n, k)
def seqUniq(n: Int, k: Int): BigInt = if (k < 1) 1 else if (k < 2) n else n * seqUniq(n - 1, k - 1)
def colUniq(n: Int, k: Int): BigInt = seqUniq(n, k) / fac(k)
def colRep(n: Int, k: Int): BigInt = colUniq(n + k - 1, k)
val choose = colUniq _

// how many ways do we get a sum of 2, 3, etc from a two dice roll
// (total, num-ways)
val outcomes = List(
  (2, 1), // 1,1
  (3, 2), // 1,2 or 2,1
  (4, 3),
  (5, 4),
  (6, 5),
  (7, 6),
  (8, 5),
  (9, 4),
  (10, 3),
  (11, 2),
  (12, 1)
)

// if the payoff is the total, than, the expected value is: 7
val ev = outcomes.map{ case (total, num) => total * num.toDouble /36 }.sum
println(round(ev, 3))