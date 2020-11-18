// run as: amm ~/src/yaz/yazilimcik/saySade.kojo
// see ./say.kojo for more info

def pow(base: Int, exp: Int): Long = if (exp < 1) 1 else base * pow(base, exp - 1)
def fac(n: Int): Long = if (n < 2) 1 else n * fac(n-1)
def seqWithRep(n: Int, k: Int): Long = pow(n, k)
def seqUniq(n: Int, k: Int): Long = if (k < 1) 1 else if (k < 2) n else n * seqUniq(n-1, k-1)
def colUniq(n: Int, k: Int): Long = seqUniq(n, k) / fac(k)
def colWithRep(n: Int, k: Int): Long = colUniq(n+k-1, k)

val bc = colUniq _
val tableOfBC = for(r <- 0 to 10; c <- 0 to r) yield (r, c, bc(r, c))
val tab2 = tableOfBC.groupBy{ case (r, c, bc) => r }
val l1: ((Int, Int, Long)) => Long = { case (_,_,bc) => bc }
println("Binomial coefficients (a+b)^r = a^r + c1xa^(r-1)xb + c2xa^(r-2)xb^2 + ... + c(r-1)xaxb^(r-1) + crxb^r")
for(r <- 0 to 9) println(s"r=$r => ${tab2(r).map(l1).mkString(",")}")

def multi(n: Int, a: List[Int]): Long = {
    if (a.sum != n) throw new Exception(s"a:$a does not sum up to n=$n")
    fac(n) / a.map(fac(_)).product
}
def bc2(n: Int, k: Int): Long = multi(n, List(k, n-k))
assert(bc2(9, 4) == bc(9, 4))
assert(bc2(10, 2) == bc(10, 2))

println("=======================================")
println("k colUniq colWithRep seqUniq seqWithRep")
println("= ======= ========== ======= ==========")
for(k <- 0 to 5) println(f"$k%s ${colUniq(5, k)}%7d ${colWithRep(5, k)}%10d ${seqUniq(5, k)}%7d ${seqWithRep(5, k)}%10d")
println("=======================================")
print("The wrong analogy seqWithRep(n,k)/fac(k) gives: ")
println((for(k <- 0 to 5) yield(seqWithRep(5, k)/fac(k))).mkString(" "))

def sortLists(xs: List[Int], ys: List[Int]): Boolean =  if (xs.isEmpty || ys.isEmpty) xs.isEmpty else if (xs.head == ys.head) sortLists(xs.tail, ys.tail) else xs.head < ys.head
def instruct1(n: Int, k: Int, set: Set[List[Int]], repeat: Boolean, order: Boolean) = {
  val orderOrChoose = if (order) "ordering" else "choosing"
  val withOrWithout = if (repeat) "with" else "without"
  val elaborate = if (order) "." else " (when we don't care about their order)."
  println(s"\nThere are ${set.size} ways of $orderOrChoose $k objects out of $n $withOrWithout repetition$elaborate.")
  println("Here are all those: " + set.toList.sortWith(sortLists).map(xs => xs.mkString).mkString("{", " ", "}"))
  println(s"And, again, there are ${set.size} of them. Count them all if you don't believe me!")
}
var set1 = Set.empty[List[Int]]
val list1 = (0 to 4).toList

for(x <- list1; y <- list1; if y != x) set1 += List(x, y).sorted
instruct1(5, 2, set1, false, false)
set1 = Set.empty
for(x <- list1; y <- list1) set1 += List(x, y).sorted
instruct1(5, 2, set1, true, false)
set1 = Set.empty
for(x <- list1; y <- list1; if y != x) set1 += List(x, y)
instruct1(5, 2, set1, false, true)
set1 = Set.empty
for(x <- list1; y <- list1) set1 += List(x, y)
instruct1(5, 2, set1, true, true)
