// run as: amm ~/src/yaz/yazilimcik/saySade.kojo
// see ./say.kojo for more info
def fac(n: Int): BigInt = if (n < 2) 1 else n * fac(n - 1)
def pow(b: Int, e: Int): BigInt = if (e < 1) 1 else b * pow(b, e - 1)
// count sequences or collections of size k chosen from a pool of n objects.
// we can allow repetition (like throwing dice, or after each choice, it is as if we put it back into the deck or bag)
// or, keep each choice unique (as if we are dealing from a pocker deck)
def seqRep(n: Int, k: Int): BigInt = pow(n, k)
def seqUnq(n: Int, k: Int): BigInt = if (k < 1) 1 else if (k < 2) n else n * seqUnq(n - 1, k - 1)
def colUnq(n: Int, k: Int): BigInt = seqUnq(n, k) / fac(k)
def colRep(n: Int, k: Int): BigInt = colUnq(n + k - 1, k)
val choose = colUnq _  // choose k from n -- equivalently choosing the other n-k, that's why it is known as binomial
val bc = colUnq _ // binomial coefficients -- the kth coefficient in the expansion of (a+b)^n, for all k=0 to n

def multi(n: Int, k: List[Int]): BigInt = {  // multinomial coefficients, choosing k1, k2, k3 ... km-2, km-1, km where sum(ki) = n
    if (k.sum != n) throw new Exception(s"k:$k does not sum up to n=$n")
    fac(n) / k.map(fac(_)).product
}
def bc2(n: Int, k: Int): BigInt = multi(n, List(k, n - k))
assert(bc2(9, 4) == bc(9, 4))
assert(bc2(10, 2) == bc(10, 2))

def sortLists(xs: List[Int], ys: List[Int]): Boolean = if (xs.isEmpty || ys.isEmpty) xs.isEmpty else if (xs.head == ys.head) sortLists(xs.tail, ys.tail) else xs.head < ys.head
def instruct1(n: Int, k: Int, set: Set[List[Int]], repeat: Boolean, order: Boolean) = {
    val cmdName = (if (order) "seq" else "col") ++ (if (repeat) "Rep" else "Unq")
    val orderOrChoose = if (order) "ordering" else "choosing"
    val withOrWithout = if (repeat) "with" else "without"
    val elaborate = if (order) "." else " (when we don't care about their order)."
    println(f"* $cmdName%6s: There are ${set.size} ways of $orderOrChoose $k objects out of $n $withOrWithout repetition$elaborate.")
    println("Here are all those: " + set.toList.sortWith(sortLists).map(xs => xs.mkString).mkString("{", " ", "}"))
    println(s"And, again, there are ${set.size} of them. Count them all if you don't believe me!"); println("")
}
var set1 = Set.empty[List[Int]]
val list1 = (0 to 4).toList

for (x <- list1; y <- list1; if y != x) set1 += List(x, y).sorted // colUnq
instruct1(5, 2, set1, false, false); set1 = Set.empty
for (x <- list1; y <- list1) set1 += List(x, y).sorted //            colRep
instruct1(5, 2, set1, true, false); set1 = Set.empty
for (x <- list1; y <- list1; if y != x) set1 += List(x, y) //        seqUnq
instruct1(5, 2, set1, false, true); set1 = Set.empty
for (x <- list1; y <- list1) set1 += List(x, y) //                   seqRep
instruct1(5, 2, set1, true, true); set1 = Set.empty

val tableOfBC = for (r <- 0 to 10; c <- 0 to r) yield (r, c, bc(r, c))
val tab2 = tableOfBC.groupBy { case (r, c, bc) => r }
val l1: ((Int, Int, BigInt)) => BigInt = { case (_, _, bc) => bc }
println("Binomial coefficients:\n\t(a+b)^n = c0xa^n + c1xa^(n-1)xb + c2xa^(n-2)xb^2 + ...\n\t          ... + c(n-1)xaxb^(n-1) + cnxb^n\n")
for (r <- 0 to 9) println(s"n=$r => ${tab2(r).map(l1).mkString(",")}"); println("")

println("=============================")
println("k colUnq colRep seqUnq seqRep")
println("= ====== ====== ====== ======")
for (k <- 0 to 5) println(f"$k%s ${colUnq(5, k)}%6d ${colRep(5, k)}%6d ${seqUnq(5, k)}%6d ${seqRep(5, k)}%6d")
println("=============================")
println("Note that seqUnq(n,k)/fac(k) gives us colUnq(n,k).")
println("However   seqRep(n,k)/fac(k) is not   colRep(n,k). Instead it gives: " + (for (k <- 0 to 5) yield (seqRep(5, k) / fac(k))).mkString(" ") + ".")
println("Enjoy counting. Saymak güzel şey!")
