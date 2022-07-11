def round(n: Number, digits: Int = 0): Double = {
    val factor = math.pow(10, digits).toDouble
    math.round(n.doubleValue * factor).toLong / factor
}
def pow(base: Int, exp: Int): BigInt = if (exp < 1) 1 else base * pow(base, exp - 1)
def fac(n: Int): BigInt = if (n < 2) 1 else n * fac(n - 1)
def seqRep(n: Int, k: Int): BigInt = pow(n, k)
def seqUniq(n: Int, k: Int): BigInt = if (k < 1) 1 else if (k < 2) n else n * seqUniq(n - 1, k - 1)
def colUniq(n: Int, k: Int): BigInt = seqUniq(n, k) / fac(k)
def colRep(n: Int, k: Int): BigInt = colUniq(n + k - 1, k)
val choose = colUniq _

val numAll = choose(52, 13)
println(s"$numAll is the number of all possible hands.")
def nice(n: BigInt) = round(n.toDouble / numAll.toDouble, 3) // print the probability in a nice way

val c6of = choose(13, 6).toInt // = c7
val c5of = choose(13, 5).toInt // = c8
val c4of = choose(13, 4).toInt // = c9
val c3of = choose(13, 3).toInt
val c2of = choose(13, 2).toInt
// number of hands with a given distribution
val n4441 = 4 * pow(c4of, 3) * 13
val n4432 = 6 * pow(c4of, 2) * 2 * c3of * c2of
val n4333 = 4 * c4of * pow(c3of, 3)

val n553 = 6 * pow(c5of, 2) * 2 * c3of
val n5521 = 6 * pow(c5of, 2) * 2 * c2of * 13
val n544 = 4 * c5of * 3 * pow(c4of, 2)
val n5431 = BigInt(4) * c5of * 3 * c4of * 2 * c3of * 13
val n5422 = 4 * c5of * 3 * c4of * pow(c2of, 2)
val n5332 = 4 * c5of * 3 * pow(c3of, 2) * c2of

val n661 = 6 * pow(c6of, 2) * 2 * 13
val n652 = BigInt(4) * c6of * 3 * c5of * 2 * c2of
val n6511 = 4 * c6of * 3 * c5of * pow(13, 2)
val n643 = BigInt(4) * c6of * 3 * c4of * 2 * c3of
val n6421 = BigInt(4) * c6of * 3 * c4of * 2 * c2of * 13
val n6331 = 4 * c6of * 3 * pow(c3of, 2) * 13
val n6322 = 4 * c6of * 3 * c3of * pow(c2of, 2)

val n76 = BigInt(4) * c6of * 3 * c6of
val n751 = BigInt(4) * c6of * 3 * c5of * 2 * 13
val n742 = BigInt(4) * c6of * 3 * c4of * 2 * c2of
val n733 = 4 * c6of * 3 * pow(c3of, 2)
val n7411 = BigInt(4) * c6of * 3 * c4of * 13 * 13
val n7321 = BigInt(4) * c6of * 3 * c3of * 2 * c2of * 13
val n7222 = 4 * c6of * pow(c2of, 3)

val n85 = BigInt(4) * c5of * 3 * c5of
val n841 = BigInt(4) * c5of * 3 * c4of * 2 * 13
val n832 = BigInt(4) * c5of * 3 * c3of * 2 * c2of
val n8311 = 4 * c5of * 3 * c3of * pow(13, 2)
val n8221 = 4 * c5of * 3 * pow(c2of, 2) * 13

val n94 = 4 * 3 * pow(c4of, 2)
val n931 = BigInt(4) * c4of * 3 * c3of * 2 * 13
val n922 = 4 * c4of * 3 * pow(c2of, 2)
val n9211 = 4 * c4of * 3 * c2of * pow(13, 2)

val na3 = 4 * 3 * pow(c3of, 2)
val na21 = BigInt(4) * c3of * 3 * c2of * 2 * 13
val na111 = 4 * c3of * pow(13, 3)
val nb2 = 4 * 3 * pow(c2of, 2)
val nb11 = 4 * c2of * 3 * pow(13, 2)
val nc1 = 4 * 3 * pow(13, 2)
val nd = 4

assert(45 / 22.0 == n4432.toDouble / n4333.toDouble, "check Reha")

val hands: List[BigInt] =
    List(n4333, n4432, n4441,
        n553, n5521, n544, n5422, n5431, n5332,
        n661, n652, n643, n6511, n6421, n6331, n6322,
        n76, n751, n742, n7411, n733, n7321, n7222,
        n85, n841, n832, n8311, n8221,
        n94, n931, n922, n9211,
        na3, na21, na111,
        nb2, nb11,
        nc1,
        nd)

println(s"${hands.size} kinds of distributions of hands do we have")
val left = numAll - hands.sum
println(s"$left of the hands are left to count.")
assert(numAll == hands.sum)
val top5 = hands.sorted.reverse.map(nice(_)).take(5)
println(s"${top5.sum} is the probability one of the top five kinds of hands will be dealt.") 

val n2p: Map[String, BigInt] = Map("4333" -> n4333, "4432" -> n4432, "4441" -> n4441,
        "553" -> n553, "5521" -> n5521, "544" -> n544, "5422" -> n5422, "5431" -> n5431, "5332" -> n5332,
        "661" -> n661, "652" -> n652, "643" -> n643, "6511" -> n6511, "6421" -> n6421, "6331" -> n6331, "6322" -> n6322,
        "76" -> n76, "751" -> n751, "742" -> n742, "7411" -> n7411, "733" -> n733, "7321" -> n7321, "7222" -> n7222,
        "85" -> n85, "841" -> n841, "832" -> n832, "8311" -> n8311, "8221" -> n8221,
        "94" -> n94, "931" -> n931, "922" -> n922, "9211" -> n9211,
        "a3" -> na3, "a21" -> na21, "a111" -> na111,
        "b2" -> nb2, "b11" -> nb11,
        "c1" -> nc1,
        "d" -> nd)

val hands2 = n2p.values
println(hands2.size)
assert(hands2.toSet == hands.toSet, "set comp")

println(f"${nice(n4441)}%3.2f is the prob of a (4,4,4,1) hand")
println("Top five distributions and their frequency:")
n2p.map{ case(name, num) => (name, nice(num)) }.toList.sortWith { (a, b) => a._2 < b._2 }.reverse.take(5).foreach(x => println(s"${x._1} -> ${x._2}"))
