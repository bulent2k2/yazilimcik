// run as: amm ~/src/yaz/yazilimcik/saySade.kojo
// see ./say.kojo for more info

def fac(n: Int): BigInt = if (n < 2) 1 else n * fac(n - 1) //> def fac(n: Int): BigInt
def pow(b: Int, e: Int): BigInt = if (e < 1) 1 else b * pow(b, e - 1) //> def pow(b: Int, e: Int): BigInt
def seqRep(n: Int, k: Int): BigInt = pow(n, k) //> def seqRep(n: Int, k: Int): BigInt
def seqUnq(n: Int, k: Int): BigInt = if (k < 1) 1 else if (k < 2) n else n * seqUnq(n - 1, k - 1) //> def seqUnq(n: Int, k: Int): BigInt
def colUnq(n: Int, k: Int): BigInt = seqUnq(n, k) / fac(k) //> def colUnq(n: Int, k: Int): BigInt
def colRep(n: Int, k: Int): BigInt = colUnq(n + k - 1, k) //> def colRep(n: Int, k: Int): BigInt
val choose = colUnq _  // choose k from n -- equivalently choosing the other n-k, that's why it is known as binomial //> val choose: (Int, Int) => BigInt = $Lambda$2513/0x000000080100f040@279fe02f
val bc = colUnq _ // binomial coefficients -- the kth coefficient in the expansion of (a+b)^n, for all k=0 to n //> val bc: (Int, Int) => BigInt = $Lambda$2514/0x000000080100e840@4c323ee1
def multi(n: Int, k: List[Int]): BigInt = {  // multinomial coefficients, choosing k1, k2, k3 ... km-2, km-1, km where sum(ki) = n
    if (k.sum != n) throw new Exception(s"k:$k does not sum up to n=$n")
    fac(n) / k.map(fac(_)).product
} //> def multi(n: Int, k: List[Int]): BigInt
def bc2(n: Int, k: Int): BigInt = multi(n, List(k, n - k)) //> def bc2(n: Int, k: Int): BigInt

def sortLists(xs: List[Int], ys: List[Int]): Boolean = if (xs.isEmpty || ys.isEmpty) xs.isEmpty else if (xs.head == ys.head) sortLists(xs.tail, ys.tail) else xs.head < ys.head //> def sortLists(xs: List[Int], ys: List[Int]): Boolean

choose(52 , 3) //> val res187: BigInt = 22100

val numDenom = 13 //> val numDenom: Int = 13
val numCards = choose(4, 2) //> val numCards: BigInt = 6
val numSingle = 48 //> val numSingle: Int = 48
val numPair = numDenom * numCards * numSingle //> val numPair: scala.math.BigInt = 3744
val numThree = numDenom * choose(4, 3) //> val numThree: scala.math.BigInt = 52
val numFave = numPair + numThree //> val numFave: scala.math.BigInt = 3796

22776 / 6.0 //> val res188: Double = 3796.0
val numAll = choose(52, 3) //> val numAll: BigInt = 22100

132600 / 22100.0//> val res77: Double = 6.0 //> val res189: Double = 6.0

numFave.toDouble / numAll.toDouble //> val res190: Double = 0.17176470588235293

