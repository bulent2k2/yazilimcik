// parayi n kere atarsak kac tanesinde yazi ve tura ayni sayida gelir
def yaziTura(n: Int): (Int, Int) = (1, 2) //> def yaziTura(n: Int): (Int, Int)

// truth table -- 01 -> 00011011 -> 
type Tablo = List[List[Int]] //> type Tablo
def katla(xxs: List[List[Int]]): List[List[Int]] = {
    def add(para: Int) = xxs.map(xs => para :: xs)
    add(0) ++ add(1)
} //> def katla(xxs: List[List[Int]]): List[List[Int]]
val birBit = List(List(0),List(1)) //> val birBit: List[List[Int]] = List(List(0), List(1))
val ikiBit = katla(birBit) //> val ikiBit: List[List[Int]] = List(List(0, 0), List(0, 1), List(1, 0), List(1, 1))
val ucBit  = katla(ikiBit) //> val ucBit: List[List[Int]] = List(List(0, 0, 0), List(0, 0, 1), List(0, 1, 0), List(0, 1, 1), List(1, 0, 0), List(1, 0, 1), List(1, 1, 0), List(1, 1, 1))
//> val res55: List[List[Int]] = List(List(0, 0), List(0, 1), List(1, 0), List(1, 1))
val dortBit = katla(ucBit) //> val dortBit: List[List[Int]] = List(List(0, 0, 0, 0), List(0, 0, 0, 1), List(0, 0, 1, 0), List(0, 0, 1, 1), List(0, 1, 0, 0), List(0, 1, 0, 1), List(0, 1, 1, 0), List(0, 1, 1, 1), List(1, 0, 0, 0), List(1, 0, 0, 1), List(1, 0, 1, 0), List(1, 0, 1, 1), List(1, 1, 0, 0), List(1, 1, 0, 1), List(1, 1, 1, 0), List(1, 1, 1, 1))
val besBit = katla(dortBit)
val altiBit = katla(besBit)

def say(xxs: Tablo): List[Int] = xxs.map(_.sum) //> def say(xxs: Tablo): List[Int]

def mlog(n: Int): Int = {
    if (n < 2) 0
    else if (n == 2) 1
    else 1 + mlog(n / 2)
} //> def mlog(n: Int): Int
def say2(xxs: Tablo): (Int, Int, Int) = {
    val space = say(xxs)
    val dim = mlog(space.size)
    if (dim % 2 == 1) (dim, 0, space.size) else 
        (dim, say(xxs).filter(xs => xs == dim/2).size, space.size)
} //> def say2(xxs: Tablo): (Int, Int, Int)

say(birBit) //> val res246: List[Int] = List(0, 1)
say(ikiBit) //> val res247: List[Int] = List(0, 1, 1, 2)
say(ucBit)//> val res57: List[Int] = List(0, 1) //> val res248: List[Int] = List(0, 1, 1, 2, 1, 2, 2, 3)
say(dortBit) //> val res249: List[Int] = List(0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4)
say(besBit) //> val res250: List[Int] = List(0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4, 1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5)
say(altiBit) //> val res251: List[Int] = List(0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4, 1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5, 1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5, 2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6)
say2(ikiBit) //> val res252: (Int, Int, Int) = (2,2,4)
say2(ucBit) //> val res253: (Int, Int, Int) = (3,0,8)
say2(altiBit) //> val res254: (Int, Int, Int) = (6,20,64)

val b7 = katla(altiBit) //> val b7: List[List[Int]] = List(List(0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 1), List(0, 0, 0, 0, 0, 1, 0), List(0, 0, 0, 0, 0, 1, 1), List(0, 0, 0, 0, 1, 0, 0), List(0, 0, 0, 0, 1, 0, 1), List(0, 0, 0, 0, 1, 1, 0), List(0, 0, 0, 0, 1, 1, 1), List(0, 0, 0, 1, 0, 0, 0), List(0, 0, 0, 1, 0, 0, 1), List(0, 0, 0, 1, 0, 1, 0), List(0, 0, 0, 1, 0, 1, 1), List(0, 0, 0, 1, 1, 0, 0), List(0, 0, 0, 1, 1, 0, 1), List(0, 0, 0, 1, 1, 1, 0), List(0, 0, 0, 1, 1, 1, 1), List(0, 0, 1, 0, 0, 0, 0), List(0, 0, 1, 0, 0, 0, 1), List(0, 0, 1, 0, 0, 1, 0), List(0, 0, 1, 0, 0, 1, 1), List(0, 0, 1, 0, 1, 0, 0), List(0, 0, 1, 0, 1, 0, 1), List(0, 0, 1, 0, 1, 1, 0), List(0, 0, 1, 0, 1, 1, 1), List(0, 0, 1, 1, 0, 0, 0), List(0, 0, 1, 1, 0, 0, 1), List(0, 0, 1, 1, 0, 1, 0), List(0, 0, 1, 1, 0, 1, 1), List(0, 0,...
val b8 = katla(b7) //> val b8: List[List[Int]] = List(List(0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 1), List(0, 0, 0, 0, 0, 0, 1, 0), List(0, 0, 0, 0, 0, 0, 1, 1), List(0, 0, 0, 0, 0, 1, 0, 0), List(0, 0, 0, 0, 0, 1, 0, 1), List(0, 0, 0, 0, 0, 1, 1, 0), List(0, 0, 0, 0, 0, 1, 1, 1), List(0, 0, 0, 0, 1, 0, 0, 0), List(0, 0, 0, 0, 1, 0, 0, 1), List(0, 0, 0, 0, 1, 0, 1, 0), List(0, 0, 0, 0, 1, 0, 1, 1), List(0, 0, 0, 0, 1, 1, 0, 0), List(0, 0, 0, 0, 1, 1, 0, 1), List(0, 0, 0, 0, 1, 1, 1, 0), List(0, 0, 0, 0, 1, 1, 1, 1), List(0, 0, 0, 1, 0, 0, 0, 0), List(0, 0, 0, 1, 0, 0, 0, 1), List(0, 0, 0, 1, 0, 0, 1, 0), List(0, 0, 0, 1, 0, 0, 1, 1), List(0, 0, 0, 1, 0, 1, 0, 0), List(0, 0, 0, 1, 0, 1, 0, 1), List(0, 0, 0, 1, 0, 1, 1, 0), List(0, 0, 0, 1, 0, 1, 1, 1), List(0, 0, 0, 1, 1, 0, 0, 0), List(0, 0, 0, 1,...
val b9 = katla(b8) //> val b9: List[List[Int]] = List(List(0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 1), List(0, 0, 0, 0, 0, 0, 0, 1, 0), List(0, 0, 0, 0, 0, 0, 0, 1, 1), List(0, 0, 0, 0, 0, 0, 1, 0, 0), List(0, 0, 0, 0, 0, 0, 1, 0, 1), List(0, 0, 0, 0, 0, 0, 1, 1, 0), List(0, 0, 0, 0, 0, 0, 1, 1, 1), List(0, 0, 0, 0, 0, 1, 0, 0, 0), List(0, 0, 0, 0, 0, 1, 0, 0, 1), List(0, 0, 0, 0, 0, 1, 0, 1, 0), List(0, 0, 0, 0, 0, 1, 0, 1, 1), List(0, 0, 0, 0, 0, 1, 1, 0, 0), List(0, 0, 0, 0, 0, 1, 1, 0, 1), List(0, 0, 0, 0, 0, 1, 1, 1, 0), List(0, 0, 0, 0, 0, 1, 1, 1, 1), List(0, 0, 0, 0, 1, 0, 0, 0, 0), List(0, 0, 0, 0, 1, 0, 0, 0, 1), List(0, 0, 0, 0, 1, 0, 0, 1, 0), List(0, 0, 0, 0, 1, 0, 0, 1, 1), List(0, 0, 0, 0, 1, 0, 1, 0, 0), List(0, 0, 0, 0, 1, 0, 1, 0, 1), List(0, 0, 0, 0, 1, 0, 1, 1, 0), List(0,...
val b10 = katla(b9) //> val b10: List[List[Int]] = List(List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 1), List(0, 0, 0, 0, 0, 0, 0, 0, 1, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 1, 1), List(0, 0, 0, 0, 0, 0, 0, 1, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 1, 0, 1), List(0, 0, 0, 0, 0, 0, 0, 1, 1, 0), List(0, 0, 0, 0, 0, 0, 0, 1, 1, 1), List(0, 0, 0, 0, 0, 0, 1, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 1, 0, 0, 1), List(0, 0, 0, 0, 0, 0, 1, 0, 1, 0), List(0, 0, 0, 0, 0, 0, 1, 0, 1, 1), List(0, 0, 0, 0, 0, 0, 1, 1, 0, 0), List(0, 0, 0, 0, 0, 0, 1, 1, 0, 1), List(0, 0, 0, 0, 0, 0, 1, 1, 1, 0), List(0, 0, 0, 0, 0, 0, 1, 1, 1, 1), List(0, 0, 0, 0, 0, 1, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 1, 0, 0, 0, 1), List(0, 0, 0, 0, 0, 1, 0, 0, 1, 0), List(0, 0, 0, 0, 0, 1, 0, 0, 1, 1), List(0, 0, 0, 0, 0, 1, 0, 1, 0, 0), List(0, 0...
val hepsi = List(birBit, ikiBit, ucBit, dortBit, besBit, altiBit, b7, b8) //> val hepsi: List[List[List[Int]]] = List(List(List(0), List(1)), List(List(0, 0), List(0, 1), List(1, 0), List(1, 1)), List(List(0, 0, 0), List(0, 0, 1), List(0, 1, 0), List(0, 1, 1), List(1, 0, 0), List(1, 0, 1), List(1, 1, 0), List(1, 1, 1)), List(List(0, 0, 0, 0), List(0, 0, 0, 1), List(0, 0, 1, 0), List(0, 0, 1, 1), List(0, 1, 0, 0), List(0, 1, 0, 1), List(0, 1, 1, 0), List(0, 1, 1, 1), List(1, 0, 0, 0), List(1, 0, 0, 1), List(1, 0, 1, 0), List(1, 0, 1, 1), List(1, 1, 0, 0), List(1, 1, 0, 1), List(1, 1, 1, 0), List(1, 1, 1, 1)), List(List(0, 0, 0, 0, 0), List(0, 0, 0, 0, 1), List(0, 0, 0, 1, 0), List(0, 0, 0, 1, 1), List(0, 0, 1, 0, 0), List(0, 0, 1, 0, 1), List(0, 0, 1, 1, 0), List(0, 0, 1, 1, 1), List(0, 1, 0, 0, 0), List(0, 1, 0, 0, 1), List(0, 1, 0, 1, 0), List(0, 1, 0, 1, 1), Li...
hepsi.map(say2).foreach(println)

def katla2(n: Int): Tablo = {
    def tmp(m: Int, t: Tablo): Tablo = {
        if (m == 1) t
        else tmp(m-1, katla(t))
    }
    tmp(n, birBit)
} //> def katla2(n: Int): Tablo

val data = for(n <- 1 to 20) yield say2(katla2(n)) //> val data: IndexedSeq[(Int, Int, Int)] = Vector((1,0,2), (2,2,4), (3,0,8), (4,6,16), (5,0,32), (6,20,64), (7,0,128), (8,70,256), (9,0,512), (10,252,1024), (11,0,2048), (12,924,4096), (13,0,8192), (14,3432,16384), (15,0,32768), (16,12870,65536), (17,0,131072), (18,48620,262144), (19,0,524288), (20,184756,1048576))
data.foreach{ case (dim, equal, total) => 
    println(s"Among all $dim throws, $equal have the same number of heads and tails out of a total of $total possibilities.")
    println(s"I.e., the fat chance is ${round(equal.toDouble/total, 3)}")
    }
data.map{ case (dim, equal, total) => (dim, round(equal.toDouble/total, 3)) }.foreach(println)

/*
 (2, 0.5)
 (4, 0.375)
 (6, 0.313)
 (8, 0.273)
 (10,0.246)
 (12,0.226)
 (14,0.209)
 (16,0.196)
 (18,0.185)
 (20,0.176)
 */
