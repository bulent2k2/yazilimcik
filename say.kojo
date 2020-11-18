// sequence of k objects from a pool of n objects. Can use an object multiple times. [order matters, repetition allowed]
def seqWithRep(n: Int, k: Int): Long = pow(n, k)
def pow(base: Int, exp: Int): Long = if (exp < 1) 1 else base * pow(base, exp - 1) 
// seq of k objects but can use an object only once. [order matters, repetition not allowed] (fac2 or seqWithoutRepetition below)
// by definition: n x (n-1) x ... x (n-k+2) x (n-k+1) = n! / (n-k)!  (RHS is more compact, but less efficient)
def seqUniq(n: Int, k: Int): Long = if (k < 1) 1 else if (k < 2) n else n * seqUniq(n-1, k-1)
// collection of k objects [order doesn't matter, repetition not allowed]
// by definition, binomial coefficient (n choose k)
def colUniq(n: Int, k: Int) = seqUniq(n, k) / fac(k)
def fac(n: Int): Int = if (n < 2) 1 else n * fac(n-1)

fac(15)/1e6
fac(3)
fac(4)
fac(8)

// how many ways we can order 8 girls and 7 boys in a line where no two girls come side by side:
fac(8) * fac(7)/10e6
fac(15)/10e6

// how many 4 letter words
pow(26, 4)
26 * 26 * 26 * 26


// how many 4 letter words with at least one vowel
// 26 - 5 consonants
// num of all - num of words with no vowel

pow(26, 4) - pow(21, 4)

(for (n <- 242 to 783; if n%6 == 0) yield n).size

def seqWithRepetition(n: Int, k: Int): Long = pow(n, k)
val t1 = for(k <- 1 to 5) yield (seqWithRepetition(5, k), seqWithRep(5, k))
assert(t1.filter{ case(a,b) => a!=b }.isEmpty)

// n * n-1 * n-2 ... n - k + 2 * n - k +1
def fac2(n: Int, k: Int): Long = if (k < 1) 1 else if (k < 2) n else n * fac2(n-1, k-1)
val seqWithoutRepetition = fac2 _
val t2 = for(k <- 1 to 5) yield (seqWithoutRepetition(5, k), seqUniq(5, k))
assert(t2.filter{ case(a,b) => a!=b }.isEmpty)

// collection (unordered) v sequence (ordered)
// a poker hand is a collection: choose 5 from 52

// binomial coefficients
// binomial coefficients, n choose k (without repetition and unordered):
//   (n, k) = (n - 1, k) + ( n -1, k - 1 )
// example: all poker hands (52, 5) = all poker hands that do not contain Ace of Diamonds, 1D: (51, 5) plus
//          all hands that contain 1D: (51, 4)
/* number of collections of k objects chosen without repetition from set of n objects is 
 *   ( n )        n!          ( n-1 )   ( n-1 )     ( n   )
 *   (   ) =  -----------   = (     ) + (     )  =  (     )
 *   ( k )    (n-k)! x k!     ( k   )   ( k-1 )     ( n-k )
 */
def chooseWithoutRepetition(n: Int, k: Int) = fac2(n, k) / fac(k)
val t3 = for(k <- 0 to 5) yield (chooseWithoutRepetition(5, k), colUniq(5, k))
assert(t3.filter{ case(a,b) => a!=b }.isEmpty)

val bc = chooseWithoutRepetition _
val tableOfBC = for(r <- 0 to 10; 
    c <- 0 to r) yield (r, c, bc(r, c))
val tab2 = tableOfBC.groupBy{ case (r, c, bc) => r }
tab2(3).map{ case (_, _, bc) => bc }
val l1: ((Int, Int, Long)) => Long = { case (_,_,bc) => bc }
l1((1,1,5))
tab2(9).map(l1)
tab2(10).map(l1)
println("Binomial coefficients (a+b)^r = a^r + c1xa^(r-1)xb + c2xa^(r-2)xb^2 + ... + c(r-1)xaxb^(r-1) + crxb^r")
for(r <- 0 to 9) println(s"r=$r => ${tab2(r).map(l1).mkString(",")}")

/* Binomial coefficients (c0 = 1, c1, c2, ... c(r-1), cr)
 *    (a+b)^r = a^r + c1xa^(r-1)xb + c2xa^(r-2)xb^2 + ... + c(r-1)xaxb^(r-1) + crxb^r
 * Pascal's triangle:
 *  r=1 => 1,1
 *  r=2 => 1,2,1
 *  r=3 => 1,3,3,1
 *  r=4 => 1,4,6,4,1
 *  r=5 => 1,5,10,10,5,1
 *  r=6 => 1,6,15,20,15,6,1
 *  r=7 => 1,7,21,35,35,21,7,1
 *  r=8 => 1,8,28,56,70,56,28,8,1
 *  r=9 => 1,9,36,84,126,126,84,36,9,1
 * 
 *  (1+1)^n = 
 *      2^n = c0 + c1 + ... + c(n-1) + cn
 *          = (n 0) + (n 1) + (n 2) + ... + (n n-1) + (n n) 
 *  1 + 5 + 10 + 10 + 5 + 1 = 16 + 16 = 32 = 2^5
 *
 *  Alternating sum of BC is 0:
 *      (1+(-1))^n = 0 = (n 0) - (n 1) + (n 2) + ... +/- (n n-1) -/+ (n n) 
 *      1 - 5 + 10 - 10 + 5 -1
 */

// Remember power set (set of all subsets of a set of n elements):
// For each element, is it in or not? Two choices.
// Using multiplication principle, we get 2^5
// That's also equal to choosing 0 elems plus 1 elems plus 2 plus ... 

// 4x3 grid manhattan city, home at ll (0,0) and work at ur (4,3). How many shortest paths?
bc(7, 3)
bc(7, 4)
// but stop by at (2x1)
bc(3, 2) + bc(4, 2)

// 7x5 grid:
bc(12, 5)

// multi nomial: partition n into k sets with sizes (a1, a2, ..., ak)
def multi(n: Int, a: List[Int]): Long = {
    if (a.sum != n) throw new Exception(s"a:$a does not sum up to n=$n")
    fac(n) / a.map(fac(_)).product
}
//multi(3, List(2, 2))
multi(9, List(5,4))
multi(9, List(4,3,2))

// binomial coefficients are just a special case of multinomial coefficients
def bc2(n: Int, k: Int): Long = multi(n, List(k, n-k))
assert(bc2(9, 4) == bc(9, 4))
assert(bc2(10, 2) == bc(10, 2))

// anagrams of STRANGE
fac(7) == multi(7, List(1,1,1,1,1,1,1))
// anagrams of CHEESES
multi(7, List(3,2,1,1))
// MISSISSIPPI 4 + 4 + 2 + 1
val m = List(4,4,2,1)
multi(m.sum, m)

// divide 5 into two sets of sizes 2 and 3:
val m2 = List(3,2)
multi(m2.sum, m2)

// e, k, b => divide into two non-empty sets: 
//   e, kb
//   ek, b
//   eb, k

// "TOTTER," "TURRET," "RETORT," "PEPPER" and "TSETSE."
List( List(3,1,1,1), List(2, 2, 1, 1), List(2,2,1,1), List(3,2,1), List(2,2,2)).map(m => multi(m.sum, m))
