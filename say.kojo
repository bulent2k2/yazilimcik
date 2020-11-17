def fac(n: Int): Int = if (n < 2) 1 else n * fac(n-1)

fac(15)/1e6
fac(3)
fac(4)
fac(8)

// how many ways we can order 8 girls and 7 boys in a line where no two girls come side by side:
fac(8) * fac(7)/10e6
fac(15)/10e6

// how many 4 letter words

def pow(base: Int, exp: Int): Long = {
    if (exp < 1) 1
    else base * pow(base, exp - 1) 
}
pow(26, 4)
26 * 26 * 26 * 26


// how many 4 letter words with at least one vowel
// 26 - 5 consonants
// num of all - num of words with no vowel

pow(26, 4) - pow(21, 4)

// collection (unordered) v sequence (ordered)
// a poker hand is a collection: choose 5 from 52

(for (n <- 242 to 783; if n%6 == 0) yield n).size

def seqWithRepetition(n: Int, k: Int): Long = pow(n, k)
for(k <- 1 to 5) yield seqWithRepetition(5, k)

// n * n-1 * n-2 ... n - k + 2 * n - k +1
def fac2(n: Int, k: Int): Long = if (k < 1) 1 else if (k < 2) n else n * fac2(n-1, k-1)
val seqWithoutRepetition = fac2 _
for(k <- 1 to 5) yield seqWithoutRepetition(5, k)

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
for(k <- 0 to 5) yield chooseWithoutRepetition(5, k)
val bc = chooseWithoutRepetition _
val tableOfBC = for(r <- 0 to 10; 
    c <- 0 to r) yield (r, c, bc(r, c))
val t2 = tableOfBC.groupBy{ case (r, c, bc) => r }
t2(3).map{ case (_, _, bc) => bc }
val l1: ((Int, Int, Long)) => Long = { case (_,_,bc) => bc }
l1((1,1,5))
t2(9).map(l1)
t2(10).map(l1)
println("Binomial coefficients (a+b)^r = a^r + c1xa^(r-1)xb + c2xa^(r-2)xb^2 + ... + c(r-1)xaxb^(r-1) + crxb^r")
for(r <- 0 to 9) println(s"r=$r => ${t2(r).map(l1).mkString(",")}")

/* Binomial coefficients (c0 = 1, c1, c2, ... c(r-1), cr)
 *    (a+b)^r = a^r + c1xa^(r-1)xb + c2xa^(r-2)xb^2 + ... + c(r-1)xaxb^(r-1) + crxb^r
 * Pascal's triangle:
 *  r=0 => 1
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
 * 
 *  1 + 5 + 10 + 10 + 5 + 1 = 16 + 16 = 32 = 2^5
 */

// Remember power set (set of all subsets of a set of n elements):
// For each element, is it in or not? Two choices.
// Using multiplication principle, we get 2^5
// That's also equal to choosing 0 elems plus 1 elems plus 2 plus ... 

// Note: (+1 + -1)^n = 0 = (n 0) - (n 1) + (n 2) - ... +/- (n n-1) -/+ (n n)
// Alternating sum of binomial coefficients is 0. 
