// The last lessons (iterated games == normal distribution)

// See usage in: ~/src/yaz/yazilimcik/saySapSade.kojo

/* Fair coin toss, count heads (payoff for heads is 1 and tails is 0)
 *  ev = 1/2
 *  var = 1/4  (sigma = 1/2)
 *  ev(G(n)) = n/2
 *  var(G(n)) = n/4
 *  G(n)norm = [G(n) - n/2]/sqrt(n/4)
 */

// G(10k) -> Prob(>5200 heads)?
// G(10k)norm = [G(10k) - 5k]/sqrt(2500)
// 5200 -> (5200 - 5k)/sqrt(2500) = 200/50 = 4

