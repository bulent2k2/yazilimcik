def round(n: Number, digits: Int = 0): Double = {
  val factor = math.pow(10, digits).toDouble
  math.round(n.doubleValue * factor).toLong / factor
}
val Random = new java.util.Random
def randomNormalDouble = Random.nextGaussian()

def pow(b: Int, e: Int): Int = if (e < 1) 1 else b * pow(b, e - 1)

/*
 * we sort the samples and then divide them into equal sized buckets
 */

// 17k samples per bucket hits stack depth (in recursive span)
// More than 1M samples! (:-)
val bucketCount = 32   // only a target
def scale(x: Double): Int = (100*x).toInt  // multiply the samples by this to get an integer
println(s"We target $bucketCount buckets (N).")
println("=== =========== ==== ===== ==== === =========== === ===")
println("Exp Num Samples  Min  Ave   Max  N    Widest    Length ")
println("                                      Bucket    Min Max")
println("=== =========== ==== ===== ==== === =========== === ===")
for (e <- 5 to 20) {
  val sampleSize  = pow(2, e) // this many samples from a normal distribution
  val maxBucketSize = sampleSize / bucketCount

  // sample Gaussian distribution:
  val norm = for (i <- 1 to sampleSize) yield (randomNormalDouble)
  def r(x: Double, numD: Int = 3) = round(x, numD)
  val (min, ave, max) = (scale(norm.min), r(norm.sum/norm.size, 2), scale(norm.max))
  // println(s"Number of samples: ${norm.size} average: ${r(norm.sum / norm.size)} min: $min max: $max")

  type Vec = IndexedSeq[Int]
  def divide(xs: Vec, min: Int = maxBucketSize): List[Vec] = {
    def recurse(ys: Vec): List[Vec] = if (ys.size <= min) List(ys) else {
      val half = ys.size / 2
      recurse(ys.take(half)) ++ recurse(ys.drop(half))
    }
    recurse(xs.sorted)
  }
  // todo: tailrecursion! Even better, xs is sorted, just pick first and last elements
  def span(xs: Vec): (Int, Int) =
    if (xs.isEmpty) throw new Exception("Empty collection has no span")
    else {
      val head = xs.head
      if (xs.tail.isEmpty) (head, head) else {
        (head, xs.last)
        /*
         val tmp = span(xs.tail)
         if (head < tmp._1) (head, tmp._2)
         else if (head > tmp._2) (tmp._1, head)
         else tmp
         */
      }
    }

  val buckets = divide(norm.map(x => scale(x)))
  val (fst, lst) = (buckets.head, buckets.last)
  val spans = buckets.map( b => span(b) )
  val lengths = spans.map( s => s._2 - s._1 )
  val (minSpan, maxSpan) = (lengths.min, lengths.max)
  val (span1, spanL) = (span(fst), span(lst))
  val spanWidest = if (span1._2 - span1._1 > spanL._2 - spanL._1) span1 else spanL
  println(f"$e%3d $sampleSize%11d $min%4d $ave%5.2f $max%4d ${buckets.size}%3d $spanWidest%11s $minSpan%3d $maxSpan%3d")
}
/*
 var b = 0
 dist.foreach { xs =>
 val s = span(xs)
 val len = s._2 - s._1
 println(f"$b%3d length: $len%4d span: $s%15s ${xs.size}")
 b += 1
 }
 */

