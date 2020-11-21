// for amm only. from: ~/src/kojo/git/kojo/src/main/scala/net/kogics/kojo/lite/CoreBuiltins.scala
if (false) {
  def round(n: Number, digits: Int = 0): Double = {
    val factor = math.pow(10, digits).toDouble
    math.round(n.doubleValue * factor).toLong / factor
  }
  val Random = new java.util.Random
  def randomNormalDouble = Random.nextGaussian()
}
