// Run as:
//   amm test_f.scala
import $exec.f

for ((l, sum, max) <- List(
  (List(1,2,3,4,5), 15, 5),
  (List(5,1,99,1,2,3,4,5), 120, 99))
) {
  assert(Lists.sum(l) == sum && Lists.suM(l) == sum)
  assert(Lists.max(l) == max && Lists.maX(l) == max)
  println(s"${Lists.suM(l)} ${Lists.sum(l)} ${Lists.maX(l)} ${Lists.max(l)}")
}

println("It's all good!")
