
clearOutput

val l1 = List("a", "b", "c", "d") //> val l1: List[String] = List(a, b, c, d)

l1.zip(0 until l1.size).foreach{ case (a, b) => println(a, b) }
println("")

for ( (a, b) <- l1.zip(0 until l1.size)) println(a, b)
println("")

val pairs = for ( (a, b) <- l1.zip(0 until l1.size)) yield(a, b) //> val pairs: List[(String, Int)] = List((a,0), (b,1), (c,2), (d,3))
println(pairs)
