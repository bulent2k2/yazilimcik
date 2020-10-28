clearOutput()
val seq = Seq(1, 3, 7, 13, 21) //> val seq: Seq[Int] = List(1, 3, 7, 13, 21)
seq.map { n => (3*n+1)/2 } //> val res163: Seq[Int] = List(2, 5, 11, 20, 32)
seq.filter { _ > 6 }  //> val res164: Seq[Int] = List(7, 13, 21)
print("seq:"); seq.foreach { n => print(s" $n") }; println("")

var s2 = Seq.empty[Int] //> var s2: Seq[Int] = List()
s2 = s2 :+ 5 //> // mutated s2
s2 = s2 :+ 10 //> // mutated s2
s2 = 3 +: s2 //> // mutated s2
print("s2 :"); s2.foreach { n => print(s" $n") }; println("")

val ages = Map(
    "Baray" -> 9.9,
    "Kayra" -> 14.3,
    "Evren" -> 18.6
    ) //> val ages: scala.collection.immutable.Map[String,Double] = Map(Baray -> 9.9, Kayra -> 14.3, Evren -> 18.6)
//ages.foreach { println(_) }

ages("Kayra") //> val res167: Double = 14.3
ages.get("Baray") //> val res168: Option[Double] = Some(9.9)
ages.get("Reyhan") //> val res169: Option[Double] = None

var a2 = Map.empty[String, Double] //> var a2: scala.collection.immutable.Map[String,Double] = Map()
a2 = ages //> // mutated a2
a2 += ("Reyhan" -> 70.7)
a2 += ("Yasar" -> 75.10)
print("ages:"); a2.foreach{ p => print(s" $p") }
