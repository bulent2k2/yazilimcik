clearOutput()
val seq = Seq(1, 3, 7, 13, 21)
seq.map { n => (3*n+1)/2 }
seq.filter { _ > 6 } 
print("seq:"); seq.foreach { n => print(s" $n") }; println("")

var s2 = Seq.empty[Int]
s2 = s2 :+ 5
s2 = s2 :+ 10
s2 = 3 +: s2
print("s2 :"); s2.foreach { n => print(s" $n") }; println("")

val ages = Map(
    "Baray" -> 9.9,
    "Kayra" -> 14.3,
    "Evren" -> 18.6
    )
//ages.foreach { println(_) }

ages("Kayra")
ages.get("Baray")
ages.get("Reyhan")

var a2 = Map.empty[String, Double]
a2 = ages
a2 += ("Reyhan" -> 70.7)
a2 += ("Yasar" -> 75.10)
print("ages:"); a2.foreach{ p => print(s" $p") }; println("")

val colors = Set(blue, red, green)
colors.contains(blue)
colors.contains(yellow)

var c2 = Set.empty[Color]
c2 = colors
c2 += yellow
c2.contains(yellow)
c2.intersect(Set(red, green)).contains(red)
 
import collection.mutable.{Queue, Stack}

val stack = Stack.empty[Int]
stack.push(3)
stack.top
stack.push(5)
stack.top
val selem = stack.pop
stack

val q = Queue.empty[Int]
q.enqueue(1)
q.front
q.enqueue(3)
q.front
val qelem = q.dequeue
q
