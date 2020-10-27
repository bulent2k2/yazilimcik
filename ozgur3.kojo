clear
invisible

val colors = Seq(red, orange, yellow, green, blue)
val c2n = Map(
    red  ->   "red   ", orange -> "orange", yellow -> "yellow", 
    green ->  "green ", blue   -> "blue  ")

val turtles = 0 until 5 map { i =>
    val t = newTurtle(i*10,0)
    t.setPenColor(colors(i))
    (t, c2n(colors(i)))
}

//setBackground(black)
turtles.take(5).foreach { case (t, c) =>
    t.act { t =>
        {
            t.setAnimationDelay(10)
            repeat(20) {
                t.forward(random(30))
                if (randomBoolean) t.right(random(90))
                else t.left(random(90))
            }
            println(s"Total area for $c is " + t.area.toInt)
            t.invisible
        }
    }
}
showAxes()
showGrid()
