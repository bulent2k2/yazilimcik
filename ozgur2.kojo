clear
invisible
val (mx, my, mdy) = (250, 150, 100)
0 to 5 foreach (i => {
    val (x, y, dy) = (random(-mx, mx), random(-my, my), random(-mdy,mdy))
    val t = newTurtle(x, y)
    t.forward(dy)
    t.write(i, x, y, dy)
})
