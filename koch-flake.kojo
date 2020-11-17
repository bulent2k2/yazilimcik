// http://www.kogics.net/public/images/screenshots/large/18-snowflake.png

def triLine(n: Double, iter: Int) { // divide an edge into four pieces
  if (iter == 1) {
    forward(n)
  } else {
    triLine(n / 3, iter - 1)
    left(60)
    triLine(n / 3, iter - 1)
    right(120)
    triLine(n / 3, iter - 1)
    left(60)
    triLine(n / 3, iter - 1)
  }
} //> def triLine(n: Double, iter: Int): Unit

def kochFlake(n: Int, iter: Int) {
  right(30)
  repeat(3) {
    triLine(n, iter)
    right(120)
  }
} //> def kochFlake(n: Int, iter: Int): Unit

cleari()
setPenThickness(1)
setPenColor(darkGray)
setFillColor(gray)
setAnimationDelay(50)
setPosition(-150, -50)
kochFlake(300, 5)
