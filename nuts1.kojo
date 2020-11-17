
// NUTS, non-uniform track sharing... This is the third key engine
// Let's start with a simple but informative example:

clear
clearOutput
invisible

type Number = Long // in many cases can be short
type Coord = Number // in general needs to be long
/* Premature opt:
type NumMaj = Number
type NumMin = Short */

trait Orientation {
    def horiz: Boolean
}
case object Horizontal extends Orientation {
    def horiz = true
}
case object Vertical extends Orientation {
    def horiz = false
}

// bitCount is logical first and then manifests as physical
// bitSize is physical but is a small size..
case class Segment(val start: Coord, val end: Coord, val bitCount: Number = 1, val bitSize: Number = 1)
case class Rectangle(val s: Segment, val spacing: Number = 0) { // orthogonal (line-end) spacing??
    def width = (s.bitSize * s.bitCount) + (spacing * (s.bitCount - 1))
}
case class Shape(val r: Rectangle, val o: Orientation, val placement: Number) {
    def xl = if (o.horiz) r.s.start else placement
    def yl = if (o.horiz) placement else r.s.start
    def length = r.s.end - r.s.start
    def xh = if (o.horiz) xl + length else xl + r.width
    def yh = if (o.horiz) yl + r.width else yl + length
    def dx = xh - xl
    def dy = yh - yl
    override def toString() = s"[($xl $yl) ($xh $yh)]"
}

// if solved, we simply use the existing placement (no randomization)
// spread is used to separate the track placement for random solution
// to help accommodate thick segments
case class Problem(val shapes: List[Shape], solved: Boolean, spread: Int = 2) {
    var solution = Seq.empty[Shape]
    val r = scala.util.Random
    val shuffledIndices = r.shuffle((0 until shapes.size).toList)
    def solve() = { /* do the track sharing == 1.5D placement */
        shapes.zip(shuffledIndices).map {
            case (s, i) =>
                {
                    val placement: Number = 
                        if (solved) s.placement
                        else spread * i // random(4 * shapes.size)
                    val shape =
                        if (s.o == Horizontal)
                            Shape(s.r, Horizontal, placement)
                        else Shape(s.r, Vertical, placement)
                    solution = solution :+ shape
                }
        }
    }
    private def mdraw(xl: Number, yl: Number, dx: Number, dy: Number) = {
        val (sx, sy) = (scalex, scaley)
        val x = sx * xl
        val y = sy * yl
        val h = sy * dy
        val w = sx * dx
        println(s" Drawing: [($x $y) (${x + w} ${y + h})]")
        draw(fillColor(white) * penColor(gray) * trans(x + shiftx, y + shifty) -> PicShape.rect(h, w))
    }

    def show() =
        if (!solution.isEmpty)
            for (s <- solution) {
                print(s"Shape: $s ")
                mdraw(s.xl, s.yl, s.dx, s.dy)
            }
}

// simple interval graph. (xl, xh)
val test1 = List((0, 10), (10, 20), (0, 5), (5, 15), (15, 20))
val shapes1 = for (
    r <- (
        for (
            s <- (for ((a, b) <- test1)
                yield Segment(a, b))
        ) yield Rectangle(s))
) yield Shape(r, Horizontal, 0)

// filled spiral. (xl, xh, width)
val test2 = List((0, 20, 10), (0, 10, 20), (10, 30, 10), (20, 30, 20), (10, 20, 10))
val shapes2 = for (
    r <- (
        for (
            s <- (
                for ((a, b, c) <- test2)
                    yield Segment(a, b, c))
        ) yield Rectangle(s))
) yield Shape(r, Horizontal, 0)

// test2 with placement (xl, xh, w, yl)
val test3 = List((0, 20, 9, 0), (0, 9, 20, 10), (10, 30, 10, 20), (20, 30, 19, 0), (10, 19, 9, 10))
val shapes3 = for ((a, b, c, d) <- test3) yield ({
    val s = Segment(a, b, c)
    val r = Rectangle(s)
    Shape(r, Horizontal, d)
})

val switch = 2
val prob = switch match {
    case 1 => Problem(shapes1, false)
    case 2 => Problem(shapes2, false, 10)
    case 3 => Problem(shapes3, true)
}

val shiftx = -345
val shifty = -245
def scalex = 9
def scaley = 9
println(s"Shifting origin to ($shiftx $shifty)")
setPosition(shiftx, shifty)
write("0,0")
println("NUTS 1")
println(s"${prob.shapes.size} shapes")
prob.solve()
prob.show()

// problem1 += Segment(0, 10)
