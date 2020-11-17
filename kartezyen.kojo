sil; gridOn(); axesOn(); hızıKur(orta)

def işlev (x: Kesir)= 0.001*x*x+0.5*x+10
def işlev2(x: Kesir)= 2000/x
val aralık=200
atla(-aralık, işlev(-aralık))

for(x <- -aralık+1 to aralık; if (x != 0 && x % 10 == 0))
  lineTo(x, işlev(x))
