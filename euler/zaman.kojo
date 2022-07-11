// evren's desktop has better version :-)
type S = İriSayı
def fib1(n: Int): S = if (n < 2) n else fib1(n - 1) + fib1(n - 2)

val s1 = buSaniye
val a1 = buAn
// CAREFUL! fib1 is slow!!! 33 is ok (0.5 sec). 50 is too long...
// repeated runs go down to 200ms
// 37 -> 1.3s
// 38 2s
// 39 3s
// 40 5s
// 41 8s
// 42 13s
// 43 22s
// 44 35s
// 45 1 minute!
// 46 92s
// 47 145s
// çıktıyıSil
val n = 38
satıryaz(s"Başlıyoruz. $n! hesabı ne kadar zaman alacak? ...")
val qed = (for(i <- 1 to n) yield(fib1(i))).filter(_ % 2 == 0).sum
val s2 = buSaniye
val a2 = buAn
satıryaz(a2 + " " + s2)
satıryaz(a2-a1 + " " + round(s2-s1, 3))
