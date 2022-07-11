dünya = başlangıç(dünya, kayGit)
repeat(forever) {
  çiz(dünya)
  dünya = (0 until KU * KU).foldLeft(Sayılar())((x, y) => x :+ yeniNesil(dünya, y))
}
// deseni kuralım
def başlangıç(v: Sayılar, desen: Dizin[(Sayı, Sayı)]) = desen.foldLeft(v)((x, y) => x.updated((y._1 + KU / 2) * KU + y._2 + KU / 2, 1))
// yeni nesli bulalım
def yeniNesil(v: Sayılar, ix: Sayı) = {
  val kural = Vector(0, 0, 0, 1, 1, 0, 0, 0, 0, 0) // oyunun kuralları
  val x = ix / KU; val y = ix % KU
  val t = (0 until 3).foldLeft(0)((st, i) => {
    st + (0 until 3).foldLeft(0)((s, j) => {
      val xt = x + i - 1; val yt = y + j - 1
      s + (if ((xt < 0) || (xt >= KU) || (yt < 0) || (yt >= KU)) 0 else v(xt * KU + yt))
    })
  })
  if (v(ix) == 1) kural(t) else { if (t == 3) 1 else 0 }
}
val yarıçap = 5 // canlı kareleri çizelim. Can mavi çember içi kırmızı daire. Yarıçapı 5
def çiz(v: Sayılar) = for (i <- 0 until KU * KU)
  if (v(i) == 1) circle(
    (i / KU) * 2 * yarıçap - KU * yarıçap,
    (i % KU) * 2 * yarıçap - KU * yarıçap, yarıçap)
// Meşhur olmuş desenlerden birkaçı
def esaslı = Dizin((0, 1), (1, 0), (1, 1), (1, 2), (2, 2)) // orijinal adı: fpent
def dokuzcanlı = Dizin((0, 1), (1, 0), (1, 1), (5, 0), (6, 0), (7, 0), (6, 2)) // diehard -- İki küçücük grup var ve kolay kolay ölmüyor
def tohum = Dizin((0, 0), (1, 0), (1, 2), (3, 1), (4, 0), (5, 0), (6, 0))
// glider adlı meşhur üretken desen
def kayGit = Dizin((-18, 3), (-18, 4), (-17, 3), (-17, 4), (-8, 2), (-8, 3), (-8, 4), (-7, 1), (-7, 5),
  (-6, 0), (-6, 6), (-5, 0), (-5, 6), (-4, 3), (-3, 1), (-3, 5), (-2, 2), (-2, 3), (-2, 4),
  (-1, 3), (2, 4), (2, 5), (2, 6), (3, 4), (3, 5), (3, 6), (4, 3), (4, 7),
  (6, 2), (6, 3), (6, 7), (6, 8), (16, 5), (16, 6), (17, 5), (17, 6))
def blok1 = Dizin((0, 0), (2, 0), (2, 1), (4, 2), (4, 3), (4, 4), (6, 3), (6, 4), (6, 5), (7, 4))
def blok2 = Dizin((0, 0), (0, 3), (0, 4), (1, 1), (1, 4), (2, 0), (2, 1), (2, 4), (3, 2), (4, 0),
  (4, 1), (4, 2), (4, 4))
def küçücük = Dizin((-18, 0), (-17, 0), (-16, 0), (-15, 0), (-14, 0), (-13, 0), (-12, 0), (-11, 0), (-9, 0), (-8, 0),
  (-7, 0), (-6, 0), (-5, 0), (-1, 0), (0, 0), (1, 0), (8, 0), (9, 0), (10, 0),
  (11, 0), (12, 0), (13, 0), (14, 0), (16, 0), (17, 0), (18, 0), (19, 0), (20, 0))
def üçlüler = Dizin((0, 2), (0, 3), (0, 4), (0, -2), (0, -3), (0, -4),
  (-2, 0), (-3, 0), (-4, 0), (2, 0), (3, 0), (4, 0))
// üçlülerden dikey olanları bağlayalım
def ü2a = Dizin((0, 0), (0, 1), (0, -1)) ++ üçlüler
// öbür türlü, yani yatay olanları bağlayalım
def ü2b = Dizin((0, 0), (1, 0), (-1, 0)) ++ üçlüler
def dörtlü = Dizin((0, 0), (1, 0), (-1, 0), (0, 2)) // dokuzcanlı'nın altkümesi
// sepet sepet yumurta
// sakın beni unutma
// şimdilik bu kadar
// yaşamın tadını çıkar
