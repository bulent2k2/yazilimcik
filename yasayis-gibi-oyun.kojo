import Staging._ // Staging birimindeki komutları kullanmak için çağıralım.
// Farklı birimlerin bazı benzer komutları oluyor ve aynı adı kullanıyorlar.
// Onun için hangisini kullanmak istiyoruz açık açık belirtmemiz gerek:
import Staging.{ animate, circle, clear, setFillColor, wipe }

çıktıyıSil; clear(); gridOn(); axesOn(); setFillColor(mavi)

// bu yazılımcıkta hızıKur gibi kaplumba komutları bir işe yaramıyor,
// çünkü çizimleri yapan kaplumba değil Staging biriminin komutları

// bu oyunun dünyası yani tahtası büyük bir kare. Kenarı KU uzunluğunda olsun
// Nasıl satranç tahtası 8x8, bu tahta 128x128 kare.
val KU = 128
// karenın kenarı kaplumbanın on adımına denk

// ilk önce, bütün kareler cansız olmalı
var v = (0 until KU * KU).foldLeft(Vector[Sayı]())((x, y) => x :+ 0)
satıryaz(s"Dünyamızda $KU'in karesi yani ${v.size} tane hane var.")
yaz(s"Ekranımız ${(canvasBounds.width / 10).toInt} kare eninde ")
satıryaz(s"ve ${(canvasBounds.height / 10).toInt} kare boyunda.")

val gösterVeDur = /**/ yanlış // doğru    // deseni göster ve dur
val yavaşÇalış  = /**/ yanlış // doğru
val nesil       = 2000                    // verilen nesle gelince dur
val dur         = /* yanlış */ doğru
val oran        = 40                      // canlandırmayı yavaşlatma oranı
// Canlandırma komutu (adı animate) bir saniyede 40 kere çalıştırılıyor
// Bir saniyede bir nesil ilerlemek için oran 40 olmalı.
// Bir saniyede 10 nesil ilerlemek için oran 4 olmalı.

// deseni seçelim:
val seç = 3
// block1 ve block2 bir kaç füze yolluyor ve sonra 1000. nesil civarı gibi duruyor.
val (desen, adı) = seç match {
    case 0 => (üçlüler, "üçlüler")
    case 1 => (glider,  "glider")  /* makineli tüfek gibi */
    case 2 => (fpent,   "fpent")   /* Yaklaşık 1000 nesil canlı sonra peryodik */
    case 3 => (diehard, "diehard") /* 130 nesil sonra can kalmiyor */
    case 4 => (block1,  "block1")
    case 5 => (block2,  "block2")
    case 6 => (tiny,    "tiny")
    case 7 => (ü2a,     "ü2a")
    case 8 => (ü2b,     "ü2b")
    case 9 => (dörtlü,  "dörtlü")
}

v = başlangıç(v, desen)

yaz(s"$seç. $adı adlı desende ${desen.size} tane canlı kare var.\nNesilleri sayalım: ")

var t = 0
val z0 = epochTime // şimdiki zamanı (geçmişte bir ana göre) anımsayalım
animate {
    if (t % oran == 0) {
        wipe() // sil
        çiz(v)
        v = (0 until KU * KU).foldLeft(Vector[Sayı]())((x, y) => x :+ yeniNesil(v, y))
        yaz(s"${t / oran +1} ")
        if (t / oran > 30 && yavaşÇalış) {
            pause(0.5) // bekle
        }
        if (gösterVeDur) stopAnimation // durdur ve çık
    }
    t += 1
  if (dur && t / oran == nesil) {
    val z1 = epochTime - z0
    satıryaz(s"\n${round(z1,2)} saniye geçti.")
    stopAnimation()
  }

}


// deseni kuralım
def başlangıç(v: Vector[Sayı], desen: List[(Sayı, Sayı)]) = desen.foldLeft(v)((x, y) => x.updated((y._1 + KU / 2) * KU + y._2 + KU / 2, 1))

// yeni nesli bulalım
def yeniNesil(v: Vector[Sayı], ix: Sayı) = {
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
// canlı kareleri çizelim. Can mavi çember içi kırmızı daire. Yarıçapı 5
val yarıçap = 5
def çiz(v: Vector[Sayı]) = for (i <- 0 until KU * KU)
    if (v(i) == 1) circle(
        (i / KU) * 2 * yarıçap - KU * yarıçap,
        (i % KU) * 2 * yarıçap - KU * yarıçap, yarıçap)

// Meşhur olmuş desenlerden birkaçı
def fpent = List((0, 1), (1, 0), (1, 1), (1, 2), (2, 2))
// Two clusters: right triangle with corner on upper right and its right neigbor with horiz triple with a center hat above
def diehard = List((0, 1), (1, 0), (1, 1), (5, 0), (6, 0), (7, 0), (6, 2))
def acorn = List((0, 0), (1, 0), (1, 2), (3, 1), (4, 0), (5, 0), (6, 0))
def glider = List((-18, 3), (-18, 4), (-17, 3), (-17, 4), (-8, 2), (-8, 3), (-8, 4), (-7, 1), (-7, 5),
    (-6, 0), (-6, 6), (-5, 0), (-5, 6), (-4, 3), (-3, 1), (-3, 5), (-2, 2), (-2, 3), (-2, 4),
    (-1, 3), (2, 4), (2, 5), (2, 6), (3, 4), (3, 5), (3, 6), (4, 3), (4, 7),
    (6, 2), (6, 3), (6, 7), (6, 8), (16, 5), (16, 6), (17, 5), (17, 6))
def block1 = List((0, 0), (2, 0), (2, 1), (4, 2), (4, 3), (4, 4), (6, 3), (6, 4), (6, 5), (7, 4))
def block2 = List((0, 0), (0, 3), (0, 4), (1, 1), (1, 4), (2, 0), (2, 1), (2, 4), (3, 2), (4, 0),
    (4, 1), (4, 2), (4, 4))
def tiny = List((-18, 0), (-17, 0), (-16, 0), (-15, 0), (-14, 0), (-13, 0), (-12, 0), (-11, 0), (-9, 0), (-8, 0),
    (-7, 0), (-6, 0), (-5, 0), (-1, 0), (0, 0), (1, 0), (8, 0), (9, 0), (10, 0),
    (11, 0), (12, 0), (13, 0), (14, 0), (16, 0), (17, 0), (18, 0), (19, 0), (20, 0))
def üçlüler = List((0, 2), (0, 3), (0, 4), (0, -2), (0, -3), (0, -4),
    (-2, 0), (-3, 0), (-4, 0), (2, 0), (3, 0), (4, 0))
// üçlülerden dikey olanları bağlayalım:
def ü2a = List((0, 0), (0, 1), (0, -1)) ++ üçlüler
// öbür türlü, yani yatay olanları bağlayalım
def ü2b = List((0, 0), (1, 0), (-1, 0)) ++ üçlüler
def dörtlü = List((0, 0), (1, 0), (-1, 0), (0, 2))  // diehard'in altkümesi