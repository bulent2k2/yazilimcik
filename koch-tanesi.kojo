// ingilizcesi için bakın:
//   http://www.kogics.net/public/images/screenshots/large/18-snowflake.png

// üçgenin bir kenarını çizelim. Ama son adıma gelmemişsek
// dörde bölerek ortasına yeni küçük bir üçgen çizelim
// Not. Bu bir özyineleme örneği. Kendi kendini çağıran bir
// işlev. Özedönen işlev mi desek?
def küçükÜçgenÇiz(kenarUzunluğu: Kesir, kaçKere: Sayı) {
  // durmayı bilmezsek sonsuza kadar gider ve ilk kenarın
  // en başında kalırız!
  if (kaçKere == 1) {
    ileri(kenarUzunluğu)
  } else {
    val birEksik   = kaçKere - 1
    val küçükKenar = kenarUzunluğu / 3
    küçükÜçgenÇiz(küçükKenar, birEksik)
    sol(60)
    küçükÜçgenÇiz(küçükKenar, birEksik)
    sağ(120)
    küçükÜçgenÇiz(küçükKenar, birEksik)
    sol(60)
    küçükÜçgenÇiz(küçükKenar, birEksik)
  }
}

def kochTanesi(kenarUzunluğu: Sayı, kaçKere: Sayı) {
  sağ(30)
  yinele(3) {
    küçükÜçgenÇiz(kenarUzunluğu, kaçKere)
    sağ(120)
  }
}

type Konum = (Kesir, Kesir)

def ayarla(yer: Konum, duraklamaSüresi: Sayı = 50) = {
  çizimiSil
  kalemKalınlığınıKur(1)
  kalemRenginiKur(darkGray)
  boyamaRenginiKur(gray)
  canlandırmaHızınıKur(duraklamaSüresi)
  atla(-yer._1, -yer._2)
}

// kaplumbacık merkezin biraz sağ (150 adım) ve biraz aşağısından (50 adım)
// başlasın. O sayede kartanesi ekranın tam ortasını bulacak.
// Bir de çizim hızını arttırmak için adımlar arasındaki duraklamanın
// süresini 20 milisaniyeye düşürelim. Yoksa 100 adım 1 saniye alır,
// yavaş olur.
ayarla((150, 50), 272)
// İlk üçgenin kenar uzunluğu 300 olsun.
// 4 kere tekrarlayalım üçgen doğurmayı. ff
kochTanesi(300, 4)
