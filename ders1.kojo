çıktıyıSil()
satıryaz("Merhaba Yaşar ve Reyhan. Haydi bir güneş çizelim!")
sil()

// sayıların üstünde Control tuşuyla birlikte fareye basın
// ve değeri değiştikçe çizim nasıl değisiyor görebilirsin...

val kolSayısı=19
val kolUzunluğu=336
val güzelRenk=true
val kolRengi=if(güzelRenk) Color(38, 176, 220) else sarı


görünmez()
setSpeed(superFast) // Türkçesi yakında çıkacak!

kalemRenginiKur(kolRengi)
val açı=360.0/kolSayısı
yinele(kolSayısı) {
    ileri(kolUzunluğu)
    ileri(-kolUzunluğu)
    sağ(açı)
}
