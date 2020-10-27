çıktıyıSil()
satıryaz("Haydi bir güneş çizelim!")

// sayıların üstünde Control tuşuyla birlikte fareye basın
// ve değeri değiştikçe çizim nasıl değisiyor görebilirsin...

val kolSayısı=95
val kolUzunluğu=336
val güzelRenk=false
val kolRengi=if(güzelRenk) Color(255, 208, 0) else sarı

sil()
görünmez()
setSpeed(superFast) // Türkçesi yakında çıkacak!

kalemRenginiKur(kolRengi)
val açı=360.0/kolSayısı
yinele(kolSayısı) {
    ileri(kolUzunluğu)
    ileri(-kolUzunluğu)
    sağ(açı)
}
