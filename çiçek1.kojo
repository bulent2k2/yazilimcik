def yaprak(boy: Sayı, açı: Sayı = 90) {
    konumVeYönüBelleğeYaz()
    //sağ(45)  // ilk yaprak dik olsun istersen başta sağa dön
    yay(boy, açı)
    sol(açı)
    yay(boy, açı)
    konumVeYönüGeriYükle()
}
def renkli(boy: Sayı, dönüşAçısı: Sayı) {
    boyamaRenginiKur(randomColor.fadeOut(0.3))
    yaprak(boy)
    sağ(dönüşAçısı) // bir sonraki yaprağı çizmek için kaplumbağacık dönsün biraz
}
sil
hızıKur(hızlı)
kalemRenginiKur(renksiz)
val tane = 18 // yaprak sayısı
yinele(tane)(renkli(200, 360/tane))
