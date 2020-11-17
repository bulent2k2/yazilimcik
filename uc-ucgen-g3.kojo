// gelin üç tane üçgen çizelim

def üçgen(boy: Sayı, renk: Color) = {
    kalemRenginiKur(renk)
    boyamaRenginiKur(renk)
    yinele(3) {
        ileri(boy)
        sağ(120)
    }
    sol(120)
}

val boyu: Sayı = 100
sil
çıktıyıSil()

setSpeed(fast) // !!!

üçgen(boyu, kırmızı)
üçgen(boyu, yeşil)
üçgen(boyu, mavi)

görünmez()
satıryaz("Beğendin mi üç renkli üç tane üçgen çizen yazılımcığı?")

// aralardaki boşluğu dolduran üç tane daha üçgen çizebilir misin?
// İpucu: yeni bir tanıma (def) gerek yok!

