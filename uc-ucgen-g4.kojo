// primary colors of light (RGB)
// and their pairwise additions (Yellow Cyan and Magenta)

type Renk = Color //> type Renk
type İkil = Boolean //> type İkil

def üçgen(boy: Sayı, renk: Renk) = {
    kalemRenginiKur(renk)
    boyamaRenginiKur(renk)
    yinele(3) {
        ileri(boy)
        sağ(120)
    }
    sol(120)
} //> def üçgen(boy: TurkishAPI.Sayı, renk: Renk): Unit

val boyu: Sayı = 100 //> val boyu: TurkishAPI.Sayı = 100

sil
çıktıyıSil()

setSpeed(fast) // !!!

üçgen(boyu, mavi)
üçgen(boyu, yeşil)
üçgen(boyu, kırmızı)
sağ(60)
üçgen(boyu, magenta)  // !!
üçgen(boyu, cyan)  // !!
üçgen(boyu, sarı)

görünmez()
satıryaz("Günün aydın olsun!")
