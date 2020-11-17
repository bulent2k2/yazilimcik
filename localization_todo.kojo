//sil
çıktıyıSil()

// !!!
val t = newTurtle; t.setSpeed(hızlı); t.arc(150, 270)
hızıKur(orta)

// ? new Kamplumbağa
def üçgen(boy: Sayı, renk: Renk): Birim = {
    kalemRenginiKur(renk)
    boyamaRenginiKur(renk)
    yinele(3) {
        ileri(boy)
        sağ(120)
    }
    sol(120)
}

val boyu: Sayı = 220



// assert(yanlış, "hata var gibi")  // TODO !!!
assert(yanlış == !doğru)  // TODO!!!

val renk = if (doğru) mavi else Renk(20, 20, 20, 100)
üçgen(boyu, renk)
üçgen(boyu, yeşil)
üçgen(boyu, kırmızı)
sağ(60)
üçgen(boyu, morumsu)
üçgen(boyu, camgöbeği)
üçgen(boyu, sarı)

görünmez()
satıryaz("Günün aydın olsun!")
kalemRenginiKur(koyuGri)
