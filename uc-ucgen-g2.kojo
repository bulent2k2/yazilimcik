// gelin üç tane üçgen çizelim
val boyu=50
sil
çıktıyıSil()
setSpeed(fast) // !!!
boyamaRenginiKur(kırmızı)
ileri(boyu)
yinele(2) {
    sol(240)
    ileri(boyu)
}

kalemRenginiKur(yeşil)
boyamaRenginiKur(yeşil)
yinele(3) {
    ileri(boyu)
    sağ(120)
}

kalemRenginiKur(mavi)
boyamaRenginiKur(mavi)
sol(60)
yinele(3){
    ileri(boyu)
    sol(120)
}
satıryaz("Beğendin mi üç renkli üç tane üçgeni?")
