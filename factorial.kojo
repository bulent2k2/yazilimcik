def fac(n: Int): Int = {
    if (n < 2) 1
    else n * fac(n-1)
} //> def fac(n: Int): Int
fac(15)/1e6 //> val res308: Double = 2004.310016
fac(3) //> val res309: Int = 6
fac(4) //> val res310: Int = 24
fac(8) //> val res311: Int = 40320

// how many ways we can order 8 girls and 7 boys in a line where no two girls come side by side:
fac(8) * fac(7)/10e6 //> val res312: Double = 20.32128
fac(15)/10e6 //> val res313: Double = 200.4310016

def pow(base: Int, exp: Int): Long = {
    if (exp < 1) 1
    else base * pow(base, exp - 1) 
} //> def pow(base: Int, exp: Int): Long
pow(26, 4) //> val res314: Long = 456976
26 * 26 * 26 * 26 //> val res315: Int = 456976
