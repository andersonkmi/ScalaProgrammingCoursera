object Week2 {
    def product1(f: Int => Int)(a: Int, b: Int): Int = {
        if(a > b) 1
        else f(a) * product1(f)(a + 1, b)
    }

    product1(x => x * x)(3, 4)

    def factorial(n: Int) = product1(x => x)(1, n)
    factorial(5)

    def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
        if(a > b) zero
        else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
    }

    def product2(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)
    product2(x => x * x)(3, 4)
}