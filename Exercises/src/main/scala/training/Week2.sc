
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

    // Average damping
    import math._
    val tolerance = 0.0001

    def isCloseEnough(x: Double, y: Double) = {
        abs((x - y) / x) / x < tolerance
    }

    def fixedPoint(f: Double => Double)(firstGuess: Double): Double = {
        def iterate(guess: Double): Double = {
            val next = f(guess)
            if(isCloseEnough(guess, next)) next
            else iterate(next)
        }
        iterate(firstGuess)
    }

    def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

    def sqrt(x: Double) = fixedPoint(averageDamp(y => x / y))(1)

    sqrt(2)
}

