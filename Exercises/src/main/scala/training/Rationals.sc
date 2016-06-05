object Rationals {
    class Rational(x: Int, y: Int) {
        private def gcd(a: Int, b: Int): Int = if(b == 0) a else gcd(b, a % b)

        def number = x / gcd(x, y)
        def denom = y / gcd(x, y)

        def add(that: Rational): Rational = {
            new Rational(number * that.denom + that.number * denom, denom + that.denom)
        }

        def neg: Rational = new Rational(-number, denom)

        def sub(that: Rational) = {
            add(that.neg)
        }

        def less(that: Rational) = number * that.denom < that.number * denom

        def max(that: Rational) = {
            if(this.less(that)) {
                that
            } else {
                this
            }
        }

        override def toString() = {
            number + "/" + denom
        }
    }

    val x = new Rational(1, 3)
    val y = new Rational(5, 7)
    val z = new Rational(3, 2)

    x.add(y)
    x.less(y)
    x.max(y)
}

