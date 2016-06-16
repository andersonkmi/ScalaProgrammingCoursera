package training

/**
  * Created by andersonkmi on 6/15/2016.
  */
object Week4 {

    abstract class Nat {
        def isZero: Boolean
        def predecessor: Nat
        def successor = new Succ(this)
        def +(that: Nat): Nat
        def -(that: Nat): Nat
    }

    object Zero extends Nat {
        def isZero: Boolean = true
        def predecessor: Nat = throw new Error("Invalid number")
        def +(that: Nat) = that
        def -(that: Nat) = if(that.isZero) this else throw new Error("Negative number")
    }

    class Succ(n: Nat) extends Nat {
        def isZero = false
        def predecessor = n
        def +(that: Nat) = new Succ(n + that)
        def -(that: Nat) = if(that.isZero) this else n - that.predecessor
    }
}
