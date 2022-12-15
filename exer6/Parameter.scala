case class P(var a:Int)
object Scope {
    def twi(var x:P, var y:P) = {
        var temp = x
        x = y
        y = temp
    }
    def swi(var x:P, var y:P) = {
        var temp = x.a
        x.a = y.a
        y.a = temp
    }
    def main(args: Array[String]) = {
        var P0 = P(0)
        var P1 = P(1)
        twi(P0, P1)
        println(P0.a + " " + P1.a)
        swi(P0, P1)
        println(P0.a + " " + P1.a)
    }
}