object HOfunc {
    // currying(f) — It takes an (Int,Int)=>Int binary function f 
    // as argument and returns a curried version of f. For example,
    // in the following code, currying(add) creates a curried version
    // of add, which can be invoked one argument at a time:
    def add(x:Int, y:Int) = x + y
    val addc = currying(add) // curried version of add
    val add3 = addc(3)         // supply one arg to addc

    def incr(x:Int) = x + 1
    val add4 = compose(incr,add3)

    def compose(f:Int=>Int, g:Int=>Int) = (x:Int) => f(g(x))
    
    // twice(f)(x) == f(f(x))
    // twice(f) — It takes an Int=>Int function f and 
    // returns a function that applies f twice to its argument:
    // implement twice using compose
    def twice(f:Int=>Int)(x:Int) = f(f(x))
    // def twice(f:(Int => Int)): (Int => Int) = (x:Int) => f(f(x))

    def currying(f:(Int,Int)=>Int) = (x:Int) => (y:Int) => f(x,y)
    // power(f,n) — It takes an Int=>Int 
    // function f and an integer n, and returns a function
    //  that applies the function f to its argument n times.

    def power(f:Int=>Int, n:Int) = (x:Int) => {
        var i = 0
        var y = x
        while (i < n) {
            y = f(y)
            i += 1
        }
        y
        // or just call twice func twice f(f(x))
    }
    def main(args: Array[String]) = {
        
        // simple unit test withou using assert
        if(add(3,4) != 7) println("add(3,4) != 7") else println("add(3,4) == 7 OK")
        if(addc(3)(4) != 7) println("addc(3)(4) != 7") else println("addc(3)(4) == 7 OK")
        if(add3(4) != 7) println("add3(4) != 7") else println("add3(4) == 7 OK")
        if(add4(4) != 8) println("add4(4) != 8") else println("add4(4) == 8 OK")
        if(twice(incr)(4) != 6) println("twice(incr)(4) != 6") else println("twice(incr)(4) == 6 OK")
        if(power(incr,3)(4) != 7) println("power(incr,3)(4) != 7") else println("power(incr,3)(4) == 7 OK")
    }
}
def addc(x:Int) = (y:Int) => x + y

rewirte the func to get rid of the lambda expression, but still retain the currying feature

def addc2(x:Int) = (y:Int) => x + y