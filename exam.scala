def select(c: Int, n: Int, z: Int, p: Int): Int = {
    if(c < 0) n
    else if(c == 0) z
    else p
}

// rewirte the expression to n : => Int

def select(c: Int, n: => Int, z: => Int, p: => Int): Int = {
    if(c < 0) n
    else if(c == 0) z
    else p
}

def f(x: Int, g :Int => Int) = g(x) + x
as curried function,
anymous function using lambda expression. Provide a function that takes a function as an argument and returns a function as a result.
val f2 = f(2, _)