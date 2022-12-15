int f(int (*g)(...)){
    return g(g);
}
int main(){
    int x = 5;
    x = f(f);
    return y;
}
// cuz f() -> f(g())
// transfer f() to lambda expression, then we get the following code
// F(x) = λx.x(x)
// F(F) = λx.F(x)(F(x)) => λx.F(x)(F(x)) = λx.(λx.x(x))(F(x)) = λx.(λx.x(x))(λx.x(x)) = λx.x(x)(x(x))
// or we can say F(x) = λx.xx
// F(F) = λx.F(x)(F(x)) => λx.F(x)(F(x)) = λx.(λx.xx)(F(x)) = λx.(λx.xx)(λx.xx) = λx.xx(λx.xx) = λx.xx(xx)
// C allows functions to be passd as arguments. Why doesn’t C compilers create closures when processing them?
// cuz C doesn't have lambda expression, so it can't create closures
// C++ allows lambda expressions. Why doesn’t C++ compilers create closures when processing them?


// What do you think is the main reason that most languages do not include set as a built-in type constructor?
// cuz set is a container, and it's not a basic type, so it's not a good idea to include it as a built-in type constructor

// (let x 2
//               (let f (fun y (+ x y))
//                    (let x 7
//                         (+ x (@ f x)))))

// transfer to C++ code
// int x = 2;
// auto f = [](int y){return x + y;};
// x = 7;
// return x + f(x);
// What is the value of this expression if its under static scoping?
// 7 + (7 + 2) = 16
// What is the value of this expression if its under dynamic scoping?
// 7 + f(7) = 7 + 7 + 7 = 21

// class P {
//          int a;
//            P (int a) { this.a = a; }
//            void print() { System.out.println(a); }
//          }
//          class TestParam {
//            static void f(P x, P y, P z) {
//              z = x; x = y; y = z;
//            }
//            // 2 1 1
//
//            static void g(P x, P y, P z) {
//              z.a = x.a; x.a = y.a; y.a = z.a;
//            }
//            // 1 2 2
//            public static void main(String argv[]) {
//              P p1 = new P(1), p2 = new P(2), p3 = new P(3);
//              f(p1, p2, p3);
//              p1.print(); p2.print(); p3.print();
//              g(p1, p2, p3);
//              p1.print(); p2.print(); p3.print();
//            }
// }

// What does this program print under Java’s usual pass-by-value semantics?
// 1 2 3
// reason: its boxed, and its pass-by-value, so the value of p1, p2, p3 is the address of the object
// so when we call f(p1, p2, p3), the value of p1, p2, p3 is the address of the object, so the value of x, y, z is the address of the object
// so after we call f, the value of p1, p2, p3 is not changed, cuz the value of p1, p2, p3 is the address of the object, and the value of x, y, z is the address of the object
// 2 1 1
// reason: cuz we are changing the value a in these objects, so the value of p1, p2, p3 is changed

// Suppose we used a deviant version of Java that passed the parameters to f and g by reference (while otherwise obeying the usual Java semantics). What would the program print?
// 2 1 1
// reason: cuz its pass-by-reference, so the value of p1, p2, p3 is the address of the object, and the value of x, y, z is the address of the object
// 1 2 2
// reason: whatever the pass-by format is, the value of p1, p2, p3 is changed, cuz we are changing the value a in these objects

// IFF its unboxed and pass-by-value, what would the program print?

// these two swap functions are the same, they will not implement the output

// (Type Inference) Consider the following expressions written in a variant of our usual toy language syntax, including integers, booleans, and single-argument functions (constructed with fun and applied with @). The grammar of types is
// τ ::= ai |int|bool| (τ1 → τ2)
// where type variables a1, a2, a3, . . . denote arbitrary unknown types.
// Give the most polymorphic possible type for each entire expression. For example, the most polymorphic type of (fun x 42) is (a1 → int). Hint: To infer the types you can extract and solve constraints, but this is not necessary; it is probably easier to figure them out informally.
// (a) (fun x (@ x true))
// answer: (a1 → bool)
// (b) (fun x ( fun y ( let z 1 ( @ x ( @ y z)))))
// toC++:
// func(x){
//     func(y){
//         int z = 1;
//         x = y(z);

//     }
// }
// answer: (a1 → (a2 → (a3 → a1)))

not = λx.xFT
and = λxy.xyF
or = λxy.xTy
T = λxy.x
F = λxy.y

not( and a b) = λxy.xyF(λxy.xyF)(λxy.xyF) = λxy.xyF(λxy.xyF) = λxy.xyF = F
or (not a) (not b) = λxy.xTy(λxy.xyF)(λxy.xyF) = λxy.xTy(λxy.xyF) = λxy.xTy = T
For a = b = T,verify that the above two expressions are equal.
// not( and a b) = λxy.xyF(λxy.xyF)(λxy.xyF) = λxy.xyF(λxy.xyF) = λxy.xyF = F
def length(L: List[Int]): Int = L match {
    case Nil => 0
    case (x :: xs) => 1 + length(xs)
}
// convert to a tail recursive function
def length(L: List[Int]): Int = {
    def lengthHelper(L: List[Int], acc: Int): Int = L match {
        case Nil => acc
        case (x :: xs) => lengthHelper(xs, acc + 1)
    }
    lengthHelper(L, 0)
}
def f(x: Int): Int = {
    if(x > 100) {
        x - 10
    }
    else {
        f(f(x + 11))
    }
}
// remove tail recursion, replace the recursive call with a loop
def f(x: Int): Int = {
    var x = x
    while(x <= 100) {
        x = x + 11
    }
    x - 10
}
def addc(x:Int) = (y:Int) => x + y

rewirte the func to get rid of the lambda expression, but still retain the currying feature

def addc2(x:Int) = (y:Int) => x + y

def addc2(x:Int)(y:Int) = x + y
// 
def f(x:Int, g:Int => Int) = g(x) + x
// f2 become the name of the function
// use f2 as val to rename the function
val f2 = f(2, _)

def select(c: Int, n: Int, z: Int, p: Int) = {
    if(c < 0) n
    else if(c == 0) z
    else p
}
// this function does not work for all cases, it fails and raise a "divided by zero" runtime error. Explain why.
// reason: if c is 0, then the function will return z, but z is not defined in the function

// scala supports by name parameters, which are evaluated only when they are used. Rewrite the function above to use by name parameters, and explain why this works.
def f(c: Int, n: => Int, z: => Int, p: => Int) = {
    if(c < 0) n
    else if(c == 0) z
    else p
}
τ → int|bool|array(τ)
e → new t[e] // given t is an array type, e is an expression of type int that evaluates to the length of the array
e → e.len // given e is an array expression, e.len is an expression of type int that evaluates to the length of the array
e → e[e] // given e1 is an array expression and e2 is an expression of type int, e1[e2] is an expression of type τ that evaluates to the element of e1 at index e2

// find two places where the type checker fails to detect an error, and explain why
// 1.
// reason: the type of e1 is int, and the type of e2 is int, so the type of e1 + e2 is int, but the type of e1[e2] is array(τ), so the type checker fails to detect an error
// 2. 
// reason: the type of e1 is array(τ), and the type of e2 is int, so the type of e1[e2] is τ, but the type of e1 + e2 is int, so the type checker fails to detect an error

case If(c,t,e) => {
    val cv = interpE(env,c)
    cv match {
    case NumV(n) => if (n!=0) interpE(env,t) else interpE(env,e)
    case _ => throw InterpException("non-numeric condition")
    }
}

def IF(c: => Boolean, t: => Int, e: => Int) = {
    if(c) t
    else e
}