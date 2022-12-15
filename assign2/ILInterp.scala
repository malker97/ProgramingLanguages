// ImpLang Interpreter
//
// Usage: linux> scala ILInterp <source file>
//
import ImpLang._

object ILInterp {
  case class InterpException(string: String) extends RuntimeException

  def interp(e:Expr, debug:Int = 0): Int = {
    val store = collection.mutable.Map[String,Int]()

    def interpE(e:Expr): Int = {
      if (debug > 1) {
        println("  expr = " + e);
        println("  store = " + store)
      }
      e match {
        case Num(n) => n
        // case Var(x) => store(x) 
        // assertResult(0){ILInterp("x")} 
        case Var(x) => store.getOrElse(x, 0)
        case Add(l,r) => interpE(l) + interpE(r)
        case Sub(l,r) => interpE(l) - interpE(r)
        case Mul(l,r) => interpE(l) * interpE(r)
        case Div(l,r) => {
          val vl = interpE(l)
          val vr = interpE(r)
          if (vr == 0)
            throw InterpException("divide by zero")
          else
            vl / vr
        }
        case Rem(l,r) => {
          val vl = interpE(l)
          val vr = interpE(r)
          if (vr == 0)
            throw InterpException("divide by zero")
          else
            vl % vr
        } 
        case Lt(l,r)  => {
          if (interpE(l) < interpE(r)) 1 else 0
        }
        case Gt(l,r)  => {
          if (interpE(l) > interpE(r)) 1 else 0
        }  
        case Eq(l,r)  => {
          if (interpE(l) == interpE(r)) 1 else 0
        }
        // assertResult(0){ILInterp("(if 1 0 1)")}
        // assertResult(0){ILInterp("(if 0 1 0)")}
        // assertResult(0){ILInterp("(if 2 0 1)")}
        case If(c,t,e)  => {
          if (interpE(c) != 0) interpE(t) else interpE(e)
        }
        case Assgn(x,e) => {
          val v = interpE(e)
          store(x) = v
          v
        }
        case Write(e)   => {
          val v = interpE(e)
          println(v)
          v
        } 
        case Seq(e1,e2) => {
          interpE(e1)
          interpE(e2)
        }
        // assertResult(0){ILInterp("(while 0 0)")}
        // assertConsoleOutput("3\n", "(write 3)")

        case While(c,b) => // add while loop
        {
          if (interpE(c) > 0) {
            interpE(b)
            interpE(While(c,b))
          } else
            0
        }
        case For(x,e1,e2,e3) => {
          val v1 = interpE(e1)
          store(x) = v1
          // if e2 > -1 then while loop
          interpE(While(Gt(Sub(e2, Var(x)), Num(-1)),
          
          Seq(e3, Assgn(x, Add(Var(x), Num(1))))))
        }

      }
    }

    val v = interpE(e)
    if (debug > 0) println("Evaluates to: " + v)
    v
  } 
  
  def apply(s:String, debug:Int = 0): Int = {
    if (debug > 0) println("Input:  " + s)
    val e = ILParse(s)
    if (debug > 0) println("AST:    " + e)
    interp(e,debug)
  }

  // Test driver
  import scala.io.Source
  def main(argv: Array[String]) = {
    try {
      val s = Source.fromFile(argv(0)).getLines.mkString("\n")
      val d = if (argv.length > 1) argv(1).toInt else 0
      val r = apply(s,d)
      println(r)
    } catch {
      case ex: ParseException =>  println("Parser Error: " + ex.string)
      case ex: InterpException => println("Interp Error: " + ex.string)
    }
  }
}
//
