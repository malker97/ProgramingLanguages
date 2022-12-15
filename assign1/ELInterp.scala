// ExpLang Interpreter
//
// Usage: linux> scala ELInterp <source file>
//
//
import ExpLang._

object ELInterp {
  case class InterpException(string: String) extends RuntimeException

  def interp(e:Expr): Either[Boolean,Int] = e match {
    case True => Left(true)
    case False => Left(false)
    case Num(n) => Right(n)
    case Not(e)  => Left(!interp(e).left.get)
    // if one is left, and the other is right, it needs to raise an “invalid type” 
    case And(l,r) => if (interp(l).isLeft && interp(r).isLeft) Left(interp(l).left.get && interp(r).left.get) else throw InterpException("Invalid type")
    case Or(l,r)  => if (interp(l).isLeft && interp(r).isLeft) Left(interp(l).left.get || interp(r).left.get) else throw InterpException("Invalid type")
    case Xor(l,r) => if (interp(l).isLeft && interp(r).isLeft) Left((interp(l).left.get && !interp(r).left.get) || (!interp(l).left.get && interp(r).left.get)) else throw InterpException("Invalid type")
    case Add(l,r) => if (interp(l).isRight && interp(r).isRight) Right(interp(l).right.get + interp(r).right.get) else throw InterpException("Invalid type")
    case Sub(l,r) => if (interp(l).isRight && interp(r).isRight) Right(interp(l).right.get - interp(r).right.get) else throw InterpException("Invalid type")
    case Mul(l,r) => if (interp(l).isRight && interp(r).isRight) Right(interp(l).right.get * interp(r).right.get) else throw InterpException("Invalid type")
    // For both Div and Rem operations, the interpreter needs to check that the divisor is non-zero, otherwise it needs to raise an “divide by zero” 
    case Div(l,r) => if (interp(r).right.get == 0) throw InterpException("divide by zero") else Right(interp(l).right.get / interp(r).right.get) 
    case Rem(l,r) => if (interp(r).right.get == 0) throw InterpException("divide by zero") else Right(interp(l).right.get % interp(r).right.get)
    // one is num, and the other is bool, it needs to raise an “invalid type”
    // If and only if l and r is num do, else raise an “invalid type”
    case Lt(l,r) => if (interp(l).isRight && interp(r).isRight) Left(interp(l).right.get < interp(r).right.get) else throw InterpException("Invalid type")
    case Gt(l,r) => if (interp(l).isRight && interp(r).isRight) Left(interp(l).right.get > interp(r).right.get) else throw InterpException("Invalid type")
    // // if both same type do, else raise an “invalid type”
    case Eq(l,r) => if (interp(l).isLeft && interp(r).isLeft) Left(interp(l).left.get == interp(r).left.get) else if (interp(l).isRight && interp(r).isRight) Left(interp(l).right.get == interp(r).right.get) else throw InterpException("Invalid type")
    // case Lt(l,r) => if (interp(l).isRight && interp(r).isRight) Left(interp(l).right.get < interp(r).right.get) else throw InterpException("Invalid type")
    // case Gt(l,r) => if (interp(l).isRight && interp(r).isRight) Left(interp(l).right.get > interp(r).right.get) else throw InterpException("Invalid type")
    // case Eq(l,r) => Left(if (interp(l).isLeft) interp(l).left.get == interp(r).left.get else interp(l).right.get == interp(r).right.get)
    // c is boolen, l and r are numbers, else raise an “invalid type”
    case If(c,l,r) => if (interp(c).isLeft && interp(l).isRight && interp(r).isRight) Right(if(interp(c).left.get) interp(l).right.get else interp(r).right.get) else throw InterpException("Invalid type")
    // Right(if(interp(c).left.get) interp(l).right.get else interp(r).right.get)// worked
    case _ => throw InterpException("Illegal expr:" + e)
  }

  def apply(s:String, debug:Int = 0) = {
    if (debug > 0) println("Input:  " + s)
    val e = ELParse(s)
    if (debug > 0) println("AST:    " + e)
    interp(e)
  }

  // Test driver
  import scala.io.Source
  def main(argv: Array[String]) = {
    try {
      val s = Source.fromFile(argv(0)).getLines.mkString("\n")
      val d = if (argv.length > 1) argv(1).toInt else 0
      val v = apply(s,d)
      v match {
        case Left(b)  => printf(b+"\n")
        case Right(i) => printf(i+"\n")
      }
    } catch {
      case ex: ParseException =>  println("Parser Error: " + ex.string)
      case ex: InterpException => println("Interp Error: " + ex.string)
    }
  }
}
//
