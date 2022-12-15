// ExpLang to StackM1 Compiler
//
// Usage: linux> scala ELComp <source file>
//
//
import ExpLang._
import StackM1._

object ELComp {
  var nextLabel: Int = 100

  case class CompileException(string: String) extends RuntimeException

  def compile(e: Expr): Program = e match {
    case True => Const(1)::Nil
    case False => Const(0)::Nil
    case Num(n) => Const(n)::Nil
    // case object SAnd extends Instr
    // case object SOr  extends Instr
    // case object SAdd extends Instr
    // case object SMul extends Instr
    // case object Divrem extends Instr
    // case object Pop extends Instr
    // case object Dup extends Instr
    // case object Swap extends Instr
    //   Label(i)  - nop, marking a label position in program
    //   Goto(i)   - branch to Label(i)
    //   Ifgt(i)   - if val > 0 branch to Label(i)
    //   Ifz(i)    - if val = 0 branch to Label(i)
    // inplment NOT with SAnd
    case Not(e) => compile(e) ::: (Const(1) ::SAdd :: Const(2) :: Divrem :: Swap :: Pop::Nil)
    case And(l,r) => compile(l) ::: compile(r) ::: SAnd::Nil
    case Or(l,r)  => compile(l) ::: compile(r) ::: SOr::Nil
    case Xor(l,r) => compile(l) ::: compile(r) ::: (SAdd :: Const(2) :: Divrem :: Swap :: Pop::Nil)
    case Add(e1,e2) => compile(e1) ::: compile(e2) ::: SAdd::Nil
    // e1 + -1*e2
    case Sub(e1,e2) => compile(e1) ::: compile(e2) ::: Const(-1)::SMul::SAdd::Nil
    case Mul(e1,e2) => compile(e1) ::: compile(e2) ::: SMul::Nil
    // Div then pop remainder
    case Div(e1,e2) => compile(e1) ::: compile(e2) ::: Divrem::Pop::Nil
    // Div Swap then pop remainder
    case Rem(e1,e2) => compile(e1) ::: compile(e2) ::: Divrem::Swap::Pop::Nil
    // Ifgt(e1 + -1*e2)
    // case Gt(e1,e2)  => compile(e1) ::: compile(e2) ::: Const(-1)::SMul::SAdd::Ifgt(nextLabel)::Const(0)::Label(nextLabel)::Nil
    // case Gt(e1,e2)  => {
    //   val label = nextLabel
    //   nextLabel += 1
    //   compile(e1) ::: compile(e2) ::: Const(-1)::SMul::SAdd::Ifgt(label)::Const(0)::Label(label)::Nil
    // }
    case Gt(e1,e2)  => {
      val lab1 = newLabel()
      val lab2 = newLabel()
      compile(e1) ::: compile(e2) ::: (Const(-1) :: SMul :: SAdd :: Ifgt(lab1) :: Const(0) :: Goto(lab2) :: Label(lab1) :: Const(1) :: Label(lab2)::Nil)
    }
    // swap e1 and e2, Ifgt(e1 + -1*e2)
    // case Lt(e1,e2)  => compile(e1) ::: compile(e2) ::: Swap::Const(-1)::SMul::SAdd::Ifgt(nextLabel)::Const(0)::Label(nextLabel)::Nil
    // case Lt(e1,e2) => {
    //   val label = nextLabel
    //   nextLabel += 1
    //   compile(e1) ::: compile(e2) ::: Swap::Const(-1)::SMul::SAdd::Ifgt(label)::Const(0)::Label(label)::Nil
    // }
    case Lt(e1,e2)  => {
      val lab1 = newLabel()
      val lab2 = newLabel()
      compile(e1) ::: compile(e2) ::: (Swap :: Const(-1) :: SMul :: SAdd :: Ifgt(lab1) :: Const(0) :: Goto(lab2) :: Label(lab1) :: Const(1) :: Label(lab2)::Nil)
    } 
    // ifz(e1 + -1*e2)
    // case Eq(e1,e2)  => compile(e1) ::: compile(e2) ::: Const(-1)::SMul::SAdd::Ifz(nextLabel)::Const(0)::Label(nextLabel)::Nil
    // case Eq(e1,e2)  => {
    //   val label = nextLabel
    //   nextLabel += 1
    //   compile(e1) ::: compile(e2) ::: Const(-1)::SMul::SAdd::Ifz(label)::Const(0)::Label(label)::Nil
    // }
    case Eq(e1,e2)  => {
      val lab1 = newLabel()
      val lab2 = newLabel()
      compile(e1) ::: compile(e2) ::: (Const(-1) :: SMul :: SAdd :: Ifz(lab1) :: Const(0) :: Goto(lab2) :: Label(lab1) :: Const(1) :: Label(lab2)::Nil)
    }
    // 
    case If(c,t,f) => {
      val lab1 = newLabel()
      val lab2 = newLabel()
      compile(c) ::: (Ifgt(lab1)::Nil) ::: compile(f) ::: (Goto(lab2)::Nil) ::: (Label(lab1)::Nil) ::: compile(t) ::: (Label(lab2)::Nil)
    }
    case _ => throw CompileException("Illegal expr:" + e)
  }

  def newLabel() = {
    val next = nextLabel
    nextLabel = nextLabel + 1
    next
  }

  def apply(s:String, debug:Int = 0) = {
    if (debug > 0) println("Input:  " + s)
    val e = ELParse(s)
    if (debug > 0) println("AST:    " + e)
    val p: Program = compile(e)
    if (debug > 0) println("Target: " + p)
    exec(p,debug)
  }

  // Test driver
  import scala.io.Source
  def main(argv: Array[String]) = {
    try {
      val s = Source.fromFile(argv(0)).getLines.mkString("\n")
      val d = if (argv.length > 1) argv(1).toInt else 0
      val v = apply(s,d)
      println(v)
    } catch {
      case ex: ParseException => println("Parser Error: " + ex.string)
      case ex: CompileException => println("Compile Error: " + ex.string)
    }
    ()
  }
}
//
