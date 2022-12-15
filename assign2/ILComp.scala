// ImpLang to RegIR Compiler
//
// Usage: linux> scala ILCompReg <source file>
//
import ImpLang._
import RegIR._

object ILComp {
  var nextLabel: Int = 100
  var nextTemp: Int = 0

  def compile(e:Expr): (Program,Src) = e match {
    case Num(i) => (Nil, Const(i))
    case Var(x) => (Nil, Name(x))
    case Add(e1,e2) => {
      val tmp = newTemp()
      val (p1,s1) = compile(e1)
      val (p2,s2) = compile(e2)
      val p = p1 ::: p2 ::: (Bop(AOP.Add,tmp,s1,s2) :: Nil)
      (p, Name(tmp))
    }
    // use the same pattern as Add
    case Sub(e1,e2) => { // same to add
      val tmp = newTemp()
      val (p1,s1) = compile(e1)
      val (p2,s2) = compile(e2)
      val p = p1 ::: p2 ::: (Bop(AOP.Sub,tmp,s1,s2) :: Nil)
      (p, Name(tmp))
    }  
    case Mul(e1,e2) => {
      val tmp = newTemp()
      val (p1,s1) = compile(e1)
      val (p2,s2) = compile(e2)
      val p = p1 ::: p2 ::: (Bop(AOP.Mul,tmp,s1,s2) :: Nil)
      (p, Name(tmp))
    }   
    case Div(e1,e2) => {
      val tmp = newTemp()
      val (p1,s1) = compile(e1)
      val (p2,s2) = compile(e2)
      val p = p1 ::: p2 ::: (Bop(AOP.Div,tmp,s1,s2) :: Nil)
      (p, Name(tmp))
    }  
    case Rem(e1,e2) => {
      val tmp = newTemp()
      val (p1,s1) = compile(e1)
      val (p2,s2) = compile(e2)
      val p = p1 ::: p2 ::: (Bop(AOP.Rem,tmp,s1,s2) :: Nil)
      (p, Name(tmp))
    }  
    // 
    case Lt(e1,e2) => {
      val tmp = newTemp()
      val lab = newLabel()
      val (p1,s1) = compile(e1) 
      val (p2,s2) = compile(e2)
      // val p = p1 ::: p2 ::: (Bop(COP.Lt,tmp,s1,s2) :: Nil)
      val p = p1 ::: p2 ::: (Mov(tmp, Const(1)):: CJump(ROP.Lt,s1,s2,lab) :: Mov(tmp, Const(0)) :: Label(lab) :: Nil)
      (p, Name(tmp))
    }
    case Gt(e1,e2) => {
      val tmp = newTemp()
      val lab = newLabel()
      val (p1,s1) = compile(e1) 
      val (p2,s2) = compile(e2)
      // same to Lt
      val p = p1 ::: p2 ::: (Mov(tmp, Const(1)):: CJump(ROP.Gt,s1,s2,lab) :: Mov(tmp, Const(0)) :: Label(lab) :: Nil) 
      (p, Name(tmp))
    }
    case Eq(e1,e2) => {
      val tmp = newTemp()
      val lab = newLabel()
      val (p1,s1) = compile(e1) 
      val (p2,s2) = compile(e2)
      val p = p1 ::: p2 ::: (Mov(tmp, Const(1)):: CJump(ROP.Eq,s1,s2,lab) :: Mov(tmp, Const(0)) :: Label(lab) :: Nil)
      (p, Name(tmp))
    }
    case If(c,t,f) => {
      val tmp = newTemp()
      val lab1 = newLabel()
      val lab2 = newLabel()
      val (pc,sc) = compile(c) 
      val (pt,st) = compile(t) 
      val (pf,sf) = compile(f)
      val p = pc ::: (CJump(ROP.Eq, sc, Const(0),lab1)::Nil) ::: pt ::: (Mov(tmp,st)::Jump(lab2)::Label(lab1)::Nil) ::: pf ::: (Mov(tmp,sf)::Label(lab2)::Nil)
      (p, Name(tmp))
    }
    case Assgn(x,e) => {
      val (pe, se) = compile(e)
      (pe ::: (Mov(x, se)::Nil), se)
    }

    case Write(e)   => {
      val (pe, se) = compile(e)
      (pe ::: (Print(se)::Nil), se)
    }

    case Seq(e1,e2) => {
      val (p1,s1) = compile(e1) 
      val (p2,s2) = compile(e2)
      (p1 ::: p2, s2)
    }
    case While(c,b) => {
      val lab1 = newLabel()
      val lab2 = newLabel()
      val (pc,sc) = compile(c)
      val (pb,sb) = compile(b)
      val p = Label(lab1)::pc:::CJump(ROP.Eq,sc,Const(0),lab2)::pb:::Jump(lab1)::Label(lab2)::Nil
      // val p = (Label(lab1)::Nil) ::: pc ::: (CJump(ROP.Eq,sc,Const(0),lab2)::Nil) ::: pb ::: (Jump(lab1) :: Label(lab2)::Nil)
      // val p = (Label(lab1)::Nil) ::: pc ::: (CJump(ROP.Eq,sc,Const(0),lab2)::Nil) ::: pb ::: (Jump(lab1) :: Label(lab2)::Nil)
      // val p = Label(lab1)::pc::(CJump(ROP.Eq,sc,Const(0),lab2)::Nil)::pb::(Jump(lab1)::Label(lab2)::Nil)
      (p, Const(0))
    }

    // cmp x e2 
    // val v1 = interpE(e1)
    // store(x) = v1
    // // if e2 > -1 then while loop
    // interpE(While(Gt(Sub(e2, Var(x)), Num(-1)),
    // Seq(e3, Assgn(x, Add(Var(x), Num(1))))))
    case For(x,e1,e2,e3) => {
      val tmp = newTemp()
      val lab1 = newLabel()
      val lab2 = newLabel()
      val (p1,s1) = compile(e1)
      val (p2,s2) = compile(e2)
      val (p3,s3) = compile(e3)

      val p = p1 ::: (Mov(x,s1)::Nil) ::: (Label(lab1)::Nil) ::: p2 ::: (CJump(ROP.Gt,Name(x),s2,lab2)::Nil) ::: p3 ::: (Bop(AOP.Add,tmp,Name(x),Const(1))::Nil) ::: (Mov(x,Name(tmp))::Nil) ::: (Jump(lab1)::Nil) ::: (Label(lab2)::Nil)
      // val p = p1 ::: (Mov(x,s1)::Nil) ::: (Label(lab1)::Nil) ::: p2 ::: (CJump(ROP.Gt,Name(x),s2,lab2)::Nil) ::: p3 ::: (Bop(AOP.Add,tmp,Name(x),Const(1))::Nil) :::(Mov(x,Name(tmp))::Nil) ::: (Jump(lab1)::Nil) ::: (Label(lab2)::Nil)
      // val p = pe1::(Mov(x,se1)::Nil)::Label(lab1)::pe2::(CJump(ROP.Eq,se2,Const(0),lab2)::Nil)::pe3::(Jump(lab1)::Label(lab2)::Nil)
      (p, Const(0))
    }
  }

  def newLabel() = {
    val next = nextLabel
    nextLabel = nextLabel + 1
    next
  }

  def newTemp() = {
    val next = "t" + nextTemp
    nextTemp = nextTemp + 1
    next
  }

  def apply(s:String, debug:Int = 0) = {
    if (debug > 0) println("Input:  " + s)
    val e = ILParse(s)
    if (debug > 0) println("AST:    " + e)
    nextLabel = 0
    val (pe,ve) = compile(e)
    val p = pe ::: (Return(ve) :: Nil)
    if (debug > 0) println("Target: " + p)
    exec(p,debug)
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
      case ex: ParseException => println("Parser Error: " + ex.string)
      case ex: ExecException =>  println("Exec Error: " + ex.string)
    }
  }
}
//
