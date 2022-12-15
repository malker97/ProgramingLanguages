//-------------------------------------------------------------------------
// Supporting code for CS558 Programming Languages. 
// Dept of Computer Science, Portland State University
// J. Li (8/22) [Partially based on A. Tolmach's code]
//-------------------------------------------------------------------------

// Stack Machine (version 1)
//
//   Const(n)  - load constant n to stack
//   And       - val1 & val2, push result to stack (bitwise and)
//   Or        - val1 | val2, push result to stack (bitwise or)
//   Add       - val1 + val2, push result to stack
//   Mul       - val1 * val2, push result to stack
//   Divrem    - val1 / val2, push div and rem results to stack
//   Pop       - pop off val
//   Dup       - replicate val
//   Swap      - swap val1 and val2
//   Label(i)  - nop, marking a label position in program
//   Goto(i)   - branch to Label(i)
//   Ifgt(i)   - if val > 0 branch to Label(i)
//   Ifz(i)    - if val = 0 branch to Label(i)
//

object StackM1 {

  sealed abstract class Instr
  case class Const(n:Int) extends Instr
  case object SAnd extends Instr
  case object SOr  extends Instr
  case object SAdd extends Instr
  case object SMul extends Instr
  case object Divrem extends Instr
  case object Pop extends Instr
  case object Dup extends Instr
  case object Swap extends Instr
  case class Label(l:Int) extends Instr
  case class Goto(l:Int) extends Instr
  case class Ifgt(l:Int) extends Instr
  case class Ifz(l:Int) extends Instr

  case class ExecException(string: String) extends RuntimeException

  type Program = List[Instr]

  // operand stack
  type Stack = collection.mutable.Stack[Int]

  // see http://docs.scala-lang.org/overviews/collections/maps.html
  // for details of Map class
  type VarStore = collection.mutable.Map[String,Int]

  def exec(prog:Program,debug: Int = 0): Int = {
    val stk: Stack = collection.mutable.Stack[Int]()
    val store: VarStore = collection.mutable.Map[String,Int]()  
    var pc = 0

    def step(): Int = 
      prog(pc) match {
        case Const(i) => {
          stk.push(i)
          pc+1
        }
        case SAnd => {
          val v2 = stk.pop()
          val v1 = stk.pop()
          stk.push(v1 & v2)
          pc+1
        }
        case SOr => {
          val v2 = stk.pop()
          val v1 = stk.pop()
          stk.push(v1 | v2)
          pc+1
        }
        case SAdd => {
          val v2 = stk.pop()
          val v1 = stk.pop()
          stk.push(v1 + v2)
          pc+1
        }
        case SMul => {
          val v2 = stk.pop()
          val v1 = stk.pop()
          stk.push(v1 * v2)
          pc+1
        }
        case Divrem => {
          val v2 = stk.pop()
          val v1 = stk.pop()
          if (v2 == 0) 
            throw ExecException("division by zero")
          else {
            stk.push(v1 / v2)
            stk.push(v1 % v2)
          }
          pc+1
        }
        case Pop => {
          stk.pop()
          pc+1
        }
        case Dup => {
          val v = stk.pop()
          stk.push(v)
          stk.push(v)
          pc+1
        }
        case Swap => {
          val v2 = stk.pop()
          val v1 = stk.pop()
          stk.push(v2)
          stk.push(v1)
          pc+1
        }
        case Label(l) =>
          pc+1
        case Goto(l) =>
          findLabel(prog,l)
        case Ifgt(l) => {
          val v = stk.pop()
          if (v > 0)
            findLabel(prog,l)
          else
            pc+1
        }
        case Ifz(l) => {
          val v = stk.pop()
          if (v == 0)
            findLabel(prog,l)
          else
            pc+1
        }
      }
    
    def findLabel(prog:Program,l:Int) = {
      def f(n:Int,prog:Program): Int = prog match {
        case Nil => throw ExecException("missing label " + l)
        case Label(l1)::rest if l == l1 => n
        case _::rest => f(n+1,rest)
      }
      f(0,prog)
    }

    while (pc < prog.length) {
      if (debug > 1) print("" + pc + "*" + prog(pc))
      pc = step()
      if (debug > 1) println (":" + stk.mkString(" "))
    }
    val r = stk.pop()

    // sanity check
    if (debug > 1) println("result: " + r)
    if (!stk.isEmpty)
      throw ExecException("stack not empty at the end of program exection")
    r
  }        
}

