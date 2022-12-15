//-------------------------------------------------------------------------
// Supporting code for CS558 Programming Languages. 
// Dept of Computer Science, Portland State University
// J. Li (8/22) [Partially based on A. Tolmach's code]
//-------------------------------------------------------------------------

// Stack Machine (version 0)
//

object StackM0 {

  sealed abstract class Instr
  case object T   extends Instr
  case object F   extends Instr
  case object NOT extends Instr
  case object AND extends Instr
  case object OR  extends Instr

  case class ExecException(string: String) extends RuntimeException

  type Program = List[Instr]

  def step(stk: scala.collection.mutable.Stack[Boolean], instr: Instr) = 
      instr match {
        case T => stk.push(true)
        case F => stk.push(false)
        case NOT => {
          val v = stk.pop()
          stk.push(!v)
        }
        case AND => {
          val v2 = stk.pop()
          val v1 = stk.pop()
          stk.push(v1 && v2)
        }   
        case OR => {
          val v2 = stk.pop()
          val v1 = stk.pop()
          stk.push(v1 || v2)
        }
      }

  import scala.collection.mutable.Stack
  def exec(p:Program, debug:Int = 0): Boolean = {
    val stk = new Stack[Boolean]()    
    def steps(instrs: List[Instr]): Boolean = instrs match {
      case Nil => stk.pop()
      case instr::instrs => {
        step(stk,instr)
        if (debug > 1) println("*" + instr + ":" + (stk.mkString(" ")))
          steps(instrs)
      }
    }
    steps(p)
  }        
}

