// ScopeLang Interpreter
//
// Usage: linux> scala SLInterp <source file>
//
import ScopeLang._

object SLInterp {
  case class InterpException(string: String) extends RuntimeException

  // Value represenation
  sealed abstract class Value
  case class NumV(n:Int) extends Value
  case class PairV(a:Addr) extends Value 

  // Storage represenation
  type Index = Int
  sealed abstract class Store {
    case class UndefinedContents(string: String) extends RuntimeException
    private val contents = collection.mutable.Map[Index,Value]()
    def get(i:Index) = contents.getOrElse(i, throw UndefinedContents("" + i))
    def set(i:Index,v:Value) = contents += (i->v)
    override def toString: String = contents.toString
  }
  // Heap
  class HeapStore extends Store {
    private var nextFreeIndex:Index = 0
    def allocate(n:Int): Addr = {
      val i = nextFreeIndex
      nextFreeIndex += n
      HeapAddr(i)
    }
    // there is no mechanism for deallocation
    override def toString: String = "[next=" + nextFreeIndex + "] " + super.toString
  }
  // Stack
  class StackStore extends Store {
    private var stackPointer:Index = 0;
    def push(): Addr = {
      val i = stackPointer
      stackPointer += 1
      StackAddr(i)
    }
    def pop() = {
      if (stackPointer > 0)
        stackPointer -= 1
      else
        throw InterpException("stack storage is empty")
    }
    def isEmpty(): Boolean = stackPointer == 0
    override def toString: String = "[sp=" + stackPointer + "] " + super.toString
  }

  // Address to storage
  sealed abstract class Addr() {
    def +(offset:Int): Addr
  }
  case class HeapAddr(index:Int) extends Addr {
    def +(offset:Int) = HeapAddr(index+offset)
  }
  case class StackAddr(index:Int) extends Addr {
    def +(offset:Int) = StackAddr(index+offset)
  }

  type Env = Map[String,Addr]

  // Main interpreter function, returns program's value (must be an int)
  def interp(e:Expr, debug:Int = 0): Int = {
    val heap = new HeapStore()
    val stack = new StackStore()
    var env: Env = Map[String,Addr]() // initial env (empty)

    // utility fuctions
    def get(a:Addr) = a match {
      case HeapAddr(i) => heap.get(i)
      case StackAddr(i) => stack.get(i)
    }

    def set(a:Addr,v:Value) = a match {
      case HeapAddr(i) => heap.set(i,v)
      case StackAddr(i) => stack.set(i,v)
    }

    def getAddr(env:Env, x:String): Addr =
      env.getOrElse(x, throw InterpException("undefined variable:" + x))

    def interpBop(env:Env, l: Expr, r:Expr, op:(Int,Int)=>Int) = {
      val lv = interpE(env,l)
      val rv = interpE(env,r)
      (lv,rv) match {
        case (NumV(ln),NumV(rn)) => NumV(op(ln,rn))
        case _ => throw InterpException("non-numeric argument to numeric operator")
      }   
    }

    // interprete an expr in a given env
    def interpE(env:Env, e:Expr): Value = {
      if (debug > 1) {
        println("expr = "+ e)
        println("env = " + env)
        println("stack = " + stack)
        println("heap = " + heap)
      } 
      e match {
        case Num(n) => NumV(n)
        case Var(x) => {
          val a = getAddr(env,x)
          get(a)
        }
        case Add(l,r) => interpBop(env,l,r,(lv,rv)=>lv+rv)
        case Sub(l,r) => interpBop(env,l,r,(lv,rv)=>lv-rv)  
        case Mul(l,r) => interpBop(env,l,r,(lv,rv)=>lv*rv)  
        case Div(l,r) => interpBop(env,l,r,(lv,rv)=> 
               if (rv!=0) lv/rv else throw InterpException("divide by zero"))
        case Rem(l,r) => interpBop(env,l,r,(lv,rv)=> 
               if (rv!=0) lv%rv else throw InterpException("divide by zero"))
        case Lt(l,r)  => interpBop(env,l,r,(lv,rv)=>if (lv<rv) 1 else 0)   
        case Gt(l,r)  => interpBop(env,l,r,(lv,rv)=>if (lv>rv) 1 else 0) 
        case Eq(l,r)  => {
          // equality is defined for all values
          // IFF they are both numbers or both pairs
          val lv = interpE(env,l)
          val rv = interpE(env,r)
          (lv,rv) match {
            case (NumV(ln),NumV(rn)) => if (ln==rn) NumV(1) else NumV(0)
            case (PairV(la),PairV(ra)) => if (la==ra) NumV(1) else NumV(0)
            case _ => throw InterpException("non-numeric argument to numeric operator")
          }
        }   
        case Deq(l,r) => {
          // cuz the pair data type is recursive define in SL, we need to use recursive method to compare
          def PairCmp(v1:Value, v2:Value):Value = (v1,v2) match {
            case (NumV(n1),NumV(n2)) => if (n1==n2) NumV(1) else NumV(0)
            case (PairV(a1),PairV(a2)) => {
              if (PairCmp(get(a1),get(a2)) == NumV(1) && PairCmp(get(a1+1),get(a2+1)) == NumV(1)) 
                NumV(1)
              else NumV(0)
            }
            case _ => NumV(0)
          }

          val lv = interpE(env,l)
          val rv = interpE(env,r)
          (lv,rv) match {
            case (NumV(ln),NumV(rn)) => if (ln==rn) NumV(1) else NumV(0)
            case (PairV(lp),PairV(rp)) => PairCmp(lv, rv)
            case _ => NumV(0)
          }
        }   
        case Assgn(x,e) => {
          val a = getAddr(env,x)
          val v = interpE(env,e)
          set(a,v)
          v
        }
          // lookup x's address from env, evaluate e, and set its value
          // to x's storage location; yield e's value as Assgn's value
          // (no need to update env...why?)
          // ... need code ...
        // case Write(e) => {
        //   // cuz PairV is recursive define in SL, we need to use recursive method to print
        //   def printPair(a:Addr): String = {
        //     val v = get(a)
        //     v match {
        //       case NumV(n) => n.toString
        //       case PairV(a) => {
        //         val car = a
        //         val cdr = a + 1
        //         "(" + printPair(car) + "." + printPair(cdr) + ")\n"
        //       }
        //     }
        //   }
        //   val v = interpE(env,e)
        //   v match {
        //     case NumV(n) => println(n)
        //     case PairV(a) => {
        //       val car = a
        //       val cdr = a + 1
        //       println("(" + printPair(car) + "." + printPair(cdr) + ")")
        //     }
        //   }
        //   v
        // }
        case Write(e) =>{ 
          // cuz PairV is recursive define in SL, we need to use recursive method to print
          def PairToStr(v:Value):String = {
              v match {
              case NumV(n) => n.toString
              case PairV(a) => "(" + PairToStr(get(a)) + "." +
                PairToStr(get(a+1)) + ")"
              }
          }

          val ve = interpE(env, e)
          ve match {
            case NumV(n) => println(n)
            case PairV(a) => {
              // call PairToStr to recursively print the pair
              println("(" + PairToStr(get(a)) + "." + 
                PairToStr(get(a+1)) + ")")
            }
            case _ => throw InterpException("write: non-numeric argument")
          }
          ve
        }
        case Seq(e1,e2) => {
          val v1 = interpE(env,e1)
          val v2 = interpE(env,e2)
          v2
        }
        case If(c,t,e)  => {
          val cv = interpE(env,c)
          cv match {
            case NumV(n) => if (n!=0) interpE(env,t) else interpE(env,e)
            case _ => throw InterpException("non-numeric argument to numeric operator")
          }
        }  
        case While(c,b) => {
          val cv = interpE(env,c)
          cv match {
            case NumV(n) => {
              if (n != 0) {
                interpE(env,b)
                interpE(env,e)
              } else {
                NumV(0)
              }
            }
            case _ => throw InterpException("non-numeric argument to numeric operator")
          }
        }
        case Let(x,b,e) => {
          val vb = interpE(env, b)
          val newAddr = stack.push()
          set(newAddr, vb)
          val ne = env + (x -> newAddr)
          val ve = interpE(ne, e)
          stack.pop()
          ve
        }
          // evaluate b and bind it's value to x, and store it on the
          // stack (use push() to get a stack address and set() to store
          // the value); x's binding needs to be added to env for 
          // evaluating e (only); x's value needs to be removed before
          // returning (use pop())
          // ... add code ...
        case Pair(l,r)  => {
          val lv = interpE(env,l)
          val rv = interpE(env,r)
          var newAddr = heap.allocate(2)
          set(newAddr, lv)
          set(newAddr + 1, rv)
          PairV(newAddr)
        }
          // allocate 2 units of space in the heap; store the pairs' two
          // values into the space (use set()); return a pair value
          // ... add code ...
        case IsPair(e)  => {
          val v = interpE(env,e)
          v match {
            case PairV(a) => NumV(1)
            case _ => NumV(0)
          }
        }
        case Fst(e) => {
          val v = interpE(env,e)
          v match {
            case PairV(a) => get(a)
            case _ => throw InterpException("non-numeric argument to numeric operator")
          }
        }
        case Snd(e) => {
          val v = interpE(env,e)
          v match {
            case PairV(a) => get(a + 1)
            case _ => throw InterpException("non-numeric argument to numeric operator")
          }
        }
        // val pv = interpE(env,p)
        //   val ev = interpE(env,e)
        //   pv match {
        //     case PairV(a) => {
        //       set(a, ev)
        //       ev
        //     }
        //     case _ => throw InterpException("non-numeric argument to numeric operator")
        //   }
        case SetFst(p,e) => {
          val pva = interpE(env, p) match {
            case PairV(a) => a
            case _ => throw InterpException("non-numeric argument to numeric operator")
          }
          val ve = interpE(env, e)
          set(pva, ve)
          PairV(pva)
        } 
        case SetSnd(p,e) => {
          val pva = interpE(env, p) match {
            case PairV(a) => a
            case _ => throw InterpException("non-numeric argument to numeric operator")
          }
          val ve = interpE(env, e)
          set(pva + 1, ve)
          PairV(pva)
        }
      }
    }

    // process the main body expression
    val v = interpE(env,e)
    if (debug > 0) println("Evaluates to: " + v)
    if (!stack.isEmpty)
      throw InterpException("stack not empty at the end of program exection")
    v match {
      case NumV(n) => n
      case _ => throw InterpException("top-level expr returns non-integer")
    }
  } 
  
  def apply(s:String, debug:Int = 0): Int = {
    if (debug > 0) println("Input:  " + s)
    val e = SLParse(s)
    if (debug > 0) println("AST:    " + e)
    interp(e,debug)
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
      case ex: ParseException =>  println("Parser Error: " + ex.string)
      case ex: InterpException => println("Interp Error: " + ex.string)
    }
  }
}
//
