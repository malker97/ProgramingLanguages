//-------------------------------------------------------------------------
// Supporting code for CS558 Programming Languages. 
// Dept of Computer Science, Portland State University
// J. Li (8/22) [Partially based on A. Tolmach's code]
//-------------------------------------------------------------------------

// FuncLang Interpreter
//
// Usage: linux> scala FLInterp <source file>
//
import FuncLang._

object FLInterp {
  case class InterpException(string: String) extends RuntimeException

  // Value represenation
  sealed abstract class Value
  case class NumV(n:Int) extends Value
  case class ClosureV(x:String,b:Expr,env:Env) extends Value

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

  // Main interpreter function
  // . useHeap - flag for choosing heap storage
  // . callByName - flag for choosing call-by-name param passing mode
  def interp(p:Expr,useHeap:Boolean=false,callByName:Boolean=false,
               debug:Int=0): Int = {
    val heap = new HeapStore()
    val stack = new StackStore()
    val env: Env = Map[String,Addr]() // initial env (empty)

    def get(a:Addr) = a match {
      case HeapAddr(i)  => heap.get(i)
      case StackAddr(i) => stack.get(i)
    }

    def set(a:Addr,v:Value) = a match {
      case HeapAddr(i)  => heap.set(i,v)
      case StackAddr(i) => stack.set(i,v)
    }

    def interpVar(env:Env,x:String): Addr =
      env.getOrElse(x, throw InterpException("undefined variable:" + x))

    def interpBop(env:Env, l: Expr, r:Expr, op:(Int,Int)=>Int) = {
      val lv = interpE(env,l)
      val rv = interpE(env,r)
      (lv,rv) match {
        case (NumV(ln),NumV(rn)) => NumV(op(ln,rn))
        case _ => throw InterpException("non-numeric argument to numeric operator")
      }   
    }

    def interpE(env:Env,e:Expr): Value = {
      if (debug > 1) {
        println("expr = "+ e)
        println("env = " + env)
        println("stack = " + stack)
        println("heap = " + heap)
      } 
      e match {
        case Num(n) => NumV(n)
        case Var(x) => get(interpVar(env,x))
        case Add(l,r) => interpBop(env,l,r,(lv,rv)=>lv+rv)
        case Sub(l,r) => interpBop(env,l,r,(lv,rv)=>lv-rv)  
        case Mul(l,r) => interpBop(env,l,r,(lv,rv)=>lv*rv)  
        case Div(l,r) => interpBop(env,l,r,(lv,rv)=> 
               if (rv!=0) lv/rv else throw InterpException("divide by zero"))
        case Rem(l,r) => interpBop(env,l,r,(lv,rv)=> 
               if (rv!=0) lv%rv else throw InterpException("divide by zero"))
        case Lt(l,r) => interpBop(env,l,r,(lv,rv)=> if (lv<rv) 1 else 0) 
        case Gt(l,r) => interpBop(env,l,r,(lv,rv)=> if (lv>rv) 1 else 0) 
        case Eq(l,r) => interpBop(env,l,r,(lv,rv)=> if (lv==rv) 1 else 0)
        case If(c,t,e) => {
          val cv = interpE(env,c)
          cv match {
            case NumV(n) => if (n!=0) interpE(env,t) else interpE(env,e)
            case _ => throw InterpException("non-numeric condition")
          }
        }
        
        // find way to remove nested match
        case Let(x,b,e) => {
          val bv = interpE(env,b)
          val add = if (useHeap) heap.allocate(1) else stack.push()
          set(add,bv)
          val newEnv = env + (x->add)
          val ev = interpE(newEnv,e)
          if(!useHeap) stack.pop()
          ev
        }  
        case LetRec(x,b,e) => {
          val add = if (useHeap) heap.allocate(1) else stack.push()
          val newEnv = env + (x->add)
          val bv = interpE(newEnv,b)
          if(useHeap){
            bv match {
              case ClosureV(_,_,_) => {
                set(add,bv)
                interpE(newEnv,e)
              }
              case _ => throw InterpException("non-closure in letrec")
            }
          }
          else {
            bv match {
              case ClosureV(_,_,_) => {
                set(add,bv)
                val ev = interpE(newEnv,e)
                stack.pop()
                ev
              }
              case _ => throw InterpException("non-closure in letrec")
            }
          }
        }
        case Fun(x,b) => {
          ClosureV(x, b, env)
        }    
        // case Apply(f,e) => {
        //   val fv = interpE(env,f)
        //   val ev = interpE(env,e)
        //   fv match {
        //     case ClosureV(fenv,fx,fb) => {
        //       if(callByName) {
        //         val a = if (useHeap) heap.allocate(1) else stack.push()
        //         set(a,ev)
        //         interpE(fenv+(fx->a),fb)
        //       } else {
        //         if(useHeap) {
        //           val a = heap.allocate(1)
        //           set(a,ev)
        //           interpE(fenv+(fx->a),fb)
        //         } else {
        //           val a = stack.push()
        //           set(a,ev)
        //           interpE(fenv+(fx->a),fb)
        //         }
        //       }
        //     }
        //     case _ => throw InterpException("non-function application")
        //   }
        // } 
        // val example1 = """(@ (@ (fun x (fun y (+ x y))) 2) 3)"""
        // assertResult(5) { FLInterp(example1,useHeap=true,callByName=true) }
       
        case Apply(f,e) => {
          interpE(env, f) match {
            case ClosureV(x, b, cl_env) => {
              if (callByName) {
                // will call a recursive function
                interpE(cl_env, recurHelper(b, x, e))
              } 
              else {
                // val bv = interpE(env,b)
                // val add = if (useHeap) heap.allocate(1) else stack.push()
                // set(add,bv)
                // val newEnv = env + (x->add)
                // val ev = interpE(newEnv,e)
                // if(!useHeap) stack.pop()
                // ev
                val ev = interpE(env, e)
                val add = if (useHeap) heap.allocate(1) else stack.push()
                set(add, ev)
                val newEnv = cl_env + (x -> add)
                val ve = interpE(newEnv, b)
                if (!useHeap) stack.pop()
                ve
              }
            }
            case _ => throw InterpException("can't apply to a non closure.")
          }
        }
      } 
    }

    // process the top-level expression
    val v = interpE(env,p)
    if (debug > 0) println("Expression evaluates to: " + v)
    v match {
      case NumV(n) => n
      case _ => throw InterpException("top-level expr returns non-integer")
    }
  }

  def apply(s:String,useHeap:Boolean=false,callByName:Boolean=false,
            debug:Int=0): Int = {
    if (debug > 0) println("Input:  " + s)
    val p = FLParse(s)
    if (debug > 0) println("AST:    " + p)
    interp(p,useHeap,callByName,debug)
  }
def recurHelper(e:Expr, x:String, y:Expr):Expr = {
  e match {
    case Num(n) => e
    case Var(x2) => {
      if (x2 == x) y
      else Var(x2)
    }
    case Add(l,r) => Add(recurHelper(l,x,y),recurHelper(r,x,y))
    case Sub(l,r) => Sub(recurHelper(l,x,y),recurHelper(r,x,y))
    case Mul(l,r) => Mul(recurHelper(l,x,y),recurHelper(r,x,y))
    case Div(l,r) => Div(recurHelper(l,x,y),recurHelper(r,x,y))
    case Rem(l,r) => Rem(recurHelper(l,x,y),recurHelper(r,x,y))
    case Lt(l,r) => Lt(recurHelper(l,x,y),recurHelper(r,x,y))
    case Gt(l,r) => Gt(recurHelper(l,x,y),recurHelper(r,x,y))
    case Eq(l,r) => Eq(recurHelper(l,x,y),recurHelper(r,x,y))
    case If(c,t,f) => If(recurHelper(c,x,y),recurHelper(t,x,y),recurHelper(f,x,y))
    case Let(w,b,f) => {
      if (w == x) Let(w,recurHelper(b,x,y),f)
      else Let(w,recurHelper(b,x,y),recurHelper(f,x,y))
    }
    case LetRec(w,b,f) => {
      if (w == x) LetRec(w,recurHelper(b,x,y),f)
      else LetRec(w,recurHelper(b,x,y),recurHelper(f,x,y))
    }
    case Fun(w,b) => {
      if (w == x) e else Fun(w,recurHelper(b,x,y))
    }
    case Apply(f, b) => { 
      Apply(recurHelper(f,x,y),recurHelper(b,x,y))
    }
  }
}
  // Test driver
  import scala.io.Source
  def main(argv: Array[String]) = {
    try {
      val s = Source.fromFile(argv(0)).getLines.mkString("\n")
      var heapFlag = false
      var cbnFlag = false
      var debugFlag = 0
      for (arg <- argv) {
        if (arg == "heap") heapFlag = true
        if (arg == "cbn") cbnFlag = true
        if (arg == "1") debugFlag = 1
        if (arg == "2") debugFlag = 2
      }
      val v = apply(s,heapFlag,cbnFlag,debugFlag)
      println(v)
    } catch {
      case ex: ParseException =>  println("Parser Error: " + ex.string)
      case ex: InterpException => println("Interp Error: " + ex.string)
    }
  }
}

//
