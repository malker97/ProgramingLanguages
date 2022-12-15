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
        case Let(x,b,e) => {
          val bv = interpE(env,b)
          val a = if (useHeap) heap.allocate(1) else stack.push()
          set(a,bv)
          interpE(env+(x->a),e)
        }  
        case LetRec(x,b,e) => {
          val a = if (useHeap) heap.allocate(1) else stack.push()
          val bv = interpE(env+(x->a),b)
          set(a,bv)
          interpE(env+(x->a),e)
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
        case Apply(f,e) => {
          interpE(env, f) match {
            case ClosureV(x, b, cl_env) => {

              if (callByName) {
                def substitute(e:Expr, x:String, y:Expr):Expr = {
                  e match {
                    case Num(n) => e
                    case Var(x) => y
                    case Add(l,r) => Add(substitute(l,x,y), substitute(r,x,y))
                    case Sub(l,r) => Sub(substitute(l,x,y), substitute(r,x,y))
                    case Mul(l,r) => Mul(substitute(l,x,y), substitute(r,x,y))
                    case Div(l,r) => Div(substitute(l,x,y), substitute(r,x,y))
                    case Rem(l,r) => Rem(substitute(l,x,y), substitute(r,x,y))
                    case Lt(l,r) => Lt(substitute(l,x,y), substitute(r,x,y))
                    case Gt(l,r) => Gt(substitute(l,x,y), substitute(r,x,y))
                    case Eq(l,r) => Eq(substitute(l,x,y), substitute(r,x,y))
                    case If(c,t,f) => If(substitute(c,x,y), substitute(t,x,y), substitute(f,x,y))
                    case Let(w,b,f) => if (w == x) e else Let(w, substitute(b,x,y), substitute(f,x,y))
                    case LetRec(w,b,f) => if (w == x) e else LetRec(w, substitute(b,x,y), substitute(f,x,y))
                    case Fun(w,b) => if (w == x) e else Fun(w, substitute(b,x,y))
                    case Apply(f, b) => { 
                      Apply(f, substitute(b,x,y))
                    }
                  }
                  //"""(@ (@ (fun x (fun y (+ x y))) 2) 3)""" what I'm trying to get to work.
                } // end of substitute rec function...
                val vs = substitute(b, x, e)
                if (debug > 0) {
                  println("CBN: " + Apply(f,e) + " => " + vs)
                }
                interpE(cl_env, vs)
              } // end of if call by name 
              else {
                val ve = interpE(env, e)
                if (useHeap) {
                  val addy = heap.allocate(1);
                  set(addy, ve)
                  val ne = cl_env + (x -> addy)
                  interpE(ne, b)
                } else {
                  val addy = stack.push()
                  set(addy, ve)
                  val ne = cl_env + (x -> addy)
                  val vb = interpE(ne, b)
                  stack.pop
                  vb
                }
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
