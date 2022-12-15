//-------------------------------------------------------------------------
// Supporting code for CS558 Programming Languages. 
// Dept of Computer Science, Portland State University
// J. Li (8/22)
//-------------------------------------------------------------------------

// ExpLang - A simple expresion language
//
// Grammar:
//   Expr -> t                    // true
//        |  f                    // false
//        |  <num>                // int literal
//        |  (not Expr)           // negation
//        |  (Op Expr Expr)       // binop expr
//        |  (if Expr Expr Expr)  // if expr
//   Op   -> and | or | xor       // Boolean op
//        | + | - | * | / | %     // integer op (% is remainder)
//        | < | > | ==            // relational op
//
import scala.util.parsing.combinator._

object ExpLang {
  sealed abstract class Expr
  case object True extends Expr
  case object False extends Expr
  case class Num(i:Int) extends Expr
  case class Not(e:Expr) extends Expr
  case class And(l:Expr,r:Expr) extends Expr
  case class Or(l:Expr,r:Expr) extends Expr
  case class Xor(l:Expr,r:Expr) extends Expr
  case class Add(l:Expr,r:Expr) extends Expr
  case class Sub(l:Expr,r:Expr) extends Expr
  case class Mul(l:Expr,r:Expr) extends Expr
  case class Div(l:Expr,r:Expr) extends Expr
  case class Rem(l:Expr,r:Expr) extends Expr
  case class Lt(l:Expr,r:Expr) extends Expr
  case class Gt(l:Expr,r:Expr) extends Expr
  case class Eq(l:Expr,r:Expr) extends Expr
  case class If(c:Expr,t:Expr,f:Expr) extends Expr

  case class ParseException(string: String) extends RuntimeException

  object ELParse extends JavaTokenParsers {
    // input program is in s-expression form
    def expr: Parser[Expr] = atm | lst
    def atm: Parser[Expr] = 
          "t" ^^ {_ => True}  | 
          "f" ^^ {_ => False} |
          wholeNumber ^^ {s => Num(s.toInt)}
    def lst: Parser[Expr] = 
          "(not" ~> expr <~ ")"      ^^ {e => Not(e)}          |
          "(and" ~> expr~expr <~ ")" ^^ {case l~r => And(l,r)} |
          "(or"  ~> expr~expr <~ ")" ^^ {case l~r => Or(l,r)}  |
          "(xor" ~> expr~expr <~ ")" ^^ {case l~r => Xor(l,r)} |
          "(+"   ~> expr~expr <~ ")" ^^ {case l~r => Add(l,r)} |
          "(-"   ~> expr~expr <~ ")" ^^ {case l~r => Sub(l,r)} |
          "(*"   ~> expr~expr <~ ")" ^^ {case l~r => Mul(l,r)} |
          "(/"   ~> expr~expr <~ ")" ^^ {case l~r => Div(l,r)} |
          "(%"   ~> expr~expr <~ ")" ^^ {case l~r => Rem(l,r)} |
          "(<"   ~> expr~expr <~ ")" ^^ {case l~r => Lt(l,r)}  |
          "(>"   ~> expr~expr <~ ")" ^^ {case l~r => Gt(l,r)}  |
          "(=="  ~> expr~expr <~ ")" ^^ {case l~r => Eq(l,r)}  |
          "(if"  ~> expr~expr~expr <~ ")" ^^ {case c~l~r => If(c,l,r)} 

    def apply(text: String): Expr = {
      val result = parseAll(expr, text)
      result match {
        case Success(r, _) => r
        case Failure(msg, n) =>
          throw ParseException(msg +
            " (input left: \"" + n.source.toString.drop(n.offset) + "\")")
        case Error(msg, n) =>
          throw ParseException(msg +
            " (input left: \"" + n.source.toString.drop(n.offset) + "\")")
      }
    }
  }

  // Test driver
  // Test driver (two options)
  // 1. Input from StdIn:  linux> scala ExpLang -
  //                       ...program (in single line)...
  // 2. Input from file:   linux> scala ExpLang <filename>
  //
  import scala.io._
  def main(argv: Array[String]) = {
    try {
      val s = if (argv(0) == "-") StdIn.readLine()
              else Source.fromFile(argv(0)).getLines.mkString("\n")
      val p = ELParse(s)
      println("Input: " + s)
      println("AST:   " + p)
    } catch {
      case ex: ParseException => { println("Parser Error:" + ex.string) }
    }
  }
}    
//
