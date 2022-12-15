//-------------------------------------------------------------------------
// Supporting code for CS558 Programming Languages. 
// Dept of Computer Science, Portland State University
// J. Li (8/22)
//-------------------------------------------------------------------------

// BoolLang - A toy Boolean-expresion language
//
// Grammar:
//   Expr -> t
//        |  f
//        |  (not Expr)
//        |  (and Expr Expr)
//        |  (or Expr Expr)
//        |  (xor Expr Expr)
//
import scala.util.parsing.combinator._

object BoolLang {
  sealed abstract class Expr
  case object True extends Expr
  case object False extends Expr
  case class Not(e:Expr) extends Expr
  case class And(l:Expr,r:Expr) extends Expr
  case class Or(l:Expr,r:Expr) extends Expr
  case class Xor(l:Expr,r:Expr) extends Expr

  case class ParseException(string: String) extends RuntimeException

  object BLParse extends JavaTokenParsers {
    // input program is in s-expression form
    def expr: Parser[Expr] = atm | lst
    def atm: Parser[Expr] = "t" ^^ {_ => True} | "f" ^^ {_ => False}
    def lst: Parser[Expr] = 
          "(not" ~> expr <~ ")"      ^^ {e => Not(e)}          |
          "(and" ~> expr~expr <~ ")" ^^ {case l~r => And(l,r)} |
          "(or"  ~> expr~expr <~ ")" ^^ {case l~r => Or(l,r)}  |
          "(xor" ~> expr~expr <~ ")" ^^ {case l~r => Xor(l,r)}

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

  // Test driver (two options)
  // 1. Input from StdIn:  linux> scala BoolLang -
  //                       ...program (in single line)...
  // 2. Input from file:   linux> scala BoolLang <filename>
  //
  import scala.io._
  def main(argv: Array[String]) = {
    try {
      val s = if (argv(0) == "-") StdIn.readLine()
              else Source.fromFile(argv(0)).getLines.mkString("\n")
      val p = BLParse(s)
      println("Input: " + s)
      println("AST:   " + p)
    } catch {
      case ex: ParseException => { println("Parser Error:" + ex.string) }
    }
  }
}    
//
