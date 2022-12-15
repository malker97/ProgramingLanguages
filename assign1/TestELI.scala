//-------------------------------------------------------------------------
// Supporting code for CS558 Programming Languages. 
// Dept of Computer Science, Portland State University
// Author: J. Li (8/22) [Based on A. Tolmach's code]
//-------------------------------------------------------------------------

// Testing ExpLang Interpreter
//
import org.scalatest.FunSuite
import ELInterp._

class TestELI extends FunSuite {

  // normal cases
  test("correctly interpret boolean expressions") {
    assertResult(Left(false))(ELInterp("(not (and (or f t) t))"))
    assertResult(Left(true))(ELInterp("(and (xor t f) (xor f t))"))
    assertResult(Left(true))(ELInterp("(xor (and t f) (or f t))"))
  }
	
  test("correctly interpret integer expressions") {
    assertResult(Right(2))(ELInterp("(+ 1 (- 3 2))"))
    assertResult(Right(11))(ELInterp("(+ 1 (* 5 (- 4 2)))"))
    assertResult(Right(9))(ELInterp("(- (* 2 5) (% 7 3))"))
  } 
  
  test("correctly interpret relational expressions") {
    assertResult(Left(true))(ELInterp("(< 1 2)"))
    assertResult(Left(false))(ELInterp("(> 3 (+ 4 5))"))
    assertResult(Left(true))(ELInterp("(== (* 2 2) (/ 8 2))"))
    assertResult(Left(false))(ELInterp("(== t f)"))
  } 
  
  test("correctly interpret if expressions") {
    assertResult(Right(2))(ELInterp("(if (not f) 2 3)"))
    assertResult(Right(3))(ELInterp("(if (> 4 5) 2 3)"))
  } 
  
  // exception cases
  test("boolean expression exceptions") {
    intercept[InterpException] { ELInterp("(and 1 t)") }
    intercept[InterpException] { ELInterp("(xor f (+ 1 2))") }
  }

  test("integer expression exceptions") {
    intercept[InterpException] { ELInterp("(+ 1 t)") }
    intercept[InterpException] { ELInterp("(/ 1 0)") }
    intercept[InterpException] { ELInterp("(% 1 (- 5 5))") }
  }

  test("relational expression exceptions") {
    intercept[InterpException] { ELInterp("(< 1 t)") }
    intercept[InterpException] { ELInterp("(== (+ 1 2) t)") }
  }

  test("if expression exceptions") {
    intercept[InterpException] { ELInterp("(if t t t)") }
    intercept[InterpException] { ELInterp("(if 1 2 3)") }
    intercept[InterpException] { ELInterp("(if t f 4)") }
  }
}
