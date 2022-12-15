//-------------------------------------------------------------------------
// Supporting code for CS558 Programming Languages. 
// Dept of Computer Science, Portland State University
// J. Li (8/2022) 
//-------------------------------------------------------------------------

// Testing BoolLang interpreter
//
import org.scalatest.FunSuite
import BLInterp._

class TestBLI extends FunSuite {
  
  test("correctly interpret simple exprs") {
    assertResult(false)(BLInterp("(not (and t (not f)))"))
    assertResult(true)(BLInterp("(and (or (or f f) t) t)"))
    assertResult(true)(BLInterp("(or (or (and f f) t) f)"))
    assertResult(true)(BLInterp("(and (xor t f) (xor f t))"))
    assertResult(true)(BLInterp("(xor (and t f) (or f t))"))
  }
	
}
