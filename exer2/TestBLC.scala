//-------------------------------------------------------------------------
// Supporting code for CS558 Programming Languages. 
// Dept of Computer Science, Portland State University
// J. Li (8/2022) 
//-------------------------------------------------------------------------

// Testing BoolLang interpreter
//
import org.scalatest.FunSuite
import BLComp._
class TestBLC extends FunSuite {
  
  test("correctly interpret simple exprs") {
    assertResult(false)(BLComp("(not (and t (not f)))"))
    assertResult(true)(BLComp("(and (or (or f f) t) t)"))
    assertResult(true)(BLComp("(or (or (and f f) t) f)"))
    assertResult(true)(BLComp("(and (xor t f) (xor f t))"))
    assertResult(true)(BLComp("(xor (and t f) (or f t))"))
  }
	
}