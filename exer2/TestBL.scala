//-------------------------------------------------------------------------
// Supporting code for CS558 Programming Languages. 
// Dept of Computer Science, Portland State University
// J. Li (8/2022)
//-------------------------------------------------------------------------

// Testing BoolLang
//
import org.scalatest.FunSuite
import BoolLang._

class TestBL extends FunSuite {
  
  // normal cases
  test("parse individual ops") {
    assert(BLParse("(not t)") == Not(True))
    assert(BLParse("(and t f)") == And(True,False))
    assert(BLParse("(or t f)") == Or(True,False))
    assert(BLParse("(xor t f)") == Xor(True,False))
  }
  
  test("parse exprs") {
    assert(BLParse("(not (or (or t t) (and f f)))") == 
           Not(Or(Or(True,True),And(False,False))))
    assert(BLParse("(and (or (xor (or t f) f) t) t)") ==
           And(Or(Xor(Or(True,False),False),True),True))
  }
  
  // exception cases
  test("parse exception for expression with 3 arguments") {
    intercept[ParseException] { BLParse("(and t t f)") }
  }
  
  test("parse exception for expression with 1 argument") {
    intercept[ParseException] { BLParse("(or f)") }
  }
  
  test("parse exception for expression with no argument") {
    intercept[ParseException] { BLParse("(not)") }
  }
  
  test("parse exception for empty expression") {
    intercept[ParseException] { BLParse("()") }
  }
  
}
