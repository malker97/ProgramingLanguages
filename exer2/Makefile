# Makefile for CS558 Exercise 2
#

SC = scalac

.SUFFIXES: .scala .class

.PRECIOUS: %.scala

.scala.class:
	$(SC) $*.scala

all: testbl testbli testblc

bl:   	 BoolLang.class
bli: 	 bl BLInterp.class
sm0:     StackM0.class
blc:	 bl sm0 BLComp.class
testbl:	 bl TestBL.class
testbli: bl bli TestBLI.class
testblc: bl sm0 blc TestBLC.class

clean:	
	rm *.class
