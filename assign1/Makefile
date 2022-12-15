# Makefile for ExpLang
#

SC = scalac

.SUFFIXES: .scala .class

.PRECIOUS: %.scala

.scala.class:
	$(SC) $*.scala

all: testeli testelc

el:   	 ExpLang.class
sm1:     StackM1.class
eli: 	 el ELInterp.class
elc:	 el sm1 ELComp.class
testeli: el eli TestELI.class
testelc: el sm1 elc TestELC.class

clean:	
	rm *.class
