FSCBIN := $(shell dirname `which fsc`)
FSC = $(FSCBIN)/fsc

.PHONY: clean


all: AI.class AlphaBetaAI.class

AI.class:
	$(FSC) AI.scala
AlphaBetaAI.class: BoardTest.class
	$(FSC) AlphaBetaAI.scala
BoardTest.class: ConsoleBoard.class
	$(FSC) BoardTest.scala
ConsoleBoard.class: Board.class
	$(FSC) ConsoleBoard.scala
Board.class: ColorStorage.class
	$(FSC) Board.scala
ColorStorage.class: Disc.class
	$(FSC) ColorStorage.scala
Disc.class: Point.class
	$(FSC) Disc.scala
Point.class:
	$(FSC) Point.scala


clean:
	rm -f *.class
