FSCBIN := $(shell dirname `which fsc`)
FSC = $(FSCBIN)/fsc

.PHONY: clean


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
