HC      = ghc
CFLAGS  = -fobjc-arc -I$(shell $(HC) --print-libdir)/include
HCFLAGS =
LDFLAGS = -package template-haskell -package language-c-quote -package language-c-inline -framework Foundation

OBJS = Main.o Main_objc.o

default: Marshal

%.o: %.hs
	$(HC) -c $< $(HCFLAGS)

Main.o:

Main_objc.m: Main.o

Marshal: $(OBJS)
	$(HC) -o $@ $^ $(LDFLAGS)

.PHONY: clean

clean:
	rm -f *.o *.hi Main_objc.[hm] Marshal
