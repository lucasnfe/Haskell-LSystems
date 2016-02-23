HASKEL = ghc
SOURCE = lsystems
BIN = lsystems

all:
	$(HASKEL) $(SOURCE).hs -o $(BIN)

clean:
	rm -rf *.o
	rm -rf *.hi
	rm -rf $(BIN)
