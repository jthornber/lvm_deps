GHC=ghc --make

SOURCE=\
	DepGraph/CPPIncludes.hs \
	DepGraph/DepGraph.hs \
	DepGraph/DepMain.hs \
	Main.hs \
	RandomStuff.hs

.PHONEY: all

all: lvm_deps

lvm_deps: $(SOURCE)
	$(GHC) Main.hs -o lvm_deps
