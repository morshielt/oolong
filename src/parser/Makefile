all:
	happy -gca ParOolong.y
	alex -g LexOolong.x
	ghc --make TestOolong.hs -o TestOolong

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocOolong.* LexOolong.* ParOolong.* LayoutOolong.* SkelOolong.* PrintOolong.* TestOolong.* AbsOolong.* TestOolong ErrM.* SharedString.* ComposOp.* oolong.dtd XMLOolong.* Makefile*
	
