CONTEXTSTACK=100
bf: Main.hs BF/*.hs
	ghc Main -odir=obj -hidir=obj -fcontext-stack=$(CONTEXTSTACK) -o bf
	rm -r obj
