# Project: 		ECDSA in Haskell
# Author:  		Bc. Vít Barták 
# Login:   		xbarta47
# Year:		    2023
# GHC Version: 	9.2.5

XLOGINXX=xbarta47
BIN=flp22-fun

.PHONY: test

ecdsa: 
	ghc -o $(BIN) -Wall -no-keep-hi-files -no-keep-o-files src/*

test:
	./test_suite.sh

pack:
	zip flp-fun-$(XLOGINXX).zip Makefile LICENSE src/* test/* doc/* curves/* ./test.sh ./test_suite.sh