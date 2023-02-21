# Project: 		ECDSA in Haskell
# Author:  		Bc. Vít Barták 
# Login:   		xbarta47
# Year:		    2023
# GHC Version: 	9.2.5

XLOGINXX=xbarta47
BIN=flp22-fun

ecdsa: 
	ghc -o $(BIN) -Wall -no-keep-hi-files -no-keep-o-files src/*

test:
	echo "test"

pack:
	zip $(XLOGINXX).zip LICENSE Makefile README.md *.hs