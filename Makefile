CABAL-CONFIGURE-FLAGS 	:= --user
CABAL-BUILD-FLAGS     	:=
VERSION					:= 0.0.5

AG						:= src/MF/Languages/PHP/AG.ag src/MF/Languages/PHP/AG/Base.ag src/MF/Languages/PHP/AG/Flow.ag src/MF/Languages/PHP/AG/Visualizer.ag src/MF/Languages/PHP/AG/PP.ag src/MF/Languages/PHP/AG/Checking.ag src/MF/Languages/PHP/AG/Typing.ag src/MF/Languages/PHP/AG/Simplify.ag src/MF/Languages/PHP/AG/Debugging.ag 

all : haskell

src/CCO/Imp/AG.hs : $(AG)
	uuagc -Hdcfws --self -P src/MF/Languages/PHP src/MF/Languages/PHP/AG.ag

haskell : src/CCO/Imp/AG.hs
	cabal install

dist:
	tar tfz imp-$(VERSION).tar.gz $(AG)

.PHONY : haskell dist
