.PHONY: all build clean configure install
shell = '$$SHELL'
all: install configure build

build:
	cabal build --jobs

#clean: nix-clean
clean:
	cabal clean
	if test -d .cabal-sandbox; then cabal sandbox delete; fi

configure:
	cabal configure --enable-tests

install:
	cabal sandbox init
	cabal install --enable-tests --jobs --only-dependencies --reorder-goals

#nix-clean:
#	if test -e default.nix; then rm default.nix; fi
#	if test -e shell.nix; then rm shell.nix; fi

nix-init: clean
	[ `cabal2nix --version` = "2.0" ] && cabal2nix --shell . > shell.nix;
	[ `cabal2nix --version` = "2.0" ] && cabal2nix . > default.nix;

nix-shell:
	nix-shell --command 'make install && IN_NIX="nix " $(shell)'
	make clean

repl:
	cabal repl
