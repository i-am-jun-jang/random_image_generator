.PHONY: test check

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

tree:
	dune exec examples/random_tree.exe  

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	rm -f rig.zip
	zip -r rig.zip . -x@exclude.lst

clean:
	dune clean
	rm -f rig.zip

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh
