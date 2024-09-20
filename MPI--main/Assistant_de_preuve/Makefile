.PHONY: all clean

all: Makefile.depends

Makefile.depends::
	ocamldep *.ml *.mli > $@

include Makefile.depends

clean:
	rm -f *.cmi *.cmo *.cmx *.o

%.cmx: %.ml
	ocamlopt -c -o $@ $<

%.cmi: %.mli
	ocamlopt -c -o $@ $<
tests : test.cmx prooftree.cmx
	ocamlopt -o tests prooftree.cmx test.cmx
my_tests : my_tests.cmx prooftree.cmx
	ocamlopt -o my_tests prooftree.cmx my_tests.cmx
