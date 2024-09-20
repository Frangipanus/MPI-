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
tests : test.cmx Prooftree.cmx
	ocamlopt -o tests Prooftree.cmx test.cmx
