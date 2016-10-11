SOURCES = crawler.ml
RESULT  = crawler
all: byte-code-library
THREADS = yes
PACKS = unix str
-include /usr/share/ocaml-tools/OCamlMakefile

crawler_client: test.ml crawler.cma
	ocamlc -o $@ unix.cma -thread threads.cma str.cma crawler.cma $<

distclean: clean
	-rm test test.cmi test.cmo *~
