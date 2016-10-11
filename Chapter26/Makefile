.PHONY: depends all clean doc

all: time.cma time.cmxa doc
depend: 
	ocamldep *.ml > .depends
-include .depends

%.o:%.c
	gcc -fPIC -c -O -Wall -I/usr/local/lib/ocaml/ $<

%.cmi:%.mli
	ocamlc -c $<
%.cmo:%.ml
	ocamlc -c $<
%.cmx:%.ml
	ocamlopt -c $<
%.ml:%.idl
	camlidl -no-include $<

%_stubs.c: %.idl
	camlidl -no-include $<

time.cma: timefunctions.o time.cmo
	ocamlmklib -o time timefunctions.o time.cmo

time.cmxa: timefunctions.o time.cmx
	ocamlmklib -o time timefunctions.o time.cmx

timelib.cma: timelib.idl
	camlidl -no-include timelib.idl
	gcc -c timelib_stubs.c
	ocamlc -c timelib.mli
	ocamlc -c timelib.ml
	ocamlmklib -L/usr/lib/ocaml/3.09.1/ -o timelib timelib_stubs.o timelib.cmo -lcamlidl

install: 
	ocamlfind install time META time.cmi time.cma libtime.a -dll dlltime.so

uninstall:
	ocamlfind remove time

doc:
	-mkdir html
	ocamldoc -html -d ./html *.ml
	ocamldoc -html -d ./html *.mli
	-mkdir man
	ocamldoc -man -d ./man *.ml
	ocamldoc -man -d ./man *.mli

clean:
	-rm *.cmo *.cmi *.cmx *.o *.so *.a *.cma *.cmxa
	-rm timelib_stubs.c timelib.mli timelib.ml 
	-rm .depends
distclean:
	-rm *.cmo *.cmi *.cmx *.o *.so *.a *.cma *.cmxa
	-rm timelib_stubs.c timelib.mli timelib.ml 
	-rm .depends
	-rm *~
	-rm -rf html
	-rm -rf man
