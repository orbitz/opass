CAMLP4=
OCAMLC_OPTS=-package core,core_extended,db

cources=

sources=$(courses) main.ml

sources_mli=$(cources:%.ml=%.mli)

native_cmx=$(sources:%.ml=%.cmx)

native_cmi=$(sources_mli:%.mli=%.cmi)

.PHONY: all clean

all: opass

opass: $(native_cmx) $(native_cmi)
	ocamlfind ocamlopt $(OCAMLC_OPTS) -thread -linkpkg -o opass $(native_cmx)

clean:
	-rm opass *.cmxa *.cmi *.cmx *.cmo *.o .d

.d: $(sources) $(sources_mli)
	ocamlfind ocamldep $(OCAMLC_OPTS) $(CAMLP4) $(sources) $(sources_mli) > .d

# Dependencies
include ../Ocamlrules.mk.in

# This won't exist until it is generated, so .deps needs to run first
-include .d
