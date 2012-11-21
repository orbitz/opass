CAMLP4=
OCAMLC_OPTS=-package core,sexplib,sexplib.syntax -syntax camlp4o

cources=db.ml

sources=$(courses)

sources_mli=$(cources:%.ml=%.mli)

native_cmx=$(sources:%.ml=%.cmx)

native_cmi=$(sources_mli:%.mli=%.cmi)

.PHONY: all clean

all: native byte

native: db.cmxa

db.cmxa: db.cmx

db.cmx: db.ml db.mli db.cmi

db.cmi: db.mli

byte: db.cma

db.cma: db.cmo

db.cmo: db.ml db.mli db.cmi

clean:
	-rm *.cmxa *.cmi *.cmx *.cmo *.o .d

.d: $(sources) $(sources_mli)
	ocamlfind ocamldep $(OCAMLC_OPTS) $(CAMLP4) $(sources) $(sources_mli) > .d

# Dependencies
include ../Ocamlrules.mk.in

# This won't exist until it is generated, so .deps needs to run first
-include .d
