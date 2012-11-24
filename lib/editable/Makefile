CAMLP4=
OCAMLC_OPTS=-package core
OCAMLOPT_OPTS=-for-pack Editable $(OCAMLC_OPTS)

cources=entry.ml form.ml engine.ml

sources=$(cources)

sources_mli=$(cources:%.ml=%.mli)

native_cmx=$(sources:%.ml=%.cmx)

byte_cmo=$(sources:%.ml=%.cmo)

native_cmi=$(sources_mli:%.mli=%.cmi)

.PHONY: all clean

all: native byte

native: editable.cmxa

editable.cmxa: editable.cmx

editable.cmx: $(native_cmx)
	ocamlfind ocamlopt -pack -o $@ $^

byte: editable.cma

editable.cma: editable.cmo

editable.cmo: $(byte_cmo)
	ocamlfind ocamlc -pack -o $@ $^

clean:
	-rm *.cmxa *.cmi *.cmx *.cmo *.o *.a .d

.d: $(sources) $(sources_mli)
	ocamlfind ocamldep $(OCAMLC_OPTS) $(CAMLP4) $(sources) $(sources_mli) > .d

# Dependencies
include ../Ocamlrules.mk.in

# This won't exist until it is generated, so .deps needs to run first
-include .d
