CAMLP4=
OCAMLC_OPTS=-package core

cources=password.ml

sources=$(courses)

sources_mli=$(cources:%.ml=%.mli)

native_cmx=$(sources:%.ml=%.cmx)

byte_cmo=$(sources:%.ml=%.cmo)

native_cmi=$(sources_mli:%.mli=%.cmi)

.PHONY: all clean

all: native byte

native: password.cmxa

password.cmxa: $(native_cmx)

password.cmx: password.ml password.mli password.cmi

password.cmi: password.mli

byte: password.cma

password.cma: $(byte_cmo)

password.cmo: password.ml password.cmi

clean:
	-rm *.cmxa *.cmi *.cmx *.cmo *.o .d

.d: $(sources) $(sources_mli)
	ocamlfind ocamldep $(OCAMLC_OPTS) $(CAMLP4) $(sources) $(sources_mli) > .d

# Dependencies
include ../Ocamlrules.mk.in

# This won't exist until it is generated, so .deps needs to run first
-include .d
