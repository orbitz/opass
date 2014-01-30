CAMLP4=
PACK=editable
OCAMLDEP_OPTS=-package core
OCAMLC_OPTS=$(OCAMLDEP_OPTS)
OCAMLOPT_OPTS=$(OCAMLDEP_OPTS)

LIB_MODULES=entry.ml form.ml engine.ml 
NON_LIB_MODULES=

BYTE_TARGETS=editable.cma
NATIVE_TARGETS=editable.cmxa

.PHONY: all test

all: native-code byte-code

test:

# Dependencies
include ../Ocamlrules.mk.in
