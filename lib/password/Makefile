CAMLP4=

OCAMLDEP_OPTS=-package core
OCAMLC_OPTS=$(OCAMLDEP_OPTS)
OCAMLOPT_OPTS=$(OCAMLDEP_OPTS)

LIB_MODULES=password.ml
NON_LIB_MODULES=

BYTE_TARGETS=password.cma
NATIVE_TARGETS=password.cmxa

.PHONY: all test

all: native-code byte-code

test:

# Dependencies
include ../Ocamlrules.mk.in
