CAMLP4=

OCAMLDEP_OPTS= -package core,sexplib,sexplib.syntax -syntax camlp4o
OCAMLC_OPTS=$(OCAMLDEP_OPTS)
OCAMLOPT_OPTS=$(OCAMLC_OPTS)

LIB_MODULES=db.ml
NON_LIB_MODULES=

BYTE_TARGETS=db.cma
NATIVE_TARGETS=db.cmxa

.PHONY: all test install

all: native-code byte-code

install:

test:

# Dependencies
include ../Ocamlrules.mk.in
