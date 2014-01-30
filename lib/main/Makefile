CAMLP4=

OCAMLDEP_OPTS=-package core,core_extended,str,db,password,editable,csv
OCAMLC_OPTS=$(OCAMLDEP_OPTS)
OCAMLOPT_OPTS=$(OCAMLDEP_OPTS)

LIB_MODULES=forms.ml db_io.ml oui.ml import_1password.ml import_txt.ml import_csv.ml
NON_LIB_MODULES=main.ml

BYTE_TARGETS=opass.byte
NATIVE_TARGETS=opass.native

.PHONY: all test

all: native-code byte-code

test:

# Dependencies
include ../Ocamlrules.mk.in
