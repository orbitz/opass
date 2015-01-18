CAMLP4=

OCAMLDEP_OPTS=-package core,str,db,password,editable,csv
OCAMLC_OPTS=$(OCAMLDEP_OPTS)
OCAMLOPT_OPTS=$(OCAMLDEP_OPTS)

LIB_MODULES=forms.ml db_io.ml oui.ml import_1password.ml import_txt.ml import_csv.ml opass_shell.ml
NON_LIB_MODULES=opass.ml

BYTE_TARGETS=opass.byte
NATIVE_TARGETS=opass.native

.PHONY: all test

all: native-code byte-code install

install: all
	cp -vf opass.native $(PREFIX)/bin/opass

test:

# Dependencies
include ../Ocamlrules.mk.in
