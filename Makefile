#export OCAMLPATH:=$(PWD)/lib:$(OCAMLPATH)

.PHONY: all clean

all:
	$(MAKE) -C lib

clean:
	$(MAKE) -C lib clean

