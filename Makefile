all:
	pds
	$(MAKE) -f pds.mk all

%:
	pds
	$(MAKE) -f pds.mk $*
