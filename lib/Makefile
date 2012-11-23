SUBDIRS = main db password editable

subdirs_clean = $(SUBDIRS:%=%_clean)

subdirs_test  = $(SUBDIRS:%=%_test)

.PHONY: all clean $(SUBDIRS) $(subdirs_clean)

all: $(SUBDIRS)

clean: $(subdirs_clean)

main: db password editable

$(SUBDIRS):
	$(MAKE) -C $@ .d
	$(MAKE) -C $@

$(subdirs_test):
	$(MAKE) -C $(patsubst %_test,%,$@) test

$(subdirs_clean):
	$(MAKE) -C $(patsubst %_clean,%,$@) clean

