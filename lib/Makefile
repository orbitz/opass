SUBDIRS = db main

subdirs_clean = $(all_apps:%=%_clean)

subdirs_test  = $(all_apps:%=%_test)

.PHONY: all test clean $(SUBDIRS) $(subdirs_clean) \

all: $(SUBDIRS)

test: $(subdirs_test)

clean: $(subdirs_clean)

main: db

$(SUBDIRS):
	$(MAKE) -C $@ .d
	$(MAKE) -C $@

$(subdirs_test):
	$(MAKE) -C $(patsubst %_test,%,$@) test

$(subdirs_clean):
	$(MAKE) -C $(patsubst %_clean,%,$@) clean

