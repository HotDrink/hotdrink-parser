# Umbrella Makefile.

MAKEDIR := make
include $(MAKEDIR)/Makefile.common

PRIMARY := parser
BUILDDIR := build

##################################################
# self configuration (do not touch)

export MAKEDIR
export BUILDDIR

##################################################
# targets

.PHONY : all debug release doc test

all :
	@$(call defer,$(MAKEDIR)/Makefile.$(PRIMARY))

debug :
	@$(call defer,$(MAKEDIR)/Makefile.$(PRIMARY))

release :
	@$(call defer,$(MAKEDIR)/Makefile.$(PRIMARY))

doc :
	@$(call defer,$(MAKEDIR)/Makefile.$(PRIMARY))

test :
	@$(MAKE) -f $(MAKEDIR)/Makefile.test

##################################################
# cleaning

.PHONY : clean clean-obj clean-exe clean-doc clean-test

clean : clean-exe
	@$(MAKE) -f $(MAKEDIR)/Makefile.test clean-exe
	-rm -rf $(BUILDDIR)

clean-obj :
	@$(call defer,$(MAKEDIR)/Makefile.$(PRIMARY))

clean-exe :
	@$(call defer,$(MAKEDIR)/Makefile.$(PRIMARY))

clean-doc :
	@$(call defer,$(MAKEDIR)/Makefile.$(PRIMARY))

clean-test :
	@$(MAKE) -f $(MAKEDIR)/Makefile.test clean

