SWIFT_LEVEL := ..
include $(SWIFT_LEVEL)/Makefile

# Test in all immediate subdirectories if unset.
ifdef TESTSUITE
TESTDIRS := $(TESTSUITE:%=$(PROJ_SRC_DIR)/%)
else
TESTDIRS ?= $(PROJ_SRC_DIR)
endif

# 'lit' wants objdir paths, so it will pick up the lit.site.cfg.
TESTDIRS := $(TESTDIRS:$(PROJ_SRC_DIR)%=$(PROJ_OBJ_DIR)%)

# Allow EXTRA_TESTDIRS to provide additional test directories.
TESTDIRS += $(EXTRA_TESTDIRS)

ifndef TESTARGS
ifdef VERBOSE
TESTARGS = -v
else
TESTARGS = -s -v 
endif
endif

# Make sure any extra test suites can find the main site config.
LIT_ARGS := --param swift_site_config=$(PROJ_OBJ_DIR)/lit.site.cfg

ifdef VG
  LIT_ARGS += "--vg"
endif

all:: 
	@ echo '--- Running swift tests for $(TARGET_TRIPLE) ---'
	@ $(PYTHON) $(LLVM_SRC_ROOT)/utils/lit/lit.py \
	  $(LIT_ARGS) $(TESTARGS) $(TESTDIRS)


clean::
	@ find . -name Output | xargs rm -fr

.PHONY: all report clean
