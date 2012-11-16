##===- Makefile --------------------------------------------*- Makefile -*-===##
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
##===----------------------------------------------------------------------===##

# If SWIFT_LEVEL is not set, then we are the top-level Makefile. Otherwise, we
# are being included from a subdirectory makefile.

ifndef SWIFT_LEVEL

IS_TOP_LEVEL := 1
SWIFT_LEVEL := .
DIRS := lib tools runtime stdlib examples benchmark unittests   # include docs

PARALLEL_DIRS :=

endif

CXX.Flags += -std=gnu++0x

ifeq ($(MAKECMDGOALS),libs-only)
  DIRS := $(filter-out tools docs, $(DIRS))
  OPTIONAL_DIRS :=
endif

###
# Common Makefile code, shared by all Swift Makefiles.

# Set LLVM source root level.
LEVEL := $(SWIFT_LEVEL)/../..

# Include LLVM common makefile.
include $(LEVEL)/Makefile.common

# Set common Swift build flags.
CPP.Flags += -I$(PROJ_SRC_DIR)/$(SWIFT_LEVEL)/include \
             -I$(PROJ_OBJ_DIR)/$(SWIFT_LEVEL)/include \
             -I$(PROJ_SRC_DIR)/$(SWIFT_LEVEL)/../clang/include \
             -I$(PROJ_OBJ_DIR)/$(SWIFT_LEVEL)/../clang/include
ifdef SWIFT_VENDOR
CPP.Flags += -DSWIFT_VENDOR='"$(SWIFT_VENDOR) "'
endif

###
# Swift Top Level specific stuff.

ifeq ($(IS_TOP_LEVEL),1)

ifneq ($(PROJ_SRC_ROOT),$(PROJ_OBJ_ROOT))
$(RecursiveTargets)::
	$(Verb) if [ ! -f test/Makefile ]; then \
	  $(MKDIR) test; \
	  $(CP) $(PROJ_SRC_DIR)/test/Makefile test/Makefile; \
	fi
endif

wc::
	wc `find . -name \*.cpp -o -name \*.h`

test::
	@ $(MAKE) -C test

report::
	@ $(MAKE) -C test report

clean::
	@ $(MAKE) -C test clean

libs-only: all

tags::
	$(Verb) etags `find . -type f -name '*.h' -or -name '*.cpp' | \
	  grep -v /lib/Headers | grep -v /test/`

cscope.files:
	find tools lib include -name '*.cpp' \
	                    -or -name '*.def' \
	                    -or -name '*.td' \
	                    -or -name '*.h' > cscope.files

.PHONY: test report clean cscope.files

endif
