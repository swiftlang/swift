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

DIRS := lib tools   # include docs
PARALLEL_DIRS :=

# See compiler-rt/Makefile for a description of the problems with building 
# target-side libraries with this build machinery.
SWIFT_LIBDIRS := stdlib benchmark unittests

ifeq ($(NO_RUNTIME_LIBS),1)
  # No-runtime build. No stdlib or examples. 
  $(warning Not building Swift stdlib because NO_RUNTIME_LIBS is set.)
else
  DIRS += $(SWIFT_LIBDIRS)
  ifeq ($(BUILD_EXAMPLES),1)
    PARALLEL_DIRS += examples
  endif
endif

ifeq ($(MAKECMDGOALS),libs-only)
  DIRS := $(filter-out tools docs, $(DIRS))
  OPTIONAL_DIRS :=
endif

endif


LEVEL := $(SWIFT_LEVEL)/../..
include $(LEVEL)/Makefile.common


###
# Paths to Swift tools and files

ifndef SWIFT_COMPILER
  ifeq ($(LLVM_CROSS_COMPILING),1)
    SWIFT_COMPILER := $(shell xcrun --find swift)
  else
    SWIFT_COMPILER := $(LLVMToolDir)/swift$(EXEEXT)
  endif
endif

ifndef MODULES_SDK
  ifdef SDKROOT
    MODULES_SDK := $(SDKROOT)
  else ifneq ($(findstring -darwin_ios,$(TARGET_TRIPLE)),)
    MODULES_SDK := $(shell xcrun --sdk iphoneos --show-sdk-path)
  else ifneq ($(findstring -darwin_sim,$(TARGET_TRIPLE)),)
    MODULES_SDK := $(shell xcrun --sdk iphonesimulator --show-sdk-path)
  else ifneq ($(findstring -darwin,$(TARGET_TRIPLE)),)
    MODULES_SDK := $(shell xcrun --sdk macosx --show-sdk-path)
  else
    MODULES_SDK := 
  endif
endif

ifndef SWIFT_HEADER_DIR
  SWIFT_HEADER_DIR := $(PROJ_OBJ_ROOT)/$(BuildMode)/lib/swift
endif


###
# Settings for .swift files in Swift compiler and stdlib

# Debug symbols for .swift files
ifeq ($(DEBUG_SYMBOLS),1)
  SWIFT_DEBUGFLAG := -g
else
  SWIFT_DEBUGFLAG :=
endif

ifndef SWIFT_OPTIMIZED
  SWIFT_OPTIMIZED := 1
endif

# Optimization flags for .swift files
ifeq ($(SWIFT_OPTIMIZED),1)
  SWIFT_OPTFLAG := -O3
else
  SWIFT_OPTFLAG := -O0
endif

# All options for .swift files
SWIFT_FLAGS := $(SWIFT_DEBUGFLAG) $(SWIFT_OPTFLAG) -triple $(TARGET_TRIPLE) -I=$(SWIFT_HEADER_DIR) -sdk=$(MODULES_SDK)


###
# Settings for C files in Swift compiler and stdlib

CXX.Flags += -std=gnu++0x -Wno-nested-anon-types -Wdocumentation

CPP.Flags += -I$(PROJ_SRC_DIR)/$(SWIFT_LEVEL)/include \
             -I$(PROJ_OBJ_DIR)/$(SWIFT_LEVEL)/include
ifneq ($(CLANG_SRC_ROOT),)
CPP.Flags += -I$(CLANG_SRC_ROOT)/include \
             -I$(LLVM_OBJ_ROOT)/tools/clang/include
else
CPP.Flags += -I$(LLVM_SRC_ROOT)/tools/clang/include \
             -I$(LLVM_OBJ_ROOT)/tools/clang/include
endif
ifdef SWIFT_VENDOR
CPP.Flags += -DSWIFT_VENDOR='"$(SWIFT_VENDOR) "'
endif

ifeq ($(ENABLE_OPTIMIZED),1)
  ifeq ($(ARCH),x86_64)
    CFLAGS += -momit-leaf-frame-pointer
    CXXFLAGS += -momit-leaf-frame-pointer
  endif
  ifeq ($(ARCH),x86)
    CFLAGS += -momit-leaf-frame-pointer
    CXXFLAGS += -momit-leaf-frame-pointer
  endif
endif


###
# Create a symlink lib/swift to lib so that we can provide a consistent
# path schema for installed and build-directory environments.

$(SWIFT_HEADER_DIR):
	ln -s . $(SWIFT_HEADER_DIR)

# Create lib/swift symlink when building the swift compiler
# in case we're cross-compiling and not building the libraries
$(ToolDir)/swift$(EXEEXT): $(SWIFT_HEADER_DIR)


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

test-perf::
	@ $(MAKE) -C test test-perf

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

.PHONY: test clean cscope.files

endif
