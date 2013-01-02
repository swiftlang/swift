##===- objc/Foundation/Makefile ----------------------------*- Makefile -*-===##
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

SWIFT_LEVEL := ../..
include $(SWIFT_LEVEL)/../../Makefile.config

LIBRARYNAME := swiftFoundation
LINK_LIBS_IN_SHARED := 1
SHARED_LIBRARY := 1
SWIFT_SOURCES := Foundation.swift
SOURCES := Foundation.swift BridgeNSString.mm
NO_BUILD_ARCHIVE := 1
#USEDLIBS := swift_runtime.a

include $(SWIFT_LEVEL)/Makefile

ifeq ($(ENABLE_OPTIMIZED),1)
  SWIFT_OPTFLAG := -O2
  ifeq ($(ARCH),x86_64)
    CFLAGS += -momit-leaf-frame-pointer
    CXXFLAGS += -momit-leaf-frame-pointer
  else ifeq ($(ARCH),x86)
    CFLAGS += -momit-leaf-frame-pointer
    CXXFLAGS += -momit-leaf-frame-pointer
  endif
else
  SWIFT_OPTFLAG := -O0
endif

SWIFT_OPTFLAG := -O3
SWIFT_SDKFLAG := -sdk=$(SWIFT_SDK)

LDFLAGS += -framework Foundation \
	   -lswift_stdlib -lswift_runtime -Wl,-force_load,$(LibDir)/libswift_runtime.a

HeaderDir := $(PROJ_OBJ_ROOT)/$(BuildMode)/lib/swift/
PATH_TO_SWIFT_SOURCES := $(addprefix $(PROJ_SRC_DIR)/, $(SWIFT_SOURCES))
$(HeaderDir)/Foundation.swift: $(PATH_TO_SWIFT_SOURCES) $(HeaderDir)/.dir
	$(Echo) FIXME -- HACK -- Concatenating $(SWIFT_SOURCES) to build dir
	$(Verb) cat $(PATH_TO_SWIFT_SOURCES) > $@

SWIFT_COMPILER := $(LLVMToolDir)/swift$(EXEEXT)
$(ObjDir)/Foundation.o: $(HeaderDir)/Foundation.swift $(ObjDir)/.dir $(BUILT_SOURCES) $(PROJ_MAKEFILE) $(SWIFT_COMPILER) $(LibDir)/libswift_runtime.a
	$(Echo) "Compiling $(notdir $<) for $(BuildMode) build"
	$(Verb) $(SWIFT_COMPILER) -c $(SWIFT_SDKFLAG) $< -o $@ -parse-as-library -constraint-checker $(SWIFT_OPTFLAG)

all-local:: $(HeaderDir)/Foundation.swift
