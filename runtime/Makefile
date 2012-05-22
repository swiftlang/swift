##===- runtime/Makefile ------------------------------------*- Makefile -*-===##
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

SWIFT_LEVEL := ..

LIBRARYNAME := swift_runtime
LINK_LIBS_IN_SHARED := 1
SHARED_LIBRARY := 1
SOURCES := Alloc.cpp Stubs.cpp ObjCBridge.mm swift.swift
NO_BUILD_ARCHIVE := 1

include $(SWIFT_LEVEL)/../../Makefile.config

include $(SWIFT_LEVEL)/Makefile

ifeq ($(ENABLE_OPTIMIZED),1)
  SWIFT_OPTFLAG := -O2
  ifeq ($(ARCH),x86_64)
    CFLAGS += -momit-leaf-frame-pointer
    CXXFLAGS += -momit-leaf-frame-pointer
  endif
  ifeq ($(ARCH),x86)
    CFLAGS += -momit-leaf-frame-pointer
    CXXFLAGS += -momit-leaf-frame-pointer
  endif
else
  SWIFT_OPTFLAG := -O0
endif

LDFLAGS += -framework Foundation

SWIFT_COMPILER := $(LLVMToolDir)/swift$(EXEEXT)
$(ObjDir)/%.o: %.swift $(ObjDir)/.dir $(BUILT_SOURCES) $(PROJ_MAKEFILE) $(SWIFT_COMPILER)
	$(Echo) "Compiling $*.swift for $(BuildMode) build"
	$(Verb) $(SWIFT_COMPILER) -c $< -o $(ObjDir)/swift.o -parse-as-library $(SWIFT_OPTFLAG)

HeaderDir := $(PROJ_OBJ_ROOT)/$(BuildMode)/lib/swift/
$(HeaderDir)/swift.swift: $(PROJ_SRC_DIR)/swift.swift $(HeaderDir)/.dir
	$(Verb) cp $< $@
	$(Echo) Copying $(notdir $<) to build dir

all-local:: $(HeaderDir)/swift.swift
