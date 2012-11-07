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
include $(SWIFT_LEVEL)/../../Makefile.config

LIBRARYNAME := swift_runtime
SOURCES := FastEntryPoints.s Alloc.cpp KnownMetadata.cpp \
	   Metadata.cpp Stubs.cpp ObjCBridge.mm SwiftObject.mm

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

SWIFT_OPTFLAG := -O3

$(ObjDir)/%.o: %.s $(ObjDir)/.dir $(BUILT_SOURCES) $(PROJ_MAKEFILE)
	$(Echo) "Compiling $*.s for $(BuildMode) build" $(PIC_FLAG)
	$(Verb) if $(Compile.C) $(DEPEND_OPTIONS) $< -o $(ObjDir)/$*.o ; \
	        $(DEPEND_MOVEFILE)
