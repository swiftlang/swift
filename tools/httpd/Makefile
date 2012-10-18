##===- tools/httpd/Makefile --------------------------------*- Makefile -*-===##
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

TOOLNAME := httpd
SOURCES := httpd.swift

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

LDFLAGS += -framework Foundation -lswift_runtime

SWIFT_COMPILER := $(LLVMToolDir)/swift$(EXEEXT)
$(ObjDir)/%.o: %.swift $(ObjDir)/.dir $(BUILT_SOURCES) $(PROJ_MAKEFILE) $(SWIFT_COMPILER)
	$(Echo) "Compiling $(notdir $<) for $(BuildMode) build"
	$(Verb) $(SWIFT_COMPILER) -c $< -o $@ $(SWIFT_OPTFLAG)

$(ObjDir)/%.o: %.s $(ObjDir)/.dir $(BUILT_SOURCES) $(PROJ_MAKEFILE)
	$(Echo) "Compiling $*.s for $(BuildMode) build" $(PIC_FLAG)
	$(Verb) if $(Compile.C) $(DEPEND_OPTIONS) $< -o $(ObjDir)/$*.o ; \
	        $(DEPEND_MOVEFILE)

#all-local:: httpd
