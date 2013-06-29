##===- stdlib/POSIX/Makefile -------------------------------*- Makefile -*-===##
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

LIBRARYNAME := swift_stdlib_posix
LINK_LIBS_IN_SHARED := 1
SHARED_LIBRARY := 1

#
# Determine POSIX_SOURCES, the list of source files in the POSIX library.
#
# sed is used here to strip comments.
POSIX_SOURCES := $(shell sed -e "s/\#.*//g" "$(PROJ_SRC_DIR)/tools/swift/stdlib/POSIX/POSIXLibSources.txt" )

SOURCES := POSIX.swift
NO_BUILD_ARCHIVE := 1
#USEDLIBS := swift_runtime.a

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

LDFLAGS += -framework Foundation \
	   -lswift_stdlib_core

PATH_TO_POSIX_SOURCES := $(addprefix $(PROJ_SRC_DIR)/, $(POSIX_SOURCES))

$(SWIFT_HEADER_DIR)/POSIX.swift: $(PATH_TO_POSIX_SOURCES) $(SWIFT_HEADER_DIR)
	$(Echo) FIXME -- HACK -- Concatenating $(POSIX_SOURCES) to build dir
	$(Verb) cat $(PATH_TO_POSIX_SOURCES) > $@

SWIFT_COMPILER := $(LLVMToolDir)/swift$(EXEEXT)

$(ObjDir)/POSIX.o: $(SWIFT_HEADER_DIR)/POSIX.swift $(ObjDir)/.dir $(BUILT_SOURCES) $(PROJ_MAKEFILE) $(SWIFT_COMPILER) $(LibDir)/libswift_runtime.a
	$(Echo) "Compiling $(notdir $<) for $(BuildMode) build"
	$(Verb) $(SWIFT_COMPILER) -o $@ -parse-as-library $(PATH_TO_POSIX_SOURCES)

$(SharedLibDir)/swift/%.$(SHLIBEXT): $(SharedLibDir)/swift/.dir
	ln -s ../$(@F) $@

$(SharedLibDir)/%.$(SHLIBEXT): $(SharedLibDir)/swift/%.$(SHLIBEXT)

all-local:: $(SWIFT_HEADER_DIR)/POSIX.swift
