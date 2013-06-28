##===- stdlib/Makefile -------------------------------------*- Makefile -*-===##
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

LIBRARYNAME := swift_stdlib
LINK_LIBS_IN_SHARED := 1
SHARED_LIBRARY := 1

#
# Determine SWIFT_SOURCES, the list of source files in the swift library.
#
# sed is used here to strip comments.
SWIFT_SOURCES := $(shell sed -e "s/\#.*//g" "$(PROJ_SRC_DIR)/tools/swift/stdlib/SwiftLibSources.txt" )

POSIX_SOURCES := $(shell sed -e "s/\#.*//g" "$(PROJ_SRC_DIR)/tools/swift/stdlib/POSIXLibSources.txt" )

# Replace Word.swift with Word64.swift or Word32.swift, according to
# the target architecture
ifeq ($(ARCH),x86_64)
  WORD_SIZE := 64
endif
ifeq ($(ARCH),x86)
  WORD_SIZE := 32
endif
SWIFT_SOURCES := $(subst Word.swift,Word$(WORD_SIZE).swift,$(SWIFT_SOURCES))

# Replace Assert.swift with AssertDebug.swift if assertions are enabled
ifneq ($(DISABLE_ASSERTIONS),1)
  SWIFT_SOURCES := $(subst Assert.swift,AssertDebug.swift,$(SWIFT_SOURCES))
endif

# Remove Float80.swift on non-x86 architectures
ifneq ($(ARCH),x86_64)
	ifneq ($(ARCH),x86)
	  SWIFT_SOURCES := $(filter-out core/Float80.swift,$(SWIFT_SOURCES))
	endif
endif
#
# Done with SWIFT_SOURCES
#

SOURCES := swift.swift POSIX.swift
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
	   -lswift_runtime -Wl,-force_load,$(LibDir)/libswift_runtime.a

PATH_TO_SWIFT_SOURCES := $(addprefix $(PROJ_SRC_DIR)/, $(SWIFT_SOURCES))
PATH_TO_POSIX_SOURCES := $(addprefix $(PROJ_SRC_DIR)/, $(POSIX_SOURCES))

$(SWIFT_HEADER_DIR)/swift.swift: $(PATH_TO_SWIFT_SOURCES) $(SWIFT_HEADER_DIR)
	$(Echo) FIXME -- HACK -- Concatenating $(SWIFT_SOURCES) to build dir
	$(Verb) cat $(PATH_TO_SWIFT_SOURCES) > $@

$(SWIFT_HEADER_DIR)/POSIX.swift: $(PATH_TO_POSIX_SOURCES) $(SWIFT_HEADER_DIR)
	$(Echo) FIXME -- HACK -- Concatenating $(POSIX_SOURCES) to build dir
	$(Verb) cat $(PATH_TO_POSIX_SOURCES) > $@

SWIFT_COMPILER := $(LLVMToolDir)/swift$(EXEEXT)

$(ObjDir)/swift.o: $(SWIFT_HEADER_DIR)/swift.swift $(ObjDir)/.dir $(BUILT_SOURCES) $(PROJ_MAKEFILE) $(SWIFT_COMPILER) $(LibDir)/libswift_runtime.a
	$(Echo) "Compiling $(notdir $<) for $(BuildMode) build"
	$(Verb) $(SWIFT_COMPILER) -o $@ -parse-stdlib -parse-as-library $(PATH_TO_SWIFT_SOURCES)

$(ObjDir)/POSIX.o: $(SWIFT_HEADER_DIR)/POSIX.swift $(ObjDir)/.dir $(BUILT_SOURCES) $(PROJ_MAKEFILE) $(SWIFT_COMPILER) $(LibDir)/libswift_runtime.a $(ObjDir)/swift.o
	$(Echo) "Compiling $(notdir $<) for $(BuildMode) build"
	$(Verb) $(SWIFT_COMPILER) -o $@ -parse-as-library $(PATH_TO_POSIX_SOURCES)

$(SharedLibDir)/swift/%.$(SHLIBEXT): $(SharedLibDir)/swift/.dir
	ln -s ../$(@F) $@

$(SharedLibDir)/%.$(SHLIBEXT): $(SharedLibDir)/swift/%.$(SHLIBEXT)

all-local:: $(SWIFT_HEADER_DIR)/swift.swift $(SWIFT_HEADER_DIR)/POSIX.swift
