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
SWIFT_SOURCES := Policy.swift Bool.swift
ifeq ($(ARCH),x86_64)
  SWIFT_SOURCES += Word64.swift
endif
ifeq ($(ARCH),x86)
  SWIFT_SOURCES += Word32.swift
endif
SWIFT_SOURCES += FixedPoint.swift Float32.swift Float64.swift \
		 Object.swift CTypes.swift Misc.swift Range.swift \
		 UnsafePointer.swift \
		 Slice.swift StringByte.swift String.swift Char.swift \
		 Dictionary.swift Vector.swift \
		 Algorithm.swift \
		 Demangle.swift InputStream.swift OutputStream.swift \
		 CString.swift \
		 POSIX/Descriptor.swift POSIX/File.swift POSIX/Misc.swift \
		 REPL.swift Environment.swift
ifeq ($(DISABLE_ASSERTIONS),1)
  SWIFT_SOURCES += Assert.swift
else
  SWIFT_SOURCES += AssertDebug.swift
endif
ifeq ($(ARCH),x86_64)
  SWIFT_SOURCES += Float80.swift
endif
ifeq ($(ARCH),x86)
  SWIFT_SOURCES += Float80.swift
endif
SOURCES := swift.swift
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

$(SWIFT_HEADER_DIR)/swift.swift: $(PATH_TO_SWIFT_SOURCES) $(SWIFT_HEADER_DIR)
	$(Echo) FIXME -- HACK -- Concatenating $(SWIFT_SOURCES) to build dir
	$(Verb) cat $(PATH_TO_SWIFT_SOURCES) > $@

SWIFT_COMPILER := $(LLVMToolDir)/swift$(EXEEXT)
$(ObjDir)/swift.o: $(SWIFT_HEADER_DIR)/swift.swift $(ObjDir)/.dir $(BUILT_SOURCES) $(PROJ_MAKEFILE) $(SWIFT_COMPILER) $(LibDir)/libswift_runtime.a
	$(Echo) "Compiling $(notdir $<) for $(BuildMode) build"
	$(Verb) $(SWIFT_COMPILER) -o $@ -parse-as-library $(PATH_TO_SWIFT_SOURCES)

$(SharedLibDir)/swift/%.$(SHLIBEXT): $(SharedLibDir)/swift/.dir
	ln -s ../$(@F) $@

$(SharedLibDir)/%.$(SHLIBEXT): $(SharedLibDir)/swift/%.$(SHLIBEXT)

all-local:: $(SWIFT_HEADER_DIR)/swift.swift
