##===- stdlib/core/Makefile --------------------------------*- Makefile -*-===##
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

LIBRARYNAME := swift_stdlib_core
LINK_LIBS_IN_SHARED := 1
SHARED_LIBRARY := 1

#
# Determine SWIFT_SOURCES, the list of source files in the swift library.
#
# sed is used here to strip comments.
SWIFT_SOURCES := $(shell sed -e "s/\#.*//g" "$(PROJ_SRC_DIR)/tools/swift/stdlib/core/SwiftLibSources.txt" )
AXLE_SWIFT_SOURCES := $(shell sed -e "s/\#.*//g" "$(PROJ_SRC_DIR)/tools/swift/stdlib/core/AxleSwiftLibSources.txt" )

# Replace Word.swift with Word64.swift or Word32.swift, according to
# the target architecture
ifeq ($(ARCH),x86_64)
  WORD_SIZE := 64
else ifeq ($(ARCH),x86)
  WORD_SIZE := 32
else ifeq ($(ARCH),ARM64)
  WORD_SIZE := 64
else ifeq ($(ARCH),ARM)
  WORD_SIZE := 32
else
  $(error unknown architecture $(ARCH))
endif
SWIFT_SOURCES := $(subst WordXX.swift,Word$(WORD_SIZE).swift,$(SWIFT_SOURCES))

# Replace Assert.swift with AssertDebug.swift if assertions are enabled
ifneq ($(DISABLE_ASSERTIONS),1)
  SWIFT_SOURCES := $(subst Assert.swift,AssertDebug.swift,$(SWIFT_SOURCES))
endif

# Compile only 32-bit and 64-bit floating point on non-x86 architectures
ifneq ($(ARCH),x86_64)
  ifneq ($(ARCH),x86)
    SWIFT_SOURCES := $(subst FloatAll.swift,FloatDouble.swift,$(SWIFT_SOURCES))
  endif
endif

#
# Done with SWIFT_SOURCES
#

SOURCES := swift.swift
NO_BUILD_ARCHIVE := 1
#USEDLIBS := swift_runtime.a

include $(SWIFT_LEVEL)/Makefile

LDFLAGS += -Xlinker -reexport-lobjc \
	   -Xlinker -force_load -Xlinker $(LibDir)/libswift_runtime.a

PATH_TO_SWIFT_SOURCES := $(addprefix $(PROJ_SRC_DIR)/, $(SWIFT_SOURCES))

$(SWIFT_HEADER_DIR)/swift.swiftmodule: $(ObjDir)/swift.o $(SWIFT_HEADER_DIR)

$(ObjDir)/swift.o: $(PATH_TO_SWIFT_SOURCES) $(ObjDir)/.dir $(BUILT_SOURCES) $(PROJ_MAKEFILE) $(SWIFT_COMPILER) $(LibDir)/libswift_runtime.a
	$(Echo) "Compiling $(notdir $@) for $(BuildMode) build"
	$(Verb) $(SWIFT_COMPILER) $(SWIFT_FLAGS) -module-name=swift -emit-module -module-link-name $(LIBRARYNAME) -parse-stdlib -parse-as-library $(PATH_TO_SWIFT_SOURCES) -c -o $@ 
	@mv $(ObjDir)/swift.swiftmodule $(SWIFT_HEADER_DIR)


# Build Axle version of the stdlib.
AXLE_SWIFT_SOURCES := $(subst WordXX.swift,Word$(WORD_SIZE).swift,$(AXLE_SWIFT_SOURCES))
ifneq ($(DISABLE_ASSERTIONS),1)
  AXLE_SWIFT_SOURCES := $(subst Assert.swift,AssertDebug.swift,$(AXLE_SWIFT_SOURCES))
endif

PATH_TO_AXLE_SWIFT_SOURCES := $(addprefix $(PROJ_SRC_DIR)/, $(AXLE_SWIFT_SOURCES))
$(AXLE_SWIFT_HEADER_DIR)/swift.swiftmodule: $(ObjDir)/swift_axle.o $(AXLE_SWIFT_HEADER_DIR)

$(AXLE_SWIFT_HEADER_DIR):
	$(MKDIR) $(AXLE_SWIFT_HEADER_DIR)

# Serialize builds of swift module since they are using the same module name.
$(ObjDir)/swift_axle.o: $(PATH_TO_AXLE_SWIFT_SOURCES) $(ObjDir)/.dir $(BUILT_SOURCES) $(PROJ_MAKEFILE) $(SWIFT_COMPILER) $(LibDir)/libswift_runtime.a $(AXLE_SWIFT_HEADER_DIR) $(ObjDir)/swift.o 
	$(Echo) "Compiling $(notdir $@) for $(BuildMode) build"
	$(Verb) $(SWIFT_COMPILER) $(SWIFT_FLAGS) -o $@ -c -emit-module -module-name swift -parse-stdlib -parse-as-library $(PATH_TO_AXLE_SWIFT_SOURCES) 
	$(Echo) "mv $(ObjDir)/swift.swiftmodule $(AXLE_SWIFT_HEADER_DIR)"
	@mv $(ObjDir)/swift.swiftmodule $(AXLE_SWIFT_HEADER_DIR)


$(SharedLibDir)/swift/%.$(SHLIBEXT): $(SharedLibDir)/swift/.dir
	ln -s ../$(@F) $@

$(SharedLibDir)/%.$(SHLIBEXT): $(SharedLibDir)/swift/%.$(SHLIBEXT)

all-local:: $(SWIFT_HEADER_DIR)/swift.swiftmodule $(AXLE_SWIFT_HEADER_DIR)/swift.swiftmodule
