##===- stdlib/axle/Makefile --------------------------------*- Makefile -*-===##
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

LIBRARYNAME := swift_axle
LINK_LIBS_IN_SHARED := 1
SHARED_LIBRARY := 1

#
# Determine SWIFT_SOURCES, the list of source files in the swift library.
#
# sed is used here to strip comments.
SWIFT_SOURCES := $(shell sed -e "s/\#.*//g" "$(PROJ_SRC_DIR)/tools/swift/stdlib/axle/SwiftAxleSources.txt" )

# Replace Assert.swift with AssertDebug.swift if assertions are enabled
ifneq ($(DISABLE_ASSERTIONS),1)
  SWIFT_SOURCES := $(subst Assert.swift,AssertDebug.swift,$(SWIFT_SOURCES))
endif

# Remove half (f16) from compiling on anything but for air arch.
ifneq ($(ARCH),air)
  SWIFT_SOURCES := $(filter-out Vec2h.swift,$(SWIFT_SOURCES))
endif

#
# Done with SWIFT_SOURCES
#

SOURCES := axle.swift
NO_BUILD_ARCHIVE := 1

include $(SWIFT_LEVEL)/Makefile

LDFLAGS += -lswift_stdlib_core

PATH_TO_SWIFT_SOURCES := $(addprefix $(PROJ_SRC_DIR)/, $(SWIFT_SOURCES))

$(AXLE_SWIFT_HEADER_DIR):
	$(MKDIR) $(AXLE_SWIFT_HEADER_DIR)

$(AXLE_SWIFT_HEADER_DIR)/axle.swiftmodule: $(ObjDir)/axle.o $(AXLE_SWIFT_HEADER_DIR)

$(ObjDir)/axle.o: $(PATH_TO_SWIFT_SOURCES) $(ObjDir)/.dir $(BUILT_SOURCES) $(PROJ_MAKEFILE) $(SWIFT_COMPILER) $(LibDir)/libswift_runtime.a $(AXLE_SWIFT_HEADER_DIR)
	$(Echo) "Compiling $(notdir $@) for $(BuildMode) build"
	$(Verb) $(SWIFT_COMPILER) $(SWIFT_FLAGS) -o $@ -c -emit-module -module-link-name $(LIBRARYNAME) -parse-as-library $(PATH_TO_SWIFT_SOURCES)
	@mv $(ObjDir)/axle.swiftmodule $(AXLE_SWIFT_HEADER_DIR)

$(SharedLibDir)/swift/%.$(SHLIBEXT): $(SharedLibDir)/swift/.dir
	ln -s ../$(@F) $@

$(SharedLibDir)/%.$(SHLIBEXT): $(SharedLibDir)/swift/%.$(SHLIBEXT)

all-local:: $(AXLE_SWIFT_HEADER_DIR)/axle.swiftmodule
