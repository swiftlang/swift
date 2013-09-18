##===- stdlib/objc/Makefile --------------------------------*- Makefile -*-===##
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
LIBRARYNAME := swiftObjectiveC
LINK_LIBS_IN_SHARED := 1
SHARED_LIBRARY := 1
SWIFT_SOURCES := ObjectiveC.swift
SOURCES := ObjectiveC.swift BridgeObjectiveC.mm
NO_BUILD_ARCHIVE := 1

DIRS := Foundation

# Add OS-specific libraries
ifneq ($(findstring -darwin_ios,$(TARGET_TRIPLE)),)
  DIRS += UIKit
else ifneq ($(findstring -darwin_sim,$(TARGET_TRIPLE)),)
  DIRS += UIKit
else ifneq ($(findstring -darwin,$(TARGET_TRIPLE)),)
  DIRS += AppKit
endif

include $(SWIFT_LEVEL)/Makefile

LDFLAGS += -lobjc -lswift_stdlib_core

PATH_TO_SWIFT_SOURCES := $(addprefix $(PROJ_SRC_DIR)/, $(SWIFT_SOURCES))
$(SWIFT_HEADER_DIR)/ObjectiveC.swift: $(PATH_TO_SWIFT_SOURCES) $(SWIFT_HEADER_DIR)/.dir
	$(Echo) FIXME -- HACK -- Concatenating $(SWIFT_SOURCES) to build dir
	$(Verb) cat $(PATH_TO_SWIFT_SOURCES) > $@

$(SWIFT_HEADER_DIR)/ObjectiveC.swiftmodule: $(ObjDir)/ObjectiveC.o $(SWIFT_HEADER_DIR)
$(ObjDir)/ObjectiveC.o: $(PATH_TO_SWIFT_SOURCES) $(ObjDir)/.dir $(BUILT_SOURCES) $(PROJ_MAKEFILE) $(SWIFT_COMPILER) $(LibDir)/libswift_runtime.a
	$(Echo) "Compiling $(notdir $@) for $(BuildMode) build"
	$(Verb) $(SWIFT_COMPILER) $(SWIFT_FLAGS) -o $@ -c -emit-module -module-link-name $(LIBRARYNAME) -parse-as-library $(PATH_TO_SWIFT_SOURCES)
	@mv $(ObjDir)/ObjectiveC.swiftmodule $(SWIFT_HEADER_DIR)

all-local:: $(SWIFT_HEADER_DIR)/ObjectiveC.swiftmodule

