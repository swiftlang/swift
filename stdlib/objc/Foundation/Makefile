##===- stdlib/objc/Foundation/Makefile ---------------------*- Makefile -*-===##
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

SWIFT_LEVEL := ../../..
include $(SWIFT_LEVEL)/../../Makefile.config

LIBRARYNAME := swiftFoundation
LINK_LIBS_IN_SHARED := 1
SHARED_LIBRARY := 1
SWIFT_SOURCES := Foundation.swift
SOURCES := Foundation.swift BridgeNSString.mm
NO_BUILD_ARCHIVE := 1
#USEDLIBS := swift_runtime.a

ifneq ($(findstring -darwin_ios,$(TARGET_TRIPLE)),)
  # SWIFT_SOURCES += Foundation-ios.swift
else ifneq ($(findstring -darwin_sim,$(TARGET_TRIPLE)),)
  # SWIFT_SOURCES += Foundation-ios.swift
else ifneq ($(findstring -darwin,$(TARGET_TRIPLE)),)
  SWIFT_SOURCES += Foundation-macosx.swift
endif

include $(SWIFT_LEVEL)/Makefile

LDFLAGS += -framework Foundation \
	   -lswift_stdlib_core -lswift_runtime -lswiftObjectiveC -Wl,-force_load,$(LibDir)/libswift_runtime.a

PATH_TO_SWIFT_SOURCES := $(addprefix $(PROJ_SRC_DIR)/, $(SWIFT_SOURCES))

$(SWIFT_HEADER_DIR)/Foundation.swiftmodule: $(ObjDir)/Foundation.o $(SWIFT_HEADER_DIR)
$(ObjDir)/Foundation.o: $(PATH_TO_SWIFT_SOURCES) $(ObjDir)/.dir $(BUILT_SOURCES) $(PROJ_MAKEFILE) $(SWIFT_COMPILER) $(LibDir)/libswift_runtime.a
	$(Echo) "Compiling $(notdir $@) for $(BuildMode) build"
	$(Verb) $(SWIFT_COMPILER) $(SWIFT_FLAGS) -o $@ -c -emit-module -module-link-name $(LIBRARYNAME) -parse-as-library $(PATH_TO_SWIFT_SOURCES)
	@mv $(ObjDir)/Foundation.swiftmodule $(SWIFT_HEADER_DIR)

all-local:: $(SWIFT_HEADER_DIR)/Foundation.swiftmodule
