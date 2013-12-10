##===- stdlib/objc/AppKit/Makefile -------------------------*- Makefile -*-===##
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

LIBRARYNAME := swiftAppKit
LINK_LIBS_IN_SHARED := 1
SHARED_LIBRARY := 1
SWIFT_SOURCES := AppKit.swift
SOURCES := AppKit.o
NO_BUILD_ARCHIVE := 1
#USEDLIBS := swift_runtime.a

include $(SWIFT_LEVEL)/Makefile

LDFLAGS += -framework AppKit \
	   -lswift_stdlib_core -lswiftObjectiveC -lswiftFoundation

PATH_TO_SWIFT_SOURCES := $(addprefix $(PROJ_SRC_DIR)/, $(SWIFT_SOURCES))

$(SWIFT_HEADER_DIR)/AppKit.swiftmodule: $(ObjDir)/AppKit.o $(SWIFT_HEADER_DIR)
$(ObjDir)/AppKit.o: $(PATH_TO_SWIFT_SOURCES) $(ObjDir)/.dir $(BUILT_SOURCES) $(PROJ_MAKEFILE) $(SWIFT_COMPILER) $(LibDir)/libswift_runtime.a
	$(Echo) "Compiling $(notdir $@) for $(BuildMode) build"
	$(Verb) $(SWIFT_COMPILER) $(SWIFT_FLAGS) -o $@ -c -emit-module -module-link-name $(LIBRARYNAME) -parse-as-library $(PATH_TO_SWIFT_SOURCES)
	@mv $(ObjDir)/AppKit.swiftmodule $(SWIFT_HEADER_DIR)

all-local:: $(SWIFT_HEADER_DIR)/AppKit.swiftmodule
