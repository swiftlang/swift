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

LIBRARYNAME := swift_runtime
LINK_LIBS_IN_SHARED := 1
SHARED_LIBRARY := 1
SOURCES := Alloc.cpp Stubs.cpp swift.swift
NO_BUILD_ARCHIVE := 1

include $(SWIFT_LEVEL)/../../Makefile.config

include $(SWIFT_LEVEL)/Makefile

SWIFT_COMPILER := $(LLVMToolDir)/swift$(EXEEXT)
$(ObjDir)/%.o: %.swift $(ObjDir)/.dir $(BUILT_SOURCES) $(PROJ_MAKEFILE) $(SWIFT_COMPILER)
	$(Echo) "Compiling $*.swift for $(BuildMode) build"
	$(Verb) $(SWIFT_COMPILER) -c $< -o $(ObjDir)/swift.o -parse-as-library
