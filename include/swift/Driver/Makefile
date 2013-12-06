##===- include/Swift/Driver/Makefile -----------------------*- Makefile -*-===##
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
BUILT_SOURCES = Options.inc

TABLEGEN_INC_FILES_COMMON = 1

include $(SWIFT_LEVEL)/Makefile

$(ObjDir)/Options.inc.tmp : Options.td $(LLVM_TBLGEN) $(ObjDir)/.dir
	$(Echo) "Building Swift Driver Option table with tblgen"
	$(Verb) $(LLVMTableGen) -gen-opt-parser-defs -o $(call SYSPATH, $@) $<
