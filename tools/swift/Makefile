##===- tools/swift/Makefile -------------------------------*- Makefile -*-===##
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

TOOLNAME = swift

# Include this here so we can get the configuration of the targets that have
# been configured for construction. We have to do this early so we can set up
# LINK_COMPONENTS before including Makefile.rules
include $(SWIFT_LEVEL)/../../Makefile.config

# $(TARGETS_TO_BUILD) and ipo are necessary for code generation. bitwriter is
# necessary to write out a .bc file. jit and linker are used for
# JIT execution.
LINK_COMPONENTS := $(TARGETS_TO_BUILD) bitwriter ipo jit linker mcjit
USEDLIBS = swiftIRGen.a swiftParse.a swiftSema.a swiftAST.a swiftBasic.a
LLVMLibsOptions := $(LLVMLibsOptions) -ledit

include $(SWIFT_LEVEL)/Makefile

