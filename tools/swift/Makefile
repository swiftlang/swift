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
LINK_COMPONENTS := $(TARGETS_TO_BUILD) bitreader bitwriter ipo jit linker mcjit asmparser
USEDLIBS = swiftIRGen.a swiftParse.a swiftClangImporter.a swiftSema.a swiftSIL.a swiftSILGen.a swiftAST.a swiftBasic.a
LLVMLibsOptions :=     \
  -lclangDriver        \
  -lclangFrontendTool  \
  -lclangFrontend      \
  -lclangSerialization \
  -lclangParse         \
  -lclangSema          \
  -lclangAnalysis      \
  -lclangEdit	       \
  -lclangRewriteCore   \
  -lclangLex	       \
  -lclangAST	       \
  -lclangBasic         \
  $(LLVMLibsOptions)   \
  -ledit

# if $(SWIFT_SDK) is installed, include that in the driver
SWIFT_SDK := /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.9.sdk
ifeq ($(shell test -d $(SWIFT_SDK) && echo OK), OK)
  CXXFLAGS += -DSWIFT_MODULES_SDK="$(SWIFT_SDK)"
endif

include $(SWIFT_LEVEL)/Makefile

