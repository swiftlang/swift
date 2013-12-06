##===- tools/driver/Makefile -------------------------------*- Makefile -*-===##
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

TOOLNAME = swift_driver

# Include this here so we can get the configuration of the targets that have
# been configured for construction. We have to do this early so we can set up
# LINK_COMPONENTS before including Makefile.rules
include $(SWIFT_LEVEL)/../../Makefile.config

# Embed an SDK path into the driver
CXXFLAGS += -DSWIFT_MODULES_SDK="\"$(MODULES_SDK)\""

# $(TARGETS_TO_BUILD) and ipo are necessary for code generation. bitwriter is
# necessary to write out a .bc file. jit and linker are used for
# JIT execution.
LINK_COMPONENTS := $(TARGETS_TO_BUILD) bitreader bitwriter irreader ipo jit linker mcjit asmparser option
USEDLIBS = swiftDriver.a swiftFrontend.a swiftIRGen.a swiftParse.a swiftClangImporter.a swiftSema.a swiftSerialization.a swiftSIL.a swiftSILGen.a swiftSILPassesUtils.a swiftSILPasses.a swiftAST.a swiftBasic.a swiftIDE.a
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
  -ledit               \
  -framework CoreFoundation

include $(SWIFT_LEVEL)/Makefile
