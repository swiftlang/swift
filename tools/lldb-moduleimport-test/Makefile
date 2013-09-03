##===- tools/lldb-moduleimport-test/Makefile -----------------------*- Makefile -*-===##
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

TOOLNAME = lldb-moduleimport-test

include $(SWIFT_LEVEL)/../../Makefile.config

LINK_COMPONENTS := $(TARGETS_TO_BUILD) bitreader mcjit option
USEDLIBS = swiftFrontend.a swiftParse.a \
	swiftClangImporter.a swiftASTSectionImporter.a \
	swiftSIL.a swiftSema.a swiftSerialization.a swiftAST.a swiftBasic.a

LLVMLibsOptions :=     \
  -lclangDriver        \
  -lclangEdit          \
  -lclangFrontend      \
  -lclangSerialization \
  -lclangParse         \
  -lclangSema          \
  -lclangAnalysis      \
  -lclangLex	       \
  -lclangAST	       \
  -lclangBasic         \
  $(LLVMLibsOptions)

include $(SWIFT_LEVEL)/Makefile

