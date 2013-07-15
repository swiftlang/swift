##===- unittests/IDE/Makefile ----------------------------*- Makefile -*-===##
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

SWIFT_LEVEL = ../..
TESTNAME = IDE
include $(SWIFT_LEVEL)/../../Makefile.config
LINK_COMPONENTS := $(TARGETS_TO_BUILD)
USEDLIBS = swiftIDE.a swiftClangImporter.a swiftSema.a swiftSIL.a swiftIRGen.a swiftParse.a swiftAST.a swiftBasic.a
LLVMLibsOptions :=     \
  -lclangDriver        \
  -lclangFrontendTool  \
  -lclangFrontend      \
  -lclangSerialization \
  -lclangParse         \
  -lclangSema          \
  -lclangAnalysis      \
  -lclangEdit          \
  -lclangRewriteCore   \
  -lclangLex           \
  -lclangAST           \
  -lclangBasic

include $(SWIFT_LEVEL)/unittests/Makefile
