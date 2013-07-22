##===- tools/sil-opt/Makefile -----------------------------*- Makefile -*-===##
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

TOOLNAME = sil-opt

# Include this here so we can get the configuration of the targets that have
# been configured for construction. We have to do this early so we can set up
# LINK_COMPONENTS before including Makefile.rules
include $(SWIFT_LEVEL)/../../Makefile.config

LINK_COMPONENTS := option bitreader
USEDLIBS = swiftFrontend.a swiftParse.a swiftSema.a swiftSerialization.a swiftSIL.a swiftSILGen.a swiftSILPasses.a swiftAST.a swiftBasic.a

## FIXME: None of this should be necessary, but we have to link in clang
## because of rdar://14513108
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
  -lclangBasic         \
  $(LLVMLibsOptions)   \
  -ledit               \
  -framework CoreFoundation

include $(SWIFT_LEVEL)/Makefile

