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

LINK_COMPONENTS := support
USEDLIBS = swiftFrontend.a swiftParse.a swiftClangImporter.a swiftSema.a swiftSerialization.a swiftAST.a swiftBasic.a

LLVMLibsOptions :=     \
  $(LLVMLibsOptions)

include $(SWIFT_LEVEL)/Makefile

