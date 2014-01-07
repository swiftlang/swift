##===- tools/swift2objc/Makefile ---------------------------*- Makefile -*-===##
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

TOOLNAME = swift2objc

# Include this here so we can get the configuration of the targets that have
# been configured for construction. We have to do this early so we can set up
# LINK_COMPONENTS before including Makefile.rules
include $(SWIFT_LEVEL)/../../Makefile.config

LINK_COMPONENTS := option bitreader
USEDLIBS = swiftDriver.a swiftFrontend.a swiftParse.a swiftSema.a swiftSerialization.a \
           swiftAST.a swiftBasic.a swiftClangImporter.a

LLVMLibsOptions :=     \
  -lclangAST	       \
  -lclangLex	       \
  -lclangBasic         \
  $(LLVMLibsOptions)

include $(SWIFT_LEVEL)/Makefile

