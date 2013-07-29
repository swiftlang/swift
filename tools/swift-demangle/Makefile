##===- tools/swift-demangle/Makefile -----------------------*- Makefile -*-===##
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

TOOLNAME = swift-demangle

LINK_COMPONENTS := support
USEDLIBS = swiftSIL.a

# if $(SWIFT_SDK) is installed, include that
SWIFT_SDK := /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.9.sdk
ifeq ($(shell test -d $(SWIFT_SDK) && echo OK), OK)
  CXXFLAGS += -DSWIFT_MODULES_SDK="\"$(SWIFT_SDK)\""
endif

include $(SWIFT_LEVEL)/Makefile

