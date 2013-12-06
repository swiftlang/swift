##===- swift/lib/Basic/Makefile ----------------------------*- Makefile -*-===##
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
include $(SWIFT_LEVEL)/../../Makefile.config

LIBRARYNAME := swiftBasic

include $(SWIFT_LEVEL)/Makefile

# Unless specified explicitly, use "1.0" as the Swift version.
ifndef SWIFT_VERSION
SWIFT_VERSION := 1.0
endif

SWIFT_VERSION_COMPONENTS := $(subst ., ,$(SWIFT_VERSION))
SWIFT_VERSION_MAJOR := $(word 1,$(SWIFT_VERSION_COMPONENTS))
SWIFT_VERSION_MINOR := $(word 2,$(SWIFT_VERSION_COMPONENTS))
SWIFT_VERSION_PATCHLEVEL := $(word 3,$(SWIFT_VERSION_COMPONENTS))
ifeq ($(SWIFT_VERSION_PATCHLEVEL),)
SWIFT_HAS_VERSION_PATCHLEVEL := 0
else
SWIFT_HAS_VERSION_PATCHLEVEL := 1
endif

ifeq ($(SWIFT_HAS_VERSION_PATCHLEVEL),0)
CXXFLAGS += -DSWIFT_VERSION=$(SWIFT_VERSION) -DSWIFT_VERSION_MAJOR=$(SWIFT_VERSION_MAJOR) -DSWIFT_VERSION_MINOR=$(SWIFT_VERSION_MINOR) -DSWIFT_HAS_VERSION_PATCHLEVEL=0
else
CXXFLAGS += -DSWIFT_VERSION=$(SWIFT_VERSION) -DSWIFT_VERSION_MAJOR=$(SWIFT_VERSION_MAJOR) -DSWIFT_VERSION_MINOR=$(SWIFT_VERSION_MINOR) -DSWIFT_VERSION_PATCHLEVEL=$(SWIFT_VERSION_PATCHLEVEL) -DSWIFT_HAS_VERSION_PATCHLEVEL=1
endif
