##===- stdlib/swift2objc/Makefile --------------------------*- Makefile -*-===##
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
include $(SWIFT_LEVEL)/Makefile

HeaderDir := $(PROJ_OBJ_ROOT)/$(BuildMode)/include/swift

HEADERS := module.map $(notdir $(wildcard $(PROJ_SRC_DIR)/*.h))
OBJHEADERS := $(addprefix $(HeaderDir)/, $(HEADERS))

$(OBJHEADERS): $(HeaderDir)/%: $(PROJ_SRC_DIR)/% $(HeaderDir)/.dir
	$(Verb) cp $< $@
	$(Echo) Copying $(notdir $<) to build dir

all-local:: $(OBJHEADERS)

