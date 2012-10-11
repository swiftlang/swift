##===- unittests/Makefile ----------------------------------*- Makefile -*-===##
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

# If SWIFT_LEVEL is not set, then we are the top-level Makefile. Otherwise, we
# are being included from a subdirectory makefile.
ifndef SWIFT_LEVEL

IS_UNITTEST_LEVEL := 1
SWIFT_LEVEL := ..
PARALLEL_DIRS = runtime

endif  # SWIFT_LEVEL

include $(SWIFT_LEVEL)/Makefile

ifndef IS_UNITTEST_LEVEL

MAKEFILE_UNITTEST_NO_INCLUDE_COMMON := 1
include $(LLVM_SRC_ROOT)/unittests/Makefile.unittest

endif  # IS_UNITTEST_LEVEL
