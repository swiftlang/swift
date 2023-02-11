#===--- CMakeLists.txt - Tutorial library -------------------------------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2023 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
#===----------------------------------------------------------------------===#

add_swift_target_library(swiftTutorial ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
# List of files here:
  HelloWorld.swift

# You can look at stdlib/cmake/modules/AddSwiftStdlib.cmake for more options to
# configure here.

# If your module has C or C++ sources and you need specific flags for those.
# C_COMPILE_FLAGS

# If your module has a dependency on another module (besides the actual stdlib)
# Side note: Not sure if the build system implicitly adds Core as a
# dependency, or if it's sort of just working by accident now? /shrug
# SWIFT_MODULE_DEPENDS

  SWIFT_COMPILE_FLAGS
    ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
  INSTALL_IN_COMPONENT
    stdlib)
