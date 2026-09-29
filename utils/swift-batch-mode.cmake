# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

# Included via CMAKE_PROJECT_INCLUDE for Windows builds. Enables batch
# mode for incremental compilation modes.
cmake_policy(GET CMP0157 _SwiftBatchMode_CMP0157)
if(_SwiftBatchMode_CMP0157 STREQUAL "NEW")
  add_compile_options("$<$<AND:$<COMPILE_LANGUAGE:Swift>,$<OR:$<STREQUAL:$<TARGET_PROPERTY:Swift_COMPILATION_MODE>,incremental>,$<NOT:$<BOOL:$<TARGET_PROPERTY:Swift_COMPILATION_MODE>>>>>:-enable-batch-mode>")
endif()
unset(_SwiftBatchMode_CMP0157)
