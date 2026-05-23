# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

# Included via CMAKE_PROJECT_INCLUDE for Android builds that use Swift.
#
# Handles Clang-only driver flags that Swift doesn't understand:
# - --ld-path: specifies the linker binary (Clang driver flag)
# - -Qunused-arguments: suppresses Clang warnings about unused flags
#
# The NDK's -Wl,<arg> linker flags are handled in build.ps1 by pre-setting
# CMAKE_*_LINKER_FLAGS with -Xlinker <arg> form before CMake runs.

# --ld-path for using the built lld instead of the NDK linker.
if(SWIFT_ANDROID_LD_PATH)
  add_link_options($<$<LINK_LANGUAGE:C,CXX,ASM>:--ld-path=${SWIFT_ANDROID_LD_PATH}>)
endif()

# Clang-only driver flag, not understood by Swift.
add_link_options($<$<LINK_LANGUAGE:C,CXX,ASM>:-Qunused-arguments>)
