# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for Swift project authors

add_libswift_module(Optimizer
    DEPENDS SIL ExperimentalRegex)

add_subdirectory(Analysis)
add_subdirectory(InstructionPasses)
add_subdirectory(PassManager)
add_subdirectory(FunctionPasses)
add_subdirectory(Utilities)
