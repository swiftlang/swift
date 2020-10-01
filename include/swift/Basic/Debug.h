//===--- Debug.h - Compiler debugging helpers -------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_BASIC_DEBUG_H
#define SWIFT_BASIC_DEBUG_H

#include "llvm/Support/Compiler.h"


/// Adds attributes to the provided method signature indicating that it is a
/// debugging helper that should never be called directly from compiler code.
/// This deprecates the method so it won't be called directly and marks it as
/// used so it won't be eliminated as dead code.
#define SWIFT_DEBUG_HELPER(method) \
  LLVM_ATTRIBUTE_DEPRECATED(method LLVM_ATTRIBUTE_USED, \
                            "only for use in the debugger")

/// Declares a const void instance method with the name and parameters provided.
/// Methods declared with this macro should never be called except in the
/// debugger.
#define SWIFT_DEBUG_DUMPER(nameAndParams) \
  SWIFT_DEBUG_HELPER(void nameAndParams const)

/// Declares an instance `void dump() const` method. Methods declared with this
/// macro should never be called except in the debugger.
#define SWIFT_DEBUG_DUMP \
  SWIFT_DEBUG_DUMPER(dump())

#endif


