//===------------------- FunctionBodySkipping.h -----------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_FUNCTIONBODYSKIPPING_H
#define SWIFT_BASIC_FUNCTIONBODYSKIPPING_H

#include "llvm/Support/DataTypes.h"

namespace swift {

/// Describes the function bodies that can be skipped in type-checking.
enum class FunctionBodySkipping : uint8_t {
  /// Do not skip type-checking for any function bodies.
  None,
  /// Only non-inlinable function bodies should be skipped.
  NonInlinable,
  /// Only non-inlinable functions bodies without type definitions should
  /// be skipped.
  NonInlinableWithoutTypes,
  /// All function bodies should be skipped, where not otherwise required
  /// for type inference.
  All
};

} // end namespace swift

#endif // SWIFT_BASIC_FUNCTIONBODYSKIPPING_H
