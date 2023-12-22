//===--- Ownership.h ------------------------------------------------------===//
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
//
// This file defines ownership types exposed through the ABI.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_ABI_OWNERSHIP_H
#define SWIFT_ABI_OWNERSHIP_H

#include <stdint.h>

namespace swift {

/// Different kinds of value ownership supported by Swift.
enum class ValueOwnership : uint8_t {
  /// the context-dependent default ownership (sometimes shared,
  /// sometimes owned)
  Default,
  /// an 'inout' exclusive, mutating borrow
  InOut,
  /// a 'borrowing' nonexclusive, usually nonmutating borrow
  Shared,
  /// a 'consuming' ownership transfer
  Owned,

  Last_Kind = Owned
};

} // namespace swift

#endif // SWIFT_ABI_OWNERSHIP_H
