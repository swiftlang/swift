//===--- Ownership.h - Swift ASTs for Reference Ownership -------*- C++ -*-===//
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
// This file defines common structures for working with the different
// kinds of reference ownership supported by Swift, such as 'weak' and
// 'unowned'.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_OWNERSHIP_H
#define SWIFT_OWNERSHIP_H

#include <stdint.h>

namespace swift {

/// Different kinds of reference ownership supported by Swift.
// This enum is used in diagnostics. If you add a case here, the diagnostics
// must be updated as well.
enum class Ownership : uint8_t {
  /// \brief a strong reference (the default semantics)
  Strong,

  /// \brief a 'weak' reference
  Weak,

  /// \brief an 'unowned' reference
  Unowned,

  /// \brief an 'unowned(unsafe)' reference
  Unmanaged,
};
  
} // end namespace swift

#endif
