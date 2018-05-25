//===--- Ownership.h - Swift ASTs for Ownership ---------------*- C++ -*-===//
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
// This file defines common structures for working with the different kinds of
// reference ownership supported by Swift, such as 'weak' and 'unowned', as well
// as the different kinds of value ownership, such as 'inout' and '__shared'.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_OWNERSHIP_H
#define SWIFT_OWNERSHIP_H

#include "swift/Basic/InlineBitfield.h"
#include "llvm/Support/raw_ostream.h"
#include <stdint.h>

namespace swift {

/// Different kinds of reference ownership supported by Swift.
// This enum is used in diagnostics. If you add a case here, the diagnostics
// must be updated as well.
enum class ReferenceOwnership : uint8_t {
  /// \brief a strong reference (the default semantics)
  Strong,

  /// \brief a 'weak' reference
  Weak,

  /// \brief an 'unowned' reference
  Unowned,

  /// \brief an 'unowned(unsafe)' reference
  Unmanaged,

  Last_Kind = Unmanaged
};

enum : unsigned { NumReferenceOwnershipBits =
  countBitsUsed(static_cast<unsigned>(ReferenceOwnership::Last_Kind)) };

/// Diagnostic printing of \c StaticSpellingKind.
llvm::raw_ostream &operator<<(llvm::raw_ostream &OS, ReferenceOwnership RO);

/// Different kinds of value ownership supported by Swift.
enum class ValueOwnership : uint8_t {
  /// \brief the context-dependent default ownership (sometimes shared,
  /// sometimes owned)
  Default,
  /// \brief an 'inout' mutating pointer-like value
  InOut,
  /// \brief a '__shared' non-mutating pointer-like value
  Shared,
  /// \brief an '__owned' value
  Owned
};

} // end namespace swift

#endif
