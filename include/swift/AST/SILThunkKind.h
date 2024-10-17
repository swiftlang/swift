//===--- SILThunkKind.h ---------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file This file contains a kind that defines the types of thunks that can be
/// generated using a thunk inst. It provides an AST level interface that lets
/// one generate the derived function kind and is also used to make adding such
/// kinds to the mangler trivial.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_SILTHUNKKIND_H
#define SWIFT_AST_SILTHUNKKIND_H

#include "swift/AST/Types.h"

namespace swift {

class SILType;

struct SILThunkKind {
  enum InnerTy {
    Invalid = 0,

    /// A thunk that just calls the passed in function. Used for testing
    /// purposes of the underlying thunking machinery.
    Identity = 1,

    /// A thunk that checks if a value is on the main actor and if it isn't
    /// jumps to the main actor. It expects that the passed in function does not
    /// have any arguments, results, is synchronous, and does not throw.
    HopToMainActorIfNeeded = 2,

    MaxValue = HopToMainActorIfNeeded,
  };

  InnerTy innerTy;

  SILThunkKind() : innerTy(InnerTy::Invalid) {}
  SILThunkKind(InnerTy innerTy) : innerTy(innerTy) {}
  SILThunkKind(unsigned inputInnerTy) : innerTy(InnerTy(inputInnerTy)) {
    assert(inputInnerTy <= MaxValue && "Invalid value");
  }

  operator InnerTy() const { return innerTy; }

  /// Given the current enum state returned the derived output function from
  /// \p inputFunction.
  ///
  /// Defined in Instructions.cpp
  CanSILFunctionType getDerivedFunctionType(SILFunction *fn,
                                            CanSILFunctionType inputFunction,
                                            SubstitutionMap subMap) const;

  /// Given the current enum state returned the derived output function from
  /// \p inputFunction.
  ///
  /// Defined in Instructions.cpp
  SILType getDerivedFunctionType(SILFunction *fn, SILType inputFunctionType,
                                 SubstitutionMap subMap) const;

  Demangle::MangledSILThunkKind getMangledKind() const {
    switch (innerTy) {
    case Invalid:
      return Demangle::MangledSILThunkKind::Invalid;
    case Identity:
      return Demangle::MangledSILThunkKind::Identity;
    case HopToMainActorIfNeeded:
      return Demangle::MangledSILThunkKind::HopToMainActorIfNeeded;
    }
  }
};

} // namespace swift

#endif
