//===--- FormalType.h - Formal types of functions ---------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the FormalType class, which encapsulates a set of
// data about the formal type of a function declaration.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_FORMALTYPE_H
#define SWIFT_IRGEN_FORMALTYPE_H

#include "swift/AST/Type.h"

namespace swift {
  enum class AbstractCC : unsigned char;

namespace irgen {

/// The formal type of a function.
class FormalType {
  CanType Ty;
  AbstractCC CC;
  unsigned NaturalUncurry;

public:
  FormalType(CanType type, AbstractCC cc, unsigned naturalUncurry)
    : Ty(type), CC(cc), NaturalUncurry(naturalUncurry) {}

  /// Return the natural, unsubstituted type of this function.
  CanType getType() const { return Ty; }

  /// Return the abstract calling convention of this function.
  AbstractCC getAbstractCC() const { return CC; }

  /// Return the natural uncurry level of this function.
  unsigned getNaturalUncurryLevel() const { return NaturalUncurry; }
};

} // end namespace irgen
} // end namespace swift

#endif
