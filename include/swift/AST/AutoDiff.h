//===--- AutoDiff.h - Swift Automatic Differentiation ---------------------===//
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
//  SWIFT_ENABLE_TENSORFLOW
//  This file defines AST support for automatic differentiation.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_AUTODIFF_H
#define SWIFT_AST_AUTODIFF_H

#include "ASTContext.h"
#include "llvm/ADT/SmallBitVector.h"
#include "swift/Basic/Range.h"

namespace swift {

class ParsedAutoDiffParameter {
public:
  enum class Kind { Named, Ordered, Self };

private:
  SourceLoc Loc;
  Kind Kind;
  union Value {
    struct { Identifier Name; }; // Named
    struct { unsigned Index; }; // Ordered
    struct {};                  // Self
    Value(Identifier name) : Name(name) {}
    Value(unsigned index) : Index(index) {}
    Value() {}
  } V;

public:
  ParsedAutoDiffParameter(SourceLoc loc, enum Kind kind, Value value)
    : Loc(loc), Kind(kind), V(value) {}

  ParsedAutoDiffParameter(SourceLoc loc, enum Kind kind, unsigned index)
  : Loc(loc), Kind(kind), V(index) {}

  static ParsedAutoDiffParameter getNamedParameter(SourceLoc loc,
                                                   Identifier name) {
    return { loc, Kind::Named, name };
  }

  static ParsedAutoDiffParameter getOrderedParameter(SourceLoc loc,
                                                     unsigned index) {
    return { loc, Kind::Ordered, index };
  }

  static ParsedAutoDiffParameter getSelfParameter(SourceLoc loc) {
    return { loc, Kind::Self, {} };
  }

  Identifier getName() const {
    assert(Kind == Kind::Named);
    return V.Name;
  }

  unsigned getIndex() const {
    return V.Index;
  }

  enum Kind getKind() const {
    return Kind;
  }

  SourceLoc getLoc() const {
    return Loc;
  }

  bool isEqual(const ParsedAutoDiffParameter &other) const {
    if (getKind() != other.getKind())
      return false;
    if (getKind() == Kind::Named)
      return getName() == other.getName();
    return getKind() == Kind::Self;
  }
};

} // end namespace swift

#endif // SWIFT_AST_AUTODIFF_H
