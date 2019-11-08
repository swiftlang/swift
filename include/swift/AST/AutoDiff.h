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

#include "swift/AST/IndexSubset.h"
#include "swift/Basic/Range.h"

namespace swift {

class ParsedAutoDiffParameter {
public:
  enum class Kind { Named, Ordered, Self };

private:
  SourceLoc loc;
  Kind kind;
  union Value {
    struct { Identifier name; } Named;
    struct { unsigned index; } Ordered;
    struct {} self;
    Value(Identifier name) : Named({name}) {}
    Value(unsigned index) : Ordered({index}) {}
    Value() {}
  } value;

public:
  ParsedAutoDiffParameter(SourceLoc loc, Kind kind, Value value)
    : loc(loc), kind(kind), value(value) {}

  ParsedAutoDiffParameter(SourceLoc loc, Kind kind, unsigned index)
    : loc(loc), kind(kind), value(index) {}

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
    assert(kind == Kind::Named);
    return value.Named.name;
  }

  unsigned getIndex() const {
    return value.Ordered.index;
  }

  Kind getKind() const {
    return kind;
  }

  SourceLoc getLoc() const {
    return loc;
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
