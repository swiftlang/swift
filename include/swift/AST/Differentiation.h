//===--- Differentiation.h - Swift Automatic Differentiation --------------===//
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
//  This file defines AST support for automatic differentiation.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_DIFFERENTIATION_H
#define SWIFT_AST_DIFFERENTIATION_H

#include "ASTContext.h"
#include "DeclNameLoc.h"
#include <utility>

namespace swift {

class AutoDiffArgument {
public:
  enum class Kind { Index, Self };

private:
  SourceLoc Loc;
  Kind Kind;
  union Value {
    struct { unsigned Index; }; // Index
    struct {};                  // Self
    // TODO: Other argument kinds, e.g. identifier?
    Value(unsigned index) : Index(index) {}
    Value() {}
  } V;

public:
  AutoDiffArgument(SourceLoc loc, enum Kind kind, Value value)
    : Loc(loc), Kind(kind), V(value) {}

  static AutoDiffArgument getIndexArgument(SourceLoc loc, unsigned index) {
    return { loc, Kind::Index, { index } };
  }

  static AutoDiffArgument getSelfArgument(SourceLoc loc) {
    return { loc, Kind::Self, {} };
  }

  unsigned getIndex() const {
    assert(Kind == Kind::Index);
    return V.Index;
  }

  enum Kind getKind() const {
    return Kind;
  }

  SourceLoc getLoc() const {
    return Loc;
  }
};

} // end namespace swift

#endif
