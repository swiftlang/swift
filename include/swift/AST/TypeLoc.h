//===--- TypeLoc.h - Swift Language Type Locations --------------*- C++ -*-===//
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
// This file defines the TypeLoc struct and related structs.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_TYPELOC_H
#define SWIFT_TYPELOC_H

#include "swift/Basic/SourceLoc.h"
#include "swift/AST/Type.h"

namespace swift {
  class ASTContext;

/// TypeLoc - Provides source location information for a parsed type.
/// A TypeLoc is stored in AST nodes which use an explicitly written type.
struct TypeLoc {
private:
  Type T;
  // FIXME: Currently, there's only one kind of TypeLoc; we need multiple kinds
  // for more accurate source location information.
  SourceRange Range;

public:
  TypeLoc() {}
  TypeLoc(Type T, SourceRange Range) : T(T), Range(Range) {}

private:
  explicit TypeLoc(Type T) : T(T) {}

public:
  // FIXME: We generally shouldn't need to build TypeLocs without a location.
  static TypeLoc withoutLoc(Type T) {
    return TypeLoc(T);
  }

  SourceRange getSourceRange() const {
    return Range;
  }
  bool hasLocation() const {
    return Range.isValid();
  }
  Type getType() const {
    return T;
  }
  void setInvalidType(ASTContext &C);
};

} // end namespace llvm

#endif
