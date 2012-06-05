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
// This file defines the TypeLoc class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_TYPELOC_H
#define SWIFT_TYPELOC_H

#include "swift/Basic/SourceLoc.h"

namespace swift {
  class ASTContext;

/// TypeLoc - Provides source location information for a parsed type.
/// A TypeLoc* is stored in AST nodes which use an explicitly written type.
class TypeLoc {
private:
  TypeLoc(SourceRange Range);
  // FIXME: Currently, there's only one kind of TypeLoc; we need multiple kinds
  // for more accurate source location information.
  SourceRange Range;

  void *operator new(size_t Bytes) throw() = delete;
  void operator delete(void *Data) throw() = delete;
  void *operator new(size_t Bytes, void *Mem) throw() = delete;
  void *operator new(size_t Bytes, ASTContext &C,
                     unsigned Alignment = 8);
public:
  SourceRange getSourceRange() {
    return Range;
  }

  static TypeLoc *get(ASTContext &Context, SourceRange Range);
};

} // end namespace llvm

#endif
