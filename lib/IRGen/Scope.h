//===--- Scope.h - Declarations for scope RAII objects ----------*- C++ -*-===//
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
// This file defines the Scope and FullExpr RAII objects.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_SCOPE_H
#define SWIFT_IRGEN_SCOPE_H

#include "IRGenFunction.h"

namespace swift {
namespace irgen {

/// A Scope is a RAII object recording that a scope (e.g. a brace
/// statement) has been entered.
class Scope {
  IRGenFunction &IGF;
  IRGenFunction::CleanupsDepth Depth;
public:
  explicit Scope(IRGenFunction &IGF)
    : IGF(IGF), Depth(IGF.getCleanupsDepth()) {}

  void pop() {
    assert(Depth.isValid() && "popping a scope twice!");
    if (Depth != IGF.getCleanupsDepth()) {
      IGF.endScope(Depth);
    }
    Depth = IRGenFunction::CleanupsDepth::invalid();
  }

  ~Scope() {
    if (Depth.isValid()) IGF.endScope(Depth);
  }
};

/// A FullExpr is a RAII object recording that a full-expression has
/// been entered.  A full-expression is essentially a very small scope
/// for the temporaries in an expression, with the added complexity
/// that (eventually, very likely) we have to deal with expressions
/// that are only conditionally evaluated.
class FullExpr : private Scope {
public:
  explicit FullExpr(IRGenFunction &IGF) : Scope(IGF) {}
  using Scope::pop;
};

} // end namespace irgen
} // end namespace swift

#endif
