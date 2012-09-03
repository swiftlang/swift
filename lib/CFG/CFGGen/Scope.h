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

#ifndef SCOPE_H
#define SCOPE_H

#include "CFGGen.h"

namespace swift {
namespace Lowering {

/// A Scope is a RAII object recording that a scope (e.g. a brace
/// statement) has been entered.
class Scope {
  CFGGen &Gen;
  CleanupsDepth Depth;
  CleanupsDepth SavedInnermostScope;

  void popImpl() {
    Gen.Cleanups.checkIterator(Depth);
    Gen.Cleanups.checkIterator(Gen.InnermostScope);
    assert(Gen.InnermostScope == Depth && "popping scopes out of order");

    Gen.InnermostScope = SavedInnermostScope;
    Gen.endScope(Depth);
    Gen.Cleanups.checkIterator(Gen.InnermostScope);
  }

public:
  explicit Scope(CFGGen &Gen)
    : Gen(Gen), Depth(Gen.getCleanupsDepth()),
      SavedInnermostScope(Gen.InnermostScope) {
    assert(Depth.isValid());
    Gen.Cleanups.checkIterator(Gen.InnermostScope);
    Gen.InnermostScope = Depth;
  }

  void pop() {
    assert(Depth.isValid() && "popping a scope twice!");
    popImpl();
    Depth = CleanupsDepth::invalid();
  }

  ~Scope() {
    if (Depth.isValid()) popImpl();
  }
};

/// A FullExpr is a RAII object recording that a full-expression has
/// been entered.  A full-expression is essentially a very small scope
/// for the temporaries in an expression, with the added complexity
/// that (eventually, very likely) we have to deal with expressions
/// that are only conditionally evaluated.
class FullExpr : private Scope {
public:
  explicit FullExpr(CFGGen &Gen) : Scope(Gen) {}
  using Scope::pop;
};

} // end namespace irgen
} // end namespace swift

#endif
