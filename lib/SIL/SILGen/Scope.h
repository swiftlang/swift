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

#include "Cleanup.h"

namespace swift {
namespace Lowering {

/// A Scope is a RAII object recording that a scope (e.g. a brace
/// statement) has been entered.
template<CleanupsDepth CleanupManager::*SCOPE>
class LLVM_LIBRARY_VISIBILITY ScopeImpl {
  CleanupManager &Cleanups;
  CleanupsDepth Depth;
  CleanupsDepth SavedScope;

  void popImpl() {
    Cleanups.Stack.checkIterator(Depth);
    Cleanups.Stack.checkIterator(Cleanups.*SCOPE);
    assert(Cleanups.*SCOPE == Depth && "popping scopes out of order");

    Cleanups.*SCOPE = SavedScope;
    Cleanups.endScope(Depth);
    Cleanups.Stack.checkIterator(Cleanups.*SCOPE);
  }

public:
  explicit ScopeImpl(CleanupManager &Cleanups)
    : Cleanups(Cleanups), Depth(Cleanups.getCleanupsDepth()),
      SavedScope(Cleanups.*SCOPE) {
    assert(Depth.isValid());
    Cleanups.Stack.checkIterator(Cleanups.*SCOPE);
    Cleanups.*SCOPE = Depth;
  }

  void pop() {
    assert(Depth.isValid() && "popping a scope twice!");
    popImpl();
    Depth = CleanupsDepth::invalid();
  }

  ~ScopeImpl() {
    if (Depth.isValid()) popImpl();
  }
  
};

using Scope = CleanupManager::Scope;

/// A FullExpr is a RAII object recording that a full-expression has
/// been entered.  A full-expression is essentially a very small scope
/// for the temporaries in an expression, with the added complexity
/// that (eventually, very likely) we have to deal with expressions
/// that are only conditionally evaluated.
class LLVM_LIBRARY_VISIBILITY FullExpr : private Scope {
public:
  explicit FullExpr(CleanupManager &Cleanups) : Scope(Cleanups) {}
  using Scope::pop;
};

} // end namespace Lowering
} // end namespace swift

#endif
