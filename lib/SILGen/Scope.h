//===--- Scope.h - Declarations for scope RAII objects ----------*- C++ -*-===//
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
// This file defines the Scope and FullExpr RAII objects.
//
//===----------------------------------------------------------------------===//

#ifndef SCOPE_H
#define SCOPE_H

#include "SILGenFunction.h"
#include "swift/SIL/SILDebugScope.h"
#include "Cleanup.h"

namespace swift {
namespace Lowering {

/// A Scope is a RAII object recording that a scope (e.g. a brace
/// statement) has been entered.
class LLVM_LIBRARY_VISIBILITY Scope {
  CleanupManager &Cleanups;
  CleanupsDepth Depth;
  CleanupsDepth SavedInnermostScope;
  CleanupLocation Loc;

  void popImpl() {
    Cleanups.Stack.checkIterator(Depth);
    Cleanups.Stack.checkIterator(Cleanups.InnermostScope);
    assert(Cleanups.InnermostScope == Depth && "popping scopes out of order");

    Cleanups.InnermostScope = SavedInnermostScope;
    Cleanups.endScope(Depth, Loc);
    Cleanups.Stack.checkIterator(Cleanups.InnermostScope);
    Cleanups.popTopDeadCleanups(Cleanups.InnermostScope);
  }

public:
  explicit Scope(CleanupManager &Cleanups, CleanupLocation L)
    : Cleanups(Cleanups), Depth(Cleanups.getCleanupsDepth()),
      SavedInnermostScope(Cleanups.InnermostScope),
      Loc(L) {
    assert(Depth.isValid());
    Cleanups.Stack.checkIterator(Cleanups.InnermostScope);
    Cleanups.InnermostScope = Depth;
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
class LLVM_LIBRARY_VISIBILITY FullExpr : private Scope {
public:
  explicit FullExpr(CleanupManager &Cleanups, CleanupLocation Loc)
    : Scope(Cleanups, Loc) {}
  using Scope::pop;
};

/// A LexicalScope is a Scope that is also exposed to the debug info.
class LLVM_LIBRARY_VISIBILITY LexicalScope : private Scope {
  SILGenFunction& SGF;
public:
  explicit LexicalScope(CleanupManager &Cleanups,
                        SILGenFunction& SGF,
                        CleanupLocation Loc)
    : Scope(Cleanups, Loc), SGF(SGF) {
    SGF.enterDebugScope(Loc);
  }
  using Scope::pop;

  ~LexicalScope() {
    SGF.leaveDebugScope();
  }
};

/// A scope that only exists in the debug info.
class LLVM_LIBRARY_VISIBILITY DebugScope {
  SILGenFunction &SGF;

public:
  explicit DebugScope(SILGenFunction &SGF, CleanupLocation Loc) : SGF(SGF) {
    SGF.enterDebugScope(Loc);
  }

  ~DebugScope() { SGF.leaveDebugScope(); }
};

} // end namespace Lowering
} // end namespace swift

#endif
