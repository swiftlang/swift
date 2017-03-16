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

#ifndef SWIFT_SILGEN_SCOPE_H
#define SWIFT_SILGEN_SCOPE_H

#include "SILGenFunction.h"
#include "swift/SIL/SILDebugScope.h"
#include "Cleanup.h"

namespace swift {
namespace Lowering {

/// A Scope is a RAII object recording that a scope (e.g. a brace
/// statement) has been entered.
class LLVM_LIBRARY_VISIBILITY Scope {
  CleanupManager &cleanups;
  CleanupsDepth depth;
  CleanupsDepth savedInnermostScope;
  CleanupLocation loc;

public:
  explicit Scope(CleanupManager &cleanups, CleanupLocation loc)
      : cleanups(cleanups), depth(cleanups.getCleanupsDepth()),
        savedInnermostScope(cleanups.innermostScope), loc(loc) {
    assert(depth.isValid());
    cleanups.stack.checkIterator(cleanups.innermostScope);
    cleanups.innermostScope = depth;
  }

  void pop() {
    assert(depth.isValid() && "popping a scope twice!");
    popImpl();
    depth = CleanupsDepth::invalid();
  }
  
  ~Scope() {
    if (depth.isValid())
      popImpl();
  }

  bool isValid() const { return depth.isValid(); }

private:
  void popImpl() {
    cleanups.stack.checkIterator(depth);
    cleanups.stack.checkIterator(cleanups.innermostScope);
    assert(cleanups.innermostScope == depth && "popping scopes out of order");

    cleanups.innermostScope = savedInnermostScope;
    cleanups.endScope(depth, loc);
    cleanups.stack.checkIterator(cleanups.innermostScope);
    cleanups.popTopDeadCleanups(cleanups.innermostScope);
  }
};

/// A FullExpr is a RAII object recording that a full-expression has
/// been entered.  A full-expression is essentially a very small scope
/// for the temporaries in an expression, with the added complexity
/// that (eventually, very likely) we have to deal with expressions
/// that are only conditionally evaluated.
class LLVM_LIBRARY_VISIBILITY FullExpr : private Scope {
public:
  explicit FullExpr(CleanupManager &cleanups, CleanupLocation loc)
      : Scope(cleanups, loc) {}
  using Scope::pop;
};

/// A LexicalScope is a Scope that is also exposed to the debug info.
class LLVM_LIBRARY_VISIBILITY LexicalScope : private Scope {
  SILGenFunction& SGF;
public:
  explicit LexicalScope(SILGenFunction &SGF, CleanupLocation loc)
      : Scope(SGF.Cleanups, loc), SGF(SGF) {
    SGF.enterDebugScope(loc);
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
  explicit DebugScope(SILGenFunction &SGF, CleanupLocation loc) : SGF(SGF) {
    SGF.enterDebugScope(loc);
  }

  ~DebugScope() { SGF.leaveDebugScope(); }
};

} // end namespace Lowering
} // end namespace swift

#endif
