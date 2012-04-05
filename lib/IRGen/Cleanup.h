//===--- Cleanup.h - Declarations for Cleanup IR Generation -----*- C++ -*-===//
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
// This file defines some types that are generically useful for
// dealing with cleanups in IR Generation.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_CLEANUP_H
#define SWIFT_IRGEN_CLEANUP_H

#include "IRGenFunction.h"

namespace swift {
namespace irgen {

class CleanupControl;
class CleanupOutflows;

/// A Cleanup is an object placed on IRGenFunction's cleanups stack to
/// cause something to occur when a scope or full-expression is
/// concluded.
class Cleanup {
  size_t AllocatedSize;
  unsigned State : 2;
  unsigned UsedWhileActive : 1;
  unsigned UsedWhileInactive : 1;
  unsigned HasFallthroughOutflow : 1;
  unsigned HasControlFlag : 1;
  unsigned NextDestLabel;
  llvm::BasicBlock *NormalEntryBB;
  CleanupOutflows *Outflows;
  void *ControlBegin;
  void *ControlEnd;

  friend Cleanup &IRGenFunction::initCleanup(Cleanup &, size_t, CleanupState);
protected:
  Cleanup() {}
  // The fields are initialized by IRGenFunction::initCleanup.

public:
  /// Return the allocated size of this object.  This is required by
  /// DiverseStack for iteration.
  size_t allocated_size() const { return AllocatedSize; }

  CleanupState getState() const { return CleanupState(State); }
  void setState(CleanupState state) { State = unsigned(state); }

  CleanupControl getControl() const;
  void setControl(const CleanupControl &control);

  /// Is this cleanup active for code currently being executed?
  bool isActive() const { return getState() == CleanupState::Active; }
  bool isDead() const { return getState() == CleanupState::Dead; }

  // Observe that some control flow is trying to break through this
  // cleanup.
  void addUse() {
    if (isActive()) UsedWhileActive = true;
    else UsedWhileInactive = true;
  }
  void addActiveUse() { assert(isActive()); UsedWhileActive = true; }
  void addInactiveUse() { assert(!isActive()); UsedWhileInactive = true; }
  bool isUsedWhileActive() const { return UsedWhileActive; }
  bool isUsedWhileInactive() const { return UsedWhileInactive; }

  llvm::BasicBlock *getNormalEntryBlock() const { return NormalEntryBB; }
  void setNormalEntryBlock(llvm::BasicBlock *BB) { NormalEntryBB = BB; }

  CleanupOutflows *getOutflows() const { return Outflows; }
  void setOutflows(CleanupOutflows *outs) { Outflows = outs; }

  /// Does this cleanup have a fallthrough outflow, i.e. are there
  /// branches leading through it to the enclosing cleanup?  This is
  /// stored here so that we don't need to 
  bool hasFallthroughOutflow() const { return HasFallthroughOutflow; }
  void addFallthroughOutflow() { HasFallthroughOutflow = true; }

  unsigned getNextDestLabel() const { return NextDestLabel; }
  void setNextDestLabel(unsigned label) {
    assert(label > NextDestLabel); NextDestLabel = label;
  }

  virtual void emit(IRGenFunction &IGF) const = 0;

private:
  virtual void _anchor();
};

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
      IGF.popCleanups(Depth);
    }
    Depth = IRGenFunction::CleanupsDepth::invalid();
  }

  ~Scope() {
    if (Depth.isValid()) IGF.popCleanups(Depth);
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
