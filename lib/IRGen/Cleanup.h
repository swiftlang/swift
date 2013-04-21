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
// This file defines the Cleanup type.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_CLEANUP_H
#define SWIFT_IRGEN_CLEANUP_H

#include "IRGenFunction.h"

namespace swift {
namespace irgen {

// These classes are private to GenCleanup.cpp.
class CleanupControl;

/// A Cleanup is an object placed on IRGenFunction's cleanups stack to
/// cause something to occur when a scope or full-expression is
/// concluded.
class Cleanup {
  unsigned AllocatedSize;
  unsigned State : 2;
   unsigned HasControlFlag : 1;
  unsigned NextDestLabel : 26;
  llvm::BasicBlock *NormalEntryBB;
  void *ControlBegin;
  void *ControlEnd;

  friend Cleanup &IRGenFunction::initCleanup(Cleanup &, size_t, CleanupState);
protected:
  Cleanup() {}
  virtual ~Cleanup() {}
  // The fields are initialized by IRGenFunction::initCleanup.

public:
  /// Return the allocated size of this object.  This is required by
  /// DiverseStack for iteration.
  size_t allocated_size() const { return AllocatedSize; }

  CleanupState getState() const { return CleanupState(State); }
  void setState(CleanupState state) {
    State = unsigned(state);
    assert(getState() == state);
  }

  CleanupControl getControl() const;
  void setControl(const CleanupControl &control);

  /// Is this cleanup active for code currently being executed?
  bool isActive() const { return getState() == CleanupState::Active; }
  bool isDead() const { return getState() == CleanupState::Dead; }

  llvm::BasicBlock *getNormalEntryBlock() const { return NormalEntryBB; }
  void setNormalEntryBlock(llvm::BasicBlock *BB) { NormalEntryBB = BB; }

  unsigned getNextDestLabel() const { return NextDestLabel; }
  void setNextDestLabel(unsigned label) {
    assert(label > NextDestLabel); NextDestLabel = label;
  }

  virtual void emit(IRGenFunction &IGF) const = 0;

private:
  virtual void _anchor();
};

} // end namespace irgen
} // end namespace swift

#endif
