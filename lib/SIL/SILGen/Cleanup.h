//===--- Cleanup.h - Declarations for SIL Cleanup Generation ----*- C++ -*-===//
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
// This file defines the Cleanup and CleanupManager classes.
//
//===----------------------------------------------------------------------===//

#ifndef CLEANUP_H
#define CLEANUP_H

#include "JumpDest.h"

namespace swift {
  class BasicBlock;
  class Function;
  
namespace Lowering {
  class SILGen;
  class CleanupOutflows;

/// The valid states that a cleanup can be in.
enum class CleanupState {
  /// The cleanup is inactive but may be activated later.
  Dormant,
  
  /// The cleanup is currently active.
  Active,
  
  /// The cleanup is inactive and will not be activated later.
  Dead
};

class LLVM_LIBRARY_VISIBILITY Cleanup {
  unsigned allocatedSize;
  CleanupState state;
  
  friend class CleanupManager;
protected:
  Cleanup() {}
  virtual ~Cleanup() {}
  
public:
  /// Return the allocated size of this object.  This is required by
  /// DiverseStack for iteration.
  size_t allocated_size() const { return allocatedSize; }
  
  CleanupState getState() const { return state; }
  void setState(CleanupState newState) { state = newState; }
  bool isActive() const { return state == CleanupState::Active; }
  bool isDead() const { return state == CleanupState::Dead; }

  virtual void emit(SILGen &Gen) = 0;
};

class LLVM_LIBRARY_VISIBILITY CleanupManager {
  friend class Scope;

  SILGen &Gen;
  
  /// Stack - Currently active cleanups in this scope tree.
  DiverseStack<Cleanup, 128> Stack;
  
  CleanupsDepth InnermostScope;
  
  void popAndEmitTopCleanup();
  
  Cleanup &initCleanup(Cleanup &cleanup, size_t allocSize, CleanupState state);
public:
  CleanupManager(SILGen &Gen)
    : Gen(Gen), InnermostScope(Stack.stable_end()) {
  }
  
  /// Return a stable reference to the current cleanup.
  CleanupsDepth getCleanupsDepth() const {
    return Stack.stable_begin();
  }
  
  /// emitBranchAndCleanups - Emit a branch to the given jump destination,
  /// threading out through any cleanups we might need to run.  This does not
  /// pop the cleanup stack.
  void emitBranchAndCleanups(JumpDest Dest);
  
  /// pushCleanup - Push a new cleanup.
  template<class T, class... A>
  T &pushCleanupInState(CleanupState state,
                        A &&... args) {
    assert(state != CleanupState::Dead);

#ifndef NDEBUG
    CleanupsDepth oldTop = Stack.stable_begin();
#endif
    
    T &cleanup = Stack.push<T, A...>(::std::forward<A>(args)...);
    T &result = static_cast<T&>(initCleanup(cleanup, sizeof(T), state));
    
#ifndef NDEBUG
    auto newTop = Stack.begin(); ++newTop;
    assert(newTop == Stack.find(oldTop));
#endif
    return result;
  }
  template<class T, class... A>
  T &pushCleanup(A &&... args) {
    return pushCleanupInState<T, A...>(CleanupState::Active,
                                       ::std::forward<A>(args)...);
  }
  
  void endScope(CleanupsDepth depth);
};

  

  /// endScope - used by the Scope class.
  void endScope(CleanupsDepth Depth);

} // end namespace Lowering
} // end namespace swift

#endif

