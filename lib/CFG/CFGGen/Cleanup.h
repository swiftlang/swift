//===--- Cleanup.h - Declarations for CFG Cleanup Generation ----*- C++ -*-===//
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
namespace Lowering {
  class CFGGen;

class CleanupManager {
  friend class Scope;

  CFGGen &Gen;
  
  /// Stack - Currently active cleanups in this scope tree.
  DiverseStack<Cleanup, 128> Stack;
  
  CleanupsDepth InnermostScope;
  
  /// endScope - used by the Scope class.
  void endScope(CleanupsDepth Depth);

public:
  CleanupManager(CFGGen &Gen)
    : Gen(Gen), InnermostScope(Stack.stable_end()) {
  }
  
  /// Retun a stable reference to the current cleanup.
  CleanupsDepth getCleanupsDepth() const {
    return Stack.stable_begin();
  }
  
  /// emitBranchAndCleanups - Emit a branch to the given jump destination,
  /// threading out through any cleanups we might need to run.  This does not
  /// pop the cleanup stack.
  void emitBranchAndCleanups(JumpDest Dest);


};

  

} // end namespace Lowering
} // end namespace swift

#endif

