//===--- Dominance.h - SIL dominance analysis ------------------*- C++ -*-===//
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
// This file provides interfaces for computing and working with
// control-flow dominance in SIL.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_DOMINANCE_H
#define SWIFT_SIL_DOMINANCE_H

#include "llvm/ADT/DenseMap.h"
#include "swift/SIL/SILBasicBlock.h"

namespace swift {

  /// A class for computing basic dominance information.
  class DominanceInfo {
    /// A map from basic blocks to their immediate dominators.
    llvm::DenseMap<SILBasicBlock*, SILBasicBlock*> ImmediateDominators;

    typedef std::pair<SILBasicBlock*, SILBasicBlock*> DominanceCacheKey;
    mutable llvm::DenseMap<DominanceCacheKey, bool> DominanceCache;

  public:
    DominanceInfo(SILFunction *F);

    SILBasicBlock *getImmediateDominator(SILBasicBlock *BB) const {
      auto it = ImmediateDominators.find(BB);
      return (it != ImmediateDominators.end() ? it->second : nullptr);
    }

    /// Does instruction A strictly dominate instruction B?
    bool dominates(SILInstruction *a, SILInstruction *b) const;

    /// Does basic block A non-strictly dominate basic block B?
    bool dominates(SILBasicBlock *a, SILBasicBlock *b) const;
  };

}  // end namespace swift

#endif
