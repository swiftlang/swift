//===--- ValueLifetime.h - ValueLifetimeAnalysis ----------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// Utilities used by the SILOptimizer for SSA analysis and update.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_UTILS_CFG_H
#define SWIFT_SILOPTIMIZER_UTILS_CFG_H

#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILBuilder.h"

namespace swift {

/// This computes the lifetime of a single SILValue.
///
/// This does not compute a set of jointly postdominating use points. Instead it
/// assumes that the value's existing uses already jointly postdominate the
/// definition. This makes sense for values that are returned +1 from an
/// instruction, like partial_apply, and therefore must be released on all paths
/// via strong_release or apply.
class ValueLifetimeAnalysis {
public:

  /// The lifetime frontier for the value. It is the list of instructions
  /// following the last uses of the value. All the frontier instructions
  /// end the value's lifetime.
  typedef llvm::SmallVector<SILInstruction *, 4> Frontier;

  /// Constructor for the value \p Def with a specific set of users of Def's
  /// users.
  ValueLifetimeAnalysis(SILInstruction *Def, ArrayRef<SILInstruction*> UserList) :
      DefValue(Def), UserSet(UserList.begin(), UserList.end()) {
    propagateLiveness();
  }

  /// Constructor for the value \p Def considering all the value's uses.
  ValueLifetimeAnalysis(SILInstruction *Def) : DefValue(Def) {
    for (auto result : Def->getResults()) {
      for (Operand *op : result->getUses()) {
        UserSet.insert(op->getUser());
      }
    }
    propagateLiveness();
  }

  enum Mode {
    /// Don't split critical edges if the frontier instructions are located on
    /// a critical edges. Instead fail.
    DontModifyCFG,
    
    /// Split critical edges if the frontier instructions are located on
    /// a critical edges.
    AllowToModifyCFG,
    
    /// Require that all users must commonly post-dominate the definition. In
    /// other words: All paths from the definition to the function exit must
    /// contain at least one use. Fail if this is not the case.
    UsersMustPostDomDef
  };

  /// Computes and returns the lifetime frontier for the value in \p Fr.
  ///
  /// Returns true if all instructions in the frontier could be found in
  /// non-critical edges.
  /// Returns false if some frontier instructions are located on critical edges.
  /// In this case, if \p mode is AllowToModifyCFG, those critical edges are
  /// split, otherwise nothing is done and the returned \p Fr is not valid.
  ///
  /// If \p deadEndBlocks is provided, all dead-end blocks are ignored. This
  /// prevents unreachable-blocks to be included in the frontier.
  bool computeFrontier(Frontier &Fr, Mode mode,
                       DeadEndBlocks *DEBlocks = nullptr);

  /// Returns true if the instruction \p Inst is located within the value's
  /// lifetime.
  /// It is assumed that \p Inst is located after the value's definition.
  bool isWithinLifetime(SILInstruction *Inst);

  /// Returns true if the value is alive at the begin of block \p BB.
  bool isAliveAtBeginOfBlock(SILBasicBlock *BB) {
    return LiveBlocks.count(BB) && BB != DefValue->getParent();
  }

  /// Checks if there is a dealloc_ref inside the value's live range.
  bool containsDeallocRef(const Frontier &Frontier);

  /// For debug dumping.
  void dump() const;

private:

  /// The value.
  SILInstruction *DefValue;

  /// The set of blocks where the value is live.
  llvm::SmallSetVector<SILBasicBlock *, 16> LiveBlocks;

  /// The set of instructions where the value is used, or the users-list
  /// provided with the constructor.
  llvm::SmallPtrSet<SILInstruction*, 16> UserSet;

  /// Propagates the liveness information up the control flow graph.
  void propagateLiveness();

  /// Returns the last use of the value in the live block \p BB.
  SILInstruction *findLastUserInBlock(SILBasicBlock *BB);
};


} // end namespace swift

#endif
