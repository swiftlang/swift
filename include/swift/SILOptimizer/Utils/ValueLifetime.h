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

/// Computes the lifetime frontier for a given value with respect to a
/// given set of uses. The lifetime frontier is the list of instructions
/// following the last uses. The set of uses can be passed by the clients
/// of the analysis and can be a super set of the uses of the SILValue
/// e.g. it can be the set of transitive uses of the SILValue.
class ValueLifetimeAnalysis {
public:

  /// The lifetime frontier for the value. It is the list of instructions
  /// following the last uses of the value. All the frontier instructions
  /// end the value's lifetime.
  typedef llvm::SmallVector<SILInstruction *, 4> Frontier;

  /// Constructor for the value \p Def with a specific set of users of Def's
  /// users.
  ValueLifetimeAnalysis(SILInstruction *def,
                        ArrayRef<SILInstruction *> userList)
      : defValue(def), userSet(userList.begin(), userList.end()) {
    propagateLiveness();
  }

  /// Constructor for the value \p def considering all the value's uses.
  ValueLifetimeAnalysis(SILInstruction *def) : defValue(def) {
    for (auto result : def->getResults()) {
      for (Operand *op : result->getUses()) {
        userSet.insert(op->getUser());
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

  /// Computes and returns the lifetime frontier for the value in \p frontier
  /// with respect to the set of uses in the userSet.
  ///
  /// Returns true if all instructions in the frontier could be found in
  /// non-critical edges.
  /// Returns false if some frontier instructions are located on critical edges.
  /// In this case, if \p mode is AllowToModifyCFG, those critical edges are
  /// split, otherwise the returned \p frontier consists of only those
  /// instructions of the frontier that are not in the critical edges. Note that
  /// the method getCriticalEdges can be used to retrieve the critical edges.
  ///
  /// If \p deBlocks is provided, all dead-end blocks are ignored. This
  /// prevents unreachable-blocks to be included in the frontier.
  bool computeFrontier(Frontier &frontier, Mode mode,
                       DeadEndBlocks *deBlocks = nullptr);

  ArrayRef<std::pair<TermInst *, unsigned>> getCriticalEdges() {
    return criticalEdges;
  }

  /// Returns true if the instruction \p Inst is located within the value's
  /// lifetime.
  /// It is assumed that \p inst is located after the value's definition.
  bool isWithinLifetime(SILInstruction *inst);

  /// Returns true if the value is alive at the begin of block \p bb.
  bool isAliveAtBeginOfBlock(SILBasicBlock *bb) {
    return liveBlocks.count(bb) &&
           (bb != defValue->getParent() || hasUsersBeforeDef);
  }

  /// Checks if there is a dealloc_ref inside the value's live range.
  bool containsDeallocRef(const Frontier &frontier);

  /// For debug dumping.
  void dump() const;

private:

  /// The value.
  SILInstruction *defValue;

  /// The set of blocks where the value is live.
  llvm::SmallSetVector<SILBasicBlock *, 16> liveBlocks;

  /// The set of instructions where the value is used, or the users-list
  /// provided with the constructor.
  llvm::SmallPtrSet<SILInstruction *, 16> userSet;

  /// Indicates whether the basic block containing def has users of def that
  /// precede def. This field is initialized by propagateLiveness.
  bool hasUsersBeforeDef;

  /// Critical edges that couldn't be split to compute the frontier. This could
  /// be non-empty when the analysis is invoked with DontModifyCFG mode.
  llvm::SmallVector<std::pair<TermInst *, unsigned>, 16> criticalEdges;

  /// Propagates the liveness information up the control flow graph.
  void propagateLiveness();

  /// Returns the last use of the value in the live block \p bb.
  SILInstruction *findLastUserInBlock(SILBasicBlock *bb);
};


} // end namespace swift

#endif
