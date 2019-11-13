//===--- BasicBlockOptUtils.h - SIL basic block utilities -------*- C++ -*-===//
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
/// Utilities used by the SILOptimizer for analyzing and operating on whole
/// basic blocks, including as removal, cloning, and SSA update.
///
/// CFGOptUtils.h provides lower-level CFG branch and edge utilities.
///
/// SIL/BasicBlockUtils.h provides essential SILBasicBlock utilities.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_UTILS_BASICBLOCKOPTUTILS_H
#define SWIFT_SILOPTIMIZER_UTILS_BASICBLOCKOPTUTILS_H

#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILCloner.h"

namespace swift {

class BasicBlockCloner;
class SILLoop;
class SILLoopInfo;

/// Remove all instructions in the body of \p bb in safe manner by using
/// undef.
void clearBlockBody(SILBasicBlock *bb);

/// Handle the mechanical aspects of removing an unreachable block.
void removeDeadBlock(SILBasicBlock *bb);

/// Remove all unreachable blocks in a function.
bool removeUnreachableBlocks(SILFunction &f);

/// Return true if there are any users of v outside the specified block.
inline bool isUsedOutsideOfBlock(SILValue v) {
  auto *bb = v->getParentBlock();
  for (auto *use : v->getUses())
    if (use->getUser()->getParent() != bb)
      return true;
  return false;
}

/// Rotate a loop's header as long as it is exiting and not equal to the
/// passed basic block.
/// If \p RotateSingleBlockLoops is true a single basic block loop will be
/// rotated once. ShouldVerify specifies whether to perform verification after
/// the transformation.
/// Returns true if the loop could be rotated.
bool rotateLoop(SILLoop *loop, DominanceInfo *domInfo, SILLoopInfo *loopInfo,
                bool rotateSingleBlockLoops, SILBasicBlock *upToBB,
                bool shouldVerify);

/// Helper function to perform SSA updates in case of jump threading.
void updateSSAAfterCloning(BasicBlockCloner &cloner, SILBasicBlock *srcBB,
                           SILBasicBlock *destBB);

/// Clone a single basic block and any required successor edges within the same
/// function.
class BasicBlockCloner : public SILCloner<BasicBlockCloner> {
  using SuperTy = SILCloner<BasicBlockCloner>;
  friend class SILCloner<BasicBlockCloner>;

protected:
  /// The original block to be cloned.
  SILBasicBlock *origBB;

public:
  /// An ordered list of old to new available value pairs.
  ///
  /// updateSSAAfterCloning() expects this public field to hold values that may
  /// be remapped in the cloned block and live out.
  SmallVector<std::pair<SILValue, SILValue>, 16> AvailVals;

  // Clone blocks starting at `origBB`, within the same function.
  BasicBlockCloner(SILBasicBlock *origBB)
      : SILCloner(*origBB->getParent()), origBB(origBB) {}

  void cloneBlock(SILBasicBlock *insertAfterBB = nullptr) {
    SmallVector<SILBasicBlock *, 4> successorBBs;
    successorBBs.reserve(origBB->getSuccessors().size());
    llvm::copy(origBB->getSuccessors(), std::back_inserter(successorBBs));
    cloneReachableBlocks(origBB, successorBBs, insertAfterBB);
  }

  /// Clone the given branch instruction's destination block, splitting
  /// its successors, and rewrite the branch instruction.
  void cloneBranchTarget(BranchInst *bi) {
    assert(origBB == bi->getDestBB());

    cloneBlock(/*insertAfter*/ bi->getParent());

    SILBuilderWithScope(bi).createBranch(bi->getLoc(), getNewBB(),
                                         bi->getArgs());
    bi->eraseFromParent();
  }

  /// Get the newly cloned block corresponding to `origBB`.
  SILBasicBlock *getNewBB() {
    return remapBasicBlock(origBB);
  }

  /// Call this after processing all instructions to fix the control flow
  /// graph. The branch cloner may have left critical edges.
  bool splitCriticalEdges(DominanceInfo *domInfo, SILLoopInfo *loopInfo);

protected:
  // MARK: CRTP overrides.

  /// Override getMappedValue to allow values defined outside the block to be
  /// cloned to be reused in the newly cloned block.
  SILValue getMappedValue(SILValue value) {
    if (auto si = value->getDefiningInstruction()) {
      if (!isBlockCloned(si->getParent()))
        return value;
    } else if (auto bbArg = dyn_cast<SILArgument>(value)) {
      if (!isBlockCloned(bbArg->getParent()))
        return value;
    } else {
      assert(isa<SILUndef>(value) && "Unexpected Value kind");
      return value;
    }
    // `value` is not defined outside the cloned block, so consult the cloner's
    // map of cloned values.
    return SuperTy::getMappedValue(value);
  }

  void mapValue(SILValue origValue, SILValue mappedValue) {
    SuperTy::mapValue(origValue, mappedValue);
    AvailVals.emplace_back(origValue, mappedValue);
  }
};

// Helper class that provides a callback that can be used in
// inliners/cloners for collecting new call sites.
class CloneCollector {
public:
  typedef std::pair<SILInstruction *, SILInstruction *> value_type;
  typedef std::function<void(SILInstruction *, SILInstruction *)> CallbackType;
  typedef std::function<bool (SILInstruction *)> FilterType;

private:
  FilterType filter;

  // Pairs of collected instructions; (new, old)
  llvm::SmallVector<value_type, 4> instructionpairs;

  void collect(SILInstruction *oldI, SILInstruction *newI) {
    if (filter(newI))
      instructionpairs.push_back(std::make_pair(newI, oldI));
  }

public:
  CloneCollector(FilterType filter) : filter(filter) {}

  CallbackType getCallback() {
    return std::bind(&CloneCollector::collect, this, std::placeholders::_1,
                     std::placeholders::_2);
  }

  llvm::SmallVectorImpl<value_type> &getInstructionPairs() {
    return instructionpairs;
  }
};

/// Sink address projections to their out-of-block uses. This is
/// required after cloning a block and before calling
/// updateSSAAfterCloning to avoid address-type phis.
///
/// This clones address projections at their use points, but does not
/// mutate the block containing the projections.
class SinkAddressProjections {
  // Projections ordered from last to first in the chain.
  SmallVector<SingleValueInstruction *, 4> projections;
  SmallSetVector<SILValue, 4> inBlockDefs;

public:
  /// Check for an address projection chain ending at \p inst. Return true if
  /// the given instruction is successfully analyzed.
  ///
  /// If \p inst does not produce an address, then return
  /// true. getInBlockDefs() will contain \p inst if any of its
  /// (non-address) values are used outside its block.
  ///
  /// If \p inst does produce an address, return true only of the
  /// chain of address projections within this block is clonable at
  /// their use sites. getInBlockDefs will return all non-address
  /// operands in the chain that are also defined in this block. These
  /// may require phis after cloning the projections.
  bool analyzeAddressProjections(SILInstruction *inst);

  /// After analyzing projections, returns the list of (non-address) values
  /// defined in the same block as the projections which will have uses outside
  /// the block after cloning.
  ArrayRef<SILValue> getInBlockDefs() const {
    return inBlockDefs.getArrayRef();
  }
  /// Clone the chain of projections at their use sites.
  ///
  /// Return true if anything was done.
  ///
  /// getInBlockProjectionOperandValues() can be called before or after cloning.
  bool cloneProjections();
};

/// Utility class for cloning init values into the static initializer of a
/// SILGlobalVariable.
class StaticInitCloner : public SILCloner<StaticInitCloner> {
  friend class SILInstructionVisitor<StaticInitCloner>;
  friend class SILCloner<StaticInitCloner>;

  /// The number of not yet cloned operands for each instruction.
  llvm::DenseMap<SILInstruction *, int> numOpsToClone;

  /// List of instructions for which all operands are already cloned (or which
  /// don't have any operands).
  llvm::SmallVector<SILInstruction *, 8> readyToClone;

public:
  StaticInitCloner(SILGlobalVariable *gVar)
      : SILCloner<StaticInitCloner>(gVar) {}

  /// Add \p InitVal and all its operands (transitively) for cloning.
  ///
  /// Note: all init values must are added, before calling clone().
  void add(SILInstruction *initVal);

  /// Clone \p InitVal and all its operands into the initializer of the
  /// SILGlobalVariable.
  ///
  /// \return Returns the cloned instruction in the SILGlobalVariable.
  SingleValueInstruction *clone(SingleValueInstruction *initVal);

  /// Convenience function to clone a single \p InitVal.
  static void appendToInitializer(SILGlobalVariable *gVar,
                                  SingleValueInstruction *initVal) {
    StaticInitCloner cloner(gVar);
    cloner.add(initVal);
    cloner.clone(initVal);
  }

protected:
  SILLocation remapLocation(SILLocation loc) {
    return ArtificialUnreachableLocation();
  }
};

} // namespace swift

#endif
