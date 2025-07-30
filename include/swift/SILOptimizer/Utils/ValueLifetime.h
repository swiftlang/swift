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

#include "swift/Basic/STLExtras.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/BasicBlockBits.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"

namespace swift {

/// Record the last use points and CFG edges that form the boundary of a
/// SILValue's lifetime.
///
/// Only valid when critical edges are disallowed (currently OSSA-only).
///
/// Useful when the client processes the boundary differently depending on the
/// last user.
///
/// TODO: When critical edges are universally prohibited, completely replace the
/// Frontier implementation by adding a utility method that populates a vector
/// of insertion points based on this boundary, where each each
/// block-terminating lastUser adds an insertion point at each successor block.
///
/// Note: A dead live range will have no last users and no boundary
/// edges. Always check if the definition passed to ValueLifetimeAnalysis is
/// dead before assuming that this boundary covers all points at which a value
/// must be destroyed. Normally, a live range with uses will have at least one
/// lastUser or boundaryEdge. But with infinite loops, it is possible for both
/// lastUsers and boundaryEdges to be empty even if there are uses within the
/// loop.
///
/// TODO: combine this with PrunedLivenessBoundary.
struct ValueLifetimeBoundary {
  SmallVector<SILInstruction *, 8> lastUsers;
  SmallVector<SILBasicBlock *, 8> boundaryEdges;

  /// Visit the point at which a lifetime-ending instruction must be inserted,
  /// excluding dead-end blocks. This is only useful when it is known that none
  /// of the lastUsers ends the lifetime, for example when creating a new borrow
  /// scope to enclose all uses.
  void visitInsertionPoints(
      llvm::function_ref<void(SILBasicBlock::iterator insertPt)> visitor,
      DeadEndBlocks *deBlocks = nullptr);
};

/// Computes the lifetime frontier for a given value with respect to a
/// given set of uses. The lifetime frontier is the list of instructions
/// following the last uses. The set of uses can be passed by the clients
/// of the analysis and can be a super set of the uses of the SILValue
/// e.g. it can be the set of transitive uses of the SILValue.
class ValueLifetimeAnalysis {
  /// The instruction or argument that define the value.
  PointerUnion<SILInstruction *, SILArgument *> defValue;

  /// The set of blocks where the value is live.
  llvm::SmallVector<SILBasicBlock *, 16> liveBlocks;
  
  /// True for blocks which are in liveBlocks.
  BasicBlockFlag inLiveBlocks;

  /// The set of instructions where the value is used, or the users-list
  /// provided with the constructor.
  llvm::SmallPtrSet<SILInstruction *, 16> userSet;

  /// Indicates whether the basic block containing def has users of def that
  /// precede def. This field is initialized by propagateLiveness.
  bool hasUsersBeforeDef;

  /// Critical edges that couldn't be split to compute the frontier. This could
  /// be non-empty when the analysis is invoked with DontModifyCFG mode.
  llvm::SmallVector<std::pair<TermInst *, unsigned>, 16> criticalEdges;

public:

  /// The lifetime frontier for the value. It is the list of instructions
  /// following the last uses of the value. All the frontier instructions
  /// end the value's lifetime.
  using Frontier = SmallVector<SILInstruction *, 4>;

  /// A type erased version of frontier so callers can customize the inline
  /// size.
  using FrontierImpl = SmallVectorImpl<SILInstruction *>;

  /// Constructor for the value \p def with a specific range of users.
  ///
  /// We templatize over the RangeTy so that we can initialize
  /// ValueLifetimeAnalysis with misc iterators including transform
  /// iterators.
  template <typename RangeTy>
  ValueLifetimeAnalysis(SILArgument *def, const RangeTy &useRange)
      : defValue(def), inLiveBlocks(def->getFunction()), userSet() {
    for (SILInstruction *use : useRange)
      userSet.insert(use);
    propagateLiveness();
  }

  ValueLifetimeAnalysis(
      SILArgument *def,
      llvm::iterator_range<ValueBaseUseIterator> useRange)
      : defValue(def), inLiveBlocks(def->getFunction()), userSet() {
    for (Operand *use : useRange)
      userSet.insert(use->getUser());
    propagateLiveness();
  }

  template <typename RangeTy>
  ValueLifetimeAnalysis(
      SILInstruction *def, const RangeTy &useRange)
      : defValue(def), inLiveBlocks(def->getFunction()), userSet() {
    for (SILInstruction *use : useRange)
      userSet.insert(use);
    propagateLiveness();
  }

  ValueLifetimeAnalysis(
      SILInstruction *def,
      llvm::iterator_range<ValueBaseUseIterator> useRange)
      : defValue(def), inLiveBlocks(def->getFunction()), userSet() {
    for (Operand *use : useRange)
      userSet.insert(use->getUser());
    propagateLiveness();
  }

  ValueLifetimeAnalysis(SILArgument *def, ArrayRef<Operand *> useRange)
      : defValue(def), inLiveBlocks(def->getFunction()), userSet() {
    for (auto *use : useRange)
      userSet.insert(use->getUser());
    propagateLiveness();
  }

  ValueLifetimeAnalysis(SILInstruction *def, ArrayRef<Operand *> useRange)
      : defValue(def), inLiveBlocks(def->getFunction()), userSet() {
    for (auto *use : useRange)
      userSet.insert(use->getUser());
    propagateLiveness();
  }

  /// Compute the LifetimeBoundary--the last users and boundary edges. This
  /// always succeeds.
  ///
  /// Precondition: no critical edges
  ///
  /// Note: this should never use DeadEndBlocks. If the client cares about
  /// dead-end CFG edges because it is inserting destroys, it can handle those
  /// specially when processing LifetimeBoundary.boundaryEdges.
  void computeLifetimeBoundary(ValueLifetimeBoundary &boundary);

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
  /// An edge is also considered as "critical" if it has a single predecessor
  /// but the predecessor's terminal instruction is a user of the value.
  ///
  /// If \p deBlocks is provided, all dead-end blocks are ignored. This
  /// prevents unreachable-blocks to be included in the frontier.
  bool computeFrontier(FrontierImpl &frontier, Mode mode,
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
    return inLiveBlocks.get(bb) &&
           (hasUsersBeforeDef || bb != getDefValueParentBlock());
  }

  /// Checks if there is a dealloc_ref or dealloc_stack_ref inside the
  /// value's live range.
  bool containsDeallocRef(const FrontierImpl &frontier);

  /// For debug dumping.
  void dump() const;

private:
  SILFunction *getFunction() const {
    if (auto *inst = defValue.dyn_cast<SILInstruction *>()) {
      return inst->getFunction();
    }
    return cast<SILArgument *>(defValue)->getFunction();
  }

  SILBasicBlock *getDefValueParentBlock() const {
    if (auto *inst = defValue.dyn_cast<SILInstruction *>()) {
      return inst->getParent();
    }
    return cast<SILArgument *>(defValue)->getParent();
  }

  /// Propagates the liveness information up the control flow graph.
  void propagateLiveness();

  /// Returns the last use of the value in the live block \p bb.
  SILInstruction *findLastUserInBlock(SILBasicBlock *bb);

  void computeLifetime(
      llvm::function_ref<bool(SILBasicBlock *)> visitBlock,
      llvm::function_ref<void(SILInstruction *)> visitLastUser,
      llvm::function_ref<void(SILBasicBlock *predBB, SILBasicBlock *succBB)>
          visitBoundaryEdge);
};

/// Destroys \p valueOrStackLoc at \p frontier.
///
/// If  \p valueOrStackLoc is an alloc_stack, inserts destroy_addr and
/// dealloc_stack at each instruction of the \p frontier.
/// Otherwise \p valueOrStackLoc must be a value type and in this case, inserts
/// destroy_value at each instruction of the \p frontier.
void endLifetimeAtFrontier(SILValue valueOrStackLoc,
                           const ValueLifetimeAnalysis::FrontierImpl &frontier,
                           SILBuilderContext &builderCtxt,
                           InstModCallbacks callbacks);

} // end namespace swift

#endif
