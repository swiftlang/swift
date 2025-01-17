//===- CanonicalizeOSSALifetime.h - Canonicalize OSSA lifetimes -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// Canonicalize the copies and destroys of a single owned OSSA value.
///
/// This top-level API rewrites the extended OSSA lifetime of a SILValue:
///
///     void canonicalizeValueLifetime(SILValue def, CanonicalizeOSSALifetime &)
///
/// The "extended lifetime" of the references defined by 'def' transitively
/// includes the uses of 'def' itself along with the uses of any copies of
/// 'def'. Canonicalization provably minimizes the OSSA lifetime and its copies
/// by rewriting all copies and destroys. Only consuming uses that are not on
/// the liveness boundary require a copy.
///
/// Example #1: The last consuming use ends the reference lifetime.
///
///     bb0(%arg : @owned $T, %addr : @trivial $*T):
///       %copy = copy_value %arg : $T
///       store %copy to [init] %addr : $*T
///       debug_value %addr : $*T, expr op_deref
///       destroy_value %arg : $T
///
/// Will be transformed to:
///
///     bb0(%arg : @owned $T, %addr : @trivial $*T):
///       store %copy to [init] %addr : $*T
///       debug_value %addr : $*T, expr op_deref
///
/// Example #2: Destroys are hoisted to the last use. Copies are inserted only
/// at consumes within the lifetime (to directly satisfy ownership conventions):
///
///     bb0(%arg : @owned $T, %addr : @trivial $*T):
///       %copy1 = copy_value %arg : $T
///       store %arg to [init] %addr : $*T
///       %_ = apply %_(%copy1) : $@convention(thin) (@guaranteed T) -> ()
///       debug_value %addr : $*T, expr op_deref
///       destroy_value %copy1 : $T
///
/// Will be transformed to:
///
///     bb0(%arg : @owned $T, %addr : @trivial $*T):
///       %copy1 = copy_value %arg : $T
///       store %copy1 to [init] %addr : $*T
///       %_ = apply %_(%arg) : $@convention(thin) (@guaranteed T) -> ()
///       destroy_value %arg : $T
///       debug_value %addr : $*T, expr op_deref
///
/// Example #3: Handle control flow.
///
///     bb0(%arg : @owned $T):
///       cond_br %_, bb1, bb2
///     bb1:
///       br bb3
///     bb2:
///       %copy = copy_value %arg : $T
///       %_ = apply %_(%copy) : $@convention(thin) (@guaranteed T) -> ()
///       destroy_value %copy : $T
///       br bb3
///     bb3:
///       destroy_value %arg : $T
///
/// Will be transformed to:
///
///     bb0(%arg : @owned $T):
///       cond_br %_, bb1, bb2
///     bb1:
///       destroy_value %arg : $T
///       br bb3
///     bb2:
///       %_ = apply %_(%arg) : $@convention(thin) (@guaranteed T) -> ()
///       destroy_value %arg : $T
///       br bb3
///     bb2:
///       // The original destroy is deleted.
///
/// Pass Requirements:
///
///   This utility invalidates instructions but both uses and preserves
///   NonLocalAccessBlockAnalysis.
///
///   The use-def walks in this utility, e.g. getCanonicalCopiedDef, assume no
///   cycles in the data flow graph without a phi.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_UTILS_CANONICALOSSALIFETIME_H
#define SWIFT_SILOPTIMIZER_UTILS_CANONICALOSSALIFETIME_H

#include "swift/Basic/SmallPtrSetVector.h"
#include "swift/Basic/TaggedUnion.h"
#include "swift/SIL/PrunedLiveness.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Analysis/DeadEndBlocksAnalysis.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/Analysis/NonLocalAccessBlockAnalysis.h"
#include "swift/SILOptimizer/Utils/InstructionDeleter.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/Statistic.h"

namespace swift {

class BasicCalleeAnalysis;

extern llvm::Statistic NumCopiesAndMovesEliminated;
extern llvm::Statistic NumCopiesGenerated;

/// Insert a copy on this operand. Trace and update stats.
void copyLiveUse(Operand *use, InstModCallbacks &instModCallbacks);

/// Diagnose that the given value is a move only type for which \p use causes a
/// need to copy the move only value.
///
/// copy on this operand. Trace and update stats.
void diagnoseRequiredCopyOfMoveOnly(Operand *use,
                                    InstModCallbacks &instModCallbacks);

/// Information about consumes on the extended-lifetime boundary. Consuming uses
/// within the lifetime are not included--they will consume a copy after
/// rewriting. For borrowed def values, the consumes do not include the end of
/// the borrow scope, rather the consumes transitively include consumes of any
/// owned copies of the borrowed value.
///
/// This result remains valid during copy rewriting. The only instructions
/// referenced it contains are consumes that cannot be deleted.
class CanonicalOSSAConsumeInfo final {
  /// Map blocks on the lifetime boundary to the last consuming instruction.
  llvm::SmallDenseMap<SILBasicBlock *, SILInstruction *, 4> finalBlockConsumes;

  /// The instructions on the availability boundary of the dead-end region where
  /// this value is not consumed.
  SmallPtrSet<SILInstruction *, 4> unreachableLifetimeEnds;

public:
  void clear() { finalBlockConsumes.clear(); }

  bool empty() { return finalBlockConsumes.empty(); }

  bool hasUnclaimedConsumes() const { return !finalBlockConsumes.empty(); }

  void recordFinalConsume(SILInstruction *inst) {
    assert(!finalBlockConsumes.count(inst->getParent()));
    finalBlockConsumes[inst->getParent()] = inst;
  }

  // Return true if this instruction is marked as a final consume point of the
  // current def's live range. A consuming instruction can only be claimed once
  // because instructions like `tuple` can consume the same value via multiple
  // operands.
  bool claimConsume(SILInstruction *inst) {
    auto destroyPos = finalBlockConsumes.find(inst->getParent());
    if (destroyPos != finalBlockConsumes.end() && destroyPos->second == inst) {
      finalBlockConsumes.erase(destroyPos);
      return true;
    }
    return false;
  }

  void recordUnreachableLifetimeEnd(SILInstruction *inst) {
    unreachableLifetimeEnds.insert(inst);
  }

  bool isUnreachableLifetimeEnd(SILInstruction *inst) {
    return unreachableLifetimeEnds.contains(inst);
  }

  CanonicalOSSAConsumeInfo() {}
  CanonicalOSSAConsumeInfo(CanonicalOSSAConsumeInfo const &) = delete;
  CanonicalOSSAConsumeInfo &
  operator=(CanonicalOSSAConsumeInfo const &) = delete;
  SWIFT_ASSERT_ONLY_DECL(void dump() const LLVM_ATTRIBUTE_USED);
};

enum PruneDebugInsts_t : bool {
  DontPruneDebugInsts = false,
  PruneDebugInsts = true,
};

enum MaximizeLifetime_t : bool {
  DontMaximizeLifetime = false,
  MaximizeLifetime = true,
};

/// Canonicalize OSSA lifetimes.
///
/// Allows the allocation of analysis state to be reused across calls to
/// canonicalizeValueLifetime().
///
/// TODO: Move all the private per-definition members into an implementation
/// class in the .cpp file.
class CanonicalizeOSSALifetime final {
public:
  /// Find the original definition of a potentially copied value. \p copiedValue
  /// must be an owned value. It is usually a copy but may also be a destroy.
  ///
  /// This single helper specifies the root of an owned extended lifetime. Owned
  /// extended lifetimes may not overlap. This ensures that canonicalization can
  /// run as a utility without invalidating instruction worklists in the client,
  /// as long as the client works on each canonical def independently.
  ///
  /// If the source of a copy is guaranteed, then the copy itself is the root of
  /// an owned extended lifetime. Note that it will also be part of a borrowed
  /// extended lifetime, which will be canonicalized separately by
  /// CanonicalizeBorrowScope.
  ///
  /// This use-def walk must be consistent with the def-use walks performed
  /// within the canonicalizeValueLifetime() and canonicalizeBorrowScopes()
  /// implementations.
  static SILValue getCanonicalCopiedDef(SILValue v) {
    while (auto *copy = dyn_cast<CopyValueInst>(v)) {
      auto def = copy->getOperand();
      if (def->getOwnershipKind() != OwnershipKind::Owned) {
        // This guaranteed value cannot be handled, treat the copy as an owned
        // live range def instead.
        return copy;
      }
      v = def;
    }
    return v;
  }

private:
  /// If true, then debug_value instructions outside of non-debug
  /// liveness may be pruned during canonicalization.
  const PruneDebugInsts_t pruneDebugMode;

  /// If true, lifetimes will not be shortened except when necessary to avoid
  /// copies.
  const MaximizeLifetime_t maximizeLifetime;

  // If present, will be used to ensure that the lifetime is not shortened to
  // end inside an access scope which it previously enclosed.  (Note that ending
  // before such an access scope is fine regardless.)
  //
  // For details, see extendLivenessThroughOverlappingAccess.
  NonLocalAccessBlockAnalysis *accessBlockAnalysis;
  // Lazily initialize accessBlocks only when
  // extendLivenessThroughOverlappingAccess is invoked.
  NonLocalAccessBlocks *accessBlocks = nullptr;

  DeadEndBlocksAnalysis *deadEndBlocksAnalysis;

  DominanceInfo *domTree = nullptr;

  BasicCalleeAnalysis *calleeAnalysis;

  InstructionDeleter &deleter;

  /// The SILValue to canonicalize.
  SILValue currentDef;

  /// Instructions beyond which liveness is not extended by destroy uses.
  ArrayRef<SILInstruction *> currentLexicalLifetimeEnds;

  /// Original points in the CFG where the current value's lifetime is consumed
  /// or destroyed.  Each block either contains a consuming instruction (e.g.
  /// `destroy_value`) or is on the availability boundary of the value in a
  /// dead-end region (e.g. `unreachable`).
  ///
  /// For guaranteed values it remains empty. A backward walk from these blocks
  /// must discover all uses on paths that lead to a return or throw.
  ///
  /// These blocks are not necessarily in the pruned live blocks since
  /// pruned liveness does not consider destroy_values.
  llvm::SmallSetVector<SILBasicBlock *, 8> consumingBlocks;

  /// Record all interesting debug_value instructions here rather then treating
  /// them like a normal use. An interesting debug_value is one that may lie
  /// outside the pruned liveness at the time it is discovered.
  llvm::SmallPtrSet<DebugValueInst *, 8> debugValues;

  class Def {
    struct Root {
      SILValue value;
    };
    struct Copy {
      CopyValueInst *cvi;
    };
    struct BorrowedFrom {
      BorrowedFromInst *bfi;
    };
    struct Reborrow {
      SILArgument *argument;
    };
    using Payload = TaggedUnion<Root, Copy, BorrowedFrom, Reborrow>;
    Payload payload;
    Def(Payload payload) : payload(payload) {}

  public:
    enum class Kind {
      Root,
      Copy,
      BorrowedFrom,
      Reborrow,
    };
    Kind getKind() const {
      if (payload.isa<Root>()) {
        return Kind::Root;
      }
      if (payload.isa<Copy>()) {
        return Kind::Copy;
      }
      if (payload.isa<BorrowedFrom>()) {
        return Kind::BorrowedFrom;
      }
      assert(payload.isa<Reborrow>());
      return Kind::Reborrow;
    }
    operator Kind() const { return getKind(); }
    bool operator==(Def rhs) const {
      return getKind() == rhs.getKind() && getValue() == rhs.getValue();
    }
    static Def root(SILValue value) { return {Root{value}}; }
    static Def copy(CopyValueInst *cvi) { return {Copy{cvi}}; }
    static Def borrowedFrom(BorrowedFromInst *bfi) {
      return {BorrowedFrom{bfi}};
    }
    static Def reborrow(SILArgument *argument) { return {Reborrow{argument}}; }
    SILValue getValue() const {
      switch (*this) {
      case Kind::Root:
        return payload.get<Root>().value;
      case Kind::Copy:
        return payload.get<Copy>().cvi;
      case Kind::BorrowedFrom:
        return payload.get<BorrowedFrom>().bfi;
      case Kind::Reborrow:
        return payload.get<Reborrow>().argument;
      }
      llvm_unreachable("covered switch");
    }
  };
  friend llvm::DenseMapInfo<Def>;

  /// The defs derived from currentDef whose uses are added to liveness.
  SmallVector<Def, 8> discoveredDefs;

  /// The blocks that were discovered by PrunedLiveness.
  SmallVector<SILBasicBlock *, 32> discoveredBlocks;

  /// Pruned liveness for the extended live range including copies. For this
  /// purpose, only consuming instructions are considered "lifetime
  /// ending". end_borrows do not end a liverange that may include owned copies.
  BitfieldRef<SSAPrunedLiveness> liveness;

  /// The destroys of the value.  These are not uses, but need to be recorded so
  /// that we know when the last use in a consuming block is (without having to
  /// repeatedly do use-def walks from destroys).
  SmallPtrSetVector<SILInstruction *, 8> destroys;

  /// Information about consuming instructions discovered in this canonical OSSA
  /// lifetime.
  CanonicalOSSAConsumeInfo consumes;

public:
  /// When rewriting destroys, is this an instruction which destroys should not
  /// be hoisted over to avoid churn and infinite looping.
  static bool ignoredByDestroyHoisting(SILInstructionKind kind) {
    switch (kind) {
    case SILInstructionKind::DebugValueInst:
    case SILInstructionKind::DestroyValueInst:
    case SILInstructionKind::CopyValueInst:
    case SILInstructionKind::BeginBorrowInst:
    case SILInstructionKind::EndBorrowInst:
    case SILInstructionKind::FunctionRefInst:
    case SILInstructionKind::EnumInst:
    case SILInstructionKind::StructInst:
      return true;
    default:
      return false;
    }
  }

  /// Stack-allocated liveness for a single SSA def.
  struct LivenessState {
    BitfieldRef<SSAPrunedLiveness>::StackState state;

    LivenessState(CanonicalizeOSSALifetime &parent, SILValue def,
                  ArrayRef<SILInstruction *> lexicalLifetimeEnds)
        : state(parent.liveness, def->getFunction()) {
      parent.initializeLiveness(def, lexicalLifetimeEnds);
    }
  };

  CanonicalizeOSSALifetime(
      PruneDebugInsts_t pruneDebugMode, MaximizeLifetime_t maximizeLifetime,
      SILFunction *function, NonLocalAccessBlockAnalysis *accessBlockAnalysis,
      DeadEndBlocksAnalysis *deadEndBlocksAnalysis, DominanceInfo *domTree,
      BasicCalleeAnalysis *calleeAnalysis, InstructionDeleter &deleter)
      : pruneDebugMode(pruneDebugMode), maximizeLifetime(maximizeLifetime),
        accessBlockAnalysis(accessBlockAnalysis),
        deadEndBlocksAnalysis(deadEndBlocksAnalysis), domTree(domTree),
        calleeAnalysis(calleeAnalysis), deleter(deleter) {}

  SILValue getCurrentDef() const { return currentDef; }

  void initializeLiveness(SILValue def,
                          ArrayRef<SILInstruction *> lexicalLifetimeEnds) {
    assert(consumingBlocks.empty() && debugValues.empty());
    clear();

    currentDef = def;
    currentLexicalLifetimeEnds = lexicalLifetimeEnds;

    if (maximizeLifetime || respectsDeinitBarriers()) {
      liveness->initializeDiscoveredBlocks(&discoveredBlocks);
    }
    liveness->initializeDef(getCurrentDef());
  }

  void clear() {
    // Clear the access blocks analysis pointer in case the client invalidates
    // the analysis.  If the client did, the analysis will be recomputed in
    // extendLivenessThroughOverlappingAccess; if it didn't, the analysis
    // pointer will just be set back to its old value when the analysis' cache
    // is consulted in extendLivenessThroughOverlappingAccess.
    accessBlocks = nullptr;

    consumingBlocks.clear();
    debugValues.clear();
    discoveredBlocks.clear();
    consumes.clear();
    destroys.clear();
  }

  /// Top-Level API: rewrites copies and destroys within \p def's extended
  /// lifetime.
  ///
  /// For lexical values, canonicalization respects deinit barriers, introducing
  /// copies as needed to maintain lifetimes beyond final consuming uses to the
  /// original lexical lifetime end.  When a lexical value is explicitly
  /// consumed (via the `consume` keyword), however, the lifetime does not
  /// extend to original destroys beyond that consume--the value must be dead
  /// after the corresponding marker instructions (`move_value`); to support
  /// this shortening, the marker instructions must be provided as \p
  /// lexicalLifetimeEnds. When provided, deinit barriers will be respected
  /// except to the extent doing so would result in the value being live after
  /// the marker instructions.
  ///
  /// Return true if any change was made to \p def's extended lifetime. \p def
  /// itself will not be deleted and no instructions outside of \p def's
  /// extended lifetime will be affected (only copies and destroys are
  /// rewritten).
  ///
  /// This only deletes instructions within \p def's extended lifetime. Use
  /// InstructionDeleter::cleanUpDeadInstructions() to recursively delete dead
  /// operands.
  bool canonicalizeValueLifetime(
      SILValue def, ArrayRef<SILInstruction *> lexicalLifetimeEnds = {});

  /// Compute the liveness information for \p def. But do not do any rewriting
  /// or computation of boundaries.
  ///
  /// The intention is that this is used if one wants to emit diagnostics using
  /// the liveness information before doing any rewriting.
  ///
  /// Requires an active on-stack instance of LivenessState.
  ///
  ///     LivenessState livenessState(*this, def);
  ///
  bool computeLiveness();

  /// Given the already computed liveness boundary for the given def, rewrite
  /// copies of def as appropriate.
  ///
  /// Requires an active on-stack instance of LivenessState.
  void rewriteLifetimes();

  /// Return the pure original boundary just based off of liveness information
  /// without maximizing or extending liveness.
  ///
  /// Requires an active on-stack instance of LivenessState.
  void findOriginalBoundary(PrunedLivenessBoundary &resultingOriginalBoundary);

  InstModCallbacks &getCallbacks() { return deleter.getCallbacks(); }

  using IsInterestingUser = PrunedLiveness::IsInterestingUser;

  /// Helper method that returns the isInterestingUser status of \p user in the
  /// passed in Liveness.
  ///
  /// NOTE: Only call this after calling computeLivenessBoundary or the results
  /// will not be initialized.
  IsInterestingUser isInterestingUser(SILInstruction *user) const {
    return liveness->isInterestingUser(user);
  }

  using LifetimeEndingUserRange = PrunedLiveness::LifetimeEndingUserRange;
  LifetimeEndingUserRange getLifetimeEndingUsers() const {
    return liveness->getLifetimeEndingUsers();
  }

  using NonLifetimeEndingUserRange = PrunedLiveness::NonLifetimeEndingUserRange;
  NonLifetimeEndingUserRange getNonLifetimeEndingUsers() const {
    return liveness->getNonLifetimeEndingUsers();
  }

  using UserRange = PrunedLiveness::ConstUserRange;
  UserRange getUsers() const { return liveness->getAllUsers(); }

private:
  bool respectsDeinitBarriers() const {
    if (!currentDef->isLexical())
      return false;
    if (currentDef->getFunction()->forceEnableLexicalLifetimes())
      return true;
    auto &module = currentDef->getFunction()->getModule();
    return module.getASTContext().SILOpts.supportsLexicalLifetimes(module);
  }

  void recordDebugValue(DebugValueInst *dvi) { debugValues.insert(dvi); }

  void recordConsumingUse(Operand *use) { recordConsumingUser(use->getUser()); }
  /// Record that the value is consumed at `user`.
  ///
  /// Either `user` is a consuming use (e.g. `destroy_value`) or it is the
  /// terminator of a block on the availability boundary of the value in a
  /// dead-end region (e.g. `unreachable`).
  void recordConsumingUser(SILInstruction *user) {
    consumingBlocks.insert(user->getParent());
  }
  void recordUnreachableLifetimeEnd(SILInstruction *user) {
    recordConsumingUser(user);
    consumes.recordUnreachableLifetimeEnd(user);
  }
  bool computeCanonicalLiveness();

  bool endsAccessOverlappingPrunedBoundary(SILInstruction *inst);

  void extendLivenessThroughOverlappingAccess();

  void findExtendedBoundary(PrunedLivenessBoundary const &originalBoundary,
                            PrunedLivenessBoundary &boundary);

  void extendLivenessToDeadEnds();
  void extendLivenessToDeinitBarriers();

  void extendUnconsumedLiveness(PrunedLivenessBoundary const &boundary);
  void visitExtendedUnconsumedBoundary(
      ArrayRef<SILInstruction *> ends,
      llvm::function_ref<void(SILInstruction *, PrunedLiveness::LifetimeEnding)>
          visitor);

  void insertDestroysOnBoundary(PrunedLivenessBoundary const &boundary,
                                SmallVectorImpl<DestroyValueInst *> &destroys);

  void rewriteCopies(SmallVectorImpl<DestroyValueInst *> const &destroys);
};

} // end namespace swift

#endif
