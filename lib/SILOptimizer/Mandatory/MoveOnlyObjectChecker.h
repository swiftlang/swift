//===--- MoveOnlyObjectChecker.h ------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// This is for shared code in between the move only object checker and the move
/// only address checker. This is needed since the move only address checker
/// uses the move only object checker to check values loaded from allocations
/// that it is analyzing.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_MANDATORY_MOVEONLYOBJECTCHECKER_H
#define SWIFT_SILOPTIMIZER_MANDATORY_MOVEONLYOBJECTCHECKER_H

#include "swift/Basic/FrozenMultiMap.h"
#include "swift/SIL/FieldSensitivePrunedLiveness.h"
#include "swift/SIL/PrunedLiveness.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Utils/CanonicalizeOSSALifetime.h"
#include "llvm/ADT/IntervalMap.h"
#include "llvm/Support/Compiler.h"

namespace swift {
namespace siloptimizer {

/// Wrapper around CanonicalizeOSSALifetime that we use to specialize its
/// interface for our purposes.
struct OSSACanonicalizer {
  /// A per mark must check, vector of uses that copy propagation says need a
  /// copy and thus are not final consuming uses.
  SmallVector<SILInstruction *, 32> consumingUsesNeedingCopy;

  /// A per mark must check, vector of consuming uses that copy propagation says
  /// are actual last uses.
  SmallVector<SILInstruction *, 32> consumingBoundaryUsers;

  /// A list of non-consuming boundary uses.
  SmallVector<SILInstruction *, 32> nonConsumingBoundaryUsers;

  /// The actual canonicalizer that we use.
  ///
  /// We mark this Optional to avoid UB behavior caused by us needing to
  /// initialize CanonicalizeOSSALifetime with parts of OSSACanoncializer
  /// (specifically with state in our arrays) before the actual constructor has
  /// run. Specifically this avoids:
  ///
  /// 11.9.5p1 class.cdtor: For an object with a non-trivial constructor,
  /// referring to any non-static member or base class of the object before the
  /// constructor begins execution results in undefined behavior.
  Optional<CanonicalizeOSSALifetime> canonicalizer;

  OSSACanonicalizer() {}

  void init(SILFunction *fn, DominanceInfo *domTree,
            InstructionDeleter &deleter) {
    canonicalizer.emplace(false /*pruneDebugMode*/,
                          !fn->shouldOptimize() /*maximizeLifetime*/,
                          nullptr /*accessBlockAnalysis*/, domTree, deleter);
  }

  void clear() {
    consumingUsesNeedingCopy.clear();
    consumingBoundaryUsers.clear();
  }

  bool canonicalize(SILValue value);

  bool computeLiveness(SILValue value) {
    return canonicalizer->computeLiveness(value);
  }

  void computeBoundaryData(SILValue value);

  void rewriteLifetimes() { canonicalizer->rewriteLifetimes(); }

  void findOriginalBoundary(PrunedLivenessBoundary &resultingFoundBoundary) {
    canonicalizer->findOriginalBoundary(resultingFoundBoundary);
  }

  bool foundAnyConsumingUses() const {
    return consumingUsesNeedingCopy.size() || consumingBoundaryUsers.size();
  }

  bool foundConsumingUseRequiringCopy() const {
    return consumingUsesNeedingCopy.size();
  }

  bool foundFinalConsumingUses() const { return consumingBoundaryUsers.size(); }

  bool hasPartialApplyConsumingUse() const {
    return llvm::any_of(consumingUsesNeedingCopy,
                        [](SILInstruction *user) {
                          return isa<PartialApplyInst>(user);
                        }) ||
           llvm::any_of(consumingBoundaryUsers, [](SILInstruction *user) {
             return isa<PartialApplyInst>(user);
           });
  }

  bool hasNonPartialApplyConsumingUse() const {
    return llvm::any_of(consumingUsesNeedingCopy,
                        [](SILInstruction *user) {
                          return !isa<PartialApplyInst>(user);
                        }) ||
           llvm::any_of(consumingBoundaryUsers, [](SILInstruction *user) {
             return !isa<PartialApplyInst>(user);
           });
  }
};

class DiagnosticEmitter;

struct BorrowToDestructureTransform {
  class IntervalMapAllocator {
  public:
    using Map = llvm::IntervalMap<
        unsigned, SILValue,
        llvm::IntervalMapImpl::NodeSizer<unsigned, SILValue>::LeafSize,
        llvm::IntervalMapHalfOpenInfo<unsigned>>;

    using Allocator = Map::Allocator;

  private:
    /// Lazily initialized allocator.
    Optional<Allocator> allocator;

  public:
    Allocator &get() {
      if (!allocator)
        allocator.emplace();
      return *allocator;
    }
  };

  // We reserve more bits that we need at the beginning so that we can avoid
  // reallocating and potentially breaking our internal mutable array ref
  // points into the data store.
  struct AvailableValues {
    MutableArrayRef<SILValue> values;

    SILValue operator[](unsigned index) const { return values[index]; }
    SILValue &operator[](unsigned index) { return values[index]; }
    unsigned size() const { return values.size(); }

    AvailableValues() : values() {}
    AvailableValues(MutableArrayRef<SILValue> values) : values(values) {}

    void print(llvm::raw_ostream &os, const char *prefix = nullptr) const;
    SWIFT_DEBUG_DUMP;
  };

  struct AvailableValueStore {
    std::vector<SILValue> dataStore;
    llvm::DenseMap<SILBasicBlock *, AvailableValues> blockToValues;
    unsigned nextOffset = 0;
    unsigned numBits;

    AvailableValueStore(const FieldSensitivePrunedLiveness &liveness)
        : dataStore(liveness.getDiscoveredBlocks().size() *
                    liveness.getNumSubElements()),
          numBits(liveness.getNumSubElements()) {}

    std::pair<AvailableValues *, bool> get(SILBasicBlock *block) {
      auto iter = blockToValues.try_emplace(block, AvailableValues());

      if (!iter.second) {
        return {&iter.first->second, false};
      }

      iter.first->second.values =
          MutableArrayRef<SILValue>(&dataStore[nextOffset], numBits);
      nextOffset += numBits;
      return {&iter.first->second, true};
    }
  };

  IntervalMapAllocator &allocator;
  MarkMustCheckInst *mmci;
  DiagnosticEmitter &diagnosticEmitter;
  FieldSensitiveSSAPrunedLiveRange liveness;
  SmallVector<Operand *, 8> destructureNeedingUses;
  PostOrderAnalysis *poa;
  PostOrderFunctionInfo *pofi = nullptr;
  Optional<AvailableValueStore> blockToAvailableValues;
  SILValue initialValue;
  SmallVector<SILInstruction *, 8> createdDestructures;
  SmallVector<SILPhiArgument *, 8> createdPhiArguments;

  using InterestingUser = FieldSensitivePrunedLiveness::InterestingUser;
  SmallFrozenMultiMap<SILBasicBlock *, std::pair<Operand *, InterestingUser>, 8>
      blocksToUses;

  /// A frozen multi-map we use to diagnose consuming uses that are used by the
  /// same instruction as another consuming use or non-consuming use.
  SmallFrozenMultiMap<SILInstruction *, Operand *, 8>
      instToInterestingOperandIndexMap;

  BorrowToDestructureTransform(
      IntervalMapAllocator &allocator, MarkMustCheckInst *mmci,
      DiagnosticEmitter &diagnosticEmitter, PostOrderAnalysis *poa,
      SmallVectorImpl<SILBasicBlock *> &discoveredBlocks)
      : allocator(allocator), mmci(mmci), diagnosticEmitter(diagnosticEmitter),
        liveness(mmci->getFunction(), mmci, &discoveredBlocks), poa(poa) {
    liveness.initializeDef(mmci, TypeTreeLeafTypeRange(mmci));
  }

  PostOrderFunctionInfo *getPostOrderFunctionInfo() {
    if (!pofi)
      pofi = poa->get(mmci->getFunction());
    return pofi;
  }

  /// Visit all of the uses of \p mmci and find all begin_borrows.
  ///
  /// Returns false if we found an escape and thus cannot process. It is assumed
  /// that the caller will fail in such a case.
  static bool gatherBorrows(MarkMustCheckInst *mmci,
                            StackList<BeginBorrowInst *> &borrowWorklist);

  /// Walk through our borrow's uses recursively and find uses that require only
  /// a subset of the bits of our type. These are uses that are consuming uses
  /// that are destructure uses.
  bool gatherUses(StackList<BeginBorrowInst *> &borrowWorklist);

  /// Once we have gathered up all of our destructure uses and liveness
  /// requiring uses, validate that all of our destructure uses are on our
  /// boundary. Once we have done this, we know that it is safe to perform our
  /// transform.
  void checkDestructureUsesOnBoundary() const;

  /// Check for cases where we have two consuming uses on the same instruction
  /// or a consuming/non-consuming use on the same instruction.
  void checkForErrorsOnSameInstruction();

  /// Rewrite all of the uses of our borrow on our borrow operand, performing
  /// destructures as appropriate.
  void rewriteUses();

  /// After we have rewritten uses, cleanup the IR by deleting the original
  /// borrow/struct_extract/copies and inserting compensating destroy_values.
  void cleanup(StackList<BeginBorrowInst *> &borrowWorklist);

  AvailableValues &computeAvailableValues(SILBasicBlock *block);
};

/// Search for candidate object mark_must_checks. If we find one that does not
/// fit a pattern that we understand, emit an error diagnostic telling the
/// programmer that the move checker did not know how to recognize this code
/// pattern.
///
/// \returns true if we deleted a mark_must_check inst that we didn't recognize
/// after emitting the diagnostic.
///
/// To check if an error was emitted call checker.emittedAnyDiagnostics().
///
/// NOTE: This is the routine used by the move only object checker to find mark
/// must checks to process.
bool searchForCandidateObjectMarkMustChecks(
    SILFunction *fn,
    SmallSetVector<MarkMustCheckInst *, 32> &moveIntroducersToProcess,
    DiagnosticEmitter &emitter);

bool cleanupSILAfterEmittingObjectMoveOnlyDiagnostics(SILFunction *fn);

} // namespace siloptimizer
} // namespace swift

#endif
