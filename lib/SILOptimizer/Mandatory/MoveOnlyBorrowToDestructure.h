//===--- MoveOnlyBorrowToDestructure.h ------------------------------------===//
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

#ifndef SWIFT_SILOPTIMIZER_MANDATORY_MOVEONLYBORROWTODESTRUCTURE_H
#define SWIFT_SILOPTIMIZER_MANDATORY_MOVEONLYBORROWTODESTRUCTURE_H

#include "swift/Basic/FrozenMultiMap.h"
#include "swift/SIL/FieldSensitivePrunedLiveness.h"
#include "swift/SIL/StackList.h"
#include "swift/SILOptimizer/Analysis/PostOrderAnalysis.h"

#include "llvm/ADT/IntervalMap.h"

namespace swift {
namespace siloptimizer {

class DiagnosticEmitter;

namespace borrowtodestructure {

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

struct Implementation;

} // namespace borrowtodestructure

class BorrowToDestructureTransform {
  friend borrowtodestructure::Implementation;

  using IntervalMapAllocator = borrowtodestructure::IntervalMapAllocator;
  using AvailableValueStore = borrowtodestructure::AvailableValueStore;
  using AvailableValues = borrowtodestructure::AvailableValues;

  IntervalMapAllocator &allocator;
  MarkMustCheckInst *mmci;
  DiagnosticEmitter &diagnosticEmitter;
  // Temporarily optional as this code is refactored.
  Optional<FieldSensitiveSSAPrunedLiveRange> liveness;
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

  SmallVector<SILBasicBlock *, 8> discoveredBlocks;

public:
  BorrowToDestructureTransform(IntervalMapAllocator &allocator,
                               MarkMustCheckInst *mmci,
                               DiagnosticEmitter &diagnosticEmitter,
                               PostOrderAnalysis *poa)
      : allocator(allocator), mmci(mmci), diagnosticEmitter(diagnosticEmitter),
        liveness(), poa(poa) {
    liveness.emplace(mmci->getFunction(), &discoveredBlocks);
    liveness->init(mmci);
    liveness->initializeDef(mmci, TypeTreeLeafTypeRange(mmci));
  }

  bool transform();

private:
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

} // namespace siloptimizer
} // namespace swift

#endif
