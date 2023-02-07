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

struct Implementation;

} // namespace borrowtodestructure

class BorrowToDestructureTransform {
  friend borrowtodestructure::Implementation;

  using IntervalMapAllocator = borrowtodestructure::IntervalMapAllocator;

  IntervalMapAllocator &allocator;
  MarkMustCheckInst *mmci;
  DiagnosticEmitter &diagnosticEmitter;
  // Temporarily optional as this code is refactored.
  Optional<FieldSensitiveSSAPrunedLiveRange> liveness;
  SmallVector<Operand *, 8> destructureNeedingUses;
  PostOrderAnalysis *poa;
  PostOrderFunctionInfo *pofi = nullptr;
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

  /// After we have rewritten uses, cleanup the IR by deleting the original
  /// borrow/struct_extract/copies and inserting compensating destroy_values.
  void cleanup(StackList<BeginBorrowInst *> &borrowWorklist);
};

} // namespace siloptimizer
} // namespace swift

#endif
