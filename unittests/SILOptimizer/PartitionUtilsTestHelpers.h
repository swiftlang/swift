//===--- PartitionUtilsTestHelpers.h --------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// Shared test infrastructure for the SILOptimizer Partition / IsolationHistory
/// gtest binaries. Lives in a header (rather than each .cpp redefining the
/// same struct bodies) so multiple test translation units link cleanly
/// against a single ODR-canonical definition.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_UNITTESTS_SILOPTIMIZER_PARTITIONUTILSTESTHELPERS_H
#define SWIFT_UNITTESTS_SILOPTIMIZER_PARTITIONUTILSTESTHELPERS_H

#include "swift/SILOptimizer/Utils/PartitionUtils.h"

namespace swift {

/// A friend-of-Partition tester that exposes the otherwise-private
/// elementToRegionMap so tests can assert on which region each element
/// landed in. Forward-declared in PartitionUtils.h; defined here once.
struct Partition::PartitionTester {
  const Partition &p;

  PartitionTester(const Partition &p) : p(p) {}

  unsigned getRegion(unsigned elt) const {
    return unsigned(p.elementToRegionMap.at(PartitionPrimitives::Element(elt)));
  }

  auto begin() const { return p.elementToRegionMap.begin(); }
  auto end() const { return p.elementToRegionMap.end(); }
};

/// Build a Partition with each element of \p indices in its own region.
///
/// This is the synonym for the former Partition::separateRegions factory,
/// which was removed because it had no production callers: a single history
/// sequence boundary followed by one trackNewElement per index is exactly the
/// operation sequence that factory performed. Callers must pass distinct
/// indices.
inline Partition makePartitionWithSeparateRegions(
    SILLocation loc, llvm::ArrayRef<PartitionPrimitives::Element> indices,
    IsolationHistory history) {
  Partition p(history);
  if (indices.empty())
    return p;
  p.pushHistorySequenceBoundary(loc);
  for (PartitionPrimitives::Element index : indices)
    p.trackNewElement(index);
  return p;
}

/// A no-op PartitionOpEvaluator used by tests that drive Partition state
/// through PartitionOp sequences without caring about isolation info,
/// source locations, or send-error reporting. Provides just enough hooks
/// for PartitionOpEvaluatorBaseImpl to function.
struct MockedPartitionOpEvaluator final
    : PartitionOpEvaluatorBaseImpl<MockedPartitionOpEvaluator> {
  MockedPartitionOpEvaluator(Partition &workingPartition,
                             SendingOperandSetFactory &ptrSetFactory,
                             SendingOperandToStateMap &operandToStateMap)
      : PartitionOpEvaluatorBaseImpl(workingPartition, ptrSetFactory,
                                     operandToStateMap) {}

  SILIsolationInfo getIsolationRegionInfo(PartitionPrimitives::Element) const {
    return SILIsolationInfo::getDisconnected(false /*isUnsafeNonIsolated*/);
  }

  bool shouldTryToSquelchErrors() const { return false; }

  static SILLocation getLoc(SILInstruction *) { return SILLocation::invalid(); }

  static SILLocation getLoc(Operand *) { return SILLocation::invalid(); }

  static SILIsolationInfo getIsolationInfo(const PartitionOp &) { return {}; }

  static SILInstruction *getSourceInst(const PartitionOp &) { return nullptr; }

  static bool doesFunctionHaveSendingResult(const PartitionOp &) {
    return false;
  }
};

} // namespace swift

#endif // SWIFT_UNITTESTS_SILOPTIMIZER_PARTITIONUTILSTESTHELPERS_H
