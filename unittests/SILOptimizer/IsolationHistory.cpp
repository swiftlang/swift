//===--- IsolationHistory.cpp ---------------------------------------------===//
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
/// Baseline structural tests for IsolationHistory as exposed by
/// PartitionUtils.h. These pin invariants the SendNonSendable diagnostic
/// walker relies on that hold *today*. Tests that exercise specific bug
/// fixes live alongside those fixes.
///
//===----------------------------------------------------------------------===//

#include "swift/SILOptimizer/Utils/PartitionUtils.h"

#include "PartitionUtilsTestHelpers.h"

#include "gtest/gtest.h"

using namespace swift;
using namespace swift::PartitionPrimitives;

//===----------------------------------------------------------------------===//
//                            MARK: Utilities
//===----------------------------------------------------------------------===//

namespace {

using PartitionTester = Partition::PartitionTester;

/// Walk the parent chain of \p history and bucket each node by kind.
struct HistoryNodeCounts {
  unsigned total = 0;
  unsigned addNewRegion = 0;
  unsigned removeLastFromRegion = 0;
  unsigned removeFromRegion = 0;
  unsigned mergeElementRegions = 0;
  unsigned cfgHistoryJoin = 0;
  unsigned sequenceBoundary = 0;

  static HistoryNodeCounts from(IsolationHistory history) {
    HistoryNodeCounts counts;
    for (auto *node = history.getHead(); node; node = node->getNext()) {
      ++counts.total;
      switch (node->getKind()) {
      case IsolationHistory::Node::AddNewRegionForElement:
        ++counts.addNewRegion;
        break;
      case IsolationHistory::Node::RemoveLastElementFromRegion:
        ++counts.removeLastFromRegion;
        break;
      case IsolationHistory::Node::RemoveElementFromRegion:
        ++counts.removeFromRegion;
        break;
      case IsolationHistory::Node::MergeElementRegions:
        ++counts.mergeElementRegions;
        break;
      case IsolationHistory::Node::CFGHistoryJoin:
        ++counts.cfgHistoryJoin;
        break;
      case IsolationHistory::Node::SequenceBoundary:
        ++counts.sequenceBoundary;
        break;
      }
    }
    return counts;
  }
};

/// True iff every MergeElementRegions node in \p history has a
/// SequenceBoundary somewhere on its parent path. The chain walker
/// depends on this anchoring to attribute originating notes correctly.
bool everyMergeHasAncestorBoundary(IsolationHistory history) {
  for (auto *node = history.getHead(); node; node = node->getNext()) {
    if (node->getKind() != IsolationHistory::Node::MergeElementRegions)
      continue;
    bool foundBoundary = false;
    for (auto *p = node->getNext(); p; p = p->getNext()) {
      if (p->getKind() == IsolationHistory::Node::SequenceBoundary) {
        foundBoundary = true;
        break;
      }
    }
    if (!foundBoundary)
      return false;
  }
  return true;
}

/// Pop one PartitionOp worth of history (nodes up to and including the next
/// SequenceBoundary), undoing each. Returns true if more history remains.
/// Mirrors the drain the removed Partition::popHistory used to provide, built
/// on the node-returning popHistoryOnce.
bool popOnePartitionOp(Partition &p, SmallVectorImpl<SILBasicBlock *> &blocks) {
  while (auto *node = p.popHistoryOnce(blocks)) {
    if (node->getKind() == IsolationHistory::Node::SequenceBoundary)
      break;
  }
  return p.hasHistory();
}

} // namespace

//===----------------------------------------------------------------------===//
//                              MARK: Primitives
//===----------------------------------------------------------------------===//

// pushHistorySequenceBoundary at the top of an empty history puts a
// SequenceBoundary at head with a null parent. This is the shape the chain
// walker assumes when it commits pendingTargetMerge.
TEST(IsolationHistory, BoundaryAtHead) {
  llvm::BumpPtrAllocator allocator;
  IsolationHistory::Factory historyFactory(allocator, /*enabled=*/true);

  Partition p(historyFactory.get());
  EXPECT_FALSE(p.hasHistory());

  p.pushHistorySequenceBoundary(SILLocation::invalid());

  ASSERT_TRUE(p.hasHistory());
  auto *head = p.getIsolationHistory().getHead();
  EXPECT_EQ(head->getKind(), IsolationHistory::Node::SequenceBoundary);
  EXPECT_EQ(head->getNext(), nullptr);
}

// pushNewElementRegion records an AddNewRegionForElement node at head with
// the element stored at firstArg. The returned Node* is the new head.
TEST(IsolationHistory, PushNewElementRegionPrimitive) {
  llvm::BumpPtrAllocator allocator;
  IsolationHistory::Factory historyFactory(allocator, /*enabled=*/true);

  IsolationHistory history = historyFactory.get();
  auto *node = history.pushNewElementRegion(Element(7));

  ASSERT_NE(node, nullptr);
  EXPECT_EQ(history.getHead(), node);
  EXPECT_EQ(node->getKind(), IsolationHistory::Node::AddNewRegionForElement);
  EXPECT_EQ(node->getFirstArgAsElement(), Element(7));
  EXPECT_EQ(node->getNext(), nullptr);
}

// pushMergeElementRegions records a MergeElementRegions node carrying
// elementToMergeInto at firstArg and the peer list verbatim in
// additionalElementArgs.
TEST(IsolationHistory, PushMergeElementRegionsPrimitive) {
  llvm::BumpPtrAllocator allocator;
  IsolationHistory::Factory historyFactory(allocator, /*enabled=*/true);

  IsolationHistory history = historyFactory.get();
  history.pushMergeElementRegions(Element(0), Element(2), {Element(5)});

  const auto *head = history.getHead();
  ASSERT_NE(head, nullptr);
  EXPECT_EQ(head->getKind(), IsolationHistory::Node::MergeElementRegions);
  EXPECT_EQ(head->getFirstArgAsElement(), Element(0));
  auto args = head->getAdditionalElementArgs();
  ASSERT_EQ(args.size(), 2u);
  EXPECT_EQ(args[0], Element(2));
  EXPECT_EQ(args[1], Element(5));
}

//===----------------------------------------------------------------------===//
//                       MARK: Higher Level Operations
//===----------------------------------------------------------------------===//

// An empty singleRegion records nothing — no boundary, no add, no merge.
TEST(IsolationHistory, SingleRegionEmpty) {
  llvm::BumpPtrAllocator allocator;
  IsolationHistory::Factory historyFactory(allocator, /*enabled=*/true);

  SILLocation loc = SILLocation::invalid();
  auto p = Partition::singleRegion(loc, {}, historyFactory.get());

  EXPECT_FALSE(p.hasHistory());
  EXPECT_EQ(p.historySize(), 0u);
}

// A single element: one boundary + one AddNewRegionForElement, no merges.
TEST(IsolationHistory, SingleRegionOneElement) {
  llvm::BumpPtrAllocator allocator;
  IsolationHistory::Factory historyFactory(allocator, /*enabled=*/true);

  SILLocation loc = SILLocation::invalid();
  auto p = Partition::singleRegion(loc, {Element(7)}, historyFactory.get());

  auto counts = HistoryNodeCounts::from(p.getIsolationHistory());
  EXPECT_EQ(counts.sequenceBoundary, 1u);
  EXPECT_EQ(counts.addNewRegion, 1u);
  EXPECT_EQ(counts.mergeElementRegions, 0u)
      << "Single-element singleRegion has no peers to merge — recording a "
         "merge would later crash popHistoryOnce on extraction.";
}

// singleRegion(loc, [0,1,2,3]) must record exactly N-1 MergeElementRegions
// nodes (one per non-rep element, each carrying that single peer), NOT a
// single cumulative merge of size N-1 and NOT N cumulative merges of sizes
// 1..N-1.
//
// The seemingly-obvious fix of hoisting to one push with the full peer
// list does not round-trip via popHistoryOnce: a multi-peer merge node
// pops by re-merging peers into one shared region (since
// pushMergeElementRegions is normally generated by horizontalUpdate, where
// peers really were together in one region). singleRegion's peers each
// lived in their own region, so each peer needs its own merge node.
TEST(IsolationHistory, SingleRegionRecordsOneMergePerPeer) {
  llvm::BumpPtrAllocator allocator;
  IsolationHistory::Factory historyFactory(allocator, /*enabled=*/true);

  SILLocation loc = SILLocation::invalid();
  auto p = Partition::singleRegion(
      loc, {Element(0), Element(1), Element(2), Element(3)},
      historyFactory.get());

  auto counts = HistoryNodeCounts::from(p.getIsolationHistory());
  EXPECT_EQ(counts.sequenceBoundary, 1u);
  EXPECT_EQ(counts.addNewRegion, 4u);
  // N-1 = 3 single-peer merges, mirroring assignElement's record of
  // adding a new element to an existing region.
  EXPECT_EQ(counts.mergeElementRegions, 3u);
}

// Each merge node carries exactly one peer, and the peers across all merge
// nodes cover the non-rep elements without duplicates.
TEST(IsolationHistory, SingleRegionMergeNodesAreSinglePeer) {
  llvm::BumpPtrAllocator allocator;
  IsolationHistory::Factory historyFactory(allocator, /*enabled=*/true);

  SILLocation loc = SILLocation::invalid();
  auto p = Partition::singleRegion(
      loc, {Element(0), Element(1), Element(2), Element(3)},
      historyFactory.get());

  llvm::SmallVector<Element, 4> mergedPeers;
  for (const IsolationHistory::Node *n = p.getIsolationHistory().getHead(); n;
       n = n->getNext()) {
    if (n->getKind() != IsolationHistory::Node::MergeElementRegions)
      continue;
    EXPECT_EQ(n->getFirstArgAsElement(), Element(0));
    auto args = n->getAdditionalElementArgs();
    ASSERT_EQ(args.size(), 1u)
        << "Each singleRegion merge must have exactly one peer for "
           "round-trippable popHistory.";
    mergedPeers.push_back(args[0]);
  }

  // Order along parent chain is reverse-insertion (newest first), so the
  // peers walk back from the last loop iteration to the first.
  ASSERT_EQ(mergedPeers.size(), 3u);
  EXPECT_EQ(mergedPeers[0], Element(3));
  EXPECT_EQ(mergedPeers[1], Element(2));
  EXPECT_EQ(mergedPeers[2], Element(1));
}

// popHistory must rewind a singleRegion-built partition back to having no
// element mappings. Pre-fix this asserted in popHistoryOnce: the second
// MergeElementRegions pop would try to remove elements that the first pop
// already removed.
TEST(IsolationHistory, SingleRegionRoundTrip) {
  llvm::BumpPtrAllocator allocator;
  IsolationHistory::Factory historyFactory(allocator, /*enabled=*/true);

  SILLocation loc = SILLocation::invalid();
  auto p = Partition::singleRegion(
      loc, {Element(0), Element(1), Element(2), Element(3)},
      historyFactory.get());

  // Drain history. popHistory returns true while there's more to pop;
  // joins is unused since singleRegion never records a CFGHistoryJoin.
  llvm::SmallVector<SILBasicBlock *, 4> joins;
  while (popOnePartitionOp(p, joins))
    continue;

  EXPECT_FALSE(p.hasHistory());
  EXPECT_TRUE(joins.empty());
  // After full unwind, no element should have a region assignment.
  for (Element e : {Element(0), Element(1), Element(2), Element(3)}) {
    EXPECT_FALSE(p.isTrackingElement(e))
        << "Element " << unsigned(e) << " was not removed by popHistory";
  }
}

// Pin the canonical singleRegion node order. Each non-rep element is
// recorded as `pushNewElementRegion(k); pushMergeElementRegions(0, {k})`,
// so the parent chain head→root walks:
//
//   MergeElementRegions(0, [3])
//     -> AddNewRegionForElement(3)
//     -> MergeElementRegions(0, [2])
//     -> AddNewRegionForElement(2)
//     -> MergeElementRegions(0, [1])
//     -> AddNewRegionForElement(1)
//     -> AddNewRegionForElement(0)   (rep, pushed first)
//     -> SequenceBoundary
//     -> nullptr
//
// If the structure ever drifts (boundary moves, merge re-aggregates, rep
// stops being pushed first) every consumer of IsolationHistory needs to
// know.
TEST(IsolationHistory, SingleRegionParentChainShape) {
  llvm::BumpPtrAllocator allocator;
  IsolationHistory::Factory historyFactory(allocator, /*enabled=*/true);

  SILLocation loc = SILLocation::invalid();
  auto p = Partition::singleRegion(
      loc, {Element(0), Element(1), Element(2), Element(3)},
      historyFactory.get());

  const IsolationHistory::Node *node = p.getIsolationHistory().getHead();

  // Three (merge → addNew) pairs, in reverse-insertion order: 3, 2, 1.
  for (unsigned peer : {3u, 2u, 1u}) {
    ASSERT_NE(node, nullptr);
    EXPECT_EQ(node->getKind(), IsolationHistory::Node::MergeElementRegions);
    EXPECT_EQ(node->getFirstArgAsElement(), Element(0));
    auto args = node->getAdditionalElementArgs();
    ASSERT_EQ(args.size(), 1u);
    EXPECT_EQ(args[0], Element(peer));
    node = node->getNext();

    ASSERT_NE(node, nullptr);
    EXPECT_EQ(node->getKind(), IsolationHistory::Node::AddNewRegionForElement);
    EXPECT_EQ(node->getFirstArgAsElement(), Element(peer));
    node = node->getNext();
  }

  // Then the rep element's AddNewRegionForElement (pushed first inside the
  // loop's prelude).
  ASSERT_NE(node, nullptr);
  EXPECT_EQ(node->getKind(), IsolationHistory::Node::AddNewRegionForElement);
  EXPECT_EQ(node->getFirstArgAsElement(), Element(0));
  node = node->getNext();

  // Finally the boundary that opened the sequence.
  ASSERT_NE(node, nullptr);
  EXPECT_EQ(node->getKind(), IsolationHistory::Node::SequenceBoundary);
  EXPECT_EQ(node->getNext(), nullptr);
}

// Callers are NOT required to pass indices in ascending order (e.g.
// RegionAnalysis builds the joined-argument list in function-argument order,
// but the element IDs are assigned at first-encounter and can be interleaved
// by the pre-dataflow scan). singleRegion must still pick the *minimum*
// element as the region representative, because is_canonical_correct requires
// the region label to be <= every element in the region. Using indices[0]
// instead of the minimum would trip that assertion whenever indices[0] is not
// the smallest element. Here indices[0] == 3 but the rep must be 0.
TEST(IsolationHistory, SingleRegionUnsortedRepIsMinimum) {
  llvm::BumpPtrAllocator allocator;
  IsolationHistory::Factory historyFactory(allocator, /*enabled=*/true);

  SILLocation loc = SILLocation::invalid();
  auto p = Partition::singleRegion(
      loc, {Element(3), Element(1), Element(2), Element(0)},
      historyFactory.get());

  // All four elements are tracked and land in the same region, whose label is
  // the minimum element (0) — not indices[0] (3).
  PartitionTester tester(p);
  for (Element e : {Element(0), Element(1), Element(2), Element(3)}) {
    ASSERT_TRUE(p.isTrackingElement(e))
        << "Element " << unsigned(e) << " was not tracked";
    EXPECT_EQ(tester.getRegion(unsigned(e)), 0u)
        << "Element " << unsigned(e)
        << " should be in the region labelled by the minimum element (0)";
  }

  // Every merge node must name the minimum element (0) as its rep, regardless
  // of the order indices were passed in.
  for (const IsolationHistory::Node *n = p.getIsolationHistory().getHead(); n;
       n = n->getNext()) {
    if (n->getKind() != IsolationHistory::Node::MergeElementRegions)
      continue;
    EXPECT_EQ(n->getFirstArgAsElement(), Element(0))
        << "singleRegion must merge peers into the minimum element's region";
  }
}

// Partition::singleRegion requires distinct indices. A repeated element
// would push pushNewElementRegion(index) + pushMergeElementRegions(rep,
// index) twice for the same element, which popHistoryOnce cannot rewind
// (the second MergeElementRegions pop tries to removeElement an element
// the first pop already removed, tripping "Failed to erase?!"). Rather
// than silently de-duplicate, singleRegion treats a duplicate as a caller
// bug and asserts. Only observable with assertions enabled.
#ifndef NDEBUG
TEST(IsolationHistoryDeathTest, SingleRegionDuplicateIndexAsserts) {
  llvm::BumpPtrAllocator allocator;
  IsolationHistory::Factory historyFactory(allocator, /*enabled=*/true);
  SILLocation loc = SILLocation::invalid();

  // Element 1 listed twice — caller bug.
  EXPECT_DEATH(Partition::singleRegion(loc,
                                       {Element(0), Element(1), Element(1)},
                                       historyFactory.get()),
               "does not support duplicate indices");
}
#endif

// makePartitionWithSeparateRegions pushes a single boundary and one
// AddNewRegionForElement per index — sanity check that we don't accidentally
// synthesize a merge, that every input element is tracked afterwards, and
// that each lives in a distinct region.
TEST(IsolationHistory, SeparateRegionsShape) {
  llvm::BumpPtrAllocator allocator;
  IsolationHistory::Factory historyFactory(allocator, /*enabled=*/true);

  SILLocation loc = SILLocation::invalid();
  auto p = makePartitionWithSeparateRegions(
      loc, {Element(0), Element(1), Element(2)}, historyFactory.get());

  auto counts = HistoryNodeCounts::from(p.getIsolationHistory());
  EXPECT_EQ(counts.sequenceBoundary, 1u);
  EXPECT_EQ(counts.addNewRegion, 3u);
  EXPECT_EQ(counts.mergeElementRegions, 0u);

  for (Element e : {Element(0), Element(1), Element(2)})
    EXPECT_TRUE(p.isTrackingElement(e))
        << "Element " << unsigned(e) << " was not tracked";

  // Each element must land in a distinct region — that's the whole
  // point of separate regions vs singleRegion.
  PartitionTester tester(p);
  unsigned r0 = tester.getRegion(0);
  unsigned r1 = tester.getRegion(1);
  unsigned r2 = tester.getRegion(2);
  EXPECT_NE(r0, r1);
  EXPECT_NE(r0, r2);
  EXPECT_NE(r1, r2);
}

// popHistory drains a separate-regions partition back to having no element
// mappings. Unlike singleRegion, the per-element push pattern is the same
// shape (one AddNew per element) regardless of N — no merge nodes to interact
// with — so the round-trip works on today's tree (with distinct indices).
TEST(IsolationHistory, SeparateRegionsRoundTrip) {
  llvm::BumpPtrAllocator allocator;
  IsolationHistory::Factory historyFactory(allocator, /*enabled=*/true);

  SILLocation loc = SILLocation::invalid();
  auto p = makePartitionWithSeparateRegions(
      loc, {Element(0), Element(1), Element(2)}, historyFactory.get());

  llvm::SmallVector<SILBasicBlock *, 4> joins;
  while (popOnePartitionOp(p, joins))
    continue;

  EXPECT_FALSE(p.hasHistory());
  EXPECT_TRUE(joins.empty());
  for (Element e : {Element(0), Element(1), Element(2)})
    EXPECT_FALSE(p.isTrackingElement(e))
        << "Element " << unsigned(e) << " was not removed by popHistory";
}

// Each MergeElementRegions node pre-existing in the joined predecessors
// must continue to have a SequenceBoundary on its parent path after a
// join. Today this holds because each predecessor's singleRegion /
// trackNewElement push their own boundaries; this test pins that, so a
// future change to Partition::join that drops or re-orders predecessor
// history is caught.
TEST(IsolationHistory, JoinPreservesAncestorBoundaryForExistingMerges) {
  llvm::BumpPtrAllocator allocator;
  IsolationHistory::Factory historyFactory(allocator, /*enabled=*/true);

  SILLocation loc = SILLocation::invalid();

  // fst: {0,1} merged into one region; element 2 in its own region.
  Partition fst = Partition::singleRegion(loc, {Element(0), Element(1)},
                                          historyFactory.get());
  // Pretend a second instruction landed: introduce element 2 separately.
  fst.pushHistorySequenceBoundary(loc);
  fst.trackNewElement(Element(2));

  // snd: 0, 1, 2 are each in their own region.
  Partition snd = makePartitionWithSeparateRegions(
      loc, {Element(0), Element(1), Element(2)}, historyFactory.get());

  // Both predecessors carry their own per-instruction boundaries.
  EXPECT_TRUE(everyMergeHasAncestorBoundary(fst.getIsolationHistory()));
  EXPECT_TRUE(everyMergeHasAncestorBoundary(snd.getIsolationHistory()));

  Partition joined = Partition::join(fst, snd);

  EXPECT_TRUE(everyMergeHasAncestorBoundary(joined.getIsolationHistory()));
}

// Partition::join's "sndEltNumber not in result, sndRegionNumber's rep
// IS in result" branch pushes pushMergeElementRegions(sndEltNumber,
// [Element(sndRegionNumber)]) without a preceding
// pushNewElementRegion(sndEltNumber). popHistoryOnce on the synthesized
// merge extracts the rep but leaves sndEltNumber stranded in
// elementToRegionMap, breaking round-trippability. The third branch in
// the same loop, and Partition::assignElement's analogous "new element
// added to existing region" path, both push the AddNewRegionForElement.
TEST(IsolationHistory, JoinSecondBranchPushPopAsymmetry) {
  llvm::BumpPtrAllocator allocator;
  IsolationHistory::Factory historyFactory(allocator, /*enabled=*/true);
  SILLocation loc = SILLocation::invalid();

  // fst tracks only element 0.
  auto fst = Partition::singleRegion(loc, {Element(0)}, historyFactory.get());
  // snd has elements 0 and 1 in the same region.
  auto snd = Partition::singleRegion(loc, {Element(0), Element(1)},
                                     historyFactory.get());

  // Join hits the second branch for element 1: 1 is not in result, but
  // its rep Element(0) is.
  auto joined = Partition::join(fst, snd);
  ASSERT_TRUE(joined.isTrackingElement(Element(1)));
  ASSERT_TRUE(joined.isTrackingElement(Element(0)));

  // Drain the joined partition's history. After full unwind, no element
  // should be tracked — both fst and snd's contributions should reverse.
  llvm::SmallVector<SILBasicBlock *, 4> joins;
  while (popOnePartitionOp(joined, joins))
    continue;

  EXPECT_FALSE(joined.isTrackingElement(Element(1)))
      << "Partition::join's second branch did not record an "
         "AddNewRegionForElement for the new element, so popHistoryOnce "
         "on the synthesized merge cannot remove it.";
  EXPECT_FALSE(joined.isTrackingElement(Element(0)));
}

//===----------------------------------------------------------------------===//
//                MARK: PartitionOp-driven round-trip tests
//
// Drive Partition state through PartitionOp sequences and assert that
// popHistory rewinds to a prior snapshot. Asserts on history mechanics,
// not on the partition operations themselves.
//===----------------------------------------------------------------------===//

TEST(IsolationHistory, CreateVariable) {
  llvm::BumpPtrAllocator allocator;
  Partition::SendingOperandSetFactory factory(allocator);
  IsolationHistory::Factory historyFactory(allocator, /*enabled=*/true);
  SmallVector<SILBasicBlock *, 8> joinedHistories;
  SendingOperandToStateMap transferringOpToStateMap(historyFactory);

  // First make sure that we do this correctly with an assign fresh.
  Partition p(historyFactory.get());
  {
    MockedPartitionOpEvaluator eval(p, factory, transferringOpToStateMap);
    eval.apply({PartitionOp::AssignFresh(Element(0)),
                PartitionOp::AssignFresh(Element(1))});
  }

  Partition pSnapshot = p;

  {
    MockedPartitionOpEvaluator eval(p, factory, transferringOpToStateMap);
    eval.apply({PartitionOp::AssignFresh(Element(2))});
  }

  popOnePartitionOp(p, joinedHistories);

  EXPECT_TRUE(Partition::equals(p, pSnapshot));
  EXPECT_TRUE(joinedHistories.empty());
}

TEST(IsolationHistory, AssignRegion) {
  llvm::BumpPtrAllocator allocator;
  Partition::SendingOperandSetFactory factory(allocator);
  IsolationHistory::Factory historyFactory(allocator, /*enabled=*/true);
  SendingOperandToStateMap transferringOpToStateMap(historyFactory);
  SmallVector<SILBasicBlock *, 8> joinedHistories;

  // First make sure that we do this correctly with an assign fresh.
  Partition p(historyFactory.get());
  {
    MockedPartitionOpEvaluator eval(p, factory, transferringOpToStateMap);
    eval.apply({PartitionOp::AssignFresh(Element(0)),
                PartitionOp::AssignFresh(Element(1)),
                PartitionOp::AssignFresh(Element(2))});
  }

  Partition pSnapshot = p;

  {
    MockedPartitionOpEvaluator eval(p, factory, transferringOpToStateMap);
    eval.apply({PartitionOp::AssignDirect(Element(1), Element(2))});
  }

  Partition pSnapshot2 = p;
  {
    MockedPartitionOpEvaluator eval(p, factory, transferringOpToStateMap);
    eval.apply({PartitionOp::AssignDirect(Element(0), Element(2))});
  }

  popOnePartitionOp(p, joinedHistories);

  EXPECT_TRUE(Partition::equals(p, pSnapshot2));
  EXPECT_TRUE(joinedHistories.empty());

  popOnePartitionOp(p, joinedHistories);

  EXPECT_TRUE(Partition::equals(p, pSnapshot));
  EXPECT_TRUE(joinedHistories.empty());
}

TEST(IsolationHistory, BuildNewRegionRepIsMerge) {
  llvm::BumpPtrAllocator allocator;
  Partition::SendingOperandSetFactory factory(allocator);
  IsolationHistory::Factory historyFactory(allocator, /*enabled=*/true);
  SendingOperandToStateMap transferringOpToStateMap(historyFactory);
  SmallVector<SILBasicBlock *, 8> joinedHistories;

  Partition p(historyFactory.get());
  {
    MockedPartitionOpEvaluator eval(p, factory, transferringOpToStateMap);
    eval.apply({PartitionOp::AssignFresh(Element(2)),
                PartitionOp::AssignFresh(Element(3)),
                PartitionOp::AssignFresh(Element(10)),
                PartitionOp::AssignFresh(Element(0)),
                PartitionOp::AssignDirect(Element(3), Element(2)),
                PartitionOp::AssignDirect(Element(10), Element(2)),
                PartitionOp::Merge(Element(2), Element(0),
                                   RegionMergeReason::Unknown)});
  }

  Partition pSnapshot = p;

  {
    MockedPartitionOpEvaluator eval(p, factory, transferringOpToStateMap);
    eval.apply({PartitionOp::AssignDirect(Element(1), Element(2))});
  }

  Partition pSnapshot2 = p;
  {
    MockedPartitionOpEvaluator eval(p, factory, transferringOpToStateMap);
    eval.apply({PartitionOp::AssignDirect(Element(0), Element(2))});
  }

  // Even though we pushed a new instruction, nothing changed in our region.
  EXPECT_TRUE(Partition::equals(p, pSnapshot2));

  // We pop but nothing changes since we did not need to change anything.
  popOnePartitionOp(p, joinedHistories);

  EXPECT_TRUE(Partition::equals(p, pSnapshot2));
  EXPECT_TRUE(joinedHistories.empty());

  // We pop a last time to return to our original value.
  popOnePartitionOp(p, joinedHistories);

  EXPECT_TRUE(Partition::equals(p, pSnapshot));
  EXPECT_TRUE(joinedHistories.empty());
}

TEST(IsolationHistory, ReturnFalseWhenNoneLeft) {
  llvm::BumpPtrAllocator allocator;
  Partition::SendingOperandSetFactory factory(allocator);
  IsolationHistory::Factory historyFactory(allocator, /*enabled=*/true);
  SmallVector<SILBasicBlock *, 8> joinedHistories;
  SendingOperandToStateMap transferringOpToStateMap(historyFactory);

  Partition p(historyFactory.get());

  EXPECT_FALSE(popOnePartitionOp(p, joinedHistories));
  EXPECT_TRUE(joinedHistories.empty());

  {
    MockedPartitionOpEvaluator eval(p, factory, transferringOpToStateMap);
    eval.apply({PartitionOp::AssignFresh(Element(2)),
                PartitionOp::AssignFresh(Element(3))});
  }

  EXPECT_TRUE(popOnePartitionOp(p, joinedHistories));
  EXPECT_TRUE(joinedHistories.empty());

  EXPECT_FALSE(popOnePartitionOp(p, joinedHistories));
  EXPECT_TRUE(joinedHistories.empty());
}

TEST(IsolationHistory, JoiningTwoEmpty) {
  // Make sure that we do sane things when we join empty history.
  llvm::BumpPtrAllocator allocator;
  Partition::SendingOperandSetFactory factory(allocator);
  IsolationHistory::Factory historyFactory(allocator, /*enabled=*/true);
  SmallVector<SILBasicBlock *, 8> joinedHistories;

  Partition p1(historyFactory.get());
  Partition p2(historyFactory.get());

  auto result = Partition::join(p1, p2);
  PartitionTester resultTester(result);
  EXPECT_TRUE(resultTester.begin() == resultTester.end());
  EXPECT_FALSE(result.hasHistory());
}

TEST(IsolationHistory, JoiningNotEmptyAndEmpty) {
  // Make sure that we do sane things when we join empty history.
  llvm::BumpPtrAllocator allocator;
  Partition::SendingOperandSetFactory factory(allocator);
  IsolationHistory::Factory historyFactory(allocator, /*enabled=*/true);
  SmallVector<SILBasicBlock *, 8> joinedHistories;
  SendingOperandToStateMap transferringOpToStateMap(historyFactory);

  Partition p1(historyFactory.get());
  Partition p2(historyFactory.get());

  {
    MockedPartitionOpEvaluator eval(p1, factory, transferringOpToStateMap);
    eval.apply({PartitionOp::AssignFresh(Element(2))});
  }

  EXPECT_TRUE(p1.historySize() == 2);
  EXPECT_TRUE(p2.historySize() == 0);
  auto result = Partition::join(p1, p2);
  PartitionTester resultTester(result);
  EXPECT_TRUE(std::next(resultTester.begin()) == resultTester.end());
  // Since p2 doesn't have any history, we do not actually perform any join and
  // thus do not insert a CFGHistory change.
  EXPECT_TRUE(result.historySize() == 2);
}

TEST(IsolationHistory, JoiningEmptyAndNotEmpty) {
  // Make sure that we do sane things when we join empty history.
  llvm::BumpPtrAllocator allocator;
  Partition::SendingOperandSetFactory factory(allocator);
  IsolationHistory::Factory historyFactory(allocator, /*enabled=*/true);
  SendingOperandToStateMap transferringOpToStateMap(historyFactory);
  SmallVector<SILBasicBlock *, 8> joinedHistories;

  Partition p1(historyFactory.get());
  Partition p2(historyFactory.get());

  {
    MockedPartitionOpEvaluator eval(p1, factory, transferringOpToStateMap);
    eval.apply({PartitionOp::AssignFresh(Element(2))});
  }

  EXPECT_TRUE(p1.historySize() == 2);
  EXPECT_TRUE(p2.historySize() == 0);
  auto result = Partition::join(p1, p2);
  PartitionTester resultTester(result);
  EXPECT_TRUE(std::next(resultTester.begin()) == resultTester.end());
  // Since p2 doesn't have any history, we do not actually perform any join and
  // thus do not insert a CFGHistory change.
  EXPECT_TRUE(result.historySize() == 2);
}

// Partition::merge moves *every* element of snd's region into fst's region
// (horizontalUpdate collects them into mergedElements), but the recorded
// MergeElementRegions node names only the two operands. popHistoryOnce
// reverses a merge by extracting additionalElementArgs, so an element that
// came along as a passenger is left behind in fst's region.
//
// That stranding is not self-correcting. Extraction only ever moves
// additionalElementArgs, never getFirstArgAsElement, and merge() orders the
// node so the operand from the lower-labelled region is the survivor. So the
// passenger is only pulled back out if some older node happens to name it as
// its own snd -- and the node that put it in fst's region does not mention it
// at all.
//
// Here {1, 2} is merged into {0}'s region using 2 as the operand, so the node
// is (0, [2]) and element 1 is the passenger. Rewinding that merge must
// restore {0} | {1, 2}.
TEST(IsolationHistory, MergePassengerRoundTrip) {
  llvm::BumpPtrAllocator allocator;
  Partition::SendingOperandSetFactory factory(allocator);
  IsolationHistory::Factory historyFactory(allocator, /*enabled=*/true);
  SendingOperandToStateMap opToStateMap(historyFactory);
  SmallVector<SILBasicBlock *, 8> joins;

  // Two regions: {1, 2} and {0}. Region labels are the minimum element, so
  // {1, 2} is labelled 1 and {0} is labelled 0.
  Partition p(historyFactory.get());
  {
    MockedPartitionOpEvaluator eval(p, factory, opToStateMap);
    eval.apply({PartitionOp::AssignFresh(Element(0)),
                PartitionOp::AssignFresh(Element(1)),
                PartitionOp::AssignFresh(Element(2)),
                PartitionOp::Merge(Element(1), Element(2),
                                   RegionMergeReason::Unknown)});
  }
  Partition snapshot = p;

  {
    PartitionTester before(p);
    ASSERT_EQ(before.getRegion(1), before.getRegion(2));
    ASSERT_NE(before.getRegion(0), before.getRegion(1));
  }

  // Merge {1, 2} into {0}'s region via operand 2. Since region(0) < region(1),
  // 0 is the survivor and 2 is the extracted operand; 1 is carried along by
  // horizontalUpdate without being named in the history node.
  {
    MockedPartitionOpEvaluator eval(p, factory, opToStateMap);
    eval.apply({PartitionOp::Merge(Element(0), Element(2),
                                   RegionMergeReason::Unknown)});
  }

  {
    PartitionTester after(p);
    ASSERT_EQ(after.getRegion(0), after.getRegion(1));
    ASSERT_EQ(after.getRegion(0), after.getRegion(2));
  }

  popOnePartitionOp(p, joins);
  EXPECT_TRUE(joins.empty());

  PartitionTester rewound(p);
  EXPECT_EQ(rewound.getRegion(1), rewound.getRegion(2))
      << "Element 1 was a passenger of the merge and must return to element "
         "2's region.";
  EXPECT_NE(rewound.getRegion(0), rewound.getRegion(1))
      << "Element 1 is stranded in element 0's region: Partition::merge "
         "recorded only its two operands, so popHistoryOnce extracted 2 and "
         "left 1 behind.";
  EXPECT_TRUE(Partition::equals(p, snapshot))
      << "Merge with a passenger element did not rewind cleanly.";
}

// The passengers must come back as *one* region, not as singletons. They were
// region-mates of the extracted operand before the merge, so popHistoryOnce
// re-merges each of them onto additionalElementArgs[0] after re-tracking it.
// Dropping that re-merge splits a region the program never split: each
// passenger would land in a fresh region of its own.
//
// Here {1, 2, 3} is merged into {0}'s region using 3 as the operand, so the
// node is (0, [3, 1, 2]) and rewinding must restore {0} | {1, 2, 3}.
TEST(IsolationHistory, MergePassengersRejoinOneRegion) {
  llvm::BumpPtrAllocator allocator;
  Partition::SendingOperandSetFactory factory(allocator);
  IsolationHistory::Factory historyFactory(allocator, /*enabled=*/true);
  SendingOperandToStateMap opToStateMap(historyFactory);
  SmallVector<SILBasicBlock *, 8> joins;

  Partition p(historyFactory.get());
  {
    MockedPartitionOpEvaluator eval(p, factory, opToStateMap);
    eval.apply(
        {PartitionOp::AssignFresh(Element(0)),
         PartitionOp::AssignFresh(Element(1)),
         PartitionOp::AssignFresh(Element(2)),
         PartitionOp::AssignFresh(Element(3)),
         PartitionOp::Merge(Element(1), Element(2), RegionMergeReason::Unknown),
         PartitionOp::Merge(Element(1), Element(3),
                            RegionMergeReason::Unknown)});
  }
  Partition snapshot = p;

  {
    PartitionTester before(p);
    ASSERT_EQ(before.getRegion(1), before.getRegion(2));
    ASSERT_EQ(before.getRegion(1), before.getRegion(3));
    ASSERT_NE(before.getRegion(0), before.getRegion(1));
  }

  // 3 is the operand, so 1 and 2 ride along as passengers.
  {
    MockedPartitionOpEvaluator eval(p, factory, opToStateMap);
    eval.apply({PartitionOp::Merge(Element(0), Element(3),
                                   RegionMergeReason::Unknown)});
  }

  popOnePartitionOp(p, joins);
  EXPECT_TRUE(joins.empty());

  PartitionTester rewound(p);
  EXPECT_EQ(rewound.getRegion(1), rewound.getRegion(2))
      << "Passengers 1 and 2 were region-mates before the merge and must be "
         "region-mates again.";
  EXPECT_EQ(rewound.getRegion(1), rewound.getRegion(3))
      << "Passengers must rejoin the extracted operand's region, not sit in "
         "fresh regions of their own.";
  EXPECT_NE(rewound.getRegion(0), rewound.getRegion(1));
  EXPECT_TRUE(Partition::equals(p, snapshot))
      << "Merge with multiple passengers did not rewind cleanly.";
}

// popHistoryOnce off-by-one when reversing RemoveElementFromRegion:
// pushRemoveElementFromRegion stores the surviving sibling at
// additionalElementArgs[0] (the only entry), but popHistoryOnce previously
// indexed [1] when reversing the remove, asserting in
// ArrayRef::operator[]. Build a partition where AssignDirect *moves* an
// element from a region with surviving siblings to a different region —
// the only public path that records a RemoveElementFromRegion node — and
// drain the history.
TEST(IsolationHistory, AssignDirectMovesElementRoundTrip) {
  llvm::BumpPtrAllocator allocator;
  Partition::SendingOperandSetFactory factory(allocator);
  IsolationHistory::Factory historyFactory(allocator, /*enabled=*/true);
  SendingOperandToStateMap opToStateMap(historyFactory);
  SmallVector<SILBasicBlock *, 8> joins;

  // Set up two separate regions: {0, 1} and {2}. Element 1 lives in 0's
  // region.
  Partition p(historyFactory.get());
  {
    MockedPartitionOpEvaluator eval(p, factory, opToStateMap);
    eval.apply({PartitionOp::AssignFresh(Element(0)),
                PartitionOp::AssignFresh(Element(1)),
                PartitionOp::AssignFresh(Element(2)),
                PartitionOp::Merge(Element(0), Element(1),
                                   RegionMergeReason::Unknown)});
  }
  Partition snapshot = p;

  PartitionTester before(p);
  unsigned region01 = before.getRegion(0);
  unsigned region2 = before.getRegion(2);
  EXPECT_NE(region01, region2);
  EXPECT_EQ(before.getRegion(1), region01);

  // Move element 1 over to element 2's region. This goes through the
  // non-emplace branch of assignElement: oldRegion has another element (0),
  // so RemoveElementFromRegion is pushed.
  {
    MockedPartitionOpEvaluator eval(p, factory, opToStateMap);
    eval.apply({PartitionOp::AssignDirect(Element(1), Element(2))});
  }

  PartitionTester after(p);
  EXPECT_EQ(after.getRegion(1), after.getRegion(2));
  EXPECT_NE(after.getRegion(1), after.getRegion(0));

  popOnePartitionOp(p, joins);
  EXPECT_TRUE(joins.empty());
  EXPECT_TRUE(Partition::equals(p, snapshot))
      << "AssignDirect that moved an element across regions did not "
         "rewind cleanly.";
}

//===----------------------------------------------------------------------===//
//                      MARK: History recording gate
//===----------------------------------------------------------------------===//

// A partition built from a disabled Factory records nothing at all, no matter
// what is done to it. isRecordingIsolationHistory() is the gate every consumer
// keys off before it snapshots a partition to rewind later, so pin that it
// tracks the thing that actually controls recording: with it false, there is no
// history to walk and popHistoryOnce would assert.
TEST(IsolationHistory, RecordingDisabledPartitionHasNoHistory) {
  llvm::BumpPtrAllocator allocator;
  IsolationHistory::Factory historyFactory(allocator, /*enabled=*/false);

  SILLocation loc = SILLocation::invalid();
  // Same mutations as SingleRegionRecordsOneMergePerPeer, which records a
  // boundary, four AddNewRegionForElement nodes and three merges when enabled.
  auto p = Partition::singleRegion(
      loc, {Element(0), Element(1), Element(2), Element(3)},
      historyFactory.get());

  EXPECT_FALSE(p.isRecordingIsolationHistory());
  EXPECT_FALSE(p.hasHistory());
  EXPECT_EQ(p.historySize(), 0u);
  EXPECT_EQ(p.getIsolationHistory().getHead(), nullptr);

  // The region mapping itself is unaffected by the gate -- only the history is.
  PartitionTester tester(p);
  EXPECT_EQ(tester.getRegion(0), tester.getRegion(3));
}

// isRecordingIsolationHistory() is not a synonym for hasHistory(): an enabled
// partition reports recording before anything has been pushed to it. Consumers
// rely on the distinction, since they decide whether to snapshot a partition
// before knowing whether the walk will find anything in it.
TEST(IsolationHistory, RecordingEnabledPartitionReportsRecording) {
  llvm::BumpPtrAllocator allocator;
  IsolationHistory::Factory historyFactory(allocator, /*enabled=*/true);

  Partition p(historyFactory.get());
  EXPECT_TRUE(p.isRecordingIsolationHistory());
  EXPECT_FALSE(p.hasHistory());
  EXPECT_EQ(p.historySize(), 0u);

  p.pushHistorySequenceBoundary(SILLocation::invalid());

  EXPECT_TRUE(p.isRecordingIsolationHistory());
  EXPECT_TRUE(p.hasHistory());
}

// Copying a Partition shares the immutable history node chain rather than
// duplicating it, and rewinding the copy leaves the source's history alone.
// This is what makes snapshotting a partition for a later isolation-history
// walk cheap, and what lets two walks run off one snapshot: each walk rewinds
// its own copy.
TEST(IsolationHistory, SnapshotSharesHistoryWithSource) {
  llvm::BumpPtrAllocator allocator;
  IsolationHistory::Factory historyFactory(allocator, /*enabled=*/true);

  SILLocation loc = SILLocation::invalid();
  auto p = Partition::singleRegion(
      loc, {Element(0), Element(1), Element(2), Element(3)},
      historyFactory.get());
  ASSERT_TRUE(p.hasHistory());

  Partition snapshot = p;
  EXPECT_TRUE(snapshot.isRecordingIsolationHistory());
  EXPECT_EQ(snapshot.historySize(), p.historySize());
  // The chain is shared, not copied: both heads are the same node.
  EXPECT_EQ(snapshot.getIsolationHistory().getHead(),
            p.getIsolationHistory().getHead());

  const IsolationHistory::Node *sourceHead = p.getIsolationHistory().getHead();
  unsigned sourceSize = p.historySize();

  llvm::SmallVector<SILBasicBlock *, 4> joins;
  while (popOnePartitionOp(snapshot, joins))
    continue;
  EXPECT_FALSE(snapshot.hasHistory());
  EXPECT_TRUE(joins.empty());

  EXPECT_EQ(p.getIsolationHistory().getHead(), sourceHead)
      << "Rewinding a snapshot moved the source partition's history head.";
  EXPECT_EQ(p.historySize(), sourceSize)
      << "Rewinding a snapshot consumed the source partition's history.";
}
