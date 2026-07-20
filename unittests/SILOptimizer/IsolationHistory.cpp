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

} // namespace

//===----------------------------------------------------------------------===//
//                              MARK: Primitives
//===----------------------------------------------------------------------===//

// pushHistorySequenceBoundary at the top of an empty history puts a
// SequenceBoundary at head with a null parent. This is the shape the chain
// walker assumes when it commits pendingTargetMerge.
TEST(IsolationHistory, BoundaryAtHead) {
  llvm::BumpPtrAllocator allocator;
  IsolationHistory::Factory historyFactory(allocator);

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
  IsolationHistory::Factory historyFactory(allocator);

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
  IsolationHistory::Factory historyFactory(allocator);

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

// CFGHistoryJoin is suppressed when joining the same head — pushCFGHistoryJoin
// early-returns rather than recording a self-edge that would pollute the
// walker's worklist.
TEST(IsolationHistory, CFGHistoryJoinSelfSuppressed) {
  llvm::BumpPtrAllocator allocator;
  IsolationHistory::Factory historyFactory(allocator);

  IsolationHistory history = historyFactory.get();
  history.pushHistorySequenceBoundary(SILLocation::invalid());
  auto *headBefore = history.getHead();

  // Joining ourselves at our current head should be a no-op.
  history.pushCFGHistoryJoin(headBefore);

  EXPECT_EQ(history.getHead(), headBefore);
}

// pushCFGHistoryJoin(nullptr) is the other early-return path. The walker
// occasionally hands us a null head when the predecessor never recorded
// anything — must not allocate or rewrite head.
TEST(IsolationHistory, CFGHistoryJoinNullSuppressed) {
  llvm::BumpPtrAllocator allocator;
  IsolationHistory::Factory historyFactory(allocator);

  IsolationHistory history = historyFactory.get();
  history.pushHistorySequenceBoundary(SILLocation::invalid());
  auto *headBefore = history.getHead();

  history.pushCFGHistoryJoin(static_cast<IsolationHistory::Node *>(nullptr));

  EXPECT_EQ(history.getHead(), headBefore);
}

// pushCFGHistoryJoin from an empty-headed history adopts otherNode directly
// rather than synthesizing a CFGHistoryJoin wrapper. This is the
// "predecessor's history wins" shortcut that drives
// TestHistory_JoiningEmptyAndNotEmpty's historySize == 2 expectation.
TEST(IsolationHistory, CFGHistoryJoinFromEmptyAdoptsOther) {
  llvm::BumpPtrAllocator allocator;
  IsolationHistory::Factory historyFactory(allocator);

  // Build a non-empty otherNode chain.
  IsolationHistory other = historyFactory.get();
  other.pushHistorySequenceBoundary(SILLocation::invalid());
  other.pushNewElementRegion(Element(3));
  auto *otherHead = other.getHead();

  IsolationHistory empty = historyFactory.get();
  EXPECT_EQ(empty.getHead(), nullptr);

  empty.pushCFGHistoryJoin(otherHead);

  EXPECT_EQ(empty.getHead(), otherHead)
      << "Empty history should adopt otherNode rather than wrap it.";
}

// When both histories are non-empty and have distinct heads, pushCFGHistoryJoin
// allocates a fresh CFGHistoryJoin node whose firstArgAsNode is otherNode and
// whose parent is the previous head. This is the only path that records a
// genuine CFG merge for the SendNonSendable walker to recurse into.
TEST(IsolationHistory, CFGHistoryJoinDistinctNonEmptyCreatesNode) {
  llvm::BumpPtrAllocator allocator;
  IsolationHistory::Factory historyFactory(allocator);

  IsolationHistory other = historyFactory.get();
  other.pushHistorySequenceBoundary(SILLocation::invalid());
  auto *otherHead = other.getHead();

  IsolationHistory main = historyFactory.get();
  main.pushHistorySequenceBoundary(SILLocation::invalid());
  auto *mainHeadBefore = main.getHead();

  main.pushCFGHistoryJoin(otherHead);

  auto *newHead = main.getHead();
  ASSERT_NE(newHead, nullptr);
  EXPECT_NE(newHead, mainHeadBefore);
  EXPECT_EQ(newHead->getKind(), IsolationHistory::Node::CFGHistoryJoin);
  EXPECT_EQ(newHead->getFirstArgAsNode(), otherHead);
  EXPECT_EQ(newHead->getNext(), mainHeadBefore);
}

//===----------------------------------------------------------------------===//
//                       MARK: Higher Level Operations
//===----------------------------------------------------------------------===//

// An empty singleRegion records nothing — no boundary, no add, no merge.
TEST(IsolationHistory, SingleRegionEmpty) {
  llvm::BumpPtrAllocator allocator;
  IsolationHistory::Factory historyFactory(allocator);

  SILLocation loc = SILLocation::invalid();
  auto p = Partition::singleRegion(loc, {}, historyFactory.get());

  EXPECT_FALSE(p.hasHistory());
  EXPECT_EQ(p.historySize(), 0u);
}

// A single element: one boundary + one AddNewRegionForElement, no merges.
TEST(IsolationHistory, SingleRegionOneElement) {
  llvm::BumpPtrAllocator allocator;
  IsolationHistory::Factory historyFactory(allocator);

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
  IsolationHistory::Factory historyFactory(allocator);

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
  IsolationHistory::Factory historyFactory(allocator);

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
  IsolationHistory::Factory historyFactory(allocator);

  SILLocation loc = SILLocation::invalid();
  auto p = Partition::singleRegion(
      loc, {Element(0), Element(1), Element(2), Element(3)},
      historyFactory.get());

  // Drain history. popHistory returns true while there's more to pop;
  // joins is unused since singleRegion never records a CFGHistoryJoin.
  llvm::SmallVector<IsolationHistory, 4> joins;
  while (p.popHistory(joins))
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
  IsolationHistory::Factory historyFactory(allocator);

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
  IsolationHistory::Factory historyFactory(allocator);

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
  IsolationHistory::Factory historyFactory(allocator);
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
  IsolationHistory::Factory historyFactory(allocator);

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
  IsolationHistory::Factory historyFactory(allocator);

  SILLocation loc = SILLocation::invalid();
  auto p = makePartitionWithSeparateRegions(
      loc, {Element(0), Element(1), Element(2)}, historyFactory.get());

  llvm::SmallVector<IsolationHistory, 4> joins;
  while (p.popHistory(joins))
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
  IsolationHistory::Factory historyFactory(allocator);

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
  IsolationHistory::Factory historyFactory(allocator);
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
  llvm::SmallVector<IsolationHistory, 4> joins;
  while (joined.popHistory(joins))
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
  IsolationHistory::Factory historyFactory(allocator);
  SmallVector<IsolationHistory, 8> joinedHistories;
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

  p.popHistory(joinedHistories);

  EXPECT_TRUE(Partition::equals(p, pSnapshot));
  EXPECT_TRUE(joinedHistories.empty());
}

TEST(IsolationHistory, AssignRegion) {
  llvm::BumpPtrAllocator allocator;
  Partition::SendingOperandSetFactory factory(allocator);
  IsolationHistory::Factory historyFactory(allocator);
  SendingOperandToStateMap transferringOpToStateMap(historyFactory);
  SmallVector<IsolationHistory, 8> joinedHistories;

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

  p.popHistory(joinedHistories);

  EXPECT_TRUE(Partition::equals(p, pSnapshot2));
  EXPECT_TRUE(joinedHistories.empty());

  p.popHistory(joinedHistories);

  EXPECT_TRUE(Partition::equals(p, pSnapshot));
  EXPECT_TRUE(joinedHistories.empty());
}

TEST(IsolationHistory, BuildNewRegionRepIsMerge) {
  llvm::BumpPtrAllocator allocator;
  Partition::SendingOperandSetFactory factory(allocator);
  IsolationHistory::Factory historyFactory(allocator);
  SendingOperandToStateMap transferringOpToStateMap(historyFactory);
  SmallVector<IsolationHistory, 8> joinedHistories;

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
  p.popHistory(joinedHistories);

  EXPECT_TRUE(Partition::equals(p, pSnapshot2));
  EXPECT_TRUE(joinedHistories.empty());

  // We pop a last time to return to our original value.
  p.popHistory(joinedHistories);

  EXPECT_TRUE(Partition::equals(p, pSnapshot));
  EXPECT_TRUE(joinedHistories.empty());
}

TEST(IsolationHistory, ReturnFalseWhenNoneLeft) {
  llvm::BumpPtrAllocator allocator;
  Partition::SendingOperandSetFactory factory(allocator);
  IsolationHistory::Factory historyFactory(allocator);
  SmallVector<IsolationHistory, 8> joinedHistories;
  SendingOperandToStateMap transferringOpToStateMap(historyFactory);

  Partition p(historyFactory.get());

  EXPECT_FALSE(p.popHistory(joinedHistories));
  EXPECT_TRUE(joinedHistories.empty());

  {
    MockedPartitionOpEvaluator eval(p, factory, transferringOpToStateMap);
    eval.apply({PartitionOp::AssignFresh(Element(2)),
                PartitionOp::AssignFresh(Element(3))});
  }

  EXPECT_TRUE(p.popHistory(joinedHistories));
  EXPECT_TRUE(joinedHistories.empty());

  EXPECT_FALSE(p.popHistory(joinedHistories));
  EXPECT_TRUE(joinedHistories.empty());
}

TEST(IsolationHistory, JoiningTwoEmpty) {
  // Make sure that we do sane things when we join empty history.
  llvm::BumpPtrAllocator allocator;
  Partition::SendingOperandSetFactory factory(allocator);
  IsolationHistory::Factory historyFactory(allocator);
  SmallVector<IsolationHistory, 8> joinedHistories;

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
  IsolationHistory::Factory historyFactory(allocator);
  SmallVector<IsolationHistory, 8> joinedHistories;
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
  IsolationHistory::Factory historyFactory(allocator);
  SendingOperandToStateMap transferringOpToStateMap(historyFactory);
  SmallVector<IsolationHistory, 8> joinedHistories;

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
  IsolationHistory::Factory historyFactory(allocator);
  SendingOperandToStateMap opToStateMap(historyFactory);
  SmallVector<IsolationHistory, 8> joins;

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

  p.popHistory(joins);
  EXPECT_TRUE(joins.empty());
  EXPECT_TRUE(Partition::equals(p, snapshot))
      << "AssignDirect that moved an element across regions did not "
         "rewind cleanly.";
}
