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
    for (auto *node = history.getHead(); node; node = node->getParent()) {
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
  for (auto *node = history.getHead(); node; node = node->getParent()) {
    if (node->getKind() != IsolationHistory::Node::MergeElementRegions)
      continue;
    bool foundBoundary = false;
    for (auto *p = node->getParent(); p; p = p->getParent()) {
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
  EXPECT_EQ(head->getParent(), nullptr);
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
  EXPECT_EQ(node->getParent(), nullptr);
}

// pushMergeElementRegions records a MergeElementRegions node carrying
// elementToMergeInto at firstArg and the peer list verbatim in
// additionalElementArgs.
TEST(IsolationHistory, PushMergeElementRegionsPrimitive) {
  llvm::BumpPtrAllocator allocator;
  IsolationHistory::Factory historyFactory(allocator);

  IsolationHistory history = historyFactory.get();
  history.pushMergeElementRegions(Element(0), {Element(2), Element(5)});

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
  EXPECT_EQ(newHead->getParent(), mainHeadBefore);
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

// separateRegions pushes a single boundary and one AddNewRegionForElement
// per index — sanity check that we don't accidentally synthesize a merge,
// that every input element is tracked afterwards, and that each lives in
// a distinct region.
TEST(IsolationHistory, SeparateRegionsShape) {
  llvm::BumpPtrAllocator allocator;
  IsolationHistory::Factory historyFactory(allocator);

  SILLocation loc = SILLocation::invalid();
  auto p = Partition::separateRegions(loc, {Element(0), Element(1), Element(2)},
                                      historyFactory.get());

  auto counts = HistoryNodeCounts::from(p.getIsolationHistory());
  EXPECT_EQ(counts.sequenceBoundary, 1u);
  EXPECT_EQ(counts.addNewRegion, 3u);
  EXPECT_EQ(counts.mergeElementRegions, 0u);

  for (Element e : {Element(0), Element(1), Element(2)})
    EXPECT_TRUE(p.isTrackingElement(e))
        << "Element " << unsigned(e) << " was not tracked by separateRegions";

  // Each element must land in a distinct region — that's the whole
  // contract of separateRegions vs singleRegion.
  PartitionTester tester(p);
  unsigned r0 = tester.getRegion(0);
  unsigned r1 = tester.getRegion(1);
  unsigned r2 = tester.getRegion(2);
  EXPECT_NE(r0, r1);
  EXPECT_NE(r0, r2);
  EXPECT_NE(r1, r2);
}

// popHistory drains a separateRegions-built partition back to having no
// element mappings. Unlike singleRegion, separateRegions's push pattern is
// the same shape (one AddNew per element) regardless of N — no merge nodes
// to interact with — so the round-trip works on today's tree (with
// distinct indices).
TEST(IsolationHistory, SeparateRegionsRoundTrip) {
  llvm::BumpPtrAllocator allocator;
  IsolationHistory::Factory historyFactory(allocator);

  SILLocation loc = SILLocation::invalid();
  auto p = Partition::separateRegions(loc, {Element(0), Element(1), Element(2)},
                                      historyFactory.get());

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
  Partition snd = Partition::separateRegions(
      loc, {Element(0), Element(1), Element(2)}, historyFactory.get());

  // Both predecessors carry their own per-instruction boundaries.
  EXPECT_TRUE(everyMergeHasAncestorBoundary(fst.getIsolationHistory()));
  EXPECT_TRUE(everyMergeHasAncestorBoundary(snd.getIsolationHistory()));

  Partition joined = Partition::join(fst, snd);

  EXPECT_TRUE(everyMergeHasAncestorBoundary(joined.getIsolationHistory()));
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
