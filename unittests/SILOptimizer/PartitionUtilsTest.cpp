//===--- PartitionUtilsTest.cpp -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SILOptimizer/Utils/PartitionUtils.h"

#include "gtest/gtest.h"

#include <array>

using namespace swift;
using namespace swift::PartitionPrimitives;

//===----------------------------------------------------------------------===//
//                                 Utilities
//===----------------------------------------------------------------------===//

struct Partition::PartitionTester {
  const Partition &p;

  PartitionTester(const Partition &p) : p(p) {}

  unsigned getRegion(unsigned elt) const {
    return unsigned(p.elementToRegionMap.at(Element(elt)));
  }
};

namespace {

using PartitionTester = Partition::PartitionTester;

struct MockedPartitionOpEvaluator final
    : PartitionOpEvaluatorBaseImpl<MockedPartitionOpEvaluator> {
  MockedPartitionOpEvaluator(Partition &workingPartition,
                             SendingOperandSetFactory &ptrSetFactory,
                             SendingOperandToStateMap &operandToStateMap)
      : PartitionOpEvaluatorBaseImpl(workingPartition, ptrSetFactory,
                                     operandToStateMap) {}

  // Just say that we always have a disconnected value.
  SILIsolationInfo getIsolationRegionInfo(Element elt) const {
    return SILIsolationInfo::getDisconnected(false /*isUnsafeNonIsolated*/);
  }

  bool shouldTryToSquelchErrors() const { return false; }

  static SILLocation getLoc(SILInstruction *inst) {
    return SILLocation::invalid();
  }

  static SILLocation getLoc(Operand *op) { return SILLocation::invalid(); }

  static SILIsolationInfo getIsolationInfo(const PartitionOp &partitionOp) {
    return {};
  }

  static SILInstruction *getSourceInst(const PartitionOp &partitionOp) {
    return nullptr;
  }

  static bool doesFunctionHaveSendingResult(const PartitionOp &partitionOp) {
    return false;
  }
};

} // namespace

namespace {

struct MockedPartitionOpEvaluatorWithFailureCallback final
    : PartitionOpEvaluatorBaseImpl<
          MockedPartitionOpEvaluatorWithFailureCallback> {
  using FailureCallbackTy =
      std::function<void(const PartitionOp &, unsigned, Operand *)>;
  FailureCallbackTy failureCallback;

  MockedPartitionOpEvaluatorWithFailureCallback(
      Partition &workingPartition, SendingOperandSetFactory &ptrSetFactory,
      SendingOperandToStateMap &operandToStateMap,
      FailureCallbackTy failureCallback)
      : PartitionOpEvaluatorBaseImpl(workingPartition, ptrSetFactory,
                                     operandToStateMap),
        failureCallback(failureCallback) {}

  void handleError(PartitionOpError error) {
    switch (error.getKind()) {
    case PartitionOpError::UnknownCodePattern:
    case PartitionOpError::SentNeverSendable:
    case PartitionOpError::AssignNeverSendableIntoSendingResult:
    case PartitionOpError::InOutSendingNotInitializedAtExit:
    case PartitionOpError::InOutSendingNotDisconnectedAtExit:
    case PartitionOpError::NonSendableIsolationCrossingResult:
      llvm_unreachable("Unsupported");
    case PartitionOpError::LocalUseAfterSend: {
      auto state = error.getLocalUseAfterSendError();
      failureCallback(*state.op, state.sentElement, state.sendingOp);
    }
    }
  }

  // Just say that we always have a disconnected value.
  SILIsolationInfo getIsolationRegionInfo(Element elt) const {
    return SILIsolationInfo::getDisconnected(false /*nonisolated(unsafe)*/);
  }

  bool shouldTryToSquelchErrors() const { return false; }

  static SILLocation getLoc(SILInstruction *inst) {
    return SILLocation::invalid();
  }

  static SILLocation getLoc(Operand *op) { return SILLocation::invalid(); }

  static SILIsolationInfo getIsolationInfo(const PartitionOp &partitionOp) {
    return {};
  }

  static SILInstruction *getSourceInst(const PartitionOp &partitionOp) {
    return nullptr;
  }
};

} // namespace

//===----------------------------------------------------------------------===//
//                                   Tests
//===----------------------------------------------------------------------===//

// When we send we need a specific send instruction. We do not ever
// actually dereference the instruction, so just use some invalid ptr values so
// we can compare.
Operand *operandSingletons[5] = {
    (Operand *)0xDEAD0000, (Operand *)0xFEAD0000, (Operand *)0xAEDF0000,
    (Operand *)0xFEDA0000, (Operand *)0xFBDA0000,
};

SILInstruction *instSingletons[5] = {
    (SILInstruction *)0xBEAD0000, (SILInstruction *)0xBEAD0000,
    (SILInstruction *)0xBEDF0000, (SILInstruction *)0xBEDA0000,
    (SILInstruction *)0xBBDA0000,
};

SILLocation fakeLoc = SILLocation::invalid();

// This test tests that if a series of merges is split between two partitions
// p1 and p2, but also applied in its entirety to p3, then joining p1 and p2
// yields p3.
TEST(PartitionUtilsTest, TestMergeAndJoin) {
  llvm::BumpPtrAllocator allocator;
  Partition::SendingOperandSetFactory factory(allocator);
  IsolationHistory::Factory historyFactory(allocator);
  SendingOperandToStateMap sendingOpToStateMap(historyFactory);

  Partition p1(historyFactory.get());
  Partition p2(historyFactory.get());
  Partition p3(historyFactory.get());

  {
    MockedPartitionOpEvaluator eval(p1, factory, sendingOpToStateMap);
    eval.apply({PartitionOp::AssignFresh(Element(0)),
                PartitionOp::AssignFresh(Element(1)),
                PartitionOp::AssignFresh(Element(2)),
                PartitionOp::AssignFresh(Element(3))});
  }

  {
    MockedPartitionOpEvaluator eval(p2, factory, sendingOpToStateMap);
    eval.apply({PartitionOp::AssignFresh(Element(5)),
                PartitionOp::AssignFresh(Element(6)),
                PartitionOp::AssignFresh(Element(7)),
                PartitionOp::AssignFresh(Element(0))});
  }

  {
    MockedPartitionOpEvaluator eval(p3, factory, sendingOpToStateMap);
    eval.apply({PartitionOp::AssignFresh(Element(2)),
                PartitionOp::AssignFresh(Element(3)),
                PartitionOp::AssignFresh(Element(4)),
                PartitionOp::AssignFresh(Element(5))});
  }

  EXPECT_FALSE(Partition::equals(p1, p2));
  EXPECT_FALSE(Partition::equals(p2, p3));
  EXPECT_FALSE(Partition::equals(p1, p3));

  {
    MockedPartitionOpEvaluator eval(p1, factory, sendingOpToStateMap);
    eval.apply({PartitionOp::AssignFresh(Element(4)),
                PartitionOp::AssignFresh(Element(5)),
                PartitionOp::AssignFresh(Element(6)),
                PartitionOp::AssignFresh(Element(7)),
                PartitionOp::AssignFresh(Element(8))});
  }

  {
    MockedPartitionOpEvaluator eval(p2, factory, sendingOpToStateMap);
    eval.apply({PartitionOp::AssignFresh(Element(1)),
                PartitionOp::AssignFresh(Element(2)),
                PartitionOp::AssignFresh(Element(3)),
                PartitionOp::AssignFresh(Element(4)),
                PartitionOp::AssignFresh(Element(8))});
  }

  {
    MockedPartitionOpEvaluator eval(p3, factory, sendingOpToStateMap);
    eval.apply({PartitionOp::AssignFresh(Element(6)),
                PartitionOp::AssignFresh(Element(7)),
                PartitionOp::AssignFresh(Element(0)),
                PartitionOp::AssignFresh(Element(1)),
                PartitionOp::AssignFresh(Element(8))});
  }

  EXPECT_TRUE(Partition::equals(p1, p2));
  EXPECT_TRUE(Partition::equals(p2, p3));
  EXPECT_TRUE(Partition::equals(p1, p3));

  auto expect_join_eq = [&]() {
    Partition joined = Partition::join(p1, p2);
    EXPECT_TRUE(Partition::equals(p3, joined));
  };

  auto apply_to_p1_and_p3 = [&](PartitionOp op) {
    {
      MockedPartitionOpEvaluator eval(p1, factory, sendingOpToStateMap);
      eval.apply(op);
    }

    {
      MockedPartitionOpEvaluator eval(p3, factory, sendingOpToStateMap);
      eval.apply(op);
    }
    expect_join_eq();
  };

  auto apply_to_p2_and_p3 = [&](PartitionOp op) {
    {
      MockedPartitionOpEvaluator eval(p2, factory, sendingOpToStateMap);
      eval.apply(op);
    }

    {
      MockedPartitionOpEvaluator eval(p3, factory, sendingOpToStateMap);
      eval.apply(op);
    }
    expect_join_eq();
  };

  apply_to_p1_and_p3(PartitionOp::Merge(Element(1), Element(2)));
  apply_to_p2_and_p3(PartitionOp::Merge(Element(7), Element(8)));
  apply_to_p1_and_p3(PartitionOp::Merge(Element(2), Element(7)));
  apply_to_p2_and_p3(PartitionOp::Merge(Element(1), Element(3)));
  apply_to_p1_and_p3(PartitionOp::Merge(Element(3), Element(4)));

  EXPECT_FALSE(Partition::equals(p1, p2));
  EXPECT_FALSE(Partition::equals(p2, p3));
  EXPECT_FALSE(Partition::equals(p1, p3));

  apply_to_p2_and_p3(PartitionOp::Merge(Element(2), Element(5)));
  apply_to_p1_and_p3(PartitionOp::Merge(Element(5), Element(6)));
  apply_to_p2_and_p3(PartitionOp::Merge(Element(1), Element(6)));
  apply_to_p1_and_p3(PartitionOp::Merge(Element(2), Element(6)));
  apply_to_p2_and_p3(PartitionOp::Merge(Element(3), Element(7)));
  apply_to_p1_and_p3(PartitionOp::Merge(Element(7), Element(8)));
}

TEST(PartitionUtilsTest, Join1) {
  llvm::BumpPtrAllocator allocator;
  Partition::SendingOperandSetFactory factory(allocator);
  IsolationHistory::Factory historyFactory(allocator);
  SendingOperandToStateMap transferringOpToStateMap(historyFactory);

  Element data1[] = {Element(0), Element(1), Element(2),
                     Element(3), Element(4), Element(5)};
  Partition p1 = Partition::separateRegions(fakeLoc, llvm::ArrayRef(data1),
                                            historyFactory.get());

  {
    MockedPartitionOpEvaluator eval(p1, factory, transferringOpToStateMap);
    eval.apply({PartitionOp::Assign(Element(0), Element(0)),
                PartitionOp::Assign(Element(1), Element(0)),
                PartitionOp::Assign(Element(2), Element(2)),
                PartitionOp::Assign(Element(3), Element(3)),
                PartitionOp::Assign(Element(4), Element(3)),
                PartitionOp::Assign(Element(5), Element(2))});
  }

  Partition p2 = Partition::separateRegions(fakeLoc, llvm::ArrayRef(data1),
                                            historyFactory.get());
  {
    MockedPartitionOpEvaluator eval(p2, factory, transferringOpToStateMap);
    eval.apply({PartitionOp::Assign(Element(0), Element(0)),
                PartitionOp::Assign(Element(1), Element(0)),
                PartitionOp::Assign(Element(2), Element(2)),
                PartitionOp::Assign(Element(3), Element(3)),
                PartitionOp::Assign(Element(4), Element(3)),
                PartitionOp::Assign(Element(5), Element(5))});
  }

  auto result = Partition::join(p1, p2);
  PartitionTester tester(result);
  ASSERT_EQ(tester.getRegion(0), 0);
  ASSERT_EQ(tester.getRegion(1), 0);
  ASSERT_EQ(tester.getRegion(2), 2);
  ASSERT_EQ(tester.getRegion(3), 3);
  ASSERT_EQ(tester.getRegion(4), 3);
  ASSERT_EQ(tester.getRegion(5), 2);
}

TEST(PartitionUtilsTest, Join2) {
  llvm::BumpPtrAllocator allocator;
  Partition::SendingOperandSetFactory factory(allocator);
  IsolationHistory::Factory historyFactory(allocator);
  SendingOperandToStateMap transferringOpToStateMap(historyFactory);

  Element data1[] = {Element(0), Element(1), Element(2),
                     Element(3), Element(4), Element(5)};
  Partition p1 = Partition::separateRegions(fakeLoc, llvm::ArrayRef(data1),
                                            historyFactory.get());

  {
    MockedPartitionOpEvaluator eval(p1, factory, transferringOpToStateMap);
    eval.apply({PartitionOp::Assign(Element(0), Element(0)),
                PartitionOp::Assign(Element(1), Element(0)),
                PartitionOp::Assign(Element(2), Element(2)),
                PartitionOp::Assign(Element(3), Element(3)),
                PartitionOp::Assign(Element(4), Element(3)),
                PartitionOp::Assign(Element(5), Element(2))});
  }

  Element data2[] = {Element(4), Element(5), Element(6),
                     Element(7), Element(8), Element(9)};
  Partition p2 = Partition::separateRegions(fakeLoc, llvm::ArrayRef(data2),
                                            historyFactory.get());
  {
    MockedPartitionOpEvaluator eval(p2, factory, transferringOpToStateMap);
    eval.apply({PartitionOp::Assign(Element(4), Element(4)),
                PartitionOp::Assign(Element(5), Element(5)),
                PartitionOp::Assign(Element(6), Element(4)),
                PartitionOp::Assign(Element(7), Element(7)),
                PartitionOp::Assign(Element(8), Element(7)),
                PartitionOp::Assign(Element(9), Element(4))});
  }

  auto result = Partition::join(p1, p2);
  PartitionTester tester(result);
  ASSERT_EQ(tester.getRegion(0), 0);
  ASSERT_EQ(tester.getRegion(1), 0);
  ASSERT_EQ(tester.getRegion(2), 2);
  ASSERT_EQ(tester.getRegion(3), 3);
  ASSERT_EQ(tester.getRegion(4), 3);
  ASSERT_EQ(tester.getRegion(5), 2);
  ASSERT_EQ(tester.getRegion(6), 3);
  ASSERT_EQ(tester.getRegion(7), 7);
  ASSERT_EQ(tester.getRegion(8), 7);
  ASSERT_EQ(tester.getRegion(9), 3);
}

TEST(PartitionUtilsTest, Join2Reversed) {
  llvm::BumpPtrAllocator allocator;
  Partition::SendingOperandSetFactory factory(allocator);
  IsolationHistory::Factory historyFactory(allocator);
  SendingOperandToStateMap transferringOpToStateMap(historyFactory);

  Element data1[] = {Element(0), Element(1), Element(2),
                     Element(3), Element(4), Element(5)};
  Partition p1 = Partition::separateRegions(fakeLoc, llvm::ArrayRef(data1),
                                            historyFactory.get());

  {
    MockedPartitionOpEvaluator eval(p1, factory, transferringOpToStateMap);
    eval.apply({PartitionOp::Assign(Element(0), Element(0)),
                PartitionOp::Assign(Element(1), Element(0)),
                PartitionOp::Assign(Element(2), Element(2)),
                PartitionOp::Assign(Element(3), Element(3)),
                PartitionOp::Assign(Element(4), Element(3)),
                PartitionOp::Assign(Element(5), Element(2))});
  }

  Element data2[] = {Element(4), Element(5), Element(6),
                     Element(7), Element(8), Element(9)};
  Partition p2 = Partition::separateRegions(fakeLoc, llvm::ArrayRef(data2),
                                            historyFactory.get());
  {
    MockedPartitionOpEvaluator eval(p2, factory, transferringOpToStateMap);
    eval.apply({PartitionOp::Assign(Element(4), Element(4)),
                PartitionOp::Assign(Element(5), Element(5)),
                PartitionOp::Assign(Element(6), Element(4)),
                PartitionOp::Assign(Element(7), Element(7)),
                PartitionOp::Assign(Element(8), Element(7)),
                PartitionOp::Assign(Element(9), Element(4))});
  }

  auto result = Partition::join(p2, p1);
  PartitionTester tester(result);
  ASSERT_EQ(tester.getRegion(0), 0);
  ASSERT_EQ(tester.getRegion(1), 0);
  ASSERT_EQ(tester.getRegion(2), 2);
  ASSERT_EQ(tester.getRegion(3), 3);
  ASSERT_EQ(tester.getRegion(4), 3);
  ASSERT_EQ(tester.getRegion(5), 2);
  ASSERT_EQ(tester.getRegion(6), 3);
  ASSERT_EQ(tester.getRegion(7), 7);
  ASSERT_EQ(tester.getRegion(8), 7);
  ASSERT_EQ(tester.getRegion(9), 3);
}

TEST(PartitionUtilsTest, JoinLarge) {
  llvm::BumpPtrAllocator allocator;
  Partition::SendingOperandSetFactory factory(allocator);
  IsolationHistory::Factory historyFactory(allocator);
  SendingOperandToStateMap transferringOpToStateMap(historyFactory);

  Element data1[] = {
      Element(0),  Element(1),  Element(2),  Element(3),  Element(4),
      Element(5),  Element(6),  Element(7),  Element(8),  Element(9),
      Element(10), Element(11), Element(12), Element(13), Element(14),
      Element(15), Element(16), Element(17), Element(18), Element(19),
      Element(20), Element(21), Element(22), Element(23), Element(24),
      Element(25), Element(26), Element(27), Element(28), Element(29)};
  Partition p1 = Partition::separateRegions(fakeLoc, llvm::ArrayRef(data1),
                                            historyFactory.get());
  {
    MockedPartitionOpEvaluator eval(p1, factory, transferringOpToStateMap);
    eval.apply({PartitionOp::Assign(Element(0), Element(29)),
                PartitionOp::Assign(Element(1), Element(17)),
                PartitionOp::Assign(Element(2), Element(0)),
                PartitionOp::Assign(Element(3), Element(12)),
                PartitionOp::Assign(Element(4), Element(13)),
                PartitionOp::Assign(Element(5), Element(9)),
                PartitionOp::Assign(Element(6), Element(15)),
                PartitionOp::Assign(Element(7), Element(27)),
                PartitionOp::Assign(Element(8), Element(3)),
                PartitionOp::Assign(Element(9), Element(3)),
                PartitionOp::Assign(Element(10), Element(3)),
                PartitionOp::Assign(Element(11), Element(21)),
                PartitionOp::Assign(Element(12), Element(14)),
                PartitionOp::Assign(Element(13), Element(25)),
                PartitionOp::Assign(Element(14), Element(1)),
                PartitionOp::Assign(Element(15), Element(25)),
                PartitionOp::Assign(Element(16), Element(12)),
                PartitionOp::Assign(Element(17), Element(3)),
                PartitionOp::Assign(Element(18), Element(25)),
                PartitionOp::Assign(Element(19), Element(13)),
                PartitionOp::Assign(Element(20), Element(19)),
                PartitionOp::Assign(Element(21), Element(7)),
                PartitionOp::Assign(Element(22), Element(19)),
                PartitionOp::Assign(Element(23), Element(27)),
                PartitionOp::Assign(Element(24), Element(1)),
                PartitionOp::Assign(Element(25), Element(9)),
                PartitionOp::Assign(Element(26), Element(18)),
                PartitionOp::Assign(Element(27), Element(29)),
                PartitionOp::Assign(Element(28), Element(28)),
                PartitionOp::Assign(Element(29), Element(13))});
  }

  Element data2[] = {
      Element(15), Element(16), Element(17), Element(18), Element(19),
      Element(20), Element(21), Element(22), Element(23), Element(24),
      Element(25), Element(26), Element(27), Element(28), Element(29),
      Element(30), Element(31), Element(32), Element(33), Element(34),
      Element(35), Element(36), Element(37), Element(38), Element(39),
      Element(40), Element(41), Element(42), Element(43), Element(44)};
  Partition p2 = Partition::separateRegions(fakeLoc, llvm::ArrayRef(data2),
                                            historyFactory.get());
  {
    MockedPartitionOpEvaluator eval(p2, factory, transferringOpToStateMap);
    eval.apply({PartitionOp::Assign(Element(15), Element(31)),
                PartitionOp::Assign(Element(16), Element(34)),
                PartitionOp::Assign(Element(17), Element(35)),
                PartitionOp::Assign(Element(18), Element(41)),
                PartitionOp::Assign(Element(19), Element(15)),
                PartitionOp::Assign(Element(20), Element(32)),
                PartitionOp::Assign(Element(21), Element(17)),
                PartitionOp::Assign(Element(22), Element(31)),
                PartitionOp::Assign(Element(23), Element(21)),
                PartitionOp::Assign(Element(24), Element(33)),
                PartitionOp::Assign(Element(25), Element(25)),
                PartitionOp::Assign(Element(26), Element(31)),
                PartitionOp::Assign(Element(27), Element(16)),
                PartitionOp::Assign(Element(28), Element(35)),
                PartitionOp::Assign(Element(29), Element(40)),
                PartitionOp::Assign(Element(30), Element(33)),
                PartitionOp::Assign(Element(31), Element(34)),
                PartitionOp::Assign(Element(32), Element(22)),
                PartitionOp::Assign(Element(33), Element(42)),
                PartitionOp::Assign(Element(34), Element(37)),
                PartitionOp::Assign(Element(35), Element(34)),
                PartitionOp::Assign(Element(36), Element(18)),
                PartitionOp::Assign(Element(37), Element(32)),
                PartitionOp::Assign(Element(38), Element(22)),
                PartitionOp::Assign(Element(39), Element(44)),
                PartitionOp::Assign(Element(40), Element(20)),
                PartitionOp::Assign(Element(41), Element(37)),
                PartitionOp::Assign(Element(43), Element(29)),
                PartitionOp::Assign(Element(44), Element(25))});
  }

  auto result = Partition::join(p1, p2);
  PartitionTester tester(result);
  ASSERT_EQ(tester.getRegion(0), 0);
  ASSERT_EQ(tester.getRegion(1), 1);
  ASSERT_EQ(tester.getRegion(2), 0);
  ASSERT_EQ(tester.getRegion(3), 3);
  ASSERT_EQ(tester.getRegion(4), 4);
  ASSERT_EQ(tester.getRegion(5), 5);
  ASSERT_EQ(tester.getRegion(6), 6);
  ASSERT_EQ(tester.getRegion(7), 3);
  ASSERT_EQ(tester.getRegion(8), 3);
  ASSERT_EQ(tester.getRegion(9), 3);
  ASSERT_EQ(tester.getRegion(10), 3);
  ASSERT_EQ(tester.getRegion(11), 11);
  ASSERT_EQ(tester.getRegion(12), 0);
  ASSERT_EQ(tester.getRegion(13), 13);
  ASSERT_EQ(tester.getRegion(14), 1);
  ASSERT_EQ(tester.getRegion(15), 13);
  ASSERT_EQ(tester.getRegion(16), 0);
  ASSERT_EQ(tester.getRegion(17), 3);
  ASSERT_EQ(tester.getRegion(18), 13);
  ASSERT_EQ(tester.getRegion(19), 13);
  ASSERT_EQ(tester.getRegion(20), 13);
  ASSERT_EQ(tester.getRegion(21), 3);
  ASSERT_EQ(tester.getRegion(22), 13);
  ASSERT_EQ(tester.getRegion(23), 3);
  ASSERT_EQ(tester.getRegion(24), 1);
  ASSERT_EQ(tester.getRegion(25), 3);
  ASSERT_EQ(tester.getRegion(26), 13);
  ASSERT_EQ(tester.getRegion(27), 0);
  ASSERT_EQ(tester.getRegion(28), 3);
  ASSERT_EQ(tester.getRegion(29), 13);
  ASSERT_EQ(tester.getRegion(30), 1);
  ASSERT_EQ(tester.getRegion(31), 0);
  ASSERT_EQ(tester.getRegion(32), 13);
  ASSERT_EQ(tester.getRegion(33), 33);
  ASSERT_EQ(tester.getRegion(34), 34);
  ASSERT_EQ(tester.getRegion(35), 34);
  ASSERT_EQ(tester.getRegion(36), 13);
  ASSERT_EQ(tester.getRegion(37), 13);
  ASSERT_EQ(tester.getRegion(38), 13);
  ASSERT_EQ(tester.getRegion(39), 39);
  ASSERT_EQ(tester.getRegion(40), 13);
  ASSERT_EQ(tester.getRegion(41), 13);
  ASSERT_EQ(tester.getRegion(42), 33);
  ASSERT_EQ(tester.getRegion(43), 13);
  ASSERT_EQ(tester.getRegion(44), 3);
}

// This test tests the semantics of assignment.
TEST(PartitionUtilsTest, TestAssign) {
  llvm::BumpPtrAllocator allocator;
  Partition::SendingOperandSetFactory factory(allocator);
  IsolationHistory::Factory historyFactory(allocator);
  SendingOperandToStateMap transferringOpToStateMap(historyFactory);

  Partition p1(historyFactory.get());
  Partition p2(historyFactory.get());
  Partition p3(historyFactory.get());

  MockedPartitionOpEvaluator evalP1(p1, factory, transferringOpToStateMap);
  evalP1.apply({PartitionOp::AssignFresh(Element(0)),
                PartitionOp::AssignFresh(Element(1)),
                PartitionOp::AssignFresh(Element(2)),
                PartitionOp::AssignFresh(Element(3))});

  MockedPartitionOpEvaluator evalP2(p2, factory, transferringOpToStateMap);
  evalP2.apply({PartitionOp::AssignFresh(Element(0)),
                PartitionOp::AssignFresh(Element(1)),
                PartitionOp::AssignFresh(Element(2)),
                PartitionOp::AssignFresh(Element(3))});

  MockedPartitionOpEvaluator evalP3(p3, factory, transferringOpToStateMap);
  evalP3.apply({PartitionOp::AssignFresh(Element(0)),
                PartitionOp::AssignFresh(Element(1)),
                PartitionOp::AssignFresh(Element(2)),
                PartitionOp::AssignFresh(Element(3))});

  // expected: p1: ((Element(0)) (Element(1)) (Element(2)) (Element(3))), p2:
  // ((Element(0)) (Element(1)) (Element(2)) (Element(3))), p3: ((Element(0))
  // (Element(1)) (Element(2)) (Element(3)))

  EXPECT_TRUE(Partition::equals(p1, p2));
  EXPECT_TRUE(Partition::equals(p2, p3));
  EXPECT_TRUE(Partition::equals(p1, p3));

  evalP1.apply(PartitionOp::Assign(Element(0), Element(1)));
  evalP2.apply(PartitionOp::Assign(Element(1), Element(0)));
  evalP3.apply(PartitionOp::Assign(Element(2), Element(1)));

  // expected: p1: ((0 1) (Element(2)) (Element(3))), p2: ((0 1) (Element(2))
  // (Element(3))), p3: ((Element(0)) (1 2) (Element(3)))

  EXPECT_TRUE(Partition::equals(p1, p2));
  EXPECT_FALSE(Partition::equals(p2, p3));
  EXPECT_FALSE(Partition::equals(p1, p3));

  evalP1.apply(PartitionOp::Assign(Element(2), Element(0)));
  evalP2.apply(PartitionOp::Assign(Element(2), Element(1)));
  evalP3.apply(PartitionOp::Assign(Element(0), Element(2)));

  // expected: p1: ((0 1 2) (Element(3))), p2: ((0 1 2) (Element(3))), p3: ((0 1
  // 2) (Element(3)))

  EXPECT_TRUE(Partition::equals(p1, p2));
  EXPECT_TRUE(Partition::equals(p2, p3));
  EXPECT_TRUE(Partition::equals(p1, p3));

  evalP1.apply(PartitionOp::Assign(Element(0), Element(3)));
  evalP2.apply(PartitionOp::Assign(Element(1), Element(3)));
  evalP3.apply(PartitionOp::Assign(Element(2), Element(3)));

  // expected: p1: ((1 2) (0 3)), p2: ((0 2) (1 3)), p3: ((0 1) (2 3))

  EXPECT_FALSE(Partition::equals(p1, p2));
  EXPECT_FALSE(Partition::equals(p2, p3));
  EXPECT_FALSE(Partition::equals(p1, p3));

  evalP1.apply(PartitionOp::Assign(Element(1), Element(0)));
  evalP2.apply(PartitionOp::Assign(Element(2), Element(1)));
  evalP3.apply(PartitionOp::Assign(Element(0), Element(2)));

  // expected: p1: ((Element(2)) (0 1 3)), p2: ((Element(0)) (1 2 3)), p3:
  // ((Element(1)) (0 2 3))

  EXPECT_FALSE(Partition::equals(p1, p2));
  EXPECT_FALSE(Partition::equals(p2, p3));
  EXPECT_FALSE(Partition::equals(p1, p3));

  evalP1.apply(PartitionOp::Assign(Element(2), Element(3)));
  evalP2.apply(PartitionOp::Assign(Element(0), Element(3)));
  evalP3.apply(PartitionOp::Assign(Element(1), Element(3)));

  // expected: p1: ((0 1 2 3)), p2: ((0 1 2 3)), p3: ((0 1 2 3))

  EXPECT_TRUE(Partition::equals(p1, p2));
  EXPECT_TRUE(Partition::equals(p2, p3));
  EXPECT_TRUE(Partition::equals(p1, p3));
}

// This test tests that consumption consumes entire regions as expected
TEST(PartitionUtilsTest, TestConsumeAndRequire) {
  llvm::BumpPtrAllocator allocator;
  Partition::SendingOperandSetFactory factory(allocator);
  IsolationHistory::Factory historyFactory(allocator);
  SendingOperandToStateMap transferringOpToStateMap(historyFactory);

  Partition p(historyFactory.get());

  {
    MockedPartitionOpEvaluator eval(p, factory, transferringOpToStateMap);
    eval.apply({PartitionOp::AssignFresh(Element(0)),
                PartitionOp::AssignFresh(Element(1)),
                PartitionOp::AssignFresh(Element(2)),
                PartitionOp::AssignFresh(Element(3)),
                PartitionOp::AssignFresh(Element(4)),
                PartitionOp::AssignFresh(Element(5)),
                PartitionOp::AssignFresh(Element(6)),
                PartitionOp::AssignFresh(Element(7)),
                PartitionOp::AssignFresh(Element(8)),
                PartitionOp::AssignFresh(Element(9)),
                PartitionOp::AssignFresh(Element(10)),
                PartitionOp::AssignFresh(Element(11)),

                PartitionOp::Assign(Element(1), Element(0)),
                PartitionOp::Assign(Element(2), Element(1)),

                PartitionOp::Assign(Element(4), Element(3)),
                PartitionOp::Assign(Element(5), Element(4)),

                PartitionOp::Assign(Element(7), Element(6)),
                PartitionOp::Assign(Element(9), Element(8)),

                // expected: p: ((0 1 2) (3 4 5) (6 7) (8 9) (Element(10))
                // (Element(11)))

                PartitionOp::Send(Element(2), operandSingletons[0]),
                PartitionOp::Send(Element(7), operandSingletons[1]),
                PartitionOp::Send(Element(10), operandSingletons[2])});
  }

  // expected: p: ({0 1 2 6 7 10} (3 4 5) (8 9) (Element(11)))

  auto never_called = [](const PartitionOp &, unsigned, Operand *) {
    EXPECT_TRUE(false);
  };

  int times_called = 0;
  auto increment_times_called = [&](const PartitionOp &, unsigned, Operand *) {
    times_called++;
  };

  {
    MockedPartitionOpEvaluatorWithFailureCallback eval(
        p, factory, transferringOpToStateMap, increment_times_called);
    eval.apply({PartitionOp::Require(Element(0)),
                PartitionOp::Require(Element(1)),
                PartitionOp::Require(Element(2))});
  }
  EXPECT_EQ(times_called, 3);

  {
    MockedPartitionOpEvaluatorWithFailureCallback eval(
        p, factory, transferringOpToStateMap, never_called);
    eval.apply({PartitionOp::Require(Element(3)),
                PartitionOp::Require(Element(4)),
                PartitionOp::Require(Element(5))});
  }

  {
    MockedPartitionOpEvaluatorWithFailureCallback eval(
        p, factory, transferringOpToStateMap, increment_times_called);
    eval.apply(
        {PartitionOp::Require(Element(6)), PartitionOp::Require(Element(7))});
  }

  {
    MockedPartitionOpEvaluatorWithFailureCallback eval(
        p, factory, transferringOpToStateMap, never_called);
    eval.apply(
        {PartitionOp::Require(Element(8)), PartitionOp::Require(Element(9))});
  }

  {
    MockedPartitionOpEvaluatorWithFailureCallback eval(
        p, factory, transferringOpToStateMap, increment_times_called);
    eval.apply(PartitionOp::Require(Element(10)));
  }

  {
    MockedPartitionOpEvaluatorWithFailureCallback eval(
        p, factory, transferringOpToStateMap, never_called);
    eval.apply(PartitionOp::Require(Element(11)));
  }

  EXPECT_EQ(times_called, 6);
}

// This test tests that the copy constructor is usable to create fresh
// copies of partitions
TEST(PartitionUtilsTest, TestCopyConstructor) {
  llvm::BumpPtrAllocator allocator;
  Partition::SendingOperandSetFactory factory(allocator);
  IsolationHistory::Factory historyFactory(allocator);
  SendingOperandToStateMap transferringOpToStateMap(historyFactory);

  Partition p1(historyFactory.get());
  {
    MockedPartitionOpEvaluator eval(p1, factory, transferringOpToStateMap);
    eval.apply(PartitionOp::AssignFresh(Element(0)));
  }

  // Make copy.
  Partition p2 = p1;

  // Change p1 again.
  {
    MockedPartitionOpEvaluator eval(p1, factory, transferringOpToStateMap);
    eval.apply(PartitionOp::Send(Element(0), operandSingletons[0]));
  }

  {
    bool failure = false;
    MockedPartitionOpEvaluatorWithFailureCallback eval(
        p1, factory, transferringOpToStateMap,
        [&](const PartitionOp &, unsigned, Operand *) { failure = true; });
    eval.apply(PartitionOp::Require(Element(0)));
    EXPECT_TRUE(failure);
  }

  {
    MockedPartitionOpEvaluatorWithFailureCallback eval(
        p2, factory, transferringOpToStateMap,
        [](const PartitionOp &, unsigned, Operand *) { EXPECT_TRUE(false); });
    eval.apply(PartitionOp::Require(Element(0)));
  }
}

TEST(PartitionUtilsTest, TestUndoTransfer) {
  llvm::BumpPtrAllocator allocator;
  Partition::SendingOperandSetFactory factory(allocator);
  IsolationHistory::Factory historyFactory(allocator);
  SendingOperandToStateMap transferringOpToStateMap(historyFactory);

  Partition p(historyFactory.get());
  MockedPartitionOpEvaluatorWithFailureCallback eval(
      p, factory, transferringOpToStateMap,
      [&](const PartitionOp &, unsigned, Operand *) { EXPECT_TRUE(false); });

  // Shouldn't error on this.
  eval.apply({PartitionOp::AssignFresh(Element(0)),
              PartitionOp::Send(Element(0), operandSingletons[0]),
              PartitionOp::UndoSend(Element(0), instSingletons[0]),
              PartitionOp::Require(Element(0), instSingletons[0])});
}

TEST(PartitionUtilsTest, TestLastEltInTransferredRegion) {
  llvm::BumpPtrAllocator allocator;
  Partition::SendingOperandSetFactory factory(allocator);
  IsolationHistory::Factory historyFactory(allocator);
  SendingOperandToStateMap transferringOpToStateMap(historyFactory);

  // First make sure that we do this correctly with an assign fresh.
  Partition p(historyFactory.get());
  {
    MockedPartitionOpEvaluator eval(p, factory, transferringOpToStateMap);
    eval.apply({PartitionOp::AssignFresh(Element(0)),
                PartitionOp::AssignFresh(Element(1)),
                PartitionOp::AssignFresh(Element(2)),
                PartitionOp::Send(Element(0), operandSingletons[0]),
                PartitionOp::AssignFresh(Element(0))});
  }
  p.validateRegionToSendingOpMapRegions();

  // Now make sure that we do this correctly with assign.
  Partition p2(historyFactory.get());
  {
    MockedPartitionOpEvaluator eval(p2, factory, transferringOpToStateMap);
    eval.apply({PartitionOp::AssignFresh(Element(0)),
                PartitionOp::AssignFresh(Element(1)),
                PartitionOp::AssignFresh(Element(2)),
                PartitionOp::Send(Element(0), operandSingletons[0]),
                PartitionOp::Assign(Element(0), Element(2))});
  }
  p2.validateRegionToSendingOpMapRegions();
}

TEST(PartitionUtilsTest, TestHistory_CreateVariable) {
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

TEST(PartitionUtilsTest, TestHistory_AssignRegion) {
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
    eval.apply({PartitionOp::Assign(Element(1), Element(2))});
  }

  Partition pSnapshot2 = p;
  {
    MockedPartitionOpEvaluator eval(p, factory, transferringOpToStateMap);
    eval.apply({PartitionOp::Assign(Element(0), Element(2))});
  }

  p.popHistory(joinedHistories);

  EXPECT_TRUE(Partition::equals(p, pSnapshot2));
  EXPECT_TRUE(joinedHistories.empty());

  p.popHistory(joinedHistories);

  EXPECT_TRUE(Partition::equals(p, pSnapshot));
  EXPECT_TRUE(joinedHistories.empty());
}

TEST(PartitionUtilsTest, TestHistory_BuildNewRegionRepIsMergee) {
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
                PartitionOp::Assign(Element(3), Element(2)),
                PartitionOp::Assign(Element(10), Element(2)),
                PartitionOp::Merge(Element(2), Element(0))});
  }

  Partition pSnapshot = p;

  {
    MockedPartitionOpEvaluator eval(p, factory, transferringOpToStateMap);
    eval.apply({PartitionOp::Assign(Element(1), Element(2))});
  }

  Partition pSnapshot2 = p;
  {
    MockedPartitionOpEvaluator eval(p, factory, transferringOpToStateMap);
    eval.apply({PartitionOp::Assign(Element(0), Element(2))});
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

TEST(PartitionUtilsTest, TestHistory_ReturnFalseWhenNoneLeft) {
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

TEST(PartitionUtilsTest, TestHistory_JoiningTwoEmpty) {
  // Make sure that we do sane things when we join empty history.
  llvm::BumpPtrAllocator allocator;
  Partition::SendingOperandSetFactory factory(allocator);
  IsolationHistory::Factory historyFactory(allocator);
  SmallVector<IsolationHistory, 8> joinedHistories;

  Partition p1(historyFactory.get());
  Partition p2(historyFactory.get());

  auto result = Partition::join(p1, p2);
  EXPECT_TRUE(result.begin() == result.end());
  EXPECT_FALSE(result.hasHistory());
}

TEST(PartitionUtilsTest, TestHistory_JoiningNotEmptyAndEmpty) {
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
  EXPECT_TRUE(std::next(result.begin()) == result.end());
  // Since p2 doesn't have any history, we do not actually perform any join and
  // thus do not insert a CFGHistory change.
  EXPECT_TRUE(result.historySize() == 2);
}

TEST(PartitionUtilsTest, TestHistory_JoiningEmptyAndNotEmpty) {
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
  EXPECT_TRUE(std::next(result.begin()) == result.end());
  // Since p2 doesn't have any history, we do not actually perform any join and
  // thus do not insert a CFGHistory change.
  EXPECT_TRUE(result.historySize() == 2);
}
