//===------ AutoDiff.cpp - Autodiff runtime function tests ----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// SWIFT_ENABLE_TENSORFLOW
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/AutoDiff.h"
#include "gtest/gtest.h"

using namespace swift;

TEST(TestAutoDiffTape, testBasic) {
  auto tapePtr = swift_autodiffCreateTape(nullptr);
  auto tape = reinterpret_cast<AutoDiffTape *>(tapePtr);
  EXPECT_EQ(tape->elements.size(), 0);

  size_t value1[1] = { 100 };
  size_t value2[1] = { 200 };
  size_t value3[1] = { 300 };
  swift_autodiffPushToTape(tapePtr, (OpaqueValue *) value1);
  swift_autodiffPushToTape(tapePtr, (OpaqueValue *) value2);
  swift_autodiffPushToTape(tapePtr, (OpaqueValue *) value3);
  EXPECT_EQ(tape->elements.size(), 3);

  auto cast = [&](OpaqueValue *ptr) {
    return reinterpret_cast<size_t *>(ptr);
  };
  size_t *popped3 = cast(swift_autodiffPopFromTape(tapePtr));
  size_t *popped2 = cast(swift_autodiffPopFromTape(tapePtr));
  size_t *popped1 = cast(swift_autodiffPopFromTape(tapePtr));
  EXPECT_EQ(*popped3, 300);
  EXPECT_EQ(*popped2, 200);
  EXPECT_EQ(*popped1, 100);

  swift_autodiffDestroyTape(tapePtr);
}
