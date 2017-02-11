//===--- DiverseStackTest.cpp ---------------------------------------------===//
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

#include "swift/Basic/DiverseStack.h"
#include "gtest/gtest.h"

using namespace swift;

namespace {

struct ParentType {
  uint8_t allocatedSize;

public:
  ParentType(uint8_t allocatedSize) : allocatedSize(allocatedSize) {}

  unsigned allocated_size() const { return allocatedSize; }
};

struct TwoByteType : ParentType {
  uint8_t Value;

  TwoByteType(uint8_t Value) : ParentType(sizeof(*this)), Value(Value) {}
};

struct ThreeByteType : ParentType {
  uint16_t Value;

  ThreeByteType(uint16_t Value) : ParentType(sizeof(*this)), Value(Value) {}
};

} // end anonymous namespace

TEST(DiverseStack, MonomorphicPushPop) {
  DiverseStack<ParentType, 128> Stack;

  EXPECT_TRUE(Stack.empty());

  constexpr size_t TwoByteDataSize = 5;
  uint8_t InputData[TwoByteDataSize] = {5, 9, 1, 2, 10};
  for (unsigned i = 0; i < TwoByteDataSize; ++i) {
    Stack.push<TwoByteType>(TwoByteType(InputData[i]));
  }

  EXPECT_FALSE(Stack.empty());

  for (int i = TwoByteDataSize - 1; i >= 0; --i) {
    TwoByteType T = reinterpret_cast<TwoByteType &>(Stack.top());
    Stack.pop();
    EXPECT_EQ(T.Value, InputData[i]);
  }

  EXPECT_TRUE(Stack.empty());
}

// We test the property here that iterating forward through the stack iterates
// in stack order. This is a bit counter-intuitive for people used to vector
// stacks.
TEST(DiverseStack, Iterate) {
  DiverseStack<ParentType, 128> Stack;

  constexpr size_t TwoByteDataSize = 5;
  uint8_t InputData[TwoByteDataSize] = {5, 9, 1, 2, 10};
  for (unsigned i = 0; i < TwoByteDataSize; ++i) {
    Stack.push<TwoByteType>(TwoByteType(InputData[i]));
  }

  const uint8_t *Ptr = &InputData[TwoByteDataSize - 1];
  for (auto II = Stack.begin(), IE = Stack.end(); II != IE;) {
    TwoByteType T = reinterpret_cast<TwoByteType &>(*II);
    EXPECT_EQ(T.Value, *Ptr);
    --Ptr;
    ++II;
  }
}
