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
#include <random>

using namespace swift;

namespace {

enum class ValueKind {
  TwoByte = 0,
  ThreeByte = 1,
};

struct ParentType {
  uint8_t allocatedSize;
  ValueKind kind;

public:
  ParentType(uint8_t allocatedSize, ValueKind kind)
      : allocatedSize(allocatedSize), kind(kind) {}

  unsigned allocated_size() const { return allocatedSize; }
  ValueKind getKind() const { return kind; }
};

struct TwoByteType : ParentType {
  uint8_t Value;

  TwoByteType(uint8_t Value)
      : ParentType(sizeof(*this), ValueKind::TwoByte), Value(Value) {}
};

struct ThreeByteType : ParentType {
  uint16_t Value;

  ThreeByteType(uint16_t Value)
      : ParentType(sizeof(*this), ValueKind::ThreeByte), Value(Value) {}
};

struct RandomValueGenerator {
  std::mt19937 gen;
  std::uniform_int_distribution<uint8_t> randomEightBitValueGenerator;
  std::uniform_int_distribution<uint16_t> randomSixteenBitValueGenerator;

  // Randomly generated bits. This is frozen to ensure that the test doesn't
  // change in between runs.
  static constexpr unsigned seed() { return 0xb2f2c1c8; }

  RandomValueGenerator()
      : gen(seed()), randomEightBitValueGenerator(),
        randomSixteenBitValueGenerator() {}
  ~RandomValueGenerator() = default;

  RandomValueGenerator(const RandomValueGenerator &) = delete;
  RandomValueGenerator(RandomValueGenerator &&) = delete;
  RandomValueGenerator &operator=(const RandomValueGenerator &) = delete;
  RandomValueGenerator &operator=(RandomValueGenerator &&) = delete;

  void push(DiverseStackImpl<ParentType> &Stack,
            std::vector<TwoByteType> &TwoByteVector,
            std::vector<ThreeByteType> &ThreeByteVector,
            std::vector<ValueKind> &ControlVector) {
    uint8_t value = randomEightBitValueGenerator(gen) % 2;
    if (value) {
      auto Next = TwoByteType(randomEightBitValueGenerator(gen));
      Stack.push<TwoByteType>(Next);
      ControlVector.emplace_back(ValueKind::TwoByte);
      TwoByteVector.push_back(Next);
      return;
    }

    auto Next = ThreeByteType(randomSixteenBitValueGenerator(gen));
    Stack.push<ThreeByteType>(Next);
    ControlVector.emplace_back(ValueKind::ThreeByte);
    ThreeByteVector.push_back(Next);
  }

  void push(DiverseStackImpl<ParentType> &Stack) {
    uint8_t value = randomEightBitValueGenerator(gen) % 2;
    if (value) {
      auto Next = TwoByteType(randomEightBitValueGenerator(gen));
      Stack.push<TwoByteType>(Next);
      return;
    }

    auto Next = ThreeByteType(randomSixteenBitValueGenerator(gen));
    Stack.push<ThreeByteType>(Next);
  }
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

TEST(DiverseStack, PolymorphicPushPop) {
  RandomValueGenerator RandomGen;
  DiverseStack<ParentType, 128> Stack;
  std::vector<TwoByteType> TwoByteVector;
  std::vector<ThreeByteType> ThreeByteVector;
  std::vector<ValueKind> ControlVector;

  EXPECT_TRUE(Stack.empty());

  unsigned NumValues = 1024;
  for (unsigned i = 0; i < NumValues; ++i) {
    RandomGen.push(Stack, TwoByteVector, ThreeByteVector, ControlVector);
  }

  EXPECT_EQ(ControlVector.size(), NumValues);
  EXPECT_GE(ControlVector.size(), TwoByteVector.size());
  EXPECT_GE(ControlVector.size(), ThreeByteVector.size());

  while (!ControlVector.empty()) {
    EXPECT_FALSE(Stack.empty());
    ValueKind VectorSwitch = ControlVector.back();
    ControlVector.pop_back();

    if (VectorSwitch == ValueKind::TwoByte) {
      TwoByteType Expected = TwoByteVector.back();
      TwoByteVector.pop_back();
      TwoByteType Actual = static_cast<TwoByteType &>(Stack.top());
      Stack.pop<TwoByteType>();
      EXPECT_EQ(Expected.Value, Actual.Value);
      continue;
    }

    assert(VectorSwitch == ValueKind::ThreeByte);
    ThreeByteType Expected = ThreeByteVector.back();
    ThreeByteVector.pop_back();
    ThreeByteType Actual = static_cast<ThreeByteType &>(Stack.top());
    EXPECT_EQ(Expected.Value, Actual.Value);
    Stack.pop<ThreeByteType>();
  }

  EXPECT_TRUE(ControlVector.empty());
  EXPECT_TRUE(TwoByteVector.empty());
  EXPECT_TRUE(ThreeByteVector.empty());
  EXPECT_TRUE(Stack.empty());
}

TEST(DiverseStack, StableIndexLookup) {
  RandomValueGenerator RandomGen;
  DiverseStack<ParentType, sizeof(unsigned) * 4 * 8> Stack;

  unsigned FirstStop = 3;
  unsigned NumValues = 1024;
  for (unsigned i = 0; i < FirstStop; ++i) {
    RandomGen.push(Stack);
  }

  decltype(Stack)::stable_iterator Iter = Stack.stable_begin();
  ParentType &Parent = *Stack.find(Iter);
  ValueKind SavedKind = Parent.getKind();
  unsigned SavedValue;
  if (SavedKind == ValueKind::TwoByte) {
    SavedValue = static_cast<TwoByteType &>(Parent).Value;
  } else {
    SavedValue = static_cast<ThreeByteType &>(Parent).Value;
  }

  for (unsigned i = FirstStop; i < NumValues; ++i) {
    RandomGen.push(Stack);
  }

  Stack.pop();
  Stack.pop();
  Stack.pop();

  Parent = *Stack.find(Iter);
  EXPECT_EQ(SavedKind, Parent.getKind());
  if (SavedKind == ValueKind::TwoByte) {
    EXPECT_EQ(SavedValue, static_cast<TwoByteType &>(Parent).Value);
  } else {
    EXPECT_EQ(SavedValue, static_cast<ThreeByteType &>(Parent).Value);
  }
}

TEST(DiverseStack, PopMany) {
  RandomValueGenerator RandomGen;
  DiverseStack<ParentType, sizeof(unsigned) * 4 * 8> Stack;

  unsigned FirstStop = 3;
  unsigned NumValues = 1024;
  for (unsigned i = 0; i < FirstStop; ++i) {
    RandomGen.push(Stack);
  }

  decltype(Stack)::stable_iterator Iter = Stack.stable_begin();
  ParentType &Parent = *Stack.find(Iter);
  ValueKind SavedKind = Parent.getKind();
  unsigned SavedValue;
  if (SavedKind == ValueKind::TwoByte) {
    SavedValue = static_cast<TwoByteType &>(Parent).Value;
  } else {
    SavedValue = static_cast<ThreeByteType &>(Parent).Value;
  }

  for (unsigned i = FirstStop; i < NumValues; ++i) {
    RandomGen.push(Stack);
  }

  // Pop until Iter is the top of the stack.
  Stack.pop(Iter);

  Parent = *Stack.find(Iter);
  EXPECT_EQ(SavedKind, Parent.getKind());
  if (SavedKind == ValueKind::TwoByte) {
    EXPECT_EQ(SavedValue, static_cast<TwoByteType &>(Parent).Value);
  } else {
    EXPECT_EQ(SavedValue, static_cast<ThreeByteType &>(Parent).Value);
  }

  EXPECT_EQ(Iter, Stack.stable_begin());
}
