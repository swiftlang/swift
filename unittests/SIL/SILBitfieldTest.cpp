//===--- SILBitfieldTest.cpp ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "gtest/gtest.h"


namespace swift {

class BasicBlockBitfield;

// Fake SILFunction and SILBasicBlock.

struct SILFunction {
  BasicBlockBitfield *newestAliveBlockBitfield = nullptr;
  uint64_t currentBitfieldID = 1;
};

struct SILBasicBlock {
  SILFunction *function;
  uint32_t customBits = 0;
  uint64_t lastInitializedBitfieldID = 0;

  enum { numCustomBits = 32 };
  enum { maxBitfieldID = std::numeric_limits<uint64_t>::max() };

  SILBasicBlock(SILFunction *function): function(function) {}

  SILFunction *getFunction() const { return function; }
  
  unsigned getCustomBits() const { return customBits; }
  void setCustomBits(unsigned value) { customBits = value; }
  
};

}

#define SWIFT_SIL_SILFUNCTION_H

#include "swift/SIL/BasicBlockBits.h"

using namespace swift;

namespace {

TEST(SILBitfieldTest, Basic) {
  SILFunction f;
  SILBasicBlock b(&f);

  {
    BasicBlockFlag A(&f);
    EXPECT_FALSE(A.get(&b));

    EXPECT_FALSE(A.testAndSet(&b));
    EXPECT_TRUE(A.get(&b));

    EXPECT_TRUE(A.testAndSet(&b));
    EXPECT_TRUE(A.get(&b));

    A.reset(&b);
    EXPECT_FALSE(A.get(&b));

    {
      BasicBlockBitfield B(&f, 5);
      EXPECT_EQ(B.get(&b), 0u);

      B.set(&b, 27);
      EXPECT_FALSE(A.get(&b));
      EXPECT_EQ(B.get(&b), 27u);

      A.set(&b);
      EXPECT_TRUE(A.get(&b));
      EXPECT_EQ(B.get(&b), 27u);

      B.set(&b, 2);
      EXPECT_TRUE(A.get(&b));
      EXPECT_EQ(B.get(&b), 2u);

      B.set(&b, 31);
      EXPECT_TRUE(A.get(&b));
      EXPECT_EQ(B.get(&b), 31u);
    }
    {
      BasicBlockBitfield C(&f, 2);
      EXPECT_EQ(C.get(&b), 0u);

      BasicBlockFlag D(&f);
      EXPECT_FALSE(D.get(&b));

      BasicBlockBitfield E(&f, 3);

      E.set(&b, 5);
      EXPECT_TRUE(A.get(&b));
      EXPECT_EQ(C.get(&b), 0u);
      EXPECT_FALSE(D.get(&b));
      EXPECT_EQ(E.get(&b), 5u);

      C.set(&b, 1);
      EXPECT_TRUE(A.get(&b));
      EXPECT_EQ(C.get(&b), 1u);
      EXPECT_FALSE(D.get(&b));
      EXPECT_EQ(E.get(&b), 5u);
    }
  }
  {
    BasicBlockBitfield F(&f, 32);
    EXPECT_EQ(F.get(&b), 0u);
    F.set(&b, 0xdeadbeaf);
    EXPECT_EQ(F.get(&b), 0xdeadbeaf);
  }
}

}
