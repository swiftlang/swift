//===--- TypeTests.cpp - Tests for miscellaneous Type behavior -----------===//
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

#include "TestContext.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Types.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace swift::unittest;

// Check that IntegerType::get(const APSInt&, ...) handles signedness correctly.

TEST(IntegerType, UnsignedValueWithHighBitSetIsNotNegative) {
  TestContext C;

  // 3000000000 > 2^31, so bit 31 -- the sign bit under a signed
  // interpretation -- is set.
  APSInt count(APInt(32, 3000000000ULL), /*isUnsigned=*/true);
  ASSERT_TRUE(count.APInt::isNegative())
      << "test is only meaningful if the top bit is actually set";

  auto *ty = IntegerType::get(count, C.Ctx);

  EXPECT_FALSE(ty->isNegative());
  EXPECT_EQ(ty->getDigitsText(), "3000000000");
}

TEST(IntegerType, UnsignedValueWithHighBitClearRoundTrips) {
  TestContext C;

  APSInt count(APInt(32, 4), /*isUnsigned=*/true);
  ASSERT_FALSE(count.APInt::isNegative());

  auto *ty = IntegerType::get(count, C.Ctx);

  EXPECT_FALSE(ty->isNegative());
  EXPECT_EQ(ty->getDigitsText(), "4");
}

TEST(IntegerType, SignedValueWithHighBitSetIsNegative) {
  TestContext C;

  // The same bit pattern as the unsigned test above, but this time the
  // APSInt is signed, so the top bit does mean negative.
  APSInt value(APInt(32, 3000000000ULL), /*isUnsigned=*/false);
  ASSERT_TRUE(value.isNegative());

  auto *ty = IntegerType::get(value, C.Ctx);

  EXPECT_TRUE(ty->isNegative());
  EXPECT_EQ(ty->getDigitsText(), "1294967296");
}

TEST(IntegerType, SignedValueWithHighBitClearRoundTrips) {
  TestContext C;

  APSInt value(APInt(32, 4), /*isUnsigned=*/false);
  ASSERT_FALSE(value.isNegative());

  auto *ty = IntegerType::get(value, C.Ctx);

  EXPECT_FALSE(ty->isNegative());
  EXPECT_EQ(ty->getDigitsText(), "4");
}
