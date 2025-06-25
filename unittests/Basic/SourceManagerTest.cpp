//===--- SourceManagerTest.cpp --------------------------------------------===//
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

#include "swift/Basic/SourceManager.h"
#include "llvm/Support/MemoryBuffer.h"
#include "gtest/gtest.h"
#include <vector>

using namespace swift;
using namespace llvm;

static std::vector<SourceLoc> tokenize(SourceManager &SM, StringRef Source) {
  unsigned ID = SM.addMemBufferCopy(Source);
  const MemoryBuffer *Buf = SM.getLLVMSourceMgr().getMemoryBuffer(ID);

  auto BeginLoc = SourceLoc::getFromPointer(Buf->getBuffer().begin());
  std::vector<SourceLoc> Result;
  Result.push_back(BeginLoc);
  for (unsigned i = 1, e = Source.size(); i != e; ++i) {
    if (Source[i - 1] == ' ')
      Result.push_back(BeginLoc.getAdvancedLoc(i));
  }
  return Result;
}

TEST(SourceManager, IsBeforeInBuffer) {
  SourceManager SM;
  auto Locs = tokenize(SM, "aaa bbb ccc ddd");

  EXPECT_TRUE(SM.isBeforeInBuffer(Locs[0], Locs[1]));
  EXPECT_TRUE(SM.isBeforeInBuffer(Locs[1], Locs[2]));
  EXPECT_TRUE(SM.isBeforeInBuffer(Locs[2], Locs[3]));
  EXPECT_TRUE(SM.isBeforeInBuffer(Locs[0], Locs[3]));

  EXPECT_TRUE(SM.isBeforeInBuffer(Locs[0], Locs[0].getAdvancedLoc(1)));
  EXPECT_TRUE(SM.isBeforeInBuffer(Locs[0].getAdvancedLoc(1), Locs[1]));
}

TEST(SourceManager, RangeContainsTokenLoc) {
  SourceManager SM;
  auto Locs = tokenize(SM, "aaa bbb ccc ddd");

  SourceRange R_aa(Locs[0], Locs[0]);
  SourceRange R_ab(Locs[0], Locs[1]);
  SourceRange R_ac(Locs[0], Locs[2]);

  SourceRange R_bc(Locs[1], Locs[2]);

  EXPECT_TRUE(SM.rangeContainsTokenLoc(R_aa, Locs[0]));
  EXPECT_FALSE(SM.rangeContainsTokenLoc(R_aa, Locs[0].getAdvancedLoc(1)));

  EXPECT_TRUE(SM.rangeContainsTokenLoc(R_ab, Locs[0]));
  EXPECT_TRUE(SM.rangeContainsTokenLoc(R_ab, Locs[0].getAdvancedLoc(1)));
  EXPECT_TRUE(SM.rangeContainsTokenLoc(R_ab, Locs[1]));
  EXPECT_FALSE(SM.rangeContainsTokenLoc(R_ab, Locs[1].getAdvancedLoc(1)));

  EXPECT_TRUE(SM.rangeContainsTokenLoc(R_ac, Locs[0]));
  EXPECT_TRUE(SM.rangeContainsTokenLoc(R_ac, Locs[0].getAdvancedLoc(1)));
  EXPECT_TRUE(SM.rangeContainsTokenLoc(R_ac, Locs[1]));
  EXPECT_TRUE(SM.rangeContainsTokenLoc(R_ac, Locs[1].getAdvancedLoc(1)));
  EXPECT_TRUE(SM.rangeContainsTokenLoc(R_ac, Locs[2]));
  EXPECT_FALSE(SM.rangeContainsTokenLoc(R_ac, Locs[2].getAdvancedLoc(1)));

  EXPECT_FALSE(SM.rangeContainsTokenLoc(R_bc, Locs[0]));
  EXPECT_FALSE(SM.rangeContainsTokenLoc(R_bc, Locs[0].getAdvancedLoc(1)));
}

TEST(SourceManager, RangeContains) {
  SourceManager SM;
  auto Locs = tokenize(SM, "aaa bbb ccc ddd");

  SourceRange R_aa(Locs[0], Locs[0]);
  SourceRange R_ab(Locs[0], Locs[1]);
  SourceRange R_ac(Locs[0], Locs[2]);
  SourceRange R_ad(Locs[0], Locs[3]);

  SourceRange R_bc(Locs[1], Locs[2]);

  EXPECT_TRUE(SM.rangeContains(R_ab, R_aa));
  EXPECT_TRUE(SM.rangeContains(R_ac, R_aa));

  EXPECT_TRUE(SM.rangeContains(R_ac, R_ab));
  EXPECT_TRUE(SM.rangeContains(R_ad, R_ab));

  EXPECT_TRUE(SM.rangeContains(R_ad, R_ac));

  EXPECT_TRUE(SM.rangeContains(R_ac, R_bc));
  EXPECT_TRUE(SM.rangeContains(R_ad, R_bc));
}

