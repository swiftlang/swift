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
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/MemoryBuffer.h"
#include "gtest/gtest.h"
#include <functional>
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

static SourceLoc getBufferStartLoc(SourceManager &SM, unsigned BufferID) {
  return SourceLoc::getFromPointer(
      SM.getLLVMSourceMgr().getMemoryBuffer(BufferID)->getBufferStart());
}

/// Add a buffer that refers to the same memory as the buffer \p BufferID.
static unsigned addAliasBuffer(SourceManager &SM, unsigned BufferID) {
  StringRef Text = SM.getLLVMSourceMgr().getMemoryBuffer(BufferID)->getBuffer();
  return SM.addNewSourceBuffer(MemoryBuffer::getMemBuffer(
      Text, "alias", /*RequiresNullTerminator=*/false));
}

TEST(SourceManager, FindBufferContainingLocAcrossAddedBuffers) {
  // Allocate the buffers up front and add them in order of decreasing address,
  // so that each added buffer sorts before the buffers already added.
  std::vector<std::unique_ptr<MemoryBuffer>> Pending;
  for (unsigned i = 0; i != 163; ++i)
    Pending.push_back(
        MemoryBuffer::getMemBufferCopy("buffer " + std::to_string(i)));
  llvm::sort(Pending, [](const auto &LHS, const auto &RHS) {
    return std::greater<const char *>()(LHS->getBufferStart(),
                                        RHS->getBufferStart());
  });

  SourceManager SM;
  std::vector<unsigned> IDs;
  auto AddBuffers = [&](unsigned Count) {
    for (unsigned i = 0; i != Count; ++i) {
      IDs.push_back(SM.addNewSourceBuffer(std::move(Pending.front())));
      Pending.erase(Pending.begin());
    }
  };
  auto ExpectAllFound = [&] {
    for (unsigned ID : IDs) {
      EXPECT_EQ(SM.findBufferContainingLoc(getBufferStartLoc(SM, ID)), ID);
      EXPECT_EQ(SM.findBufferContainingLoc(
                    getBufferStartLoc(SM, ID).getAdvancedLoc(3)),
                ID);
    }
  };

  // Add many buffers at once.
  AddBuffers(100);
  ExpectAllFound();

  // Add buffers one at a time, looking up locations in between.
  for (unsigned i = 0; i != 10; ++i) {
    AddBuffers(1);
    ExpectAllFound();
  }

  // Add a few buffers before the next lookup.
  AddBuffers(3);
  ExpectAllFound();

  // Add many buffers before the next lookup.
  AddBuffers(50);
  ExpectAllFound();
}

TEST(SourceManager, FindBufferContainingLocPrefersLatestAlias) {
  SourceManager SM;
  std::vector<unsigned> IDs;
  for (unsigned i = 0; i != 100; ++i)
    IDs.push_back(SM.addMemBufferCopy("buffer " + std::to_string(i)));

  // An alias added before the first lookup is found instead of the original.
  unsigned FirstAlias = addAliasBuffer(SM, IDs[10]);
  EXPECT_EQ(SM.findBufferContainingLoc(getBufferStartLoc(SM, IDs[10])),
            FirstAlias);

  // An alias added after the lookup cache was built is found instead of the
  // original.
  unsigned SecondAlias = addAliasBuffer(SM, IDs[50]);
  EXPECT_EQ(SM.findBufferContainingLoc(getBufferStartLoc(SM, IDs[50])),
            SecondAlias);

  // A newer alias of an alias is found instead of both.
  unsigned ThirdAlias = addAliasBuffer(SM, FirstAlias);
  EXPECT_EQ(SM.findBufferContainingLoc(getBufferStartLoc(SM, IDs[10])),
            ThirdAlias);

  // Of several aliases added before the next lookup, the newest is found.
  addAliasBuffer(SM, IDs[50]);
  unsigned FifthAlias = addAliasBuffer(SM, IDs[50]);
  EXPECT_EQ(SM.findBufferContainingLoc(getBufferStartLoc(SM, IDs[50])),
            FifthAlias);

  // Buffers without aliases are unaffected.
  for (unsigned i = 0; i != IDs.size(); ++i) {
    if (i == 10 || i == 50)
      continue;
    EXPECT_EQ(SM.findBufferContainingLoc(getBufferStartLoc(SM, IDs[i])),
              IDs[i]);
  }
}
