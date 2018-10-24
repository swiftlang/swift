#include "swift/Parse/SyntaxParsingCache.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace llvm;

class TranslateToPreEditPositionTest : public ::testing::Test {};

TEST_F(TranslateToPreEditPositionTest, SingleEditBefore) {
  // Old: ab_xy
  // New: a1b_xy
  //
  // Edits:
  // (1) 1-2: a -> a1
  //
  // Lookup for _ at new position 4

  llvm::SmallVector<SourceEdit, 4> Edits = {
      {1, 2, 2}
  };

  size_t PreEditPos = SyntaxParsingCache::translateToPreEditPosition(4, Edits);
  EXPECT_EQ(PreEditPos, 3u);
}

TEST_F(TranslateToPreEditPositionTest, SingleEditDirectlyBefore) {
  // Old: ab_xy
  // New: ablah_xy
  //
  // Edits:
  // (1) 2-3: b -> blah
  //
  // Lookup for _ at new position 6

  llvm::SmallVector<SourceEdit, 4> Edits = {
      {2, 3, 4}
  };

  size_t PreEditPos = SyntaxParsingCache::translateToPreEditPosition(6, Edits);
  EXPECT_EQ(PreEditPos, 3u);
}

TEST_F(TranslateToPreEditPositionTest, SingleMultiCharacterEdit) {
  // Old: ab_xy
  // New: abcdef_xy
  //
  // Edits:
  // (1) 1-3: ab -> abcdef
  //
  // Lookup for _ at new position 7

  llvm::SmallVector<SourceEdit, 4> Edits = {
      {1, 3, 6}
  };

  size_t PreEditPos = SyntaxParsingCache::translateToPreEditPosition(7, Edits);
  EXPECT_EQ(PreEditPos, 3u);
}

TEST_F(TranslateToPreEditPositionTest, EditAfterLookup) {
  // Old: ab_xy
  // New: ab_xyz
  //
  // Edits:
  // (1) 4-6: xy -> xyz
  //
  // Lookup for _ at new position 3

  llvm::SmallVector<SourceEdit, 4> Edits = {{4, 6, 4}};

  size_t PreEditPos = SyntaxParsingCache::translateToPreEditPosition(3, Edits);
  EXPECT_EQ(PreEditPos, 3u);
}

TEST_F(TranslateToPreEditPositionTest, SimpleMultiEdit) {
  // Old: ab_xy
  // New: a1b2_x3y4
  //
  // Edits:
  // (1) 1-2: a -> a1
  // (2) 2-3: b -> b2
  // (3) 4-5: x -> x3
  // (4) 5-6: y -> y4
  //
  // Lookup for _ at new position 5

  llvm::SmallVector<SourceEdit, 4> Edits = {
      {1, 2, 2},
      {2, 3, 2},
      {4, 5, 2},
      {5, 6, 2},
  };

  size_t PreEditPos = SyntaxParsingCache::translateToPreEditPosition(5, Edits);
  EXPECT_EQ(PreEditPos, 3u);
}

TEST_F(TranslateToPreEditPositionTest, LongMultiEdit) {
  // Old: ab_xy
  // New: a11111b2_x3y4
  //
  // Edits:
  // (1) 1-2: a -> a11111
  // (2) 2-3: b -> b2
  // (3) 4-5: x -> x3
  // (4) 5-6: y -> y4
  //
  // Lookup for _ at new position

  llvm::SmallVector<SourceEdit, 4> Edits = {
      {1, 2, 6},
      {2, 3, 2},
      {4, 5, 2},
      {5, 6, 2},
  };

  size_t PreEditPos = SyntaxParsingCache::translateToPreEditPosition(9, Edits);
  EXPECT_EQ(PreEditPos, 3u);
}