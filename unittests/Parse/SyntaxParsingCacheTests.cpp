#include "swift/Parse/SyntaxParsingCache.h"
#include "gtest/gtest.h"

#include <iostream>

using namespace swift;
using namespace llvm;


namespace llvm {
template <typename T>
void PrintTo(const Optional<T> &optVal, ::std::ostream *os) {
  if (optVal.hasValue())
    *os << *optVal;
  else
    *os << "None";
}
} // namespace llvm

void check(ArrayRef<SourceEdit> Edits, ArrayRef<Optional<size_t>> expected) {
  for (size_t Pos = 0; Pos != expected.size(); ++Pos) {
    Optional<size_t> PrePos =
        SyntaxParsingCache::translateToPreEditPosition(Pos, Edits);
    EXPECT_EQ(PrePos, expected[Pos]) << "At post-edit position " << Pos;
  }
}

class TranslateToPreEditPositionTest : public ::testing::Test {};

TEST_F(TranslateToPreEditPositionTest, SingleEdit1) {
  // Old: ab_xy
  // New: c_xy

  llvm::SmallVector<SourceEdit, 4> Edits = {
      {0, 2, 1} // ab -> c
  };

  //            c     _  x  y
  check(Edits, {None, 2, 3, 4});
}

TEST_F(TranslateToPreEditPositionTest, SingleEdit) {
  // Old: ab_xy
  // New: ablah_xy

  llvm::SmallVector<SourceEdit, 4> Edits = {
      {1, 2, 4} // b -> blah
  };

  //            a  b     l     a     h     _  x  y
  check(Edits, {0, None, None, None, None, 2, 3, 4});
}

TEST_F(TranslateToPreEditPositionTest, SingleInsert) {
  // Old: ab_xy
  // New: 0123ab_xy

  llvm::SmallVector<SourceEdit, 4> Edits = {
      {0, 0, 4} // '' -> 0123
  };

  //             0     1     2     3     a  b  _  x  y
  check(Edits, { None, None, None, None, 0, 1, 2, 3, 4});
}

TEST_F(TranslateToPreEditPositionTest, SingleDelete) {
  // Old: ab_xyz
  // New: ab_z

  llvm::SmallVector<SourceEdit, 4> Edits = {
      {3, 5, 0} // xy -> ''
  };

  //             a  b  _  z
  check(Edits, { 0, 1, 2, 5 });
}

TEST_F(TranslateToPreEditPositionTest, SimpleMultiEdit) {
  // Old: _ab_xy
  // New: _a1b2_x3y4

  llvm::SmallVector<SourceEdit, 4> Edits = {
      {1, 2, 2}, // a -> a1
      {2, 3, 2}, // b -> b2
      {4, 5, 2}, // x -> x3
      {5, 6, 2}, // y -> y4
  };

  //            _  a     1     b     1     _  x     3     y     4
  check(Edits, {0, None, None, None, None, 3, None, None, None, None});
}

TEST_F(TranslateToPreEditPositionTest, ComplexMultiEdit) {
  // Old: foo_bar_baz
  // New: xx_edits_baz

  llvm::SmallVector<SourceEdit, 4> Edits = {
      {0, 3, 2}, // foo -> xx
      {4, 7, 0}, // bar -> ''
      {7, 7, 5}, // '' -> edits
  };

  //            x     x     _  e     d     i     t     s     _  b  a  z
  check(Edits, {None, None, 3, None, None, None, None, None, 7, 8, 9, 10});
}
