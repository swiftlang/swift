#include "swift/AST/TypeRefinementContext.h"
#include "gtest/gtest.h"

using namespace swift;

// A test fixture with version ranges
class VersionRangeLattice : public ::testing::Test {
public:
  VersionRange All = VersionRange::all();

  VersionRange GreaterThanEqual10_10 =
      VersionRange::allGTE(clang::VersionTuple(10, 10));

  VersionRange GreaterThanEqual10_9 =
      VersionRange::allGTE(clang::VersionTuple(10, 9));

  VersionRange Empty = VersionRange::empty();

  VersionRange meet(VersionRange A, VersionRange B) {
    A.meetWith(B);
    return A;
  }

  bool equals(VersionRange A, VersionRange B) {
    return A.isContainedIn(B) && B.isContainedIn(A);
  }

  bool meetEquals(VersionRange A, VersionRange B, VersionRange Expected) {
    VersionRange AMeetB = meet(A, B);
    VersionRange BMeetA = meet(A, B);

    return equals(AMeetB, Expected) && equals(BMeetA, Expected);
  }
};

// Test that All acts like the top element in the lattice with respect to
// containment.
TEST_F(VersionRangeLattice, AllIsTopElement) {
  EXPECT_TRUE(All.isContainedIn(All));

  EXPECT_TRUE(GreaterThanEqual10_10.isContainedIn(All));
  EXPECT_TRUE(Empty.isContainedIn(All));

  EXPECT_FALSE(All.isContainedIn(GreaterThanEqual10_10));
  EXPECT_FALSE(All.isContainedIn(Empty));
}

// Test that Empty acts like the bottom element in the lattice  with respect to
// containment.
TEST_F(VersionRangeLattice, EmptyIsBottomElement) {
  EXPECT_TRUE(Empty.isContainedIn(Empty));

  EXPECT_TRUE(Empty.isContainedIn(All));
  EXPECT_TRUE(Empty.isContainedIn(GreaterThanEqual10_10));

  EXPECT_FALSE(GreaterThanEqual10_10.isContainedIn(Empty));
  EXPECT_FALSE(GreaterThanEqual10_10.isContainedIn(Empty));
}

// Test containment for ranges with lower end points.
TEST_F(VersionRangeLattice, ContainmentClosedEndedPositiveInfinity) {
  EXPECT_TRUE(GreaterThanEqual10_10.isContainedIn(GreaterThanEqual10_10));

  EXPECT_TRUE(GreaterThanEqual10_10.isContainedIn(GreaterThanEqual10_9));
  EXPECT_TRUE(Empty.isContainedIn(GreaterThanEqual10_9));

  EXPECT_FALSE(GreaterThanEqual10_9.isContainedIn(GreaterThanEqual10_10));
}

// Test that All acts like the top element in the lattice with respect to
// meet.
TEST_F(VersionRangeLattice, MeetWithAll) {
  EXPECT_TRUE(meetEquals(All, All, All));
  EXPECT_TRUE(meetEquals(GreaterThanEqual10_10, All, GreaterThanEqual10_10));
  EXPECT_TRUE(meetEquals(Empty, All, Empty));
}

// Test that Empty acts like the bottom element in the lattice with respect to
// meet.
TEST_F(VersionRangeLattice, MeetWithEmpty) {
  EXPECT_TRUE(meetEquals(GreaterThanEqual10_10, Empty, Empty));
  EXPECT_TRUE(meetEquals(Empty, Empty, Empty));
}

// Test meet for ranges with lower end points.
TEST_F(VersionRangeLattice, MeetWithClosedEndedPositiveInfinity) {
  EXPECT_TRUE(meetEquals(GreaterThanEqual10_10, GreaterThanEqual10_10,
                         GreaterThanEqual10_10));
  EXPECT_TRUE(meetEquals(GreaterThanEqual10_10, GreaterThanEqual10_9,
                         GreaterThanEqual10_10));
}
