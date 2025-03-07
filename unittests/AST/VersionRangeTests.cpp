#include "swift/AST/AvailabilityRange.h"
#include "gtest/gtest.h"

using namespace swift;

// A test fixture with version ranges
class VersionRangeLattice : public ::testing::Test {
public:
  VersionRange All = VersionRange::all();

  VersionRange GreaterThanEqual10_10 =
      VersionRange::allGTE(llvm::VersionTuple(10, 10));

  VersionRange GreaterThanEqual10_9 =
      VersionRange::allGTE(llvm::VersionTuple(10, 9));

  VersionRange Empty = VersionRange::empty();

  VersionRange intersectRanges(VersionRange A, VersionRange B) {
    A.intersectWith(B);
    return A;
  }

  VersionRange unionRanges(VersionRange A, VersionRange B) {
    A.unionWith(B);
    return A;
  }

  bool equals(VersionRange A, VersionRange B) {
    return A.isContainedIn(B) && B.isContainedIn(A);
  }

  bool intersectEquals(VersionRange A, VersionRange B, VersionRange Expected) {
    VersionRange AMeetB = intersectRanges(A, B);
    VersionRange BMeetA = intersectRanges(A, B);

    return equals(AMeetB, Expected) && equals(BMeetA, Expected);
  }

  bool unionEquals(VersionRange A, VersionRange B, VersionRange Expected) {
    VersionRange AJoinB = unionRanges(A, B);
    VersionRange BJoinA = unionRanges(A, B);

    return equals(AJoinB, Expected) && equals(BJoinA, Expected);
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

// Test that Empty acts like the bottom element in the lattice with respect to
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
// intersection.
TEST_F(VersionRangeLattice, MeetWithAll) {
  EXPECT_TRUE(intersectEquals(All, All, All));
  EXPECT_TRUE(intersectEquals(GreaterThanEqual10_10, All,
                              GreaterThanEqual10_10));
  EXPECT_TRUE(intersectEquals(Empty, All, Empty));
}

// Test that All acts like the top element in the lattice with respect to
// union.
TEST_F(VersionRangeLattice, JoinWithAll) {
  EXPECT_TRUE(unionEquals(All, All, All));
  EXPECT_TRUE(unionEquals(GreaterThanEqual10_10, All, All));
  EXPECT_TRUE(unionEquals(Empty, All, All));
}

// Test that Empty acts like the bottom element in the lattice with respect to
// intersection.
TEST_F(VersionRangeLattice, MeetWithEmpty) {
  EXPECT_TRUE(intersectEquals(GreaterThanEqual10_10, Empty, Empty));
  EXPECT_TRUE(intersectEquals(Empty, Empty, Empty));
}

// Test that Empty acts like the bottom element in the lattice with respect to
// union.
TEST_F(VersionRangeLattice, JoinWithEmpty) {
  EXPECT_TRUE(unionEquals(GreaterThanEqual10_10, Empty, GreaterThanEqual10_10));
  EXPECT_TRUE(unionEquals(Empty, Empty, Empty));
}

// Test intersection for ranges with lower end points.
TEST_F(VersionRangeLattice, MeetWithClosedEndedPositiveInfinity) {
  EXPECT_TRUE(intersectEquals(GreaterThanEqual10_10, GreaterThanEqual10_10,
                              GreaterThanEqual10_10));
  EXPECT_TRUE(intersectEquals(GreaterThanEqual10_10, GreaterThanEqual10_9,
                              GreaterThanEqual10_10));
}

// Test union for ranges with lower end points.
TEST_F(VersionRangeLattice, JoinWithClosedEndedPositiveInfinity) {
  EXPECT_TRUE(unionEquals(GreaterThanEqual10_10, GreaterThanEqual10_10,
                          GreaterThanEqual10_10));
  EXPECT_TRUE(unionEquals(GreaterThanEqual10_10, GreaterThanEqual10_9,
                          GreaterThanEqual10_9));
}

TEST_F(VersionRangeLattice, ValidVersionTuples) {
  EXPECT_TRUE(VersionRange::isValidVersion(llvm::VersionTuple()));
  EXPECT_FALSE(VersionRange::isValidVersion(llvm::VersionTuple(0x7FFFFFFE)));
  EXPECT_FALSE(VersionRange::isValidVersion(llvm::VersionTuple(0x7FFFFFFF)));
}
