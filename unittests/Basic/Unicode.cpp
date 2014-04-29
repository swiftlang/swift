#include "swift/Basic/Unicode.h"
#include "gtest/gtest.h"

using namespace swift::unicode;

TEST(ExtractExtendedGraphemeCluster, Test1) {
  EXPECT_EQ("", extractFirstExtendedGraphemeCluster(""));
  EXPECT_EQ("a", extractFirstExtendedGraphemeCluster("a"));
  EXPECT_EQ("a", extractFirstExtendedGraphemeCluster("abc"));
}

TEST(IsSingleExtendedGraphemeCluster, Test1) {
  EXPECT_EQ(false, isSingleExtendedGraphemeCluster(""));
  EXPECT_EQ(true, isSingleExtendedGraphemeCluster("a"));
}
