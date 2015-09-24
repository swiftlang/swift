#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/Version.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace llvm;

class CompilerVersionTest : public ::testing::Test {};

version::CompilerVersion CV(const char *VersionString) {
  return version::CompilerVersion(VersionString, SourceLoc(), nullptr);
}

TEST_F(CompilerVersionTest, VersionComparison) {
  EXPECT_GE(CV("700"), CV("602"));
  EXPECT_GE(CV("700.0"), CV("700.1"));
  EXPECT_GE(CV("700.0.1"), CV("700.1.0"));
  EXPECT_GE(CV("700.8.23"), CV("700.0.21"));
  EXPECT_GE(CV("700.0.1.1.0"), CV("700.0.1.1"));
  EXPECT_GE(version::CompilerVersion(), version::CompilerVersion());
  EXPECT_GE(version::CompilerVersion(), CV("999.999.999.999.999"));
}
