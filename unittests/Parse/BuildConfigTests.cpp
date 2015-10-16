#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/Version.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace llvm;

class CompilerVersionTest : public ::testing::Test {};
class VersionTest : public ::testing::Test{};

version::Version CV(const char *VersionString) {
  return version::Version::parseCompilerVersionString(VersionString,
                                                      SourceLoc(),
                                                      nullptr);
}

version::Version V(const char *VersionString) {
  return version::Version::parseVersionString(VersionString,
                                              SourceLoc(),
                                              nullptr);
}

TEST_F(CompilerVersionTest, VersionComparison) {
  auto currentVersion = version::Version::getCurrentCompilerVersion();
  EXPECT_GE(CV("700"), CV("602"));
  EXPECT_GE(CV("700.*"), CV("700.*"));
  EXPECT_GE(CV("700.*.1"), CV("700.*.0"));
  EXPECT_GE(CV("700.*.23"), CV("700.*.21"));
  EXPECT_GE(CV("700.*.1.1.0"), CV("700.*.1.1"));
  EXPECT_GE(currentVersion, currentVersion);
  EXPECT_GE(currentVersion, CV("9223371.*.999.999.999"));
}

TEST_F(VersionTest, VersionComparison) {
  version::Version currentVersion;
  EXPECT_GE(V("700"), V("602"));
  EXPECT_GE(V("700.0"), V("700.0"));
  EXPECT_GE(V("700.1"), V("700.0"));
  EXPECT_GE(V("700.1.20"), V("700.0.20"));
  EXPECT_GE(V("700.0"), V("700"));
  EXPECT_GE(currentVersion, currentVersion);
  EXPECT_GE(currentVersion, V("99999.99999.99999.99999"));
}
