#include "gtest/gtest.h"
#include "llvm/ADT/Optional.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/Version.h"

using namespace swift;
using namespace llvm;

class CompilerVersionTest : public ::testing::Test {};
class VersionTest : public ::testing::Test{};

version::Version CV(const char *VersionString) {
  return version::Version::parseCompilerVersionString(VersionString,
                                                      SourceLoc(),
                                                      nullptr);
}

Optional<version::Version> V(const char *VersionString) {
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
  auto currentVersion = version::Version::getCurrentLanguageVersion();
  EXPECT_GE(V("3").getValue(), V("2").getValue());
  EXPECT_GE(V("2.0").getValue(), V("2.0").getValue());
  EXPECT_GE(V("2.1").getValue(), V("2.0").getValue());
  EXPECT_GE(V("3.1").getValue(), V("3.0.1").getValue());
  EXPECT_GE(V("2.0").getValue(), V("2").getValue());
  EXPECT_GE(currentVersion, currentVersion);
  EXPECT_GE(currentVersion, V("1.0").getValue());
  EXPECT_GE(currentVersion, V("2").getValue());
  EXPECT_FALSE(V("2.n").hasValue());
  EXPECT_FALSE(V("").hasValue());
  EXPECT_FALSE(V("\"2.0\"").hasValue());
  EXPECT_FALSE(V("2..").hasValue());
  EXPECT_FALSE(V(".").hasValue());
  EXPECT_FALSE(V("..").hasValue());
  EXPECT_TRUE(V("1.").hasValue());
  EXPECT_FALSE(V(".1").hasValue());

}
