#include "gtest/gtest.h"
#include "llvm/ADT/Optional.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/Version.h"

using namespace swift;
using namespace llvm;

class CompilerVersionTest : public ::testing::Test {};
class VersionTest : public ::testing::Test{};
class CompilerVersionUnpackingTest : public ::testing::Test {};

Optional<version::Version> CV(const char *VersionString) {
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
  EXPECT_GE(CV("700").getValue(), CV("602").getValue());
  EXPECT_GE(CV("700.*").getValue(), CV("700.*").getValue());
  EXPECT_GE(CV("700.*.1").getValue(), CV("700.*.0").getValue());
  EXPECT_GE(CV("700.*.23").getValue(), CV("700.*.21").getValue());
  EXPECT_GE(CV("700.*.1.1.0").getValue(), CV("700.*.1.1").getValue());
  EXPECT_GE(currentVersion, currentVersion);
  EXPECT_GE(currentVersion, CV("9223371.*.999.999.999").getValue());
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

TEST_F(CompilerVersionUnpackingTest, VersionComparison) {
  EXPECT_EQ(CV("700").getValue(), V("0.700").getValue());
  EXPECT_EQ(CV("700.*").getValue(), V("0.700").getValue());
  EXPECT_EQ(CV("700.*.1").getValue(), V("0.700.1").getValue());
  EXPECT_EQ(CV("700.*.23").getValue(), V("0.700.23").getValue());
  EXPECT_EQ(CV("700.*.1.1").getValue(), V("0.700.1.1").getValue());

  EXPECT_EQ(CV("1300").getValue(), V("1.300").getValue());
  EXPECT_EQ(CV("1300.*").getValue(), V("1.300").getValue());
  EXPECT_EQ(CV("1300.*.1").getValue(), V("1.300.1").getValue());
  EXPECT_EQ(CV("1300.*.23").getValue(), V("1.300.23").getValue());
  EXPECT_EQ(CV("1300.*.1.1").getValue(), V("1.300.1.1").getValue());

  EXPECT_EQ(CV("5007").getValue(), V("5.7").getValue());
  EXPECT_EQ(CV("5007.*").getValue(), V("5.7").getValue());
  EXPECT_EQ(CV("5007.*.1").getValue(), V("5.7.1").getValue());
  EXPECT_EQ(CV("5007.*.23").getValue(), V("5.7.23").getValue());
  EXPECT_EQ(CV("5007.*.1.1").getValue(), V("5.7.1.1").getValue());

  // Since this test was added during 5.7, we expect all of these comparisons to
  // be GE, either because we are comparing to the empty version or because we
  // are comparing to a version >= 5.7.0.0.0.
  auto currentVersion = version::Version::getCurrentCompilerVersion();
  EXPECT_GE(CV("700"), currentVersion);
  EXPECT_GE(CV("1300"), currentVersion);
  EXPECT_GE(CV("5007"), currentVersion);
}
