#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/Version.h"
#include "swift/Parse/ParseVersion.h"
#include "gtest/gtest.h"
#include <optional>

using namespace swift;
using namespace llvm;

class CompilerVersionTest : public ::testing::Test {};
class VersionTest : public ::testing::Test{};
class CompilerVersionUnpackingTest : public ::testing::Test {};

std::optional<version::Version> CV(const char *VersionString) {
  return VersionParser::parseCompilerVersionString(VersionString, SourceLoc(),
                                                   nullptr);
}

std::optional<version::Version> V(const char *VersionString) {
  return VersionParser::parseVersionString(VersionString, SourceLoc(), nullptr);
}

TEST_F(CompilerVersionTest, VersionComparison) {
  auto currentVersion = version::getCurrentCompilerVersion();
  EXPECT_GE(CV("700").value(), CV("602").value());
  EXPECT_GE(CV("700.*").value(), CV("700.*").value());
  EXPECT_GE(CV("700.*.1").value(), CV("700.*.0").value());
  EXPECT_GE(CV("700.*.23").value(), CV("700.*.21").value());
  EXPECT_GE(CV("700.*.1.1.0").value(), CV("700.*.1.1").value());
  EXPECT_GE(currentVersion, currentVersion);
  EXPECT_GE(currentVersion, CV("9223371.*.999.999.999").value());
}

TEST_F(VersionTest, VersionComparison) {
  auto currentVersion = version::Version::getCurrentLanguageVersion();
  EXPECT_GE(V("3").value(), V("2").value());
  EXPECT_GE(V("2.0").value(), V("2.0").value());
  EXPECT_GE(V("2.1").value(), V("2.0").value());
  EXPECT_GE(V("3.1").value(), V("3.0.1").value());
  EXPECT_GE(V("2.0").value(), V("2").value());
  EXPECT_GE(currentVersion, currentVersion);
  EXPECT_GE(currentVersion, V("1.0").value());
  EXPECT_GE(currentVersion, V("2").value());
  EXPECT_FALSE(V("2.n").has_value());
  EXPECT_FALSE(V("").has_value());
  EXPECT_FALSE(V("\"2.0\"").has_value());
  EXPECT_FALSE(V("2..").has_value());
  EXPECT_FALSE(V(".").has_value());
  EXPECT_FALSE(V("..").has_value());
  EXPECT_TRUE(V("1.").has_value());
  EXPECT_FALSE(V(".1").has_value());

}

TEST_F(CompilerVersionUnpackingTest, VersionComparison) {
  EXPECT_EQ(CV("700").value(), V("0.700").value());
  EXPECT_EQ(CV("700.*").value(), V("0.700").value());
  EXPECT_EQ(CV("700.*.1").value(), V("0.700.1").value());
  EXPECT_EQ(CV("700.*.23").value(), V("0.700.23").value());
  EXPECT_EQ(CV("700.*.1.1").value(), V("0.700.1.1").value());

  EXPECT_EQ(CV("1300").value(), V("1.300").value());
  EXPECT_EQ(CV("1300.*").value(), V("1.300").value());
  EXPECT_EQ(CV("1300.*.1").value(), V("1.300.1").value());
  EXPECT_EQ(CV("1300.*.23").value(), V("1.300.23").value());
  EXPECT_EQ(CV("1300.*.1.1").value(), V("1.300.1.1").value());

  EXPECT_EQ(CV("5007").value(), V("5.7").value());
  EXPECT_EQ(CV("5007.*").value(), V("5.7").value());
  EXPECT_EQ(CV("5007.*.1").value(), V("5.7.1").value());
  EXPECT_EQ(CV("5007.*.23").value(), V("5.7.23").value());
  EXPECT_EQ(CV("5007.*.1.1").value(), V("5.7.1.1").value());

  // Since this test was added during 5.7, we expect all of these comparisons to
  // be GE, either because we are comparing to the empty version or because we
  // are comparing to a version >= 5.7.0.0.0.
  auto currentVersion = version::getCurrentCompilerVersion();
  EXPECT_GE(CV("700"), currentVersion);
  EXPECT_GE(CV("1300"), currentVersion);
  EXPECT_GE(CV("5007"), currentVersion);
}
