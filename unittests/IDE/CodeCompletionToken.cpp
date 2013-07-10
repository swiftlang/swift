#include "swift/IDE/CodeCompletion.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace code_completion;

TEST(CodeCompletionToken, FindInEmptyFile) {
  std::string Source = "";
  unsigned Offset;
  std::string Clean = removeCodeCompletionTokens(Source, "A", &Offset);
  EXPECT_EQ(~0U, Offset);
  EXPECT_EQ("", Clean);
}

TEST(CodeCompletionToken, FindNonExistent) {
  std::string Source = "func zzz() {}";
  unsigned Offset;
  std::string Clean = removeCodeCompletionTokens(Source, "A", &Offset);
  EXPECT_EQ(~0U, Offset);
  EXPECT_EQ(Source, Clean);
}

TEST(CodeCompletionToken, RemovesOtherTokens) {
  std::string Source = "func zzz() {#^B^#}";
  unsigned Offset;
  std::string Clean = removeCodeCompletionTokens(Source, "A", &Offset);
  EXPECT_EQ(~0U, Offset);
  EXPECT_EQ("func zzz() {}", Clean);
}

TEST(CodeCompletionToken, FindBegin) {
  std::string Source = "#^A^# func";
  unsigned Offset;
  std::string Clean = removeCodeCompletionTokens(Source, "A", &Offset);
  EXPECT_EQ(1U, Offset);
  EXPECT_EQ(" func", Clean);
}

TEST(CodeCompletionToken, FindEnd) {
  std::string Source = "func #^A^#";
  unsigned Offset;
  std::string Clean = removeCodeCompletionTokens(Source, "A", &Offset);
  EXPECT_EQ(6U, Offset);
  EXPECT_EQ("func ", Clean);
}

TEST(CodeCompletionToken, FindSingleLine) {
  std::string Source = "func zzz() {#^A^#}";
  unsigned Offset;
  std::string Clean = removeCodeCompletionTokens(Source, "A", &Offset);
  EXPECT_EQ(13U, Offset);
  EXPECT_EQ("func zzz() {}", Clean);
}

TEST(CodeCompletionToken, FindMultiline) {
  std::string Source =
      "func zzz() {\n"
      "  1 + #^A^#\r\n"
      "}\n";
  unsigned Offset;
  std::string Clean = removeCodeCompletionTokens(Source, "A", &Offset);
  EXPECT_EQ(20U, Offset);
  EXPECT_EQ("func zzz() {\n  1 + \r\n}\n", Clean);
}

