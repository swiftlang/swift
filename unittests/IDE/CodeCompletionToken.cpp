#include "swift/IDE/CodeCompletion.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace ide;

static std::string replaceAtWithNull(const std::string &S) {
  std::string Result = S;
  for (char &C : Result) {
    if (C == '@')
      C = '\0';
  }
  return Result;
}

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
  EXPECT_EQ(0U, Offset);
  EXPECT_EQ(replaceAtWithNull("@ func"), Clean);
}

TEST(CodeCompletionToken, FindEnd) {
  std::string Source = "func #^A^#";
  unsigned Offset;
  std::string Clean = removeCodeCompletionTokens(Source, "A", &Offset);
  EXPECT_EQ(5U, Offset);
  EXPECT_EQ(replaceAtWithNull("func @"), Clean);
}

TEST(CodeCompletionToken, FindSingleLine) {
  std::string Source = "func zzz() {#^A^#}";
  unsigned Offset;
  std::string Clean = removeCodeCompletionTokens(Source, "A", &Offset);
  EXPECT_EQ(12U, Offset);
  EXPECT_EQ(replaceAtWithNull("func zzz() {@}"), Clean);
}

TEST(CodeCompletionToken, FindMultiline) {
  std::string Source =
      "func zzz() {\n"
      "  1 + #^A^#\r\n"
      "}\n";
  unsigned Offset;
  std::string Clean = removeCodeCompletionTokens(Source, "A", &Offset);
  EXPECT_EQ(19U, Offset);
  EXPECT_EQ(replaceAtWithNull("func zzz() {\n  1 + @\r\n}\n"), Clean);
}

