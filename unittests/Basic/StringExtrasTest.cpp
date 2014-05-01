#include "swift/Basic/StringExtras.h"
#include "llvm/ADT/SmallString.h"
#include "gtest/gtest.h"
#include <algorithm>

using namespace swift;

TEST(CamelCaseWordsTest, Iteration) {
  auto words = camel_case::getWords("URLByPrependingHTTPToURL");

  // Forward iteration count.
  EXPECT_EQ(6, std::distance(words.begin(), words.end()));

  // Reverse iteration count.
  EXPECT_EQ(6, std::distance(words.rbegin(), words.rend()));

  // Iteration contents.
  auto iter = words.begin();
  EXPECT_EQ(*iter, "URL");
  
  // Stepping forward.
  ++iter;
  EXPECT_EQ(*iter, "By");  

  // Immediately stepping back (fast path).
  --iter;
  EXPECT_EQ(*iter, "URL");  

  // Immediately stepping forward (fast path).
  ++iter;
  EXPECT_EQ(*iter, "By");

  // Stepping forward.
  ++iter;
  EXPECT_EQ(*iter, "Prepending");  

  // Stepping back twice (slow path).
  --iter;
  --iter;
  EXPECT_EQ(*iter, "URL");

  // Stepping forward to visit the remaining elements.
  ++iter;
  EXPECT_EQ(*iter, "By");
  ++iter;
  EXPECT_EQ(*iter, "Prepending");
  ++iter;
  EXPECT_EQ(*iter, "HTTP");
  ++iter;
  EXPECT_EQ(*iter, "To");
  ++iter;
  EXPECT_EQ(*iter, "URL");

  // We're done.
  ++iter;
  EXPECT_EQ(iter, words.end());
}

TEST(ToLowercaseTest, Words) {
  llvm::SmallString<64> scratch;

  EXPECT_EQ(camel_case::toLowercaseWord("By", scratch), "by");
  EXPECT_EQ(camel_case::toLowercaseWord("and", scratch), "and");
  EXPECT_EQ(camel_case::toLowercaseWord("A", scratch), "a");
  EXPECT_EQ(camel_case::toLowercaseWord("URL", scratch), "URL");
  EXPECT_EQ(camel_case::toLowercaseWord("", scratch), "");
}

TEST(ToSentencecaseTest, Words) {
  llvm::SmallString<64> scratch;

  EXPECT_EQ(camel_case::toSentencecase("by", scratch), "By");
  EXPECT_EQ(camel_case::toSentencecase("a", scratch), "A");
  EXPECT_EQ(camel_case::toSentencecase("URL", scratch), "URL");
  EXPECT_EQ(camel_case::toSentencecase("", scratch), "");
}

