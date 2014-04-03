//===--- ReSTTest.cpp - ReST parsing tests --------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/ReST/LineList.h"
#include "swift/ReST/Parser.h"
#include "gtest/gtest.h"
#include <vector>

using namespace llvm;
using namespace rest;
using namespace llvm::rest::detail;

// In the tests below, test cases that are marked "correct" produce completely
// correct results and should not be changed without a good reason.

struct ReSTTest : public ::testing::Test {
  SourceManager<unsigned> SM;

  LineList toLineList(ReSTContext &Context, StringRef Text) {
    LineListBuilder Result;
    Result.addLine(Text, SM.registerLine(Text, 0));
    return Result.takeLineList(Context);
  }

  LineList toLineList(ReSTContext &Context, std::vector<const char *> Lines) {
    LineListBuilder Result;
    for (auto S : Lines) {
      Result.addLine(S, SM.registerLine(S, 0));
    }
    return Result.takeLineList(Context);
  }
};

TEST_F(ReSTTest, LineList_getLinePart1) {
  ReSTContext Context;
  std::vector<const char *> Text = { "abcd", "efg", "hi" };
  LineListRef LL = toLineList(Context, Text);

  EXPECT_EQ("",     LL.getLinePart(0, 0, 0).Text);
  EXPECT_EQ("a",    LL.getLinePart(0, 0, 1).Text);
  EXPECT_EQ("ab",   LL.getLinePart(0, 0, 2).Text);
  EXPECT_EQ("abc",  LL.getLinePart(0, 0, 3).Text);
  EXPECT_EQ("abcd", LL.getLinePart(0, 0, 4).Text);
  EXPECT_EQ("",     LL.getLinePart(0, 1, 0).Text);
  EXPECT_EQ("b",    LL.getLinePart(0, 1, 1).Text);
  EXPECT_EQ("bc",   LL.getLinePart(0, 1, 2).Text);
  EXPECT_EQ("bcd",  LL.getLinePart(0, 1, 3).Text);
  EXPECT_EQ("cd",   LL.getLinePart(0, 2, 2).Text);
  EXPECT_EQ("d",    LL.getLinePart(0, 3, 1).Text);
  EXPECT_EQ("",     LL.getLinePart(0, 4, 0).Text);

  EXPECT_EQ("",     LL.getLinePart(1, 0, 0).Text);
  EXPECT_EQ("e",    LL.getLinePart(1, 0, 1).Text);
  EXPECT_EQ("ef",   LL.getLinePart(1, 0, 2).Text);
  EXPECT_EQ("efg",  LL.getLinePart(1, 0, 3).Text);
  EXPECT_EQ("f",    LL.getLinePart(1, 1, 1).Text);
  EXPECT_EQ("fg",   LL.getLinePart(1, 1, 2).Text);

  EXPECT_EQ("",     LL.getLinePart(2, 0, 0).Text);
  EXPECT_EQ("h",    LL.getLinePart(2, 0, 1).Text);
  EXPECT_EQ("hi",   LL.getLinePart(2, 0, 2).Text);
}

TEST_F(ReSTTest, LineList_getLinePart2) {
  ReSTContext Context;
  std::vector<const char *> Text = { "zzz", "zabcd", "efg", "hi", "zzz" };
  LineListRef LL = toLineList(Context, Text);
  LL = LL.dropFrontLines(1);
  LL = LL.subList(0, 3);
  LL.fromFirstLineDropFront(1);

  ASSERT_EQ(3u, LL.size());

  EXPECT_EQ("zabcd", LL.getLinePart(0, 0, 5).Text);
  EXPECT_EQ("efg",   LL.getLinePart(1, 0, 3).Text);
  EXPECT_EQ("hi",    LL.getLinePart(2, 0, 2).Text);
}

struct LineListIndentationTestData {
  StringRef InText;
  unsigned FirstTextCol;
  unsigned FirstTextByte;
};

struct LineListIndentationTest
    : public ReSTTest,
      public ::testing::WithParamInterface<LineListIndentationTestData> {};

TEST_P(LineListIndentationTest, Test) {
  const auto &Test = GetParam();
  ReSTContext Context;
  auto LL = toLineList(Context, Test.InText);
  EXPECT_EQ(1u, LL.size());
  EXPECT_EQ(Test.FirstTextCol, LL[0].FirstTextCol.Value);
  EXPECT_EQ(Test.FirstTextByte, LL[0].FirstTextByte);
}

struct LineListIndentationTestData LineListIndentationTests[] = {
  { "",       0, 0 },
  { " ",      1, 1 },
  { "\v",     1, 1 },
  { "\f",     1, 1 },
  { "  ",     2, 2 },
  { "\t",     8, 1 },
  { " \t",    8, 2 },
  { "\v\t",   8, 2 },
  { "\f\t",   8, 2 },
  { " \t\t",  16, 3 },
  { " \t ",   9, 3 },
  { " \t\v",  9, 3 },
  { " \t\f",  9, 3 },
  { "\t\t",   16, 2 },
  { "aaa",    0, 0 },
  { " aaa",   1, 1 },
  { "\vaaa",  1, 1 },
  { "\faaa",  1, 1 },
  { "  aaa ", 2, 2 },
  { "\taaa",  8, 1 },
  { " \t \t aaa",  17, 5 },
  { " \t \t\vaaa", 17, 5 },
  { " \t \t\faaa", 17, 5 },
};
INSTANTIATE_TEST_CASE_P(
    ReSTTest, LineListIndentationTest,
    ::testing::ValuesIn(LineListIndentationTests));

struct ClassifyLineBlankTestData {
  StringRef InText;
};

struct ClassifyLineBlankTest
    : public ReSTTest,
      public ::testing::WithParamInterface<ClassifyLineBlankTestData> {};

TEST_P(ClassifyLineBlankTest, Test) {
  const auto &Test = GetParam();
  ReSTContext Context;
  auto LL = toLineList(Context, Test.InText);
  auto Result = classifyLine(LL[0]);
  EXPECT_EQ(LineKind::Blank, Result.Kind);
}

// REST-FIXME: clarify that trailing whitespace is not significant in ReST.
struct ClassifyLineBlankTestData ClassifyLineBlankTests[] = {
  { "" },
  { " " },
  { "\t" },
  { "\v" },
  { "\f" },
  { " \t\v\f" },
};
INSTANTIATE_TEST_CASE_P(
    ReSTTest, ClassifyLineBlankTest,
    ::testing::ValuesIn(ClassifyLineBlankTests));

struct ClassifyLineBulletListTestData {
  StringRef InText;
  LineKind Kind;
  unsigned BulletAndWhitespaceBytes;
};

struct ClassifyLineBulletListTest
    : public ReSTTest,
      public ::testing::WithParamInterface<ClassifyLineBulletListTestData> {};

TEST_P(ClassifyLineBulletListTest, Test) {
  const auto &Test = GetParam();
  ReSTContext Context;
  auto LL = toLineList(Context, Test.InText);
  auto Result = classifyLine(LL[0]);
  EXPECT_EQ(Test.Kind, Result.Kind);
  if (isBullet(Test.Kind)) {
    EXPECT_EQ(Test.BulletAndWhitespaceBytes,
              Result.getBulletAndWhitespaceBytes());
  }
}

struct ClassifyLineBulletListTestData ClassifyLineBulletListTests[] = {
  { "* a",    LineKind::BulletListAsterisk, 2 },
  { " * a",   LineKind::BulletListAsterisk, 2 },
  { "\t* a",  LineKind::BulletListAsterisk, 2 },
  { "*\ta",   LineKind::BulletListAsterisk, 2 },
  { "*\va",   LineKind::BulletListAsterisk, 2 },
  { "*\fa",   LineKind::BulletListAsterisk, 2 },
  { "*  a",   LineKind::BulletListAsterisk, 3 },
  { "* \ta",  LineKind::BulletListAsterisk, 3 },
  { "* \va",  LineKind::BulletListAsterisk, 3 },
  { "* \fa",  LineKind::BulletListAsterisk, 3 },
  { "*\t a",  LineKind::BulletListAsterisk, 3 },
  { "*\v a",  LineKind::BulletListAsterisk, 3 },
  { "*\f a",  LineKind::BulletListAsterisk, 3 },
  { "* \t a", LineKind::BulletListAsterisk, 4 },
  { "*  ",    LineKind::BulletListAsterisk, 3 },
  { "* ",     LineKind::BulletListAsterisk, 2 },
  { "*",      LineKind::BulletListAsterisk, 1 },
  { "*a",     LineKind::Unknown, 0 },
  { "*0",     LineKind::Unknown, 0 },
  { " *a",    LineKind::Unknown, 0 },
  { " *0",    LineKind::Unknown, 0 },
  // U+3000 IDEOGRAPHIC SPACE is not considered whitespace by ReST.
  { "*\xe3\x80\x80", LineKind::Unknown, 0 },
  { "+ a",   LineKind::BulletListPlus, 2 },
  { "+\ta",  LineKind::BulletListPlus, 2 },
  { "+ ",    LineKind::BulletListPlus, 2 },
  { "+",     LineKind::BulletListPlus, 1 },
  { "- a",   LineKind::BulletListHyphenMinus, 2 },
  { "-\ta",  LineKind::BulletListHyphenMinus, 2 },
  { "- ",    LineKind::BulletListHyphenMinus, 2 },
  { "-",     LineKind::BulletListHyphenMinus, 1 },
  { "\xe2\x80\xa2 a",   LineKind::BulletListBullet, 4 },
  { "\xe2\x80\xa2\ta",  LineKind::BulletListBullet, 4 },
  { "\xe2\x80\xa2 ",    LineKind::BulletListBullet, 4 },
  { "\xe2\x80\xa2",     LineKind::BulletListBullet, 3 },
  { "\xe2\x80\xa3 a",   LineKind::BulletListTriangularBullet, 4 },
  { "\xe2\x80\xa3\ta",  LineKind::BulletListTriangularBullet, 4 },
  { "\xe2\x80\xa3 ",    LineKind::BulletListTriangularBullet, 4 },
  { "\xe2\x80\xa3",     LineKind::BulletListTriangularBullet, 3 },
  { "\xe2\x81\x83 a",   LineKind::BulletListHyphenBullet, 4 },
  { "\xe2\x81\x83\ta",  LineKind::BulletListHyphenBullet, 4 },
  { "\xe2\x81\x83 ",    LineKind::BulletListHyphenBullet, 4 },
  { "\xe2\x81\x83",     LineKind::BulletListHyphenBullet, 3 },
};
INSTANTIATE_TEST_CASE_P(
    ReSTTest, ClassifyLineBulletListTest,
    ::testing::ValuesIn(ClassifyLineBulletListTests));

struct ClassifyLineEnumeratedListTestData {
  StringRef InText;
  LineKind Kind;
  unsigned EnumeratorAndWhitespaceBytes;
};

struct ClassifyLineEnumeratedListTest
    : public ReSTTest,
      public ::testing::WithParamInterface<ClassifyLineEnumeratedListTestData> {
};

TEST_P(ClassifyLineEnumeratedListTest, Test) {
  const auto &Test = GetParam();
  ReSTContext Context;
  auto LL = toLineList(Context, Test.InText);
  auto Result = classifyLine(LL[0]);
  EXPECT_EQ(Test.Kind, Result.Kind);
  if (isEnumerated(Test.Kind)) {
    EXPECT_EQ(Test.EnumeratorAndWhitespaceBytes,
              Result.getEnumeratorAndWhitespaceBytes());
  }
}

struct ClassifyLineEnumeratedListTestData ClassifyLineEnumeratedListTests[] = {
  { "#",       LineKind::Unknown, 0 },
  { "#a",      LineKind::Unknown, 0 },
  { "# ",      LineKind::Unknown, 0 },
  { "# \t",    LineKind::Unknown, 0 },
  { "# a",     LineKind::Unknown, 0 },
  { "# \ta",   LineKind::Unknown, 0 },
  { "#.",      LineKind::EnumeratedListAuto, 2 },
  { "#.a",     LineKind::Unknown, 0 },
  { "#. ",     LineKind::EnumeratedListAuto, 3 },
  { "#. a",    LineKind::EnumeratedListAuto, 3 },
  { "#. \t",   LineKind::EnumeratedListAuto, 4 },
  { "#. \ta",  LineKind::EnumeratedListAuto, 4 },
  { "#)",      LineKind::EnumeratedListAuto, 2 },
  { "#)a",     LineKind::Unknown, 0 },
  { "#) ",     LineKind::EnumeratedListAuto, 3 },
  { "#) a",    LineKind::EnumeratedListAuto, 3 },
  { "#) \t",   LineKind::EnumeratedListAuto, 4 },
  { "#) \ta",  LineKind::EnumeratedListAuto, 4 },
  { "(#",      LineKind::Unknown, 0 },
  { "(#a",     LineKind::Unknown, 0 },
  { "(# ",     LineKind::Unknown, 0 },
  { "(# a",    LineKind::Unknown, 0 },
  { "(# \t",   LineKind::Unknown, 0 },
  { "(# \ta",  LineKind::Unknown, 0 },
  { "(#)",     LineKind::EnumeratedListAuto, 3 },
  { "(#)a",    LineKind::Unknown, 0 },
  { "(#) ",    LineKind::EnumeratedListAuto, 4 },
  { "(#) a",   LineKind::EnumeratedListAuto, 4 },
  { "(#) \t",  LineKind::EnumeratedListAuto, 5 },
  { "(#) \ta", LineKind::EnumeratedListAuto, 5 },
  { ".",       LineKind::Unknown, 0 },
  { "(",       LineKind::Unknown, 0 },
  { ")",       LineKind::Unknown, 0 },

  { "1",       LineKind::Unknown, 0 },
  { "1a",      LineKind::Unknown, 0 },
  { "1 ",      LineKind::Unknown, 0 },
  { "1 a",     LineKind::Unknown, 0 },
  { "1 \t",    LineKind::Unknown, 0 },
  { "1 \ta",   LineKind::Unknown, 0 },
  { "1.",      LineKind::EnumeratedListArabic, 2 },
  { "1.a",     LineKind::Unknown, 0 },
  { "1. ",     LineKind::EnumeratedListArabic, 3 },
  { "1. a",    LineKind::EnumeratedListArabic, 3 },
  { "1. \t",   LineKind::EnumeratedListArabic, 4 },
  { "1. \ta",  LineKind::EnumeratedListArabic, 4 },
  { "1)",      LineKind::EnumeratedListArabic, 2 },
  { "1)a",     LineKind::Unknown, 0 },
  { "1) ",     LineKind::EnumeratedListArabic, 3 },
  { "1) a",    LineKind::EnumeratedListArabic, 3 },
  { "1) \t",   LineKind::EnumeratedListArabic, 4 },
  { "1) \ta",  LineKind::EnumeratedListArabic, 4 },
  { "(1",      LineKind::Unknown, 0 },
  { "(1a",     LineKind::Unknown, 0 },
  { "(1 ",     LineKind::Unknown, 0 },
  { "(1 a",    LineKind::Unknown, 0 },
  { "(1 \t",   LineKind::Unknown, 0 },
  { "(1 \ta",  LineKind::Unknown, 0 },
  { "(1)",     LineKind::EnumeratedListArabic, 3 },
  { "(1)a",    LineKind::Unknown, 0 },
  { "(1) ",    LineKind::EnumeratedListArabic, 4 },
  { "(1) a",   LineKind::EnumeratedListArabic, 4 },
  { "(1) \t",  LineKind::EnumeratedListArabic, 5 },
  { "(1) \ta", LineKind::EnumeratedListArabic, 5 },

  { "12",       LineKind::Unknown, 0 },
  { "12a",      LineKind::Unknown, 0 },
  { "12 ",      LineKind::Unknown, 0 },
  { "12 a",     LineKind::Unknown, 0 },
  { "12 \t",    LineKind::Unknown, 0 },
  { "12 \ta",   LineKind::Unknown, 0 },
  { "12.",      LineKind::EnumeratedListArabic, 3 },
  { "12.a",     LineKind::Unknown, 0 },
  { "12. ",     LineKind::EnumeratedListArabic, 4 },
  { "12. a",    LineKind::EnumeratedListArabic, 4 },
  { "12. \t",   LineKind::EnumeratedListArabic, 5 },
  { "12. \ta",  LineKind::EnumeratedListArabic, 5 },
  { "12)",      LineKind::EnumeratedListArabic, 3 },
  { "12)a",     LineKind::Unknown, 0 },
  { "12) ",     LineKind::EnumeratedListArabic, 4 },
  { "12) a",    LineKind::EnumeratedListArabic, 4 },
  { "12) \t",   LineKind::EnumeratedListArabic, 5 },
  { "12) \ta",  LineKind::EnumeratedListArabic, 5 },
  { "(12",      LineKind::Unknown, 0 },
  { "(12a",     LineKind::Unknown, 0 },
  { "(12 ",     LineKind::Unknown, 0 },
  { "(12 a",    LineKind::Unknown, 0 },
  { "(12 \t",   LineKind::Unknown, 0 },
  { "(12 \ta",  LineKind::Unknown, 0 },
  { "(12)",     LineKind::EnumeratedListArabic, 4 },
  { "(12)a",    LineKind::Unknown, 4 },
  { "(12) ",    LineKind::EnumeratedListArabic, 5 },
  { "(12) a",   LineKind::EnumeratedListArabic, 5 },
  { "(12) \t",  LineKind::EnumeratedListArabic, 6 },
  { "(12) \ta", LineKind::EnumeratedListArabic, 6 },
};
INSTANTIATE_TEST_CASE_P(
    ReSTTest, ClassifyLineEnumeratedListTest,
    ::testing::ValuesIn(ClassifyLineEnumeratedListTests));

struct ClassifyLineFieldListTestData {
  StringRef InText;
  LineKind Kind;
  unsigned FieldNameBytes;
  unsigned FieldMarkerAndWhitespaceBytes;
};

struct ClassifyLineFieldListTest
    : public ReSTTest,
      public ::testing::WithParamInterface<ClassifyLineFieldListTestData> {};

TEST_P(ClassifyLineFieldListTest, Test) {
  const auto &Test = GetParam();
  ReSTContext Context;
  auto LL = toLineList(Context, Test.InText);
  auto Result = classifyLine(LL[0]);
  EXPECT_EQ(Test.Kind, Result.Kind);
  if (Test.Kind == LineKind::FieldList) {
    EXPECT_EQ(Test.FieldNameBytes, Result.getFieldNameBytes());
    EXPECT_EQ(Test.FieldMarkerAndWhitespaceBytes,
              Result.getFieldMarkerAndWhitespaceBytes());
  }
}

struct ClassifyLineFieldListTestData ClassifyLineFieldListTests[] = {
  // Missing terminating ':'.
  { ":",             LineKind::Unknown, 0, 0 },
  { ":a",            LineKind::Unknown, 0, 0 },
  { ":foo",          LineKind::Unknown, 0, 0 },
  { ":\xe4\xbe\x8b", LineKind::Unknown, 0, 0 },

  // Field name can not be empty.
  { "::",      LineKind::Unknown, 0, 0 },
  { "::foo",   LineKind::Unknown, 0, 0 },
  { ":: foo",  LineKind::Unknown, 0, 0 },
  { "::: foo", LineKind::Unknown, 0, 0 },

  // Differentiate between interpreted text roles and field lists.
  { ":foo:``",     LineKind::Unknown,   0, 0 },
  { ":foo: ``",    LineKind::FieldList, 3, 6 },
  { ":foo:`bar`",  LineKind::Unknown,   0, 0 },
  { ":foo: `bar`", LineKind::FieldList, 3, 6 },
  { ":foo:bar",    LineKind::Unknown,   0, 0 },
  { ":foo: bar",   LineKind::FieldList, 3, 6 },

  // OK.
  { ":a:",     LineKind::FieldList, 1, 3 },
  { ": a:",    LineKind::FieldList, 2, 4 },
  { ":a :",    LineKind::FieldList, 2, 4 },
  { ": a :",   LineKind::FieldList, 3, 5 },
  { ":bb:",    LineKind::FieldList, 2, 4 },
  { ":\xe4\xbe\x8b:", LineKind::FieldList, 3, 5 },
  { ":a*b:",   LineKind::FieldList, 3, 5 },
  { ":a *b*:", LineKind::FieldList, 5, 7 },
  { ":a *b:",  LineKind::FieldList, 4, 6 },
  { ":a`b:",   LineKind::FieldList, 3, 5 },
  { ":a `b`:", LineKind::FieldList, 5, 7 },

  // Count whitespace after the field marker.
  { ":foo:",       LineKind::FieldList, 3, 5 },
  { ":foo: ",      LineKind::FieldList, 3, 6 },
  { ":foo:\t",     LineKind::FieldList, 3, 6 },
  { ":foo:\v",     LineKind::FieldList, 3, 6 },
  { ":foo:\f",     LineKind::FieldList, 3, 6 },
  { ":foo: a",     LineKind::FieldList, 3, 6 },
  { ":foo:\ta",    LineKind::FieldList, 3, 6 },
  { ":foo:\va",    LineKind::FieldList, 3, 6 },
  { ":foo:\fa",    LineKind::FieldList, 3, 6 },
  { ":foo:\t ",    LineKind::FieldList, 3, 7 },
  { ":foo: \t",    LineKind::FieldList, 3, 7 },
  { ":foo: \t a",  LineKind::FieldList, 3, 8 },

  // Escaping.
  { ":\\",                     LineKind::Unknown, 0, 0 },
  { ":\\:",                    LineKind::Unknown, 0, 0 },
  { ":\\a",                    LineKind::Unknown, 0, 0 },
  { ":\\\\",                   LineKind::Unknown, 0, 0 },
  { ":foo\\",                  LineKind::Unknown, 0, 0 },
  { ":foo\\: bar",             LineKind::Unknown, 0, 0 },
  { ":f\\oo\\: bar",           LineKind::Unknown, 0, 0 },
  { ":f\\oo\\: bar\\",         LineKind::Unknown, 0, 0 },
  { ":\\::",                   LineKind::FieldList, 2, 4 },
  { ":\\a:",                   LineKind::FieldList, 2, 4 },
  { ":\\\\:",                  LineKind::FieldList, 2, 4 },
  { ":foo\\::",                LineKind::FieldList, 5, 7 },
  { ":a\\bc\\:\\:def\\ ghi:",  LineKind::FieldList, 16, 18 },
  { ":abc\\:def: foo:bar:baz", LineKind::FieldList, 8, 11 },
  { ":\\\xe4\xbe\x8b:",        LineKind::FieldList, 4, 6 },
};
INSTANTIATE_TEST_CASE_P(
    ReSTTest, ClassifyLineFieldListTest,
    ::testing::ValuesIn(ClassifyLineFieldListTests));

struct ExtractBriefTestData {
  std::vector<const char *> InText;
  std::string Brief;
  std::string DocutilsXML;
};

struct ExtractBriefTest
    : public ReSTTest,
      public ::testing::WithParamInterface<ExtractBriefTestData> {};

TEST_P(ExtractBriefTest, Test) {
  const auto &Test = GetParam();
  ReSTContext Context;
  auto LL = toLineList(Context, Test.InText);
  llvm::SmallString<64> Str;

  extractBrief(LL, Str);
  EXPECT_EQ(Test.Brief, Str.str().str());
  Str.clear();

  auto *TheDocument = parseDocument(Context, LL);
  {
    llvm::raw_svector_ostream OS(Str);
    convertToDocutilsXML(TheDocument, OS);
  }
  StringRef DocutilsXML = Str.str();
  if (DocutilsXML.startswith("<document>"))
    DocutilsXML = DocutilsXML.drop_front(10);
  if (DocutilsXML.endswith("</document>"))
    DocutilsXML = DocutilsXML.drop_back(11);
  EXPECT_EQ(Test.DocutilsXML, DocutilsXML.str());
}

struct ExtractBriefTestData ExtractBriefTests[] = {
  { {}, "", "" }, // Correct.
  { { "" }, "", "" }, // Correct.
  { { "aaa" }, "aaa", "<paragraph>aaa</paragraph>" }, // Correct.
  { { "", "aaa" }, "aaa", "<paragraph>aaa</paragraph>" }, // Correct.
  { { "", "", "aaa" }, "aaa", "<paragraph>aaa</paragraph>" }, // Correct.
  { { "aaa", "bbb" },
    "aaa bbb",
    "<paragraph>aaa\nbbb</paragraph>" }, // Correct.
  { { "aaa", " " },
    "aaa",
    "<paragraph>aaa</paragraph>" }, // Correct.
  { { "aaa", "", "bbb" },
    "aaa",
    "<paragraph>aaa</paragraph>"
    "<paragraph>bbb</paragraph>" }, // Correct.
  { { "aaa",
      "",
      "* bbb" },
    "aaa",
    "<paragraph>aaa</paragraph>"
    "<bullet_list>"
      "<list_item><paragraph>bbb</paragraph></list_item>"
    "</bullet_list>" }, // Correct.
  { { "aaa",
      "",
      "1. bbb" },
    "aaa",
    "<paragraph>aaa</paragraph>"
    "<enumerated_list>"
      "<list_item><paragraph>bbb</paragraph></list_item>"
    "</enumerated_list>" }, // Correct.
  { { "aaa",
      "",
      "(1) bbb" },
    "aaa",
    "<paragraph>aaa</paragraph>"
    "<enumerated_list>"
      "<list_item><paragraph>bbb</paragraph></list_item>"
    "</enumerated_list>" }, // Correct.
  { { "aaa",
      "* bbb" },
    "aaa * bbb",
    "<paragraph>aaa\n* bbb</paragraph>" }, // Correct.
  { { "aaa",
      "1. bbb" },
    "aaa 1. bbb",
    "<paragraph>aaa\n1. bbb</paragraph>" }, // Correct.
  { { "aaa",
      "(1) bbb" },
    "aaa (1) bbb",
    "<paragraph>aaa\n(1) bbb</paragraph>" }, // Correct.
  { { "aaa",
      ":bbb: ccc" },
      "aaa :bbb: ccc",
    "<paragraph>aaa\n:bbb: ccc</paragraph>" }, // Correct.

  // Bullet list.
  { { "* aaa",
      "bbb" },
    "",
    // FIXME: missing diagnostic.
    "<bullet_list>"
      "<list_item><paragraph>aaa</paragraph></list_item>"
    "</bullet_list>"
    "<paragraph>bbb</paragraph>" }, // Correct.
  { { "  * aaa",
      "  bbb" },
    "",
    // FIXME: missing diagnostic.
    "<block_quote>"
      "<bullet_list>"
        "<list_item><paragraph>aaa</paragraph></list_item>"
      "</bullet_list>"
      "<paragraph>bbb</paragraph>"
    "</block_quote>" }, // Correct.
  { { "  * aaa",
      "    bbb" },
    "",
    "<block_quote>"
      "<bullet_list>"
        "<list_item><paragraph>aaa\nbbb</paragraph></list_item>"
      "</bullet_list>"
    "</block_quote>" }, // Correct.
  { { "  * aaa",
      "bbb" },
    "",
    // FIXME: missing diagnostic.
    "<block_quote>"
      "<bullet_list>"
        "<list_item><paragraph>aaa</paragraph></list_item>"
      "</bullet_list>"
    "</block_quote>"
    "<paragraph>bbb</paragraph>" }, // Correct.
  { { "  * aaa",
      "     bbb" },
    "",
    "<block_quote>"
      "<bullet_list>"
        "<list_item>"
          "<definition_list>"
            "<definition_list_item>"
              "<term>aaa</term>"
              "<definition><paragraph>bbb</paragraph></definition>"
            "</definition_list_item>"
          "</definition_list>"
        "</list_item>"
      "</bullet_list>"
    "</block_quote>" },
  { { "  * aaa",
      "     bbb",
      "     ccc" },
    "",
    "<block_quote>"
      "<bullet_list>"
        "<list_item>"
          "<definition_list>"
            "<definition_list_item>"
              "<term>aaa</term>"
              "<definition><paragraph>bbb\nccc</paragraph></definition>"
            "</definition_list_item>"
          "</definition_list>"
        "</list_item>"
      "</bullet_list>"
    "</block_quote>" },
  { { "  * aaa",
      "",
      "bbb" },
    "",
    "<block_quote>"
      "<bullet_list>"
        "<list_item><paragraph>aaa</paragraph></list_item>"
      "</bullet_list>"
    "</block_quote>"
    "<paragraph>bbb</paragraph>" }, // Correct.
  { { "  *  aaa",
      "     bbb" },
    "",
    "<block_quote>"
      "<bullet_list>"
        "<list_item><paragraph>aaa\nbbb</paragraph></list_item>"
      "</bullet_list>"
    "</block_quote>" }, // Correct.
  { { "*\taaa",
      "\tbbb" },
    "",
    "<bullet_list>"
      "<list_item><paragraph>aaa\nbbb</paragraph></list_item>"
    "</bullet_list>" }, // Correct.
  { { "*  \taaa",
      "       \tbbb",
      "\tccc" },
    "",
    "<bullet_list>"
      "<list_item><paragraph>aaa\nbbb\nccc</paragraph></list_item>"
    "</bullet_list>" }, // Correct.
  { { "* aaa",
      "* bbb" },
    "",
    "<bullet_list>"
      "<list_item><paragraph>aaa</paragraph></list_item>"
      "<list_item><paragraph>bbb</paragraph></list_item>"
    "</bullet_list>" }, // Correct.
  { { "* aaa",
      "",
      "* bbb" },
    "",
    "<bullet_list>"
      "<list_item><paragraph>aaa</paragraph></list_item>"
      "<list_item><paragraph>bbb</paragraph></list_item>"
    "</bullet_list>" }, // Correct.
  { { "* aaa",
      "",
      "",
      "* bbb" },
    "",
    "<bullet_list>"
      "<list_item><paragraph>aaa</paragraph></list_item>"
      "<list_item><paragraph>bbb</paragraph></list_item>"
    "</bullet_list>" }, // Correct.
  { { "* aaa",
      "  bbb",
      "* ccc" },
    "",
    "<bullet_list>"
      "<list_item><paragraph>aaa\nbbb</paragraph></list_item>"
      "<list_item><paragraph>ccc</paragraph></list_item>"
    "</bullet_list>" }, // Correct.
  { { "* aaa",
      "  bbb",
      "",
      "* ccc" },
    "",
    "<bullet_list>"
      "<list_item><paragraph>aaa\nbbb</paragraph></list_item>"
      "<list_item><paragraph>ccc</paragraph></list_item>"
    "</bullet_list>" }, // Correct.
  { { "* aaa",
      "  bbb",
      "",
      "",
      "* ccc" },
    "",
    "<bullet_list>"
      "<list_item><paragraph>aaa\nbbb</paragraph></list_item>"
      "<list_item><paragraph>ccc</paragraph></list_item>"
    "</bullet_list>" }, // Correct.

  // Bullet list.  Different bullets.
  { { "* aaa",
      "+ bbb" },
    "",
    // FIXME: missing diagnostic.
    "<bullet_list>"
      "<list_item><paragraph>aaa</paragraph></list_item>"
    "</bullet_list>"
    "<bullet_list>"
      "<list_item><paragraph>bbb</paragraph></list_item>"
    "</bullet_list>" }, // Correct.
  { { "* aaa",
      "- bbb" },
    "",
    // FIXME: missing diagnostic.
    "<bullet_list>"
      "<list_item><paragraph>aaa</paragraph></list_item>"
    "</bullet_list>"
    "<bullet_list>"
      "<list_item><paragraph>bbb</paragraph></list_item>"
    "</bullet_list>" }, // Correct.
  { { "* aaa",
      "\xe2\x80\xa2 bbb" },
    "",
    // FIXME: missing diagnostic.
    "<bullet_list>"
      "<list_item><paragraph>aaa</paragraph></list_item>"
    "</bullet_list>"
    "<bullet_list>"
      "<list_item><paragraph>bbb</paragraph></list_item>"
    "</bullet_list>" }, // Correct.
  { { "* aaa",
      "\xe2\x80\xa3 bbb" },
    "",
    // FIXME: missing diagnostic.
    "<bullet_list>"
      "<list_item><paragraph>aaa</paragraph></list_item>"
    "</bullet_list>"
    "<bullet_list>"
      "<list_item><paragraph>bbb</paragraph></list_item>"
    "</bullet_list>" }, // Correct.
  { { "* aaa",
      "\xe2\x81\x83 bbb" },
    "",
    // FIXME: missing diagnostic.
    "<bullet_list>"
      "<list_item><paragraph>aaa</paragraph></list_item>"
    "</bullet_list>"
    "<bullet_list>"
      "<list_item><paragraph>bbb</paragraph></list_item>"
    "</bullet_list>" }, // Correct.

  // Not parsed as enumerated lists because indentation of the second line is
  // incorrect.
  { { "1. aaa",
      "bbb" },
    "1. aaa bbb",
    "<paragraph>1. aaa\nbbb</paragraph>" }, // Correct.
  { { "(1) aaa",
      "bbb" },
    "(1) aaa bbb",
    "<paragraph>(1) aaa\nbbb</paragraph>" }, // Correct.
  { { "(1) aaa",
      "* bbb" },
    "(1) aaa * bbb",
    "<paragraph>(1) aaa\n* bbb</paragraph>" }, // Correct.
  { { "(1) aaa",
      ":bbb:" },
    "(1) aaa :bbb:",
    "<paragraph>(1) aaa\n:bbb:</paragraph>" }, // Correct.
  { { "(1) aaa",
      ":bbb: ccc" },
    "(1) aaa :bbb: ccc",
    "<paragraph>(1) aaa\n:bbb: ccc</paragraph>" }, // Correct.

  // Not parsed as an enumerated list because the second line is not a NEELEL.
  { { "1. aaa",
      "2." },
    "1. aaa 2.",
    "<paragraph>1. aaa\n2.</paragraph>" }, // Correct.

  // Enumerated list.
  { { "1. aaa" },
    "",
    "<enumerated_list>"
      "<list_item><paragraph>aaa</paragraph></list_item>"
    "</enumerated_list>" }, // Correct.
  { { "  1. aaa" },
    "",
    "<block_quote>"
      "<enumerated_list>"
        "<list_item><paragraph>aaa</paragraph></list_item>"
      "</enumerated_list>"
    "</block_quote>" }, // Correct.
  { { "1. aaa",
      "2. bbb" },
    "",
    "<enumerated_list>"
      "<list_item><paragraph>aaa</paragraph></list_item>"
      "<list_item><paragraph>bbb</paragraph></list_item>"
    "</enumerated_list>" }, // Correct.
  { { "  1. aaa",
      "  2. bbb" },
    "",
    "<block_quote>"
      "<enumerated_list>"
        "<list_item><paragraph>aaa</paragraph></list_item>"
        "<list_item><paragraph>bbb</paragraph></list_item>"
      "</enumerated_list>"
    "</block_quote>" }, // Correct.
  { { "1. aaa",
      "",
      "2. bbb" },
    "",
    "<enumerated_list>"
      "<list_item><paragraph>aaa</paragraph></list_item>"
      "<list_item><paragraph>bbb</paragraph></list_item>"
    "</enumerated_list>" }, // Correct.
  { { "  1. aaa",
      "",
      "  2. bbb" },
    "",
    "<block_quote>"
      "<enumerated_list>"
        "<list_item><paragraph>aaa</paragraph></list_item>"
        "<list_item><paragraph>bbb</paragraph></list_item>"
      "</enumerated_list>"
    "</block_quote>" }, // Correct.
  { { "1. aaa",
      "",
      "",
      "2. bbb" },
    "",
    "<enumerated_list>"
      "<list_item><paragraph>aaa</paragraph></list_item>"
      "<list_item><paragraph>bbb</paragraph></list_item>"
    "</enumerated_list>" }, // Correct.
  { { "  1. aaa",
      "",
      "",
      "  2. bbb" },
    "",
    "<block_quote>"
      "<enumerated_list>"
        "<list_item><paragraph>aaa</paragraph></list_item>"
        "<list_item><paragraph>bbb</paragraph></list_item>"
      "</enumerated_list>"
    "</block_quote>" }, // Correct.
  { { "1. aaa",
      "         ",
      "         ",
      "2. bbb" },
    "",
    "<enumerated_list>"
      "<list_item><paragraph>aaa</paragraph></list_item>"
      "<list_item><paragraph>bbb</paragraph></list_item>"
    "</enumerated_list>" }, // Correct.
  { { "1. aaa",
      "   bbb" },
    "",
    "<enumerated_list>"
      "<list_item><paragraph>aaa\nbbb</paragraph></list_item>"
    "</enumerated_list>" }, // Correct.
  { { "(1) aaa",
      "    bbb" },
    "",
    "<enumerated_list>"
      "<list_item><paragraph>aaa\nbbb</paragraph></list_item>"
    "</enumerated_list>" }, // Correct.
  { { "  1. aaa",
      "     bbb" },
    "",
    "<block_quote>"
      "<enumerated_list>"
        "<list_item><paragraph>aaa\nbbb</paragraph></list_item>"
      "</enumerated_list>"
    "</block_quote>" }, // Correct.
  { { "  1. \taaa",
      "     \tbbb",
      "   \tccc",
      "\tddd" },
    "",
    "<block_quote>"
      "<enumerated_list>"
        "<list_item><paragraph>aaa\nbbb\nccc\nddd</paragraph></list_item>"
      "</enumerated_list>"
    "</block_quote>" }, // Correct.
  { { "1. aaa",
      "    bbb" },
    "",
    "<enumerated_list>"
      "<list_item>"
        "<definition_list>"
          "<definition_list_item>"
            "<term>aaa</term>"
            "<definition><paragraph>bbb</paragraph></definition>"
          "</definition_list_item>"
        "</definition_list>"
      "</list_item>"
    "</enumerated_list>" }, // Correct.
  { { "1. aaa",
      "",
      "    bbb" },
    "",
    "<enumerated_list>"
      "<list_item>"
        "<paragraph>aaa</paragraph>"
        "<block_quote><paragraph>bbb</paragraph></block_quote>"
      "</list_item>"
    "</enumerated_list>" }, // Correct.
  { { "1. aaa",
      "    bbb",
      "    ccc" },
    "",
    "<enumerated_list>"
      "<list_item>"
        "<definition_list>"
          "<definition_list_item>"
            "<term>aaa</term>"
            "<definition><paragraph>bbb\nccc</paragraph></definition>"
          "</definition_list_item>"
        "</definition_list>"
      "</list_item>"
    "</enumerated_list>" }, // Correct.
  { { "1. aaa",
      "2. bbb" },
    "",
    "<enumerated_list>"
      "<list_item><paragraph>aaa</paragraph></list_item>"
      "<list_item><paragraph>bbb</paragraph></list_item>"
    "</enumerated_list>" }, // Correct.
  { { "1. aaa",
      "   bbb",
      "2. ccc" },
    "",
    "<enumerated_list>"
      "<list_item><paragraph>aaa\nbbb</paragraph></list_item>"
      "<list_item><paragraph>ccc</paragraph></list_item>"
    "</enumerated_list>" }, // Correct.
  { { "1. aaa",
      "   bbb",
      "",
      "2. ccc" },
    "",
    "<enumerated_list>"
      "<list_item><paragraph>aaa\nbbb</paragraph></list_item>"
      "<list_item><paragraph>ccc</paragraph></list_item>"
    "</enumerated_list>" }, // Correct.
  { { "1. aaa",
      "    bbb",
      "2. ccc" },
    "",
    "<enumerated_list>"
      "<list_item>"
        "<definition_list>"
          "<definition_list_item>"
            "<term>aaa</term>"
            "<definition><paragraph>bbb</paragraph></definition>"
          "</definition_list_item>"
        "</definition_list>"
      "</list_item>"
      "<list_item><paragraph>ccc</paragraph></list_item>"
    "</enumerated_list>" }, // Correct.
  { { "1. aaa",
      "",
      "    bbb",
      "2. ccc" },
    "",
    "<enumerated_list>"
      "<list_item>"
        "<paragraph>aaa</paragraph>"
        "<block_quote><paragraph>bbb</paragraph></block_quote>"
      "</list_item>"
      "<list_item><paragraph>ccc</paragraph></list_item>"
    "</enumerated_list>" }, // Correct.

  // Enumerated list.  Different marker styles.
  { { "1. aaa",
      "(2) bbb" },
    "",
    "<paragraph>1. aaa\n(2) bbb</paragraph>" }, // Correct.
  { { "1. aaa",
      "2) bbb" },
    "",
    "<paragraph>1. aaa\n2) bbb</paragraph>" }, // Correct.
  { { "(1) aaa",
      "2. bbb" },
    "",
    "<paragraph>(1) aaa\n2. bbb</paragraph>" }, // Correct.
  { { "(1) aaa",
      "2) bbb" },
    "",
    "<paragraph>(1) aaa\n2) bbb</paragraph>" }, // Correct.
  { { "1) aaa",
      "2. bbb" },
    "",
    "<paragraph>1) aaa\n2. bbb</paragraph>" }, // Correct.
  { { "1) aaa",
      "(2) bbb" },
    "",
    "<paragraph>1) aaa\n(2) bbb</paragraph>" }, // Correct.
  { { "1. aaa",
      "2. bbb",
      "(3) ccc" },
    "",
    "<enumerated_list>"
      "<list_item><paragraph>aaa</paragraph></list_item>"
    "</enumerated_list>"
    "<paragraph>2. bbb\n(3) ccc</paragraph>" }, // Correct.
  { { "1. aaa",
      "2. bbb",
      "3) ccc" },
    "",
    "<enumerated_list>"
      "<list_item><paragraph>aaa</paragraph></list_item>"
    "</enumerated_list>"
    "<paragraph>2. bbb\n3) ccc</paragraph>" }, // Correct.
  { { "(1) aaa",
      "(2) bbb",
      "3. ccc" },
    "",
    "<enumerated_list>"
      "<list_item><paragraph>aaa</paragraph></list_item>"
    "</enumerated_list>"
    "<paragraph>(2) bbb\n3. ccc</paragraph>" }, // Correct.
  { { "(1) aaa",
      "(2) bbb",
      "3) ccc" },
    "",
    "<enumerated_list>"
      "<list_item><paragraph>aaa</paragraph></list_item>"
    "</enumerated_list>"
    "<paragraph>(2) bbb\n3) ccc</paragraph>" }, // Correct.
  { { "1) aaa",
      "2) bbb",
      "3. ccc" },
    "",
    "<enumerated_list>"
      "<list_item><paragraph>aaa</paragraph></list_item>"
    "</enumerated_list>"
    "<paragraph>2) bbb\n3. ccc</paragraph>" }, // Correct.
  { { "1) aaa",
      "2) bbb",
      "(3) ccc" },
    "",
    "<enumerated_list>"
      "<list_item><paragraph>aaa</paragraph></list_item>"
    "</enumerated_list>"
    "<paragraph>2) bbb\n(3) ccc</paragraph>" }, // Correct.
  { { "1. aaa",
      "   bbb",
      "2) ccc" },
    "",
    // FIXME: missing diagnostic.
    "<enumerated_list>"
      "<list_item><paragraph>aaa\nbbb</paragraph></list_item>"
    "</enumerated_list>"
    "<enumerated_list>"
      "<list_item><paragraph>ccc</paragraph></list_item>"
    "</enumerated_list>" }, // Correct.

  // Nested lists.
  { { "(1) (1) aaa",
      "    (2) bbb",
      "(2) ccc" },
    "",
    "<enumerated_list>"
      "<list_item>"
        "<enumerated_list>"
          "<list_item><paragraph>aaa</paragraph></list_item>"
          "<list_item><paragraph>bbb</paragraph></list_item>"
        "</enumerated_list>"
      "</list_item>"
      "<list_item><paragraph>ccc</paragraph></list_item>"
    "</enumerated_list>" }, // Correct.

  // Field list.
  { { ":aaa:" }, "",
    "<field_list>"
      "<field>"
        "<field_name>aaa</field_name>"
        "<field_body></field_body>"
      "</field>"
    "</field_list>" }, // Correct.
  { { ":aaa:",
      " bbb" }, "",
    "<field_list>"
      "<field>"
        "<field_name>aaa</field_name>"
        "<field_body><paragraph>bbb</paragraph></field_body>"
      "</field>"
    "</field_list>" }, // Correct.
  { { ":aaa:",
      " bbb",
      " ccc" }, "",
    "<field_list>"
      "<field>"
        "<field_name>aaa</field_name>"
        "<field_body><paragraph>bbb\nccc</paragraph></field_body>"
      "</field>"
    "</field_list>" }, // Correct.
  { { ":aaa:",
      "      bbb",
      "      ccc" }, "",
    "<field_list>"
      "<field>"
        "<field_name>aaa</field_name>"
        "<field_body><paragraph>bbb\nccc</paragraph></field_body>"
      "</field>"
    "</field_list>" }, // Correct.
  { { ":aaa: bbb", }, "",
    "<field_list>"
      "<field>"
        "<field_name>aaa</field_name>"
        "<field_body><paragraph>bbb</paragraph></field_body>"
      "</field>"
    "</field_list>" }, // Correct.
  { { ":aaa: bbb",
      " ccc" }, "",
    "<field_list>"
      "<field>"
        "<field_name>aaa</field_name>"
        "<field_body><paragraph>bbb\nccc</paragraph></field_body>"
      "</field>"
    "</field_list>" }, // Correct.
  { { ":aaa: bbb",
      "  ccc",
      ":ddd: eee",
      "  fff"
    }, "",
    "<field_list>"
      "<field>"
        "<field_name>aaa</field_name>"
        "<field_body><paragraph>bbb\nccc</paragraph></field_body>"
      "</field>"
      "<field>"
        "<field_name>ddd</field_name>"
        "<field_body><paragraph>eee\nfff</paragraph></field_body>"
      "</field>"
    "</field_list>" }, // Correct.
  { { ":aaa: bbb",
      "  ccc",
      ":ddddd: eee",
      "  fff"
    }, "",
    "<field_list>"
      "<field>"
        "<field_name>aaa</field_name>"
        "<field_body><paragraph>bbb\nccc</paragraph></field_body>"
      "</field>"
      "<field>"
        "<field_name>ddddd</field_name>"
        "<field_body><paragraph>eee\nfff</paragraph></field_body>"
      "</field>"
    "</field_list>" }, // Correct.
  { { ":aaa: bbb",
      " ccc",
      " ddd" }, "",
    "<field_list>"
      "<field>"
        "<field_name>aaa</field_name>"
        "<field_body><paragraph>bbb\nccc\nddd</paragraph></field_body>"
      "</field>"
    "</field_list>" }, // Correct.
  { { ":aaa: bbb",
      "       ccc" }, "",
    // Note: this should be parsed without the nested definition list, because
    // in a field list (unlike bullet and enumerated lists), the second line
    // determines the indentation of the field body.
    "<field_list>"
      "<field>"
        "<field_name>aaa</field_name>"
        "<field_body><paragraph>bbb\nccc</paragraph></field_body>"
      "</field>"
    "</field_list>" }, // Correct.
  { { ":aaa: bbb",
      "       ccc",
      "       ddd" }, "",
    // Note: similarly to the case above, this should be parsed without the
    // nested definition list.
    "<field_list>"
      "<field>"
        "<field_name>aaa</field_name>"
        "<field_body><paragraph>bbb\nccc\nddd</paragraph></field_body>"
      "</field>"
    "</field_list>" }, // Correct.
  { { ":foo: bar",
      " * aaa",
      " * bbb" }, "",
    "<field_list>"
      "<field>"
        "<field_name>foo</field_name>"
        "<field_body>"
          "<paragraph>bar\n* aaa\n* bbb</paragraph>"
        "</field_body>"
      "</field>"
    "</field_list>" }, // Correct.
  { { ":foo: bar",
      "",
      " * aaa",
      " * bbb" }, "",
    "<field_list>"
      "<field>"
        "<field_name>foo</field_name>"
        "<field_body>"
          "<paragraph>bar</paragraph>"
          "<bullet_list>"
            "<list_item><paragraph>aaa</paragraph></list_item>"
            "<list_item><paragraph>bbb</paragraph></list_item>"
          "</bullet_list>"
        "</field_body>"
      "</field>"
    "</field_list>" }, // Correct.
  { { ":foo: bar",
      " (1) aaa",
      " (2) bbb" }, "",
    "<field_list>"
      "<field>"
        "<field_name>foo</field_name>"
        "<field_body>"
          "<paragraph>bar\n(1) aaa\n(2) bbb</paragraph>"
        "</field_body>"
      "</field>"
    "</field_list>" }, // Correct.
  { { ":foo: bar",
      "",
      " (1) aaa",
      " (2) bbb" }, "",
    "<field_list>"
      "<field>"
        "<field_name>foo</field_name>"
        "<field_body>"
          "<paragraph>bar</paragraph>"
          "<enumerated_list>"
            "<list_item><paragraph>aaa</paragraph></list_item>"
            "<list_item><paragraph>bbb</paragraph></list_item>"
          "</enumerated_list>"
        "</field_body>"
      "</field>"
    "</field_list>" }, // Correct.
  { { ":foo: bar",
      "",
      "(1) aaa",
      "(2) bbb" }, "",
    "<field_list>"
      "<field>"
        "<field_name>foo</field_name>"
        "<field_body>"
          "<paragraph>bar</paragraph>"
        "</field_body>"
      "</field>"
    "</field_list>"
    "<enumerated_list>"
      "<list_item><paragraph>aaa</paragraph></list_item>"
      "<list_item><paragraph>bbb</paragraph></list_item>"
    "</enumerated_list>"
  }, // Correct.

  // Definition lists.
  { { "aaa",
      "  bbb" },
    "",
    "<definition_list>"
      "<definition_list_item>"
        "<term>aaa</term>"
        "<definition><paragraph>bbb</paragraph></definition>"
      "</definition_list_item>"
    "</definition_list>" }, // Correct.
  { { "aaa",
      "  bbb",
      "",
      "  ccc" },
    "",
    "<definition_list>"
      "<definition_list_item>"
        "<term>aaa</term>"
        "<definition><paragraph>bbb</paragraph><paragraph>ccc</paragraph></definition>"
      "</definition_list_item>"
    "</definition_list>" }, // Correct.
  { { "aaa",
      "  bbb",
      "",
      " ccc" },
    "",
    "<definition_list>"
      "<definition_list_item>"
        "<term>aaa</term>"
        "<definition>"
          "<block_quote><paragraph>bbb</paragraph></block_quote>"
          "<paragraph>ccc</paragraph>"
        "</definition>"
      "</definition_list_item>"
    "</definition_list>" }, // Correct.
  { { "aaa",
      "      bbb", "",
      "    ccc", "",
      "  ddd", "",
      "    eee", "",
      "  fff" },
    "",
    "<definition_list>"
      "<definition_list_item>"
        "<term>aaa</term>"
        "<definition>"
          "<block_quote>"
            "<block_quote><paragraph>bbb</paragraph></block_quote>"
            "<paragraph>ccc</paragraph>"
          "</block_quote>"
          "<paragraph>ddd</paragraph>"
          "<block_quote><paragraph>eee</paragraph></block_quote>"
          "<paragraph>fff</paragraph>"
        "</definition>"
      "</definition_list_item>"
    "</definition_list>" }, // Correct.
  { { "aaa",
      "  * bbb" }, "",
    "<definition_list>"
      "<definition_list_item>"
        "<term>aaa</term>"
        "<definition>"
          "<bullet_list>"
            "<list_item><paragraph>bbb</paragraph></list_item>"
          "</bullet_list>"
        "</definition>"
      "</definition_list_item>"
    "</definition_list>" }, // Correct.

  // Definition lists with classifiers.
  // FIXME: classifiers are not recognized.
  { { "aaa : xxx",
      "  bbb" }, "",
    "<definition_list>"
      "<definition_list_item>"
        "<term>aaa : xxx</term>"
        "<definition><paragraph>bbb</paragraph></definition>"
      "</definition_list_item>"
    "</definition_list>" }, // Incorrect: classifiers.
  { { "aaa : xxx : yyy",
      "  bbb" }, "",
    // REST-FIXME: the spec states that the content model for
    // definition_list_item is
    //
    //   (term, classifier?, definition)
    //
    // which should say 'classifier*' instead.
    "<definition_list>"
      "<definition_list_item>"
        "<term>aaa : xxx : yyy</term>"
        "<definition><paragraph>bbb</paragraph></definition>"
      "</definition_list_item>"
    "</definition_list>" }, // Incorrect: classifiers.
  { { "aaa : xxx",
      "    bbb",
      "",
      "ccc : yyy",
      "  ddd" },
    "",
    "<definition_list>"
      "<definition_list_item>"
        "<term>aaa : xxx</term>"
        "<definition><paragraph>bbb</paragraph></definition>"
      "</definition_list_item>"
      "<definition_list_item>"
        "<term>ccc : yyy</term>"
        "<definition><paragraph>ddd</paragraph></definition>"
      "</definition_list_item>"
    "</definition_list>" },
  { { "aaa : xxx",
      "    bbb",
      "",
      "  ccc : yyy",
      "    ddd" },
    "",
    "<definition_list>"
      "<definition_list_item>"
        "<term>aaa : xxx</term>"
          "<definition>"
            "<block_quote><paragraph>bbb</paragraph></block_quote>"
            "<definition_list>"
              "<definition_list_item>"
                "<term>ccc : yyy</term>"
                "<definition><paragraph>ddd</paragraph></definition>"
              "</definition_list_item>"
            "</definition_list>"
          "</definition>"
      "</definition_list_item>"
    "</definition_list>" }, // Incorrect: classifiers.  Nesting is correct.

  // Definition lists with inline markup inside the term line.
  { { "``aaa`` : xxx",
      "  bbb" }, "",
    "<definition_list>"
      "<definition_list_item>"
        "<term>``aaa`` : xxx</term>"
        "<definition><paragraph>bbb</paragraph></definition>"
      "</definition_list_item>"
    "</definition_list>" }, // Incorrect: classifiers, inline markup.
  { { "aaa : ``xxx``",
      "  bbb" }, "",
    // REST-FIXME: The spec states that:
    // [ReST/Syntax Details/Body Elements/Definition Lists]
    // Quote:
    //
    //     Inline markup is parsed in the term line before the classifier
    //     delimiter (" : ") is recognized.
    //
    // But contrary to that, docutils implementation recognizes inline markup
    // everywhere in the term line.  So does LLVM ReST.
    "<definition_list>"
      "<definition_list_item>"
        "<term>aaa : ``xxx``</term>"
        "<definition><paragraph>bbb</paragraph></definition>"
      "</definition_list_item>"
    "</definition_list>" }, // Incorrect: classifiers, inline markup.
  { { "``aaa`` : ``xxx``",
      "  bbb" }, "",
    "<definition_list>"
      "<definition_list_item>"
        "<term>``aaa`` : ``xxx``</term>"
        "<definition><paragraph>bbb</paragraph></definition>"
      "</definition_list_item>"
    "</definition_list>" }, // Incorrect: classifiers, inline markup.
  { { "``aaa`` : ``xxx`` : **yyy**",
      "  bbb" }, "",
    "<definition_list>"
      "<definition_list_item>"
        "<term>``aaa`` : ``xxx`` : **yyy**</term>"
        "<definition><paragraph>bbb</paragraph></definition>"
      "</definition_list_item>"
    "</definition_list>" }, // Incorrect: classifiers, inline markup.
  { { "``aaa : xxx``",
      "  bbb" }, "",
    // Classifier delimiter inside inline markup is not recognized.
    "<definition_list>"
      "<definition_list_item>"
        "<term>``aaa : xxx``</term>"
        "<definition><paragraph>bbb</paragraph></definition>"
      "</definition_list_item>"
    "</definition_list>" }, // Incorrect: inline markup.

  // Block quotes.
  { { "  aaa",
      "",
      "bbb" }, "aaa",
    "<block_quote>"
      "<paragraph>aaa</paragraph>"
    "</block_quote>"
    "<paragraph>bbb</paragraph>" }, // Correct.
  { { "    aaa",
      "",
      "  bbb",
      "",
      "ccc" }, "aaa",
    "<block_quote>"
      "<block_quote>"
        "<paragraph>aaa</paragraph>"
      "</block_quote>"
      "<paragraph>bbb</paragraph>"
    "</block_quote>"
    "<paragraph>ccc</paragraph>" }, // Correct.
  { { "    aaa",
      " ",
      "    bbb" }, "aaa",
    "<block_quote>"
      "<paragraph>aaa</paragraph>"
      "<paragraph>bbb</paragraph>"
    "</block_quote>" }, // Correct.
  { { "    aaa",
      "",
      "  bbb" }, "aaa",
    "<block_quote>"
      "<block_quote>"
        "<paragraph>aaa</paragraph>"
      "</block_quote>"
      "<paragraph>bbb</paragraph>"
    "</block_quote>" }, // Correct.
  { { "  aaa",
      "",
      "    bbb" }, "aaa",
    "<block_quote>"
      "<paragraph>aaa</paragraph>"
      "<block_quote>"
        "<paragraph>bbb</paragraph>"
      "</block_quote>"
    "</block_quote>" }, // Correct.
  { { "  aaa",
      "  bbb" }, "aaa bbb",
    "<block_quote>"
      "<paragraph>aaa\nbbb</paragraph>"
    "</block_quote>" },

  // Unexpected indentation.
  { { "aaa",
      "bbb",
      "  ccc" }, "aaa bbb",
    "<paragraph>aaa\nbbb</paragraph>"
    "<block_quote>"
      "<paragraph>ccc</paragraph>"
    "</block_quote>" }, // Correct.
  { { "aaa",
      "bbb",
      "  * ccc" }, "aaa bbb",
    "<paragraph>aaa\nbbb</paragraph>"
    "<block_quote>"
      "<bullet_list>"
        "<list_item><paragraph>ccc</paragraph></list_item>"
      "</bullet_list>"
    "</block_quote>" }, // Correct.
  { { "aaa",
      "bbb",
      "  1. ccc" }, "aaa bbb",
    "<paragraph>aaa\nbbb</paragraph>"
    "<block_quote>"
      "<enumerated_list>"
        "<list_item><paragraph>ccc</paragraph></list_item>"
      "</enumerated_list>"
    "</block_quote>" }, // Correct.
  { { "aaa",
      "bbb",
      "  (1) ccc" }, "aaa bbb",
    "<paragraph>aaa\nbbb</paragraph>"
    "<block_quote>"
      "<enumerated_list>"
        "<list_item><paragraph>ccc</paragraph></list_item>"
      "</enumerated_list>"
    "</block_quote>" }, // Correct.

  // FIXME: removing inline markup.
  { { "*aaa*" }, "*aaa*", "<paragraph>*aaa*</paragraph>" },
  { { "**aaa**" }, "**aaa**", "<paragraph>**aaa**</paragraph>" },
  { { "`aaa`" }, "`aaa`", "<paragraph>`aaa`</paragraph>" },
  { { "``aaa``" }, "``aaa``", "<paragraph>``aaa``</paragraph>" },

  // FIXME: substitution references sholud be substituted.
  { { "|aaa|" }, "|aaa|", "<paragraph>|aaa|</paragraph>" },
  { { "|aaa|_" }, "|aaa|_", "<paragraph>|aaa|_</paragraph>" },
  { { "|aaa|__" }, "|aaa|__", "<paragraph>|aaa|__</paragraph>" },

  // FIXME: removing inline markup.
  { { "_`aaa`" }, "_`aaa`", "<paragraph>_`aaa`</paragraph>" },
  { { "[1]_" }, "[1]_", "<paragraph>[1]_</paragraph>" },
  { { "[12]_" }, "[12]_", "<paragraph>[12]_</paragraph>" },
  { { "[#]_" }, "[#]_", "<paragraph>[#]_</paragraph>" },
  { { "[#aaa]_" }, "[#aaa]_", "<paragraph>[#aaa]_</paragraph>" },
  { { "[*]_" }, "[*]_", "<paragraph>[*]_</paragraph>" },
  { { "[aaa]_" }, "[aaa]_", "<paragraph>[aaa]_</paragraph>" },
  { { "aaa_" }, "aaa_", "<paragraph>aaa_</paragraph>" },
  { { "`aaa`_" }, "`aaa`_", "<paragraph>`aaa`_</paragraph>" },
  { { "aaa__" }, "aaa__", "<paragraph>aaa__</paragraph>" },
  { { "`aaa`__" }, "`aaa`__", "<paragraph>`aaa`__</paragraph>" },
  { { "`aaa <http://example.org/>`_" },
    "`aaa <http://example.org/>`_",
    "<paragraph>`aaa <http://example.org/>`_</paragraph>"},
  { { "`aaa <foo.txt\\_>`__" },
    "`aaa <foo.txt\\_>`__",
    "<paragraph>`aaa <foo.txt\\_>`__</paragraph>" },
};
INSTANTIATE_TEST_CASE_P(
    ReSTTest, ExtractBriefTest,
    ::testing::ValuesIn(ExtractBriefTests));

struct ParseReSTTestData {
  std::vector<const char *> InText;
  std::string DocutilsXML;
};

struct ParseReSTTest
    : public ReSTTest,
      public ::testing::WithParamInterface<ExtractBriefTestData> {};

TEST_P(ParseReSTTest, Test) {
  const auto &Test = GetParam();
  ReSTContext Context;
  auto LL = toLineList(Context, Test.InText);
  llvm::SmallString<64> Str;
  extractBrief(LL, Str);
  EXPECT_EQ(Test.Brief, Str.str());
}

struct ParseReSTTestData ParseReSTTests[] = {
};

// Tests for bullet lists:
//
// "* aaa"
// "+ bbb"
// error: bullet list (*) ends without a blank line
//
// "* aaa"
// " * bbb"
// ok: [bullet list "aaa"], [block quote [bullet list "bbb"]]
//
// "* aaa"
// "  * bbb"
// ok: [bullet list "aaa * bbb"]
//
// "* aaa"
// "   * bbb"
// ok: [bullet list (text "aaa", block quote with [list "bbb"])]
//
// "aaa"
// "* bbb"
// ok: plain text
//
// "* aaa"
// ""
// "*   bbb"
// ""
// " * ccc"
// ok: bullet list ("aaa", "bbb"), followed by a block quote with [list "ccc"]
//
// "* aaa"
// ""
// "  * ccc"
// ok: bullet list ("aaa", [bullet list "bbb"])

// Bullet lists without text immediately after the bullet:
//
// "*"
// "*"
// "*"
// ok: bullet list with three empty items
//
// "* "
// "aaa"
// warning: unexpected unindent
//
// "* "
// " aaa"
// ok: bullet list item with text "aaa"
// note: the text is on the *next* column after the bullet.
//
// "* "
// "      aaa"
// ok: bullet list item with text "aaa"
//
// "* "
// "      aaa"
// "      bbb"
// ok: bullet list item with text "aaa bbb"
//
// "* "
// "      aaa"
// "       bbb"
// ok: bullet list item with (text "aaa" + block quote "bbb")
//
// "*"
// " aaa"
// ok: bullet list item with text "aaa"
// REST-FIXME: arguably, this is a bug ether in docutils, or in the spec.
// According to [ReST/Syntax Details/Body Elements/Bullet Lists], the bullet
// character should be immediately followed by whitespace.  In order to avoid
// requiring trailing whitespace to make empty list items, it makes sense to
// relax the rule here.
//
// "*"
// ""
// "aaa"
// ok: bullet list with one empty item, paragraph with text "aaa"
//
// "* "
// ""
// "    bbb"
// docutils: bullet list item with text "bbb"
// REST-FIXME: the standard does not say anything in specifically about this,
// but it does look weird, and there might be an ambiguity with block quotes.
// Compare the example above to:
// "* aaa"
// ""
// "    bbb"
// Here, "bbb" is clearly a block quote nested in a list item.
// Also, [ReST/Syntax Details/Whitespace/Blank Lines] says: "Blank lines are
// used to separate paragraphs and other elements."
//
// LLVM-REST-DIFFERENCE: For now, LLVM ReST will not use a strict reading of
// the standard and will recognize a list item if the bullet is followed by a
// newline.  This allows one to strip trailing whitespace without affecting
// semantics.
//
// LLVM-REST-DIFFERENCE: "with no blank lines in between" part in the text
// below.
//
// REST-FIXME: If the line with the bullet does not have text, the text is
// allowed to start on the next line (with no blank lines in between), however,
// it should be indented relative to the bullet.
//
// "*"
//
// REST-FIXME: clarify the spec: if the line with the bullet does not have
// text, and it is followed by an empty line, then it is an empty bullet item.

// Tests for enumerated lists:
//
// "aaa"
// "(1) bbb"
// ok: plain text
//
// "1. aaa"
// ""
// "1. aaa"
// ok: two lists
//
// "1. aaa"
// ""
// "2. aaa"
// ok: one list
//
// "1. aaa"
// ""
// "3. aaa"
// ok: two lists
// warning: list starts with non-ordinal-1 enumerator
//
// "2. aaa"
// ""
// "3. aaa"
// ok: one list
// warning: list starts with non-ordinal-1 enumerator
//
// "1. aaa"
// ""
// "2) aaa"
// ok: two lists
// warning: list starts with non-ordinal-1 enumerator
//
// "1."
// "2."
// "3."
// ok: plain text
// REST-FIXME: this is inconsistent with bullet lists.  If it was a bullet
// list, then it would be parsed as three list items.
//
// "(1)"
// "(2) a"
// ok: list with two items
//
// "(1)"
// "(2) a"
// "(3)"
// ok: list with one empty item "(1)", and a plain text paragraph +
// warning about list ending without a blank line.
// REST-FIXME: when compared with previous example, this behavior is
// surprising.
//
// "1. a"
// ""
// "2."
// "3. c"
// ok: list with three items.
//
// REST-FIXME: it looks like an empty list item in an enumerated list is
// allowed if it is preceeded by an empty line.
// REST-FIXME: this is inconsistent with bullet lists.
//
// "(Ii) aaa"
// ok: plain text
//
// "(iI) aaa"
// ok: plain text
//
// "(v) aaa"
// ok: list that starts with 22
// warning: list starts with non-ordinal-1 enumerator
//
// "(iv) a"
// "(v) a"
// ok: list that starts with 4
// warning: list starts with non-ordinal-1 enumerator
//
// "(v) a"
// "(vi) a"
// ok: plain text
// REST-FIXME: this should at least emit a warning.
//
// "(v) a"
// "(#) a"
// ok: list that starts with 22
// warning: list starts with non-ordinal-1 enumerator
//
// "(v) a"
// "23. a"
// ok: plain text
// REST-FIXME: this should at least emit a warning.
//
// "(v) a"
// ""
// "(vi) a"
// ok: two lists
// x2 warning: list starts with non-ordinal-1 enumerator
//
// "(v) a"
// "a"
// ok: plain text
//
// "(v) a"
// " a"
// ok: list + paragraph
// warning: list ends without a blank line
//
// "(1) a"
// "(2) b"
// "c"
// ok: list "a" + paragraph "(2) b c"
// warning: list ends without a blank line
//
// "(1) a"
// "(3) b"
// "c"
// ok: plain text
// REST-FIXME: this should at least emit a warning.
//
//
// "1. a"
// "2."
// "   aaa"
// "3. b"
// ok: paragraph "1. a 2.", paragraph "aaa", list with one item "b"
// warning: unexpected indentation (about "aaa")
// warning: unexpected unindent, block quote ends without a blank line
//
// REST-FIXME: clarify exactly how the check on the next line is performed.  It
// looks like the exact rules are as follows.
//
// def: A NEELEL (non-empty enumerated list item line) is a line that has an
// enumerator, followed by whitespace, followed by non-empty text.
//
// a. if the next line is blank, then current line is a list item.
//
// b. if the next line is on the same indentation level as the enumerator:
// b.1. if the next line is a NEELEL, and enumeration sequence continues
// without a gap (or the next line uses the auto-enumerator '#'), and the
// enumeration sequence style and formatting match exactly, then *current line*
// is a list item. (The check on the next line is performed separately.)
//
// b.2. if the next line is a NEELEL, but other conditions to continue the list
// are not met, then current line is the start of a paragraph.
// REST-FIXME: in this case the implementation should emit a warning.
//
// b.3. if the next line is not a NEELEL, then current line is the start of a
// paragraph.
//
// c. if the next line has the same or greater amount of indentation as the
// text after the enumerator, then the current line is a list item, text inside
// the list item is subject to normal rules.
//
// d. if the next line has more indentation than the enumerator, but less than
// the item text, then the current line is a list item, and the list ends
// there.  The next line is subject to normal rules (will be parsed as a block
// quote).  This case requires a diagnostic.  Docutils uses the wording
// "Enumerated list ends without a blank line".
//
// e. if the next line has less indentation than the enumerator, then the
// current line is a list item, and the list ends there.  The next line is subject
// to normal rules, and should be matched with one of the containing blocks.
//
// Note: in the rules above it is essential that NEELEL has non-empty text,
// because if it was empty, it would fail the previous-line check, and would
// not be considered a list item.
//
// REST-FIXME: if the current line looks like an enumerated list item, but it
// does not have text after the enumerator, then it needs to pass checks both
// for previous and next lines.
//
// "0. a"
// "1. b"
// docutils/LLVM-REST: a list with two items.
// REST-FIXME: the spec seems to disallow this implicitly, by presenting the
// arabic numeral sequence as "arabic numerals: 1, 2, 3, ... (no upper limit)."
// Probably this should be disallowed, but to simplify the implementation, we
// allow it until the spec is clarified.
//
// "-1. a"
// ok: plain text

// Tests for field lists:
//
// :foo: bar
// :*foo*: bar
// :**foo**: bar
// :`foo`: bar
// :``foo``: bar
// ok: as expected, inline markup is inside the field name
//
// :*foo: bar*: baz
// :**foo: bar**: baz
// :`foo: bar`: baz
// :``foo: bar``: baz
// error: inline markup start without end in field name
// Field name only spans until "foo:".
//
// :*foo\: bar*: baz
// :**foo\: bar**: baz
// :`foo\: bar`: baz
// ok: field name is "foo: bar" text with inline markup
//
// :``foo\: bar``: baz
// ok: field name is "foo\: bar" text with inline markup
// REST-FIXME: How do we express a field name with a colon inside ``...``, but
// without getting a backslash in the output?
//
// :foo\: bar
// ok: not a bullet list, text, literally ":foo: bar"
//
// :foo\
// ok: text ":foo"
// Make sure we don't crash on this, trying to access the escaped character
// after the end of the line.
//
// "aaa"
// ":foo: bbb"
// ok: plain text

// Tests for block quotes:
//
// "*   aaa"
// ""
// "  bbb"
// docutils: [list "aaa"] [blockquote [paragraph "bbb"]]
//
// ":aaa:    bbb"
// "         ccc"
// ""
// "       ddd"
// docutils: [field list ["aaa", [definition list <does not make any sense>]] [paragraph "ddd"]],
//
// Notice the inconsistency above.
// REST-FIXME: both should be blockquotes, or blockquotes at the beginning of
// every element should be disallowed.

// Misc points:
// [ReST/Syntax Details/Body Elements/Field Lists]
// Quote:
//   Field names are case-insensitive when further processed or transformed.
//
// REST-FIXME: clarify what exactly this means for Unicode.  A reasonable thing
// would be to say that the above point only applies if the field name is
// ASCII-only.

