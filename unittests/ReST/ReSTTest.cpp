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

struct ReSTTest : public ::testing::Test {
  SourceManager<unsigned> SM;

  LineList toLineList(StringRef Text) {
    LineList Result;
    Result.addLine(Text, SM.registerLine(Text, 0));
    return Result;
  }

  LineList toLineList(std::vector<const char *> Lines) {
    LineList Result;
    for (auto S : Lines) {
      Result.addLine(S, SM.registerLine(S, 0));
    }
    return Result;
  }
};

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
  auto LL = toLineList(Test.InText);
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
  auto LL = toLineList(Test.InText);
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
  auto LL = toLineList(Test.InText);
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
  auto LL = toLineList(Test.InText);
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
  unsigned FieldNameSecondColonByte;
};

struct ClassifyLineFieldListTest
    : public ReSTTest,
      public ::testing::WithParamInterface<ClassifyLineFieldListTestData> {};

TEST_P(ClassifyLineFieldListTest, Test) {
  const auto &Test = GetParam();
  auto LL = toLineList(Test.InText);
  auto Result = classifyLine(LL[0]);
  EXPECT_EQ(Test.Kind, Result.Kind);
  if (Test.Kind == LineKind::FieldList) {
    EXPECT_EQ(Test.FieldNameSecondColonByte,
              Result.getFieldNameSecondColonByte());
  }
}

struct ClassifyLineFieldListTestData ClassifyLineFieldListTests[] = {
  // Missing terminating ':'.
  { ":",       LineKind::Unknown, 0 },
  { ":a",      LineKind::Unknown, 0 },
  { ":foo",    LineKind::Unknown, 0 },
  { ":\xe4\xbe\x8b", LineKind::Unknown, 0 },

  // Field name can not be empty.
  { "::",      LineKind::Unknown, 0 },
  { "::foo",   LineKind::Unknown, 0 },
  { ":: foo",  LineKind::Unknown, 0 },
  { "::: foo", LineKind::Unknown, 0 },

  // OK.
  { ":a:",     LineKind::FieldList, 2 },
  { ": a:",    LineKind::FieldList, 3 },
  { ":a :",    LineKind::FieldList, 3 },
  { ": a :",   LineKind::FieldList, 4 },
  { ":bb:",    LineKind::FieldList, 3 },
  { ":foo:",   LineKind::FieldList, 4 },
  { ":\xe4\xbe\x8b:", LineKind::FieldList, 4 },
  { ":a*b:",   LineKind::FieldList, 4 },
  { ":a *b*:", LineKind::FieldList, 6 },
  { ":a *b:",  LineKind::FieldList, 5 },
  { ":a`b:",   LineKind::FieldList, 4 },
  { ":a `b`:", LineKind::FieldList, 6 },

  // Escaping.
  { ":\\",                     LineKind::Unknown, 0 },
  { ":\\:",                    LineKind::Unknown, 0 },
  { ":\\a",                    LineKind::Unknown, 0 },
  { ":\\\\",                   LineKind::Unknown, 0 },
  { ":foo\\",                  LineKind::Unknown, 0 },
  { ":foo\\: bar",             LineKind::Unknown, 0 },
  { ":f\\oo\\: bar",           LineKind::Unknown, 0 },
  { ":f\\oo\\: bar\\",         LineKind::Unknown, 0 },
  { ":\\::",                   LineKind::FieldList, 3 },
  { ":\\a:",                   LineKind::FieldList, 3 },
  { ":\\\\:",                  LineKind::FieldList, 3 },
  { ":foo\\::",                LineKind::FieldList, 6 },
  { ":a\\bc\\:\\:def\\ ghi:",  LineKind::FieldList, 17 },
  { ":abc\\:def: foo:bar:baz", LineKind::FieldList, 9 },
  { ":\\\xe4\xbe\x8b:",        LineKind::FieldList, 5 },
};
INSTANTIATE_TEST_CASE_P(
    ReSTTest, ClassifyLineFieldListTest,
    ::testing::ValuesIn(ClassifyLineFieldListTests));

struct ExtractBriefTestData {
  std::vector<const char *> InText;
  std::string Brief;
};

struct ExtractBriefTest
    : public ReSTTest,
      public ::testing::WithParamInterface<ExtractBriefTestData> {};

TEST_P(ExtractBriefTest, Test) {
  const auto &Test = GetParam();
  auto LL = toLineList(Test.InText);
  llvm::SmallString<64> Str;
  extractBrief(LL, Str);
  EXPECT_EQ(Test.Brief, Str.str());
}

struct ExtractBriefTestData ExtractBriefTests[] = {
  { {}, "" },
  { { "" }, "" },
  { { "aaa" }, "aaa" },
  { { "", "aaa" }, "aaa" },
  { { "", "", "aaa" }, "aaa" },
  { { "aaa", "bbb" }, "aaa bbb" },
  { { "aaa", " " }, "aaa" },
  { { "    aaa", " ", "    bbb" }, "aaa" },
  { { "aaa", "", "bbb" }, "aaa" },
  { { "aaa", "", "* bbb" }, "aaa" },
  { { "aaa", "", "1. bbb" }, "aaa" },
  { { "aaa", "", "(1) bbb" }, "aaa" },
  { { "  aaa", "  bbb" }, "aaa bbb" },
  { { "aaa", "* bbb" }, "aaa * bbb" },
  { { "aaa", "1. bbb" }, "aaa 1. bbb" },
  { { "aaa", "(1) bbb" }, "aaa (1) bbb" },
  { { "aaa", ":bbb: ccc" }, "aaa :bbb: ccc" },

  // Parsed an a bulleted list (at least the first line).
  { { "* aaa", "bbb" }, "" },
  { { "  * aaa", "  bbb" }, "" },
  { { "  * aaa", "    bbb" }, "" },
  { { "  * aaa", "bbb" }, "" },

  // Not parsed as enumerated lists because indentation is incorrect.
  { { "1. aaa", "bbb" }, "1. aaa bbb" },
  { { "(1) aaa", "bbb" }, "(1) aaa bbb" },

  // Parsed as enumerated lists.
  { { "1. aaa", "   bbb" }, "" },
  { { "(1) aaa", "    bbb" }, "" },

  // Parsed as field list.
  { { ":aaa: bbb", " ccc" }, "" },

  // Parsed as a definition list.
  { { "aaa", "  bbb" }, "" },
  { { "aaa", "  * bbb" }, "" },

  // Unexpected indentation.
  { { "aaa", "bbb", "  ccc" }, "aaa bbb" },
  { { "aaa", "bbb", "  * ccc" }, "aaa bbb" },
  { { "aaa", "bbb", "  1. ccc" }, "aaa bbb" },
  { { "aaa", "bbb", "  (1) ccc" }, "aaa bbb" },

  // FIXME: removing inline markup.
  { { "*aaa*" }, "*aaa*" },
  { { "**aaa**" }, "**aaa**" },
  { { "`aaa`" }, "`aaa`" },
  { { "``aaa``" }, "``aaa``" },

  // FIXME: substitution references sholud be substituted.
  { { "|aaa|" }, "|aaa|" },
  { { "|aaa|_" }, "|aaa|_" },
  { { "|aaa|__" }, "|aaa|__" },

  // FIXME: removing inline markup.
  { { "_`aaa`" }, "_`aaa`" },
  { { "[1]_" }, "[1]_" },
  { { "[12]_" }, "[12]_" },
  { { "[#]_" }, "[#]_" },
  { { "[#aaa]_" }, "[#aaa]_" },
  { { "[*]_" }, "[*]_" },
  { { "[aaa]_" }, "[aaa]_" },
  { { "aaa_" }, "aaa_" },
  { { "`aaa`_" }, "`aaa`_" },
  { { "aaa__" }, "aaa__" },
  { { "`aaa`__" }, "`aaa`__" },
  { { "`aaa <http://example.org/>`_" }, "`aaa <http://example.org/>`_" },
  { { "`aaa <foo.txt\\_>`__" }, "`aaa <foo.txt\\_>`__" },
};
INSTANTIATE_TEST_CASE_P(
    ReSTTest, ExtractBriefTest,
    ::testing::ValuesIn(ExtractBriefTests));

// Tests for bulleted lists:
//
// "* aaa"
// "+ bbb"
// error: bulleted list (*) ends without a blank line
//
// "* aaa"
// "  * bbb"
// ok: bulleted list with text "aaa * bbb"
//
// "* aaa"
// "   * bbb"
// ok: bulleted list item with (text "aaa" + block quote "* bbb"
//
// "aaa"
// "* bbb"
// ok: plain text

// Bulleted lists without text immediately after the bullet:
//
// "*"
// "*"
// "*"
// ok: bulleted list with three empty items
//
// "* "
// "aaa"
// warning: unexpected unindent
//
// "* "
// " aaa"
// ok: bulleted list item with text "aaa"
// note: the text is on the *next* column after the bullet.
//
// "* "
// "      aaa"
// ok: bulleted list item with text "aaa"
//
// "* "
// "      aaa"
// "      bbb"
// ok: bulleted list item with text "aaa bbb"
//
// "* "
// "      aaa"
// "       bbb"
// ok: bulleted list item with (text "aaa" + block quote "bbb")
//
// "*"
// " aaa"
// ok: bulleted list item with text "aaa"
// REST-FIXME: arguably, this is a bug ether in docutils, or in the spec.
// According to [ReST/Syntax Details/Body Elements/Bullet Lists], the bullet
// character should be immediately followed by whitespace.  In order to avoid
// requiring trailing whitespace to make empty list items, it makes sense to
// relax the rule here.
//
// "*"
// ""
// "aaa"
// ok: bulleted list with one empty item, paragraph with text "aaa"
//
// "* "
// ""
// "    bbb"
// docutils: bulleted list item with text "bbb"
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
// "(1) (1) a"
// "    (2) b"
// "(2) c"
// ok: nested lists
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
// ok: not a bulleted list, text, literally ":foo: bar"
//
// :foo\
// ok: text ":foo"
// Make sure we don't crash on this, trying to access the escaped character
// after the end of the line.
//
// "aaa"
// ":foo: bbb"
// ok: plain text

// Complex tests:
//
// ":foo: bar"
// " (1) a"
// " (2) b"
// ok: field list with plain text
//
// ":foo: bar"
// ""
// " (1) a"
// " (2) b"
// ok: field list with text "bar" + nested list
