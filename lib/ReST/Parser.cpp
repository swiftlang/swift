//===--- Parser.cpp - ReST parser -----------------------------------------===//
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

#include "swift/ReST/Parser.h"
#include "Detail.h"
#include "swift/ReST/LineList.h"
#include "llvm/Support/ErrorHandling.h"
#include "clang/Basic/CharInfo.h"

using namespace llvm;
using namespace rest;
using namespace llvm::rest::detail;

using namespace clang;

namespace {
struct ParsedEnumerator {
  LineKind Kind;
  unsigned EnumeratorBytes;
  unsigned Value;
};
} // unnamed namespace

/// Returns true on success.
static bool tryParseEnumerator(StringRef Text, ParsedEnumerator &PE) {
  if (Text.empty()) {
    return false;
  }

  if (Text[0] == '#') {
    PE = { LineKind::EnumeratedListAuto, 1, 0 };
    return true;
  }

  if (isDigit(Text[0])) {
    unsigned EnumeratorBytes = 1;
    for (unsigned e = Text.size(); EnumeratorBytes != e; ++EnumeratorBytes) {
      if (!isDigit(Text[EnumeratorBytes]))
        break;
    }
    unsigned Value;
    if (Text.substr(0, EnumeratorBytes).getAsInteger(10, Value)) {
      // FIXME: we should produce a diagnostic if there was an overflow.
      return false;
    }
    PE = { LineKind::EnumeratedListArabic, EnumeratorBytes, Value };
    return true;
  }

  // FIXME: implement other enumerator kinds.

  return false;
}

/// Returns true on success.
static bool tryParseAfterEnumerator(StringRef Text, bool &HasText,
                                    unsigned &WhitespaceBytes) {
  if (Text.empty()) {
    HasText = false;
    WhitespaceBytes = 0;
    return true;
  }

  if (!isReSTWhitespace(Text.front()))
    return false;

  for (unsigned i = 1, e = Text.size(); i != e; ++i) {
    if (!isReSTWhitespace(Text[i])) {
      HasText = true;
      WhitespaceBytes = i;
      return true;
    }
  }
  HasText = false;
  WhitespaceBytes = Text.size();
  return true;
}

static LineClassification tryParseEnumeratorWithFormatting(StringRef Text) {
  // [ReST/Syntax Details/Body Elements/Enumerated Lists]
  // Quote:
  //    The following formatting types are recognized:
  //
  //    * suffixed with a period: "1.", "A.", "a.", "I.", "i.".
  //    * surrounded by parentheses: "(1)", "(A)", "(a)", "(I)", "(i)".
  //    * suffixed with a right-parenthesis: "1)", "A)", "a)", "I)", "i)".
  ParsedEnumerator PE;
  if (Text.startswith("(")) {
    if (!tryParseEnumerator(Text.drop_front(1), PE))
      return LineClassification::makeUnknown();

    StringRef WithoutEnumeratorValue = Text.drop_front(1 + PE.EnumeratorBytes);
    if (!WithoutEnumeratorValue.startswith(")"))
      return LineClassification::makeUnknown();

    StringRef WithoutEnumerator = WithoutEnumeratorValue.drop_front(1);
    bool HasText;
    unsigned WhitespaceBytes;
    if (tryParseAfterEnumerator(WithoutEnumerator, HasText, WhitespaceBytes))
      return LineClassification::makeEnumerated(
          PE.Kind, EnumeratorStyleKind::SurroundedByParens, HasText,
          1 + PE.EnumeratorBytes + 1 + WhitespaceBytes);

    return LineClassification::makeUnknown();
  }
  if (tryParseEnumerator(Text, PE)) {
    StringRef WithoutEnumeratorValue = Text.drop_front(PE.EnumeratorBytes);
    bool IsDotAfter = WithoutEnumeratorValue.startswith(".");
    bool IsParenAfter = WithoutEnumeratorValue.startswith(")");
    if (!IsDotAfter && !IsParenAfter)
      return LineClassification::makeUnknown();

    StringRef WithoutEnumerator = WithoutEnumeratorValue.drop_front(1);
    bool HasText;
    unsigned WhitespaceBytes;
    if (tryParseAfterEnumerator(WithoutEnumerator, HasText, WhitespaceBytes))
      return LineClassification::makeEnumerated(
          PE.Kind, IsDotAfter ? EnumeratorStyleKind::DotAfter
                              : EnumeratorStyleKind::ParenAfter,
          HasText, PE.EnumeratorBytes + 1 + WhitespaceBytes);

    return LineClassification::makeUnknown();
  }
  return LineClassification::makeUnknown();
}

llvm::rest::detail::LineClassification
llvm::rest::detail::classifyLine(const Line &L) {
  StringRef Text = L.Text.drop_front(L.FirstTextByte);

  if (Text.empty())
    return LineClassification::makeBlank();

  // [ReST/Syntax Details/Body Elements/Field Lists]
  // Quote:
  //    A field name may consist of any characters, but colons (":") inside of
  //    field names must be escaped with a backslash.  Inline markup is parsed
  //    in field names.
  //
  // The initial check is very lightweight here (just look if there is a
  // colon at the beginning), so handle this case first.
  if (Text.startswith(":") && Text.size() >= 3 && Text[1] != ':') {
    // This might be a field name.  This is a field list if the line contains a
    // colon that is not escaped, and the field name is not empty.
    // REST-FIXME: clarify that the field name can not be empty.
    unsigned i = 1;
    if (Text[i] == '\\') {
      // Skip the next character, it is escaped.
      i += 2;
    }
    for (unsigned e = Text.size(); i != e; ++i) {
      if (Text[i] == ':')
        return LineClassification::makeFieldList(L.FirstTextByte + i);
      if (Text[i] == '\\') {
        // Skip the next character, it is escaped.
        ++i;
        if (i == e)
          break;
      }
    }
  }

  // [ReST/Syntax Details/Body Elements/Bullet Lists]
  // ReST allows the following characters to start a bulleted list:
  // U+002A ASTERISK
  // U+002B PLUS SIGN
  // U+002D HYPHEN-MINUS
  // U+2022 BULLET
  // U+2023 TRIANGULAR BULLET
  // U+2043 HYPHEN BULLET
  //
  // Note: the following code tries to avoid using heavy machinery to decode
  // UTF-8.
  LineKind Kind = LineKind::Unknown;
  unsigned BulletBytes = 0;
  if (Text.startswith("*")) {
    Kind = LineKind::BulletListAsterisk;
    BulletBytes = 1;
  } else if (Text.startswith("+")) {
    Kind = LineKind::BulletListPlus;
    BulletBytes = 1;
  } else if (Text.startswith("-")) {
    Kind = LineKind::BulletListHyphenMinus;
    BulletBytes = 1;
  } else if (Text.startswith("\u2022")) {
    Kind = LineKind::BulletListBullet;
    BulletBytes = 3;
  } else if (Text.startswith("\u2023")) {
    Kind = LineKind::BulletListTriangularBullet;
    BulletBytes = 3;
  } else if (Text.startswith("\u2043")) {
    Kind = LineKind::BulletListHyphenBullet;
    BulletBytes = 3;
  }
  if (Kind != LineKind::Unknown) {
    // We have a bullet.  This is the initial line of a bullet list if the
    // bullet is at the end of the line or is followed by whitespace.
    StringRef WithoutBullet = Text.drop_front(BulletBytes);
    if (WithoutBullet.empty())
      return LineClassification::makeBullet(Kind, BulletBytes);
    if (isReSTWhitespace(WithoutBullet.front())) {
      // Currently, all ReST whitespace has a single-byte encoding in UTF-8.
      return LineClassification::makeBullet(Kind, BulletBytes + 1);
    }

    Kind = LineKind::Unknown;
  }

  {
    LineClassification MaybeEnumerator = tryParseEnumeratorWithFormatting(Text);
    if (MaybeEnumerator.Kind != LineKind::Unknown)
      return MaybeEnumerator;
  }

  // [ReST/Syntax Details/Body Elements/Option Lists]
  // FIXME: implement later.

  return LineClassification::makeUnknown();
}

