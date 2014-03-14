//===--- BriefParser.cpp - Extraction of brief comments -------------------===//
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

using namespace llvm;
using namespace rest;
using namespace llvm::rest::detail;

static LineListRef extractParagraph(LineListRef LL) {
  assert(LL.size() != 0);
  assert(LL[0].getClassification().Kind == LineKind::Unknown ||
         isEnumerated(LL[0].getClassification().Kind));
  ColumnNum Indentation = LL[0].FirstTextCol;
  unsigned i = 0;
  for (unsigned e = LL.size(); i != e; ++i) {
    if (LL[i].getClassification().Kind != LineKind::Blank) {
      if (LL[i].FirstTextCol > Indentation) {
        // Indent.
        if (i == 1) {
          // This is a definition list.
          return LL.subList(0, 0);
        }
        // Unexpected indent.  Paragraph ends here, the next line starts a new
        // block.
        break;
      } else if (LL[i].FirstTextCol < Indentation) {
        // Unindent.  Paragraph ends here, the next line should match up with
        // something else we parsed previously.
        break;
      }
    }

    switch (LL[i].getClassification().Kind) {
    case LineKind::Unknown:
      continue;

    case LineKind::Blank:
      // Paragraph ends at a blank line.
      return LL.subList(0, i);

    case LineKind::BulletListAsterisk:
    case LineKind::BulletListPlus:
    case LineKind::BulletListHyphenMinus:
    case LineKind::BulletListBullet:
    case LineKind::BulletListTriangularBullet:
    case LineKind::BulletListHyphenBullet:
      assert(!LL.isPreviousLineBlank(i));
      continue;

    case LineKind::EnumeratedListArabic:
    case LineKind::EnumeratedListUppercaseAlphabet:
    case LineKind::EnumeratedListLowercaseAlphabet:
    case LineKind::EnumeratedListUppercaseRoman:
    case LineKind::EnumeratedListLowercaseRoman:
    case LineKind::EnumeratedListUppercaseAmbiguous:
    case LineKind::EnumeratedListLowercaseAmbiguous:
    case LineKind::EnumeratedListAuto:
      assert(i == 0 || !LL.isPreviousLineBlank(i));
      continue;

    case LineKind::FieldList:
      assert(!LL.isPreviousLineBlank(i));
      continue;
    }
  }
  return LL.subList(0, i);
}

static void extractText(LineListRef LL, llvm::SmallVectorImpl<char> &Str) {
  // FIXME: filter out inline markup.
  Str.clear();
  for (unsigned i = 0, e = LL.size(); i != e; ++i) {
    StringRef Text = LL[i].Text.drop_front(LL[i].FirstTextByte);
    Str.append(Text.begin(), Text.end());
    if (i != e - 1)
      Str.push_back(' ');
  }
}

void llvm::rest::extractBrief(LineListRef LL,
                              llvm::SmallVectorImpl<char> &Str) {
  Str.clear();
  for (unsigned i = 0, e = LL.size(); i != e; ++i) {
    switch (LL[i].getClassification().Kind) {
    case LineKind::Unknown:
      extractText(extractParagraph(LL.subList(i, LL.size() - i)), Str);
      return;

    case LineKind::Blank:
      // Skip blank lines at the beginning.
      continue;

    case LineKind::BulletListAsterisk:
    case LineKind::BulletListPlus:
    case LineKind::BulletListHyphenMinus:
    case LineKind::BulletListBullet:
    case LineKind::BulletListTriangularBullet:
    case LineKind::BulletListHyphenBullet:
      // If the line looks like a bullet list item, it is always a bullet list
      // item, no further checks required.
      return;

    case LineKind::EnumeratedListArabic:
    case LineKind::EnumeratedListUppercaseAlphabet:
    case LineKind::EnumeratedListLowercaseAlphabet:
    case LineKind::EnumeratedListUppercaseRoman:
    case LineKind::EnumeratedListLowercaseRoman:
    case LineKind::EnumeratedListUppercaseAmbiguous:
    case LineKind::EnumeratedListLowercaseAmbiguous:
    case LineKind::EnumeratedListAuto:
      if (LL.isNextLineBlank(i)) {
        // It is a list item.
        return;
      }
      if (LL[i + 1].FirstTextCol <
          LL[i].FirstTextCol +
              LL[i].getClassification().getEnumeratorAndWhitespaceCols()) {
        // Next line does not have enough indentation, so this line is not a
        // list item.
        extractText(extractParagraph(LL.subList(i, LL.size() - i)), Str);
        return;
      }
      // This is a list item.
      return;

    case LineKind::FieldList:
      return;
    }
  }
  return;
}

