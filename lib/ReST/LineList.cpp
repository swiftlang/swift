//===--- LineList.h - Data structures for ReST parsing --------------------===//
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
#include "llvm/Support/Unicode.h"
#include "llvm/Support/ConvertUTF.h"
#include <utility>

using namespace llvm;
using namespace rest;
using namespace llvm::rest::detail;

void Line::computeClassification() const {
  Classification = classifyLine(*this);
}

static std::pair<ColumnNum, unsigned> measureIndentation(StringRef Text) {
  ColumnNum Col;
  for (size_t i = 0, e = Text.size(); i != e; ++i) {
    if (Text[i] == ' ' || Text[i] == '\v' || Text[i] == '\f') {
      Col++;
      continue;
    }
    if (Text[i] == '\t') {
      Col = Col.nextTabStop();
      continue;
    }
    return { Col, i };
  }
  return { Col, Text.size() };
}

void LineList::addLine(StringRef Text, SourceRange Range) {
  Line L;
  L.Text = Text;
  L.Range = Range;
  unsigned FirstTextByte;
  std::tie(L.FirstTextCol, FirstTextByte) = measureIndentation(Text);
  L.FirstTextByte = FirstTextByte;
  Lines.push_back(L);
}

// FIXME: this could work a little faster if we could call LLVM's
// charWidth().
static ColumnNum measureColumnWidth(StringRef Text) {
  ColumnNum Col;
  unsigned Length;
  for (size_t i = 0, e = Text.size(); i < e; i += Length) {
    if (Text[i] == ' ' || Text[i] == '\v' || Text[i] == '\f') {
      Col++;
      Length = 1;
      continue;
    }
    if (Text[i] == '\t') {
      Col = Col.nextTabStop();
      Length = 1;
      continue;
    }

    Length = getNumBytesForUTF8(Text[i]);
    if (Length <= 0 || i + Length > Text.size())
      return {};
    StringRef CodePoint = StringRef(Text.data() + i, Length);

    int Width = llvm::sys::unicode::columnWidthUTF8(CodePoint);
    if (Width < 0)
      return {};
    Col += Width;
  }
  return Col;
}

void LineListRef::fromFirstLineDropFront(unsigned Bytes) {
  Line OrigFirstLine = (*this)[0];
  FirstLine = Line();
  FirstLine->Text = OrigFirstLine.Text;
  FirstLine->Range = OrigFirstLine.Range;

  unsigned BytesToDrop = OrigFirstLine.FirstTextByte + Bytes;
  FirstLine->FirstTextCol =
      measureColumnWidth(OrigFirstLine.Text.substr(0, BytesToDrop));
  FirstLine->FirstTextByte = BytesToDrop;
}

