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
#include "Detail.h"
#include "swift/ReST/Parser.h"
#include "llvm/ADT/ArrayRef.h"
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

void LineListBuilder::addLine(StringRef Text, SourceRange Range) {
  if (Context.LangOpts.TemporaryHacks) {
    auto Trimmed = Text.trim();
    if (!Trimmed.empty() && (std::all_of(Trimmed.begin(), Trimmed.end(),
                                         [](char C) { return C == '='; }) ||
                             std::all_of(Trimmed.begin(), Trimmed.end(),
                                         [](char C) { return C == '-'; })))
      return;
  }
  Line L;
  L.Text = Text;
  L.Range = Range;
  unsigned FirstTextByte;
  std::tie(L.FirstTextCol, FirstTextByte) = measureIndentation(Text);
  L.FirstTextByte = FirstTextByte;
  Lines.push_back(L);
}

LineList LineListBuilder::takeLineList() {
  return LineList(Context.allocateCopy(ArrayRef<Line>(Lines)));
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

std::pair<unsigned, unsigned>
LineListRefIndex::decodeUnicodeScalarNonASCII(StringRef S) {
  assert(!S.empty());

  const UTF8 *SourceStart = reinterpret_cast<const UTF8 *>(S.data());

  const UTF8 *SourceNext = SourceStart;
  UTF32 C;
  UTF32 *TargetStart = &C;

  ConvertUTF8toUTF32(&SourceNext, SourceStart + S.size(), &TargetStart,
                     TargetStart + 1, lenientConversion);
  assert(TargetStart == &C + 1);

  return std::make_pair(C, unsigned(SourceNext - SourceStart));
}

std::pair<Optional<unsigned>, LineListRefIndex::IterationState>
LineListRefIndex::getPossiblyEscapedUnicodeScalarSlow(const LineListRef &LL,
                                                      IterationState State) {
  while (true) {
    if (isEndImpl(LL, State))
      return std::make_pair(None, State);

    auto EscapedScalarAndLength = decodeUnicodeScalar(LL, State);
    State = advanceState(LL, State, EscapedScalarAndLength.second);
    if (isReSTWhitespace(EscapedScalarAndLength.first)) {
      // Escaped whitespace is ignored.
      continue;
    }
    return std::make_pair(EscapedScalarAndLength.first, State);
  }
}

