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

