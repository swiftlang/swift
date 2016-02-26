//===--- FormatContext.cpp --------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "FormatContext.h"

size_t swift::ide::getOffsetOfLine(unsigned LineIndex, StringRef Text) {
  //  SourceLoc start = SourceLoc(llvm::SMLoc::getFromPointer(Text.begin()));
  // FIXME: We should have a cached line map in EditableTextBuffer, for now
  // we just do the slow naive thing here.
  size_t LineOffset = 0;
  unsigned CurrentLine = 0;
  while (LineOffset < Text.size() && ++CurrentLine < LineIndex) {
    LineOffset = Text.find_first_of("\r\n", LineOffset);
    if (LineOffset != std::string::npos) {
      ++LineOffset;
      if (LineOffset < Text.size() &&
          Text[LineOffset - 1] == '\r' && Text[LineOffset] == '\n')
        ++LineOffset;
    }

  }
  if (LineOffset == std::string::npos)
    LineOffset = 0;
  return LineOffset;
}

size_t swift::ide::getOffsetOfTrimmedLine(unsigned LineIndex, StringRef Text) {
  size_t LineOffset = swift::ide::getOffsetOfLine(LineIndex, Text);

  // Skip leading whitespace.
  size_t FirstNonWSOnLine = Text.find_first_not_of(" \t\v\f", LineOffset);
  if (FirstNonWSOnLine != std::string::npos)
    LineOffset = FirstNonWSOnLine;

  return LineOffset;
}
