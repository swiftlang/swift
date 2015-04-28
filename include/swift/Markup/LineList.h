//===--- LineList.h - Data structures for Markup parsing ------------------===//
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

#ifndef LLVM_MARKUP_LINELIST_H
#define LLVM_MARKUP_LINELIST_H

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include "swift/Basic/SourceLoc.h"

namespace llvm {
namespace markup {

class MarkupContext;

/// Returns the amount of indentation on the line and
/// the length of the line's length.
size_t measureIndentation(StringRef Text);

/// Represents a substring of a single line of source text.
struct Line {
  StringRef Text;
  swift::SourceRange Range;
  size_t FirstNonspaceOffset;
public:
  Line(StringRef Text, swift::SourceRange Range) : Text(Text), Range(Range),
      FirstNonspaceOffset(measureIndentation(Text)) {}

  void drop_front(size_t Amount) {
    Text = Text.drop_front(Amount);
  }
};

/// A possibly non-contiguous view into a source buffer.
class LineList {
  MutableArrayRef<Line> Lines;
public:
  LineList(MutableArrayRef<Line> Lines) : Lines(Lines) {}
  LineList() = default;

  std::string str() const;

  ArrayRef<Line> getLines() const {
    return Lines;
  }

  /// Creates a LineList from a box selection of text.
  ///
  /// \param MC MarkupContext used for allocations
  /// \param StartLine 0-based start line
  /// \param EndLine 0-based end line (1 off the end)
  /// \param StartColumn 0-based start column
  /// \param EndColumn 0-based end column (1 off the end)
  /// \returns a new LineList with the selected start and end lines/columns.
  LineList subListWithRange(MarkupContext &MC, size_t StartLine, size_t EndLine,
                            size_t StartColumn, size_t EndColumn) const;
};

class LineListBuilder {
  std::vector<Line> Lines;
  MarkupContext &Context;
public:
  LineListBuilder(MarkupContext &Context) : Context(Context) {}

  void addLine(StringRef Text, swift::SourceRange Range);
  LineList takeLineList() const;
};

} // namespace markup
} // namespace llvm

#endif // LLVM_MARKUP_LINELIST_H
