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

#include "swift/AST/RawComment.h"
#include "swift/Basic/PrimitiveParsing.h"
#include "swift/Markup/LineList.h"
#include "swift/Markup/Markup.h"

using namespace llvm;
using namespace markup;

std::string LineList::str() const {
  std::string Result;
  raw_string_ostream Stream(Result);
  for (const auto Line : Lines) {
    Stream << Line.Text << "\n";
  }
  Stream.flush();
  return Result;
}

void LineListBuilder::addLine(llvm::StringRef Text, swift::SourceRange Range) {
  Lines.push_back({Text, Range});
}

LineList LineListBuilder::takeLineList() const {
  return LineList(Context.allocateCopy(ArrayRef<Line>(Lines)));
}

static unsigned measureASCIIArt(StringRef S, unsigned NumLeadingSpaces) {
  StringRef Spaces = S.substr(0, NumLeadingSpaces);
  if (Spaces.size() != NumLeadingSpaces)
    return 0;
  if (Spaces.find_first_not_of(' ') != StringRef::npos)
    return 0;

  S = S.drop_front(NumLeadingSpaces);

  if (S.startswith(" * "))
    return NumLeadingSpaces + 3;
  if (S.startswith(" *\n") || S.startswith(" *\n\r"))
    return NumLeadingSpaces + 2;
  return 0;
}

LineList MarkupContext::getLineList(swift::RawComment RC) {
  LineListBuilder Builder(*this);

  for (const auto &C : RC.Comments) {
    if (C.isLine()) {
      // Skip comment marker.
      unsigned CommentMarkerBytes = 2 + (C.isOrdinary() ? 0 : 1);
      StringRef Cleaned = C.RawText.drop_front(CommentMarkerBytes);

      // Drop trailing newline.
      Cleaned = Cleaned.rtrim("\n\r");
      auto CleanedStartLoc =
          C.Range.getStart().getAdvancedLocOrInvalid(CommentMarkerBytes);
      auto CleanedEndLoc =
          C.Range.getStart().getAdvancedLocOrInvalid(Cleaned.size());
      Builder.addLine(Cleaned, { CleanedStartLoc, CleanedEndLoc });
    } else {
      // Skip comment markers at the beginning and at the end.
      unsigned CommentMarkerBytes = 2 + (C.isOrdinary() ? 0 : 1);
      StringRef Cleaned = C.RawText.drop_front(CommentMarkerBytes).drop_back(2);
      swift::SourceLoc CleanedStartLoc =
          C.Range.getStart().getAdvancedLocOrInvalid(CommentMarkerBytes);

      // Determine if we have leading decorations in this block comment.
      bool HasASCIIArt = false;
      if (swift::startsWithNewline(Cleaned)) {
        Builder.addLine(Cleaned.substr(0, 0), { C.Range.getStart(),
                                                C.Range.getStart() });
        unsigned NewlineBytes = swift::measureNewline(Cleaned);
        Cleaned = Cleaned.drop_front(NewlineBytes);
        CleanedStartLoc = CleanedStartLoc.getAdvancedLocOrInvalid(NewlineBytes);
        HasASCIIArt = measureASCIIArt(Cleaned, C.StartColumn - 1) != 0;
      }

      while (!Cleaned.empty()) {
        size_t Pos = Cleaned.find_first_of("\n\r");
        if (Pos == StringRef::npos)
          Pos = Cleaned.size();

        // Skip over ASCII art, if present.
        if (HasASCIIArt)
          if (unsigned ASCIIArtBytes =
              measureASCIIArt(Cleaned, C.StartColumn - 1)) {
            Cleaned = Cleaned.drop_front(ASCIIArtBytes);
            CleanedStartLoc =
            CleanedStartLoc.getAdvancedLocOrInvalid(ASCIIArtBytes);
            Pos -= ASCIIArtBytes;
          }

        StringRef Line = Cleaned.substr(0, Pos);
        auto CleanedEndLoc = CleanedStartLoc.getAdvancedLocOrInvalid(Pos);

        Cleaned = Cleaned.drop_front(Pos);
        unsigned NewlineBytes = swift::measureNewline(Cleaned);
        Cleaned = Cleaned.drop_front(NewlineBytes);
        Pos += NewlineBytes;
        CleanedStartLoc = CleanedStartLoc.getAdvancedLocOrInvalid(Pos);

        Builder.addLine(Line, { CleanedStartLoc, CleanedEndLoc });
      }
    }
  }
  return Builder.takeLineList();
}

LineList LineList::subListWithRange(MarkupContext &MC, size_t StartLine,
                                    size_t EndLine, size_t StartColumn,
                                    size_t EndColumn) const {
  auto TrimmedLines = ArrayRef<Line>(Lines.begin() + StartLine,
                                     Lines.begin() + EndLine);

  LineListBuilder Builder(MC);
  if (TrimmedLines.empty())
    return LineList();

  auto FirstLine = TrimmedLines.begin();
  auto End = TrimmedLines.end();
  auto LastLine = End - 1;

  for (auto Line = FirstLine; Line != End; ++Line) {
    auto T = Line->Text;
    auto RangeStart = Line->Range.Start;
    auto RangeEnd = Line->Range.End;

    if (Line == LastLine) {
      T = T.drop_back(T.size() - EndColumn);
      RangeEnd = RangeStart.getAdvancedLocOrInvalid(EndColumn);
    }

    if (Line == FirstLine) {
      T = T.drop_front(StartColumn);
      RangeStart = RangeStart.getAdvancedLocOrInvalid(StartColumn);
    }

    Builder.addLine(T, {RangeStart, RangeEnd});
  }

  return Builder.takeLineList();
}
