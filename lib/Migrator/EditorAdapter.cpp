//===--- EditorAdapter.cpp ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Migrator/EditorAdapter.h"
#include "swift/Migrator/Replacement.h"
#include "swift/Parse/Lexer.h"
#include "clang/Basic/SourceManager.h"

using namespace swift;
using namespace swift::migrator;

std::pair<unsigned, unsigned>
EditorAdapter::getLocInfo(swift::SourceLoc Loc) const {
  auto SwiftBufferID = SwiftSrcMgr.findBufferContainingLoc(Loc);
  auto Offset = SwiftSrcMgr.getLocOffsetInBuffer(Loc, SwiftBufferID);
  return { SwiftBufferID, Offset };
}

bool
EditorAdapter::cacheReplacement(CharSourceRange Range, StringRef Text) const {
  if (!CacheEnabled)
    return false;
  unsigned SwiftBufferID, Offset;
  std::tie(SwiftBufferID, Offset) = getLocInfo(Range.getStart());
  Replacement R { Offset, Range.getByteLength(), Text };
  if (Replacements.count(R)) {
    return true;
  } else {
    Replacements.insert(R);
  }
  return false;
}

clang::FileID
EditorAdapter::getClangFileIDForSwiftBufferID(unsigned BufferID) const {
  /// Check if we already have a mapping for this BufferID.
  auto Found = SwiftToClangBufferMap.find(BufferID);
  if (Found != SwiftToClangBufferMap.end()) {
    return Found->getSecond();
  }

  // If we don't copy the corresponding buffer's text into a new buffer
  // that the ClangSrcMgr can understand.
  auto Text = SwiftSrcMgr.getEntireTextForBuffer(BufferID);
  auto NewBuffer = llvm::MemoryBuffer::getMemBufferCopy(Text);
  auto NewFileID = ClangSrcMgr.createFileID(std::move(NewBuffer));

  SwiftToClangBufferMap.insert({BufferID, NewFileID});

  return NewFileID;
}

clang::SourceLocation
EditorAdapter::translateSourceLoc(SourceLoc SwiftLoc) const {
  unsigned SwiftBufferID, Offset;
  std::tie(SwiftBufferID, Offset) = getLocInfo(SwiftLoc);

  auto ClangFileID = getClangFileIDForSwiftBufferID(SwiftBufferID);
  return ClangSrcMgr.getLocForStartOfFile(ClangFileID).getLocWithOffset(Offset);
}

clang::SourceRange
EditorAdapter::translateSourceRange(SourceRange SwiftSourceRange) const {
  auto Start = translateSourceLoc(SwiftSourceRange.Start);
  auto End = translateSourceLoc(SwiftSourceRange.End);
  return clang::SourceRange { Start, End };
}

clang::CharSourceRange EditorAdapter::
translateCharSourceRange(CharSourceRange SwiftSourceSourceRange) const {
  auto ClangStartLoc = translateSourceLoc(SwiftSourceSourceRange.getStart());
  auto ClangEndLoc = translateSourceLoc(SwiftSourceSourceRange.getEnd());
  return clang::CharSourceRange::getCharRange(ClangStartLoc, ClangEndLoc);
}

bool EditorAdapter::insert(SourceLoc Loc, StringRef Text, bool AfterToken,
                           bool BeforePreviousInsertions) {
  // We don't have tokens on the clang side, so handle AfterToken in Swift
  if (AfterToken)
    Loc = Lexer::getLocForEndOfToken(SwiftSrcMgr, Loc);

  if (cacheReplacement(CharSourceRange { Loc, 0 }, Text)) {
    return true;
  }

  auto ClangLoc = translateSourceLoc(Loc);
  return Edits.insert(ClangLoc, Text, /*AfterToken=*/false, BeforePreviousInsertions);
}

bool EditorAdapter::insertFromRange(SourceLoc Loc, CharSourceRange Range,
                                    bool AfterToken,
                                    bool BeforePreviousInsertions) {
  // We don't have tokens on the clang side, so handle AfterToken in Swift
  if (AfterToken)
    Loc = Lexer::getLocForEndOfToken(SwiftSrcMgr, Loc);

  if (cacheReplacement(CharSourceRange { Loc, 0 },
                       SwiftSrcMgr.extractText(Range))) {
    return true;
  }

  auto ClangLoc = translateSourceLoc(Loc);
  auto ClangCharRange = translateCharSourceRange(Range);

  return Edits.insertFromRange(ClangLoc, ClangCharRange, /*AfterToken=*/false,
                               BeforePreviousInsertions);
}

bool EditorAdapter::insertWrap(StringRef Before, CharSourceRange Range,
                               StringRef After) {
  auto ClangRange = translateCharSourceRange(Range);
  return Edits.insertWrap(Before, ClangRange, After);
}

bool EditorAdapter::remove(CharSourceRange Range) {
  if (cacheReplacement(Range, "")) {
    return true;
  }
  auto ClangRange = translateCharSourceRange(Range);
  return Edits.remove(ClangRange);
}

bool EditorAdapter::replace(CharSourceRange Range, StringRef Text) {
  if (cacheReplacement(Range, Text)) {
    return true;
  }

  auto ClangRange = translateCharSourceRange(Range);
  return Edits.replace(ClangRange, Text);
}

bool EditorAdapter::replaceWithInner(CharSourceRange Range,
                                     CharSourceRange InnerRange) {

  if (cacheReplacement(Range, SwiftSrcMgr.extractText(InnerRange))) {
    return true;
  }
  auto ClangRange = translateCharSourceRange(Range);
  auto ClangInnerRange = translateCharSourceRange(InnerRange);
  return Edits.replaceWithInner(ClangRange, ClangInnerRange);
}

bool EditorAdapter::replaceText(SourceLoc Loc, StringRef Text,
                                StringRef ReplacementText) {
  auto Range = Lexer::getCharSourceRangeFromSourceRange(SwiftSrcMgr,
    { Loc, Loc.getAdvancedLoc(Text.size())});
  if (cacheReplacement(Range, Text)) {
    return true;
  }

  auto ClangLoc = translateSourceLoc(Loc);
  return Edits.replaceText(ClangLoc, Text, ReplacementText);
}

bool EditorAdapter::insertFromRange(SourceLoc Loc, SourceRange TokenRange,
                     bool AfterToken,
                     bool BeforePreviousInsertions) {
  auto CharRange = Lexer::getCharSourceRangeFromSourceRange(SwiftSrcMgr, TokenRange);
  return insertFromRange(Loc, CharRange,
                         AfterToken, BeforePreviousInsertions);
}

bool EditorAdapter::insertWrap(StringRef Before, SourceRange TokenRange,
                               StringRef After) {
  auto CharRange = Lexer::getCharSourceRangeFromSourceRange(SwiftSrcMgr, TokenRange);
  return insertWrap(Before, CharRange, After);
}

bool EditorAdapter::remove(SourceLoc TokenLoc) {
  return remove(Lexer::getCharSourceRangeFromSourceRange(SwiftSrcMgr,
                                                         TokenLoc));
}

bool EditorAdapter::remove(SourceRange TokenRange) {
  auto CharRange = Lexer::getCharSourceRangeFromSourceRange(SwiftSrcMgr, TokenRange);
  return remove(CharRange);
}

bool EditorAdapter::replace(SourceRange TokenRange, StringRef Text) {
  auto CharRange = Lexer::getCharSourceRangeFromSourceRange(SwiftSrcMgr,TokenRange);
  return replace(CharRange, Text);
}

bool EditorAdapter::replaceWithInner(SourceRange TokenRange,
                                     SourceRange TokenInnerRange) {
  auto CharRange = Lexer::getCharSourceRangeFromSourceRange(SwiftSrcMgr, TokenRange);
  auto CharInnerRange = Lexer::getCharSourceRangeFromSourceRange(SwiftSrcMgr, TokenInnerRange);
  return replaceWithInner(CharRange, CharInnerRange);
}

bool EditorAdapter::replaceToken(SourceLoc TokenLoc, StringRef Text) {
  return replace(Lexer::getTokenAtLocation(SwiftSrcMgr, TokenLoc).getRange(),
    Text);
}
