//===--- RawComment.cpp - Extraction of raw comments ----------------------===//
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
///
/// \file
/// This file implements extraction of raw comments.
///
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/RawComment.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Parse/Lexer.h"
#include "llvm/ADT/StringRef.h"

using namespace swift;

static SingleRawComment::CommentKind getCommentKind(StringRef Comment) {
  assert(Comment.size() >= 2);
  assert(Comment[0] == '/');

  if (Comment[1] == '/') {
    if (Comment.size() < 3)
      return SingleRawComment::CommentKind::OrdinaryLine;

    if (Comment[2] == '/') {
      return SingleRawComment::CommentKind::LineDoc;
    }
    return SingleRawComment::CommentKind::OrdinaryLine;
  } else {
    assert(Comment[1] == '*');
    assert(Comment.size() >= 4);
    if (Comment[2] == '*') {
      return SingleRawComment::CommentKind::BlockDoc;
    }
    return SingleRawComment::CommentKind::OrdinaryBlock;
  }
}

SingleRawComment::SingleRawComment(CharSourceRange Range,
                                   const SourceManager &SourceMgr)
    : Range(Range), RawText(SourceMgr.extractText(Range)),
      Kind(getCommentKind(RawText)),
      StartLine(SourceMgr.getLineNumber(Range.getStart())),
      EndLine(SourceMgr.getLineNumber(Range.getEnd())) {}

static void addCommentToList(SmallVectorImpl<SingleRawComment> &Comments,
                             const SingleRawComment &SRC) {
  // TODO: consider producing warnings when we decide not to merge comments.

  if (SRC.isOrdinary()) {
    Comments.clear();
    return;
  }

  // If this is the first documentation comment, save it (because there isn't
  // anything to merge it with).
  if (Comments.empty()) {
    Comments.push_back(SRC);
    return;
  }

  auto &Last = Comments.back();

  // Merge comments if they are on same or consecutive lines.
  if (Last.EndLine + 1 < SRC.StartLine) {
    Comments.clear();
    return;
  }

  Comments.push_back(SRC);
}

static RawComment toRawComment(ASTContext &Context, CharSourceRange Range) {
  if (Range.isInvalid())
    return RawComment();

  auto &SourceMgr = Context.SourceMgr;
  unsigned BufferID = SourceMgr.findBufferContainingLoc(Range.getStart());
  unsigned Offset = SourceMgr.getLocOffsetInBuffer(Range.getStart(), BufferID);
  unsigned EndOffset = SourceMgr.getLocOffsetInBuffer(Range.getEnd(), BufferID);
  LangOptions FakeLangOpts;
  Lexer L(FakeLangOpts, SourceMgr, BufferID, nullptr, /*InSILMode=*/false,
          CommentRetentionMode::ReturnAsTokens, Offset, EndOffset);
  SmallVector<SingleRawComment, 16> Comments;
  Token Tok;
  while (true) {
    L.lex(Tok);
    if (Tok.is(tok::eof))
      break;
    assert(Tok.is(tok::comment));
    addCommentToList(Comments, SingleRawComment(Tok.getRange(), SourceMgr));
  }
  RawComment Result;
  Result.Comments = Context.AllocateCopy(Comments);
  return Result;
}

RawComment Decl::getRawComment() const {
  auto &Context = getASTContext();
  if (Optional<RawComment> RC = Context.getRawComment(this))
    return RC.getValue();

  if (!getAttrs().has(AK_raw_doc_comment))
    return RawComment();

  RawComment Result = toRawComment(Context, getAttrs().CommentRange);
  Context.setRawComment(this, Result);
  return Result;
}

