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

#include "swift/AST/RawComment.h"
#include "swift/AST/Comment.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/Basic/PrimitiveParsing.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Markup/Markup.h"
#include "swift/Parse/Lexer.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/raw_ostream.h"

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
      Kind(static_cast<unsigned>(getCommentKind(RawText))),
      EndLine(SourceMgr.getLineNumber(Range.getEnd())) {
  auto StartLineAndColumn = SourceMgr.getLineAndColumn(Range.getStart());
  StartLine = StartLineAndColumn.first;
  StartColumn = StartLineAndColumn.second;
}

SingleRawComment::SingleRawComment(StringRef RawText, unsigned StartColumn)
    : RawText(RawText), Kind(static_cast<unsigned>(getCommentKind(RawText))),
      StartColumn(StartColumn), StartLine(0), EndLine(0) {}

static bool canHaveComment(const Decl *D) {
  return !D->hasClangNode() &&
         isa<ValueDecl>(D) &&
         !isa<ParamDecl>(D) &&
         (!isa<AbstractTypeParamDecl>(D) || isa<AssociatedTypeDecl>(D));
}

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
  if (!canHaveComment(this))
    return RawComment();

  // Check the cache in ASTContext.
  auto &Context = getASTContext();
  if (Optional<RawComment> RC = Context.getRawComment(this))
    return RC.getValue();

  // Check the declaration itself.
  if (auto *Attr = getAttrs().getAttribute<RawDocCommentAttr>()) {
    RawComment Result = toRawComment(Context, Attr->getCommentRange());
    Context.setRawComment(this, Result);
    return Result;
  }

  // Ask the parent module.
  if (auto *Unit =
          dyn_cast<FileUnit>(this->getDeclContext()->getModuleScopeContext())) {
    if (Optional<BriefAndRawComment> C = Unit->getCommentForDecl(this)) {
      llvm::markup::MarkupContext MC;
      Context.setBriefComment(this, C->Brief);
      Context.setRawComment(this, C->Raw);
      return C->Raw;
    }
  }

  // Give up.
  return RawComment();
}

static StringRef extractBriefComment(ASTContext &Context, RawComment RC,
                                     const Decl *D) {
  PrettyStackTraceDecl StackTrace("extracting brief comment for", D);

  if (!canHaveComment(D))
    return StringRef();

  llvm::markup::MarkupContext MC;
  auto DC = getDocComment(MC, D);
  if (!DC.hasValue())
    return StringRef();

  auto Brief = DC.getValue()->getBrief();
  if (!Brief.hasValue())
    return StringRef();

  SmallString<256> BriefStr("");
  llvm::raw_svector_ostream OS(BriefStr);
  llvm::markup::printInlinesUnder(Brief.getValue(), OS);
  OS.flush();
  if (OS.str().empty())
    return StringRef();

  return Context.AllocateCopy(OS.str());
}

StringRef Decl::getBriefComment() const {
  if (!canHaveComment(this))
    return StringRef();

  auto &Context = getASTContext();
  if (Optional<StringRef> Comment = Context.getBriefComment(this))
    return Comment.getValue();

  StringRef Result;
  auto RC = getRawComment();
  if (!RC.isEmpty())
    Result = extractBriefComment(Context, RC, this);

  Context.setBriefComment(this, Result);
  return Result;
}
