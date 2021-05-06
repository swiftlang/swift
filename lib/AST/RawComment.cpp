//===--- RawComment.cpp - Extraction of raw comments ----------------------===//
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
///
/// \file
/// This file implements extraction of raw comments.
///
//===----------------------------------------------------------------------===//

#include "swift/AST/RawComment.h"
#include "swift/AST/Comment.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/FileUnit.h"
#include "swift/AST/Module.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Defer.h"
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
      Kind(static_cast<unsigned>(getCommentKind(RawText))) {
  ColumnIndent = SourceMgr.getLineAndColumnInBuffer(Range.getStart()).second;
}

SingleRawComment::SingleRawComment(StringRef RawText, unsigned ColumnIndent)
    : RawText(RawText), Kind(static_cast<unsigned>(getCommentKind(RawText))),
      ColumnIndent(ColumnIndent) {}

static RawComment toRawComment(ASTContext &Context, CharSourceRange Range) {
  if (Range.isInvalid())
    return RawComment();

  auto &SM = Context.SourceMgr;
  unsigned BufferID = SM.findBufferContainingLoc(Range.getStart());
  unsigned Offset = SM.getLocOffsetInBuffer(Range.getStart(), BufferID);
  unsigned EndOffset = SM.getLocOffsetInBuffer(Range.getEnd(), BufferID);
  LangOptions FakeLangOpts;
  Lexer L(FakeLangOpts, SM, BufferID, nullptr, LexerMode::Swift,
          HashbangMode::Disallowed, CommentRetentionMode::ReturnAsTokens,
          TriviaRetentionMode::WithoutTrivia, Offset, EndOffset);

  SmallVector<SingleRawComment, 16> Comments;
  Token Tok;
  unsigned LastEnd = 0;
  while (true) {
    L.lex(Tok);
    if (Tok.is(tok::eof))
      break;
    assert(Tok.is(tok::comment));

    auto SRC = SingleRawComment(Tok.getRange(), SM);
    if (SRC.isOrdinary()) {
      // Skip gyb comments that are line number markers.
      if (!SRC.RawText.startswith("// ###")) {
        Comments.clear();
        LastEnd = 0;
      }
      continue;
    }

    // Merge comments if they are on same or consecutive lines.
    unsigned Start =
        SM.getLineAndColumnInBuffer(Tok.getRange().getStart()).first;
    if (LastEnd > 0 && LastEnd + 1 < Start) {
      Comments.clear();
      LastEnd = 0;
    } else {
      Comments.push_back(SRC);
      LastEnd = SM.getLineAndColumnInBuffer(Tok.getRange().getEnd()).first;
    }
  }

  RawComment Result;
  Result.Comments = Context.AllocateCopy(Comments);
  return Result;
}

RawComment Decl::getRawComment(bool SerializedOK) const {
  if (!this->canHaveComment())
    return RawComment();

  // Check the cache in ASTContext.
  auto &Context = getASTContext();
  if (Optional<std::pair<RawComment, bool>> RC = Context.getRawComment(this)) {
    auto P = RC.getValue();
    if (!SerializedOK || P.second)
      return P.first;
  }

  // Check the declaration itself.
  if (auto *Attr = getAttrs().getAttribute<RawDocCommentAttr>()) {
    RawComment Result = toRawComment(Context, Attr->getCommentRange());
    Context.setRawComment(this, Result, true);
    return Result;
  }

  if (!getDeclContext())
    return RawComment();
  auto *Unit = dyn_cast<FileUnit>(getDeclContext()->getModuleScopeContext());
  if (!Unit)
    return RawComment();

  switch (Unit->getKind()) {
  case FileUnitKind::SerializedAST: {
    if (SerializedOK) {
      auto *CachedLocs = getSerializedLocs();
      if (!CachedLocs->DocRanges.empty()) {
        SmallVector<SingleRawComment, 4> SRCs;
        for (const auto &Range : CachedLocs->DocRanges) {
          if (Range.isValid()) {
            SRCs.push_back({Range, Context.SourceMgr});
          } else {
            // if we've run into an invalid range, don't bother trying to load
            // any of the other comments
            SRCs.clear();
            break;
          }
        }

        if (!SRCs.empty()) {
          auto RC = RawComment(Context.AllocateCopy(llvm::makeArrayRef(SRCs)));
          Context.setRawComment(this, RC, true);
          return RC;
        }
      }
    }

    if (Optional<CommentInfo> C = Unit->getCommentForDecl(this)) {
      Context.setRawComment(this, C->Raw, false);
      return C->Raw;
    }

    return RawComment();
  }
  case FileUnitKind::Source:
  case FileUnitKind::Builtin:
  case FileUnitKind::Synthesized:
  case FileUnitKind::ClangModule:
  case FileUnitKind::DWARFModule:
    return RawComment();
  }
  llvm_unreachable("invalid file kind");
}

static const Decl* getGroupDecl(const Decl *D) {
  auto GroupD = D;

  // Extensions always exist in the same group with the nominal.
  if (auto ED = dyn_cast_or_null<ExtensionDecl>(D->getDeclContext()->
                                                getInnermostTypeContext())) {
    if (auto ExtNominal = ED->getExtendedNominal())
      GroupD = ExtNominal;
  }
  return GroupD;
}

Optional<StringRef> Decl::getGroupName() const {
  if (hasClangNode())
    return None;
  if (auto GroupD = getGroupDecl(this)) {
    // We can only get group information from deserialized module files.
    if (auto *Unit =
        dyn_cast<FileUnit>(GroupD->getDeclContext()->getModuleScopeContext())) {
      return Unit->getGroupNameForDecl(GroupD);
    }
  }
  return None;
}

Optional<StringRef> Decl::getSourceFileName() const {
  if (hasClangNode())
    return None;
  if (auto GroupD = getGroupDecl(this)) {
    // We can only get group information from deserialized module files.
    if (auto *Unit =
        dyn_cast<FileUnit>(GroupD->getDeclContext()->getModuleScopeContext())) {
      return Unit->getSourceFileNameForDecl(GroupD);
    }
  }
  return None;
}

Optional<unsigned> Decl::getSourceOrder() const {
  if (hasClangNode())
    return None;
  // We can only get source orders from deserialized module files.
  if (auto *Unit =
      dyn_cast<FileUnit>(this->getDeclContext()->getModuleScopeContext())) {
    return Unit->getSourceOrderForDecl(this);
  }
  return None;
}

CharSourceRange RawComment::getCharSourceRange() {
  if (this->isEmpty()) {
    return CharSourceRange();
  }

  auto Start = this->Comments.front().Range.getStart();
  if (Start.isInvalid()) {
    return CharSourceRange();
  }
  auto End = this->Comments.back().Range.getEnd();
  auto Length = static_cast<const char *>(End.getOpaquePointerValue()) -
                static_cast<const char *>(Start.getOpaquePointerValue());
  return CharSourceRange(Start, Length);
}
