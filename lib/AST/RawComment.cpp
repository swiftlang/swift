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
#include "swift/AST/CommentAST.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/Basic/PrimitiveParsing.h"
#include "swift/Basic/SourceManager.h"
#include "swift/ReST/Parser.h"
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
      Context.setBriefComment(this, C->Brief);
      Context.setRawComment(this, C->Raw);
      return C->Raw;
    }
  }

  // Give up.
  return RawComment();
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

static llvm::rest::LineList
toLineList(llvm::rest::ReSTContext &TheReSTContext,
           llvm::rest::SourceManager<SourceLoc> &RSM, RawComment RC) {
  llvm::rest::LineListBuilder Result(TheReSTContext);
  for (const auto &C : RC.Comments) {
    if (C.isLine()) {
      // Skip comment marker.
      unsigned CommentMarkerBytes = 2 + (C.isOrdinary() ? 0 : 1);
      StringRef Cleaned = C.RawText.drop_front(CommentMarkerBytes);

      // Drop trailing newline.
      Cleaned = Cleaned.rtrim("\n\r");
      SourceLoc CleanedStartLoc =
          C.Range.getStart().getAdvancedLocOrInvalid(CommentMarkerBytes);
      Result.addLine(Cleaned, RSM.registerLine(Cleaned, CleanedStartLoc));
    } else {
      // Skip comment markers at the beginning and at the end.
      unsigned CommentMarkerBytes = 2 + (C.isOrdinary() ? 0 : 1);
      StringRef Cleaned = C.RawText.drop_front(CommentMarkerBytes).drop_back(2);
      SourceLoc CleanedStartLoc =
          C.Range.getStart().getAdvancedLocOrInvalid(CommentMarkerBytes);

      // Determine if we have leading decorations in this block comment.
      bool HasASCIIArt = false;
      if (startsWithNewline(Cleaned)) {
        Result.addLine(Cleaned.substr(0, 0),
                       RSM.registerLine(Cleaned.substr(0, 0), CleanedStartLoc));
        unsigned NewlineBytes = measureNewline(Cleaned);
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
        Result.addLine(Line, RSM.registerLine(Line, CleanedStartLoc));

        Cleaned = Cleaned.drop_front(Pos);
        unsigned NewlineBytes = measureNewline(Cleaned);
        Cleaned = Cleaned.drop_front(NewlineBytes);
        Pos += NewlineBytes;
        CleanedStartLoc = CleanedStartLoc.getAdvancedLocOrInvalid(Pos);
      }
    }
  }
  return Result.takeLineList();
}

static StringRef extractBriefComment(ASTContext &Context, RawComment RC,
                                     const Decl *D) {
  PrettyStackTraceDecl StackTrace("extracting brief comment for", D);

  llvm::rest::ReSTContext TheReSTContext;
  llvm::rest::SourceManager<SourceLoc> RSM;
  llvm::rest::LineList LL = toLineList(TheReSTContext, RSM, RC);
  llvm::SmallString<256> Result;
  llvm::rest::extractBrief(LL, Result);

  if (Result.empty())
    return StringRef();
  ArrayRef<char> Copy = Context.AllocateCopy(Result);
  return StringRef(Copy.data(), Copy.size());
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

CommentContext::CommentContext() {
  TheReSTContext.LangOpts.IgnoreUniformIndentation = true;
}

CommentContext::~CommentContext() {
  for (auto *FC : FullComments) {
    FC->~FullComment();
  }
}

static bool isFieldNamed(const llvm::rest::Field *F, StringRef Name) {
  const llvm::rest::TextAndInline *ActualName = F->getName();
  // FIXME: not correct.  We should concatenate text from all children without
  // markup.
  if (ActualName->getChildren().size() != 1)
    return false;
  if (auto *TextChild =
          dyn_cast<llvm::rest::PlainText>(ActualName->getChildren().front())) {
    return TextChild->getLinePart().Text == Name;
  }
  return false;
}

namespace {
class CommentSema {
  CommentContext &Context;
  FullComment::CommentParts &Parts;

public:
  CommentSema(CommentContext &Context, FullComment *FC)
      : Context(Context), Parts(FC->getMutableParts()) {}

  void visitDocument(llvm::rest::Document *D);

  comments::ParamField *actOnParam(llvm::rest::Field *F);
};
} // unnamed namespace

void CommentSema::visitDocument(llvm::rest::Document *D) {
  bool IsFirstChild = true;
  for (auto *N : D->getChildren()) {
    // If the first document child is a paragraph, consider it a brief
    // description.
    if (IsFirstChild) {
      IsFirstChild = false;
      if (const auto *P = dyn_cast<llvm::rest::Paragraph>(N)) {
        Parts.Brief = P;
        continue;
      }
    }
    if (auto *FL = dyn_cast<llvm::rest::FieldList>(N)) {
      for (auto *F : FL->getChildren()) {
        if (isFieldNamed(F, "param"))
          Parts.Params.push_back(actOnParam(F));
        else if (isFieldNamed(F, "returns"))
          Parts.Returns.push_back(F);
        else
          Parts.MiscTopLevelNodes.push_back(F);
      }
    } else {
      Parts.MiscTopLevelNodes.push_back(N);
    }
  }
}

comments::ParamField *CommentSema::actOnParam(llvm::rest::Field *F) {
  assert(isFieldNamed(F, "param"));
  llvm::rest::LinePart ParamName;
  auto BodyChildren = F->getBodyChildren();
  if (!BodyChildren.empty()) {
    if (auto *P = dyn_cast<llvm::rest::Paragraph>(BodyChildren[0])) {
      if (auto *TAI = dyn_cast<llvm::rest::TextAndInline>(P->getMutableContent())) {
        auto MaybeParamName = extractWord(TAI);
        if (MaybeParamName)
          ParamName = MaybeParamName.getValue();
      }
    }
  }
  return new (Context.TheReSTContext)
      comments::ParamField(F->getName(), ParamName, BodyChildren);
}

static void performCommentSema(CommentContext &Context, FullComment *FC) {
  CommentSema(Context, FC).visitDocument(FC->getMutableDocument());
}

const FullComment::CommentParts &
FullComment::getParts(CommentContext &Context) const {
  if (Parts.hasValue())
    return Parts.getValue();

  performCommentSema(Context, const_cast<FullComment *>(this));
  return Parts.getValue();
}

void *FullComment::operator new(size_t Bytes, llvm::rest::ReSTContext &C,
                                unsigned Alignment) {
  return C.Allocator.Allocate(Bytes, Alignment);
}

FullComment *swift::getFullComment(CommentContext &Context, const Decl *D) {
  auto RC = D->getRawComment();
  if (RC.isEmpty())
    return nullptr;

  PrettyStackTraceDecl StackTrace("parsing comment for", D);

  llvm::rest::SourceManager<SourceLoc> RSM;
  llvm::rest::LineList LL = toLineList(Context.TheReSTContext, RSM, RC);
  auto *Doc = llvm::rest::parseDocument(Context.TheReSTContext, LL);
  return new (Context.TheReSTContext) FullComment(D, Doc);
}

