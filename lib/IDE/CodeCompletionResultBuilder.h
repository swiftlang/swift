//===--- CodeCompletionResultBuilder.h - Build completion results ---------===//
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

#ifndef SWIFT_LIB_IDE_CODE_COMPLETION_RESULT_BUILDER_H
#define SWIFT_LIB_IDE_CODE_COMPLETION_RESULT_BUILDER_H

#include "swift/IDE/CodeCompletion.h"
#include "swift/AST/Types.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/StringExtras.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSwitch.h"

namespace clang {
class Module;
}

namespace swift {
class Decl;
class ModuleDecl;

namespace ide {

class CodeCompletionResultBuilder {
  CodeCompletionResultSink &Sink;
  CodeCompletionResult::ResultKind Kind;
  SemanticContextKind SemanticContext;
  unsigned NumBytesToErase = 0;
  const Decl *AssociatedDecl = nullptr;
  Optional<CodeCompletionLiteralKind> LiteralKind;
  CodeCompletionKeywordKind KeywordKind = CodeCompletionKeywordKind::None;
  unsigned CurrentNestingLevel = 0;
  SmallVector<CodeCompletionString::Chunk, 4> Chunks;
  llvm::PointerUnion<const ModuleDecl *, const clang::Module *>
      CurrentModule;
  ArrayRef<Type> ExpectedDeclTypes;
  CodeCompletionResult::ExpectedTypeRelation ExpectedTypeRelation =
      CodeCompletionResult::Unrelated;
  bool Cancelled = false;
  ArrayRef<std::pair<StringRef, StringRef>> CommentWords;
  bool IsNotRecommended = false;
  CodeCompletionResult::NotRecommendedReason NotRecReason =
    CodeCompletionResult::NotRecommendedReason::NoReason;

  void addChunkWithText(CodeCompletionString::Chunk::ChunkKind Kind,
                        StringRef Text);

  void addChunkWithTextNoCopy(CodeCompletionString::Chunk::ChunkKind Kind,
                              StringRef Text) {
    Chunks.push_back(CodeCompletionString::Chunk::createWithText(
        Kind, CurrentNestingLevel, Text));
  }

  void addSimpleChunk(CodeCompletionString::Chunk::ChunkKind Kind) {
    Chunks.push_back(
        CodeCompletionString::Chunk::createSimple(Kind,
                                                  CurrentNestingLevel));
  }

  CodeCompletionString::Chunk &getLastChunk() {
    return Chunks.back();
  }

  CodeCompletionResult *takeResult();
  void finishResult();

public:
  CodeCompletionResultBuilder(CodeCompletionResultSink &Sink,
                              CodeCompletionResult::ResultKind Kind,
                              SemanticContextKind SemanticContext,
                              ArrayRef<Type> ExpectedDeclTypes)
      : Sink(Sink), Kind(Kind), SemanticContext(SemanticContext),
        ExpectedDeclTypes(ExpectedDeclTypes) {}

  ~CodeCompletionResultBuilder() {
    finishResult();
  }

  void cancel() {
    Cancelled = true;
  }

  void setNumBytesToErase(unsigned N) {
    NumBytesToErase = N;
  }

  void setAssociatedDecl(const Decl *D);

  void setLiteralKind(CodeCompletionLiteralKind kind) { LiteralKind = kind; }
  void setKeywordKind(CodeCompletionKeywordKind kind) { KeywordKind = kind; }
  void setNotRecommended(CodeCompletionResult::NotRecommendedReason Reason) {
    IsNotRecommended = true;
    NotRecReason = Reason;
  }

  void
  setExpectedTypeRelation(CodeCompletionResult::ExpectedTypeRelation relation) {
    ExpectedTypeRelation = relation;
  }

  void addAccessControlKeyword(AccessLevel Access) {
    switch (Access) {
    case AccessLevel::Private:
      addChunkWithTextNoCopy(
          CodeCompletionString::Chunk::ChunkKind::AccessControlKeyword,
          "private ");
      break;
    case AccessLevel::FilePrivate:
      addChunkWithTextNoCopy(
          CodeCompletionString::Chunk::ChunkKind::AccessControlKeyword,
          "fileprivate ");
      break;
    case AccessLevel::Internal:
      // 'internal' is the default, don't add it.
      break;
    case AccessLevel::Public:
      addChunkWithTextNoCopy(
          CodeCompletionString::Chunk::ChunkKind::AccessControlKeyword,
          "public ");
      break;
    case AccessLevel::Open:
      addChunkWithTextNoCopy(
          CodeCompletionString::Chunk::ChunkKind::AccessControlKeyword,
          "open ");
      break;
    }
  }

  void addOverrideKeyword() {
    addChunkWithTextNoCopy(
        CodeCompletionString::Chunk::ChunkKind::OverrideKeyword, "override ");
  }

  void addDeclIntroducer(StringRef Text) {
    addChunkWithText(CodeCompletionString::Chunk::ChunkKind::DeclIntroducer,
                     Text);
  }

  void addTextChunk(StringRef Text) {
    addChunkWithText(CodeCompletionString::Chunk::ChunkKind::Text, Text);
  }

  void addAnnotatedThrows() {
    addThrows();
    getLastChunk().setIsAnnotation();
  }

  void addThrows() {
    addChunkWithTextNoCopy(
       CodeCompletionString::Chunk::ChunkKind::ThrowsKeyword,
       " throws");
  }

  void addDeclDocCommentWords(ArrayRef<std::pair<StringRef, StringRef>> Pairs) {
    assert(Kind == CodeCompletionResult::ResultKind::Declaration);
    CommentWords = Pairs;
  }

  void addAnnotatedRethrows() {
    addRethrows();
    getLastChunk().setIsAnnotation();
  }

  void addRethrows() {
    addChunkWithTextNoCopy(
        CodeCompletionString::Chunk::ChunkKind::RethrowsKeyword,
        " rethrows");
  }

  void addAnnotatedLeftParen() {
    addLeftParen();
    getLastChunk().setIsAnnotation();
  }

  void addLeftParen() {
    addChunkWithTextNoCopy(
        CodeCompletionString::Chunk::ChunkKind::LeftParen, "(");
  }

  void addAnnotatedRightParen() {
    addRightParen();
    getLastChunk().setIsAnnotation();
  }

  void addRightParen() {
    addChunkWithTextNoCopy(
        CodeCompletionString::Chunk::ChunkKind::RightParen, ")");
  }

  void addLeftBracket() {
    addChunkWithTextNoCopy(
        CodeCompletionString::Chunk::ChunkKind::LeftBracket, "[");
  }

  void addRightBracket() {
    addChunkWithTextNoCopy(
        CodeCompletionString::Chunk::ChunkKind::RightBracket, "]");
  }

  void addLeftAngle() {
    addChunkWithTextNoCopy(
        CodeCompletionString::Chunk::ChunkKind::LeftAngle, "<");
  }

  void addRightAngle() {
    addChunkWithTextNoCopy(
        CodeCompletionString::Chunk::ChunkKind::RightAngle, ">");
  }

  void addLeadingDot() {
    addDot();
  }

  void addDot() {
    addChunkWithTextNoCopy(CodeCompletionString::Chunk::ChunkKind::Dot, ".");
  }

  void addEllipsis() {
    addChunkWithTextNoCopy(CodeCompletionString::Chunk::ChunkKind::Ellipsis,
                           "...");
  }

  void addComma() {
    addChunkWithTextNoCopy(
        CodeCompletionString::Chunk::ChunkKind::Comma, ", ");
  }

  void addExclamationMark() {
    addChunkWithTextNoCopy(
        CodeCompletionString::Chunk::ChunkKind::ExclamationMark, "!");
  }

  void addQuestionMark() {
    addChunkWithTextNoCopy(
        CodeCompletionString::Chunk::ChunkKind::QuestionMark, "?");
  }

  void addEqual() {
    addChunkWithTextNoCopy(CodeCompletionString::Chunk::ChunkKind::Equal, "=");
  }

  void addDeclAttrParamKeyword(StringRef Name, StringRef Annotation,
                               bool NeedSpecify) {
    addChunkWithText(CodeCompletionString::Chunk::ChunkKind::
                     DeclAttrParamKeyword, Name);
    if (NeedSpecify)
      addChunkWithText(CodeCompletionString::Chunk::ChunkKind::
                       DeclAttrParamColon, ": ");
    if (!Annotation.empty())
      addTypeAnnotation(Annotation);
  }

  void addDeclAttrKeyword(StringRef Name, StringRef Annotation) {
    addChunkWithText(CodeCompletionString::Chunk::ChunkKind::
                     DeclAttrKeyword, Name);
    if (!Annotation.empty())
      addTypeAnnotation(Annotation);
  }

  StringRef escapeArgumentLabel(StringRef Word,
                                bool escapeAllKeywords,
                                llvm::SmallString<16> &EscapedKeyword) {
    bool shouldEscape = false;
    if (escapeAllKeywords) {
#define KEYWORD(kw) .Case(#kw, true)
      shouldEscape = llvm::StringSwitch<bool>(Word)
#include "swift/Syntax/TokenKinds.def"
        .Default(false);
    } else {
      shouldEscape = !canBeArgumentLabel(Word);
    }

    if (!shouldEscape)
      return Word;

    EscapedKeyword.append("`");
    EscapedKeyword.append(Word);
    EscapedKeyword.append("`");
    return EscapedKeyword;
  }

  void addCallParameterColon() {
    addChunkWithText(CodeCompletionString::Chunk::ChunkKind::
                     CallParameterColon, ": ");
  }

  void addSimpleNamedParameter(StringRef name) {
    CurrentNestingLevel++;
    addSimpleChunk(CodeCompletionString::Chunk::ChunkKind::CallParameterBegin);
    // Use internal, since we don't want the name to be outside the placeholder.
    addChunkWithText(
        CodeCompletionString::Chunk::ChunkKind::CallParameterInternalName,
        name);
    CurrentNestingLevel--;
  }

  void addSimpleTypedParameter(StringRef Annotation, bool IsVarArg = false) {
    CurrentNestingLevel++;
    addSimpleChunk(CodeCompletionString::Chunk::ChunkKind::CallParameterBegin);
    addChunkWithText(CodeCompletionString::Chunk::ChunkKind::CallParameterType,
                     Annotation);
    if (IsVarArg)
      addEllipsis();
    CurrentNestingLevel--;
  }

  void addCallParameter(Identifier Name, Identifier LocalName, Type Ty,
                        bool IsVarArg, bool Outermost, bool IsInOut,
                        bool IsIUO) {
    CurrentNestingLevel++;

    addSimpleChunk(CodeCompletionString::Chunk::ChunkKind::CallParameterBegin);

    if (!Name.empty()) {
      StringRef NameStr = Name.str();

      // 'self' is a keyword, we cannot allow to insert it into the source
      // buffer.
      bool IsAnnotation = (NameStr == "self");

      llvm::SmallString<16> EscapedKeyword;
      addChunkWithText(
          CodeCompletionString::Chunk::ChunkKind::CallParameterName,
          // if the name is not annotation, we need to escape keyword
          IsAnnotation ? NameStr
                       : escapeArgumentLabel(NameStr, !Outermost,
                                             EscapedKeyword));
      if (IsAnnotation)
        getLastChunk().setIsAnnotation();

      addChunkWithTextNoCopy(
          CodeCompletionString::Chunk::ChunkKind::CallParameterColon, ": ");
      if (IsAnnotation)
        getLastChunk().setIsAnnotation();
    }

    // 'inout' arguments are printed specially.
    if (IsInOut) {
      addChunkWithTextNoCopy(
          CodeCompletionString::Chunk::ChunkKind::Ampersand, "&");
      Ty = Ty->getInOutObjectType();
    }

    if (Name.empty() && !LocalName.empty()) {
      llvm::SmallString<16> EscapedKeyword;
      // Use local (non-API) parameter name if we have nothing else.
      addChunkWithText(
          CodeCompletionString::Chunk::ChunkKind::CallParameterInternalName,
            escapeArgumentLabel(LocalName.str(), !Outermost, EscapedKeyword));
      addChunkWithTextNoCopy(
          CodeCompletionString::Chunk::ChunkKind::CallParameterColon, ": ");
    }

    // If the parameter is of the type @autoclosure ()->output, then the
    // code completion should show the parameter of the output type
    // instead of the function type ()->output.
    if (auto FuncType = Ty->getAs<AnyFunctionType>()) {
      if (FuncType->isAutoClosure()) {
        Ty = FuncType->getResult();
      }
    }

    PrintOptions PO;
    PO.SkipAttributes = true;
    PO.PrintOptionalAsImplicitlyUnwrapped = IsIUO;
    std::string TypeName = Ty->getString(PO);
    addChunkWithText(CodeCompletionString::Chunk::ChunkKind::CallParameterType,
                     TypeName);

    // Look through optional types and type aliases to find out if we have
    // function/closure parameter type that is not an autoclosure.
    Ty = Ty->lookThroughAllOptionalTypes();
    if (auto AFT = Ty->getAs<AnyFunctionType>()) {
      if (!AFT->isAutoClosure()) {
        // If this is a closure type, add ChunkKind::CallParameterClosureType.
        PrintOptions PO;
        PO.PrintFunctionRepresentationAttrs = false;
        PO.SkipAttributes = true;
        addChunkWithText(
            CodeCompletionString::Chunk::ChunkKind::CallParameterClosureType,
            AFT->getString(PO));
      }
    }

    if (IsVarArg)
      addEllipsis();
    CurrentNestingLevel--;
  }

  void addCallParameter(Identifier Name, Type Ty, bool IsVarArg, bool Outermost,
                        bool IsInOut, bool IsIUO) {
    addCallParameter(Name, Identifier(), Ty, IsVarArg, Outermost, IsInOut,
                     IsIUO);
  }

  void addGenericParameter(StringRef Name) {
    CurrentNestingLevel++;
    addSimpleChunk(
       CodeCompletionString::Chunk::ChunkKind::GenericParameterBegin);
    addChunkWithText(
      CodeCompletionString::Chunk::ChunkKind::GenericParameterName, Name);
    CurrentNestingLevel--;
  }

  void addDynamicLookupMethodCallTail() {
    addChunkWithTextNoCopy(
        CodeCompletionString::Chunk::ChunkKind::DynamicLookupMethodCallTail,
        "!");
    getLastChunk().setIsAnnotation();
  }

  void addOptionalMethodCallTail() {
    addChunkWithTextNoCopy(
        CodeCompletionString::Chunk::ChunkKind::OptionalMethodCallTail, "?");
  }

  void addTypeAnnotation(StringRef Type) {
    addChunkWithText(
        CodeCompletionString::Chunk::ChunkKind::TypeAnnotation, Type);
    getLastChunk().setIsAnnotation();
  }

  void addBraceStmtWithCursor(StringRef Description = "") {
    addChunkWithText(
        CodeCompletionString::Chunk::ChunkKind::BraceStmtWithCursor,
        Description);
  }

  void addWhitespace(StringRef space) {
    addChunkWithText(
        CodeCompletionString::Chunk::ChunkKind::Whitespace, space);
  }

  void addAnnotatedWhitespace(StringRef space) {
    addWhitespace(space);
    getLastChunk().setIsAnnotation();
  }
};

} // namespace ide
} // namespace swift

#endif // SWIFT_LIB_IDE_CODE_COMPLETION_RESULT_BUILDER_H

