//===- CodeCompletionResultBuilder.h - Bulid completion results -----------===//
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

#ifndef SWIFT_LIB_IDE_CODE_COMPLETION_RESULT_BUILDER_H
#define SWIFT_LIB_IDE_CODE_COMPLETION_RESULT_BUILDER_H

#include "swift/IDE/CodeCompletion.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"

namespace swift {
class Decl;

namespace ide {

class CodeCompletionResultBuilder {
  CodeCompletionResultSink &Sink;
  CodeCompletionResult::ResultKind Kind;
  SemanticContextKind SemanticContext;
  unsigned NumBytesToErase = 0;
  const Decl *AssociatedDecl = nullptr;
  unsigned CurrentNestingLevel = 0;
  SmallVector<CodeCompletionString::Chunk, 4> Chunks;

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
                              SemanticContextKind SemanticContext)
      : Sink(Sink), Kind(Kind), SemanticContext(SemanticContext) {
  }

  ~CodeCompletionResultBuilder() {
    finishResult();
  }

  void setNumBytesToErase(unsigned N) {
    NumBytesToErase = N;
  }

  void setAssociatedDecl(const Decl *D) {
    assert(Kind == CodeCompletionResult::ResultKind::Declaration);
    AssociatedDecl = D;
  }

  void addAccessControlKeyword(Accessibility Access) {
    switch (Access) {
    case Accessibility::Private:
      addChunkWithTextNoCopy(
          CodeCompletionString::Chunk::ChunkKind::AccessControlKeyword,
          "private ");
      break;
    case Accessibility::Internal:
      // 'internal' is the default, don't add it.
      break;
    case Accessibility::Public:
      addChunkWithTextNoCopy(
          CodeCompletionString::Chunk::ChunkKind::AccessControlKeyword,
          "public ");
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

  void addAnnotatedLeftParen() {
    addLeftParen();
    getLastChunk().setIsAnnotation();
  }

  void addLeftParen() {
    addChunkWithTextNoCopy(
        CodeCompletionString::Chunk::ChunkKind::LeftParen, "(");
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

  void addDeclAttrParamKeyword(StringRef Name, StringRef Annotation,
                               bool NeedSpecify) {
    addChunkWithText(CodeCompletionString::Chunk::ChunkKind::
                     DeclAttrParamKeyword, Name);
    if (NeedSpecify)
      addChunkWithText(CodeCompletionString::Chunk::ChunkKind::
                       DeclAttrParamEqual, "=");
    if(!Annotation.empty())
      addTypeAnnotation(Annotation);
  }

  void addCallParameter(Identifier Name, Identifier LocalName, Type Ty,
                        bool IsVarArg) {
    CurrentNestingLevel++;

    addSimpleChunk(CodeCompletionString::Chunk::ChunkKind::CallParameterBegin);

    if (!Name.empty()) {
      StringRef NameStr = Name.str();

      // 'self' is a keyword, we can not allow to insert it into the source
      // buffer.
      bool IsAnnotation = (NameStr == "self");

      addChunkWithText(
          CodeCompletionString::Chunk::ChunkKind::CallParameterName, NameStr);
      if (IsAnnotation)
        getLastChunk().setIsAnnotation();

      addChunkWithTextNoCopy(
          CodeCompletionString::Chunk::ChunkKind::CallParameterColon, ": ");
      if (IsAnnotation)
        getLastChunk().setIsAnnotation();
    }

    // 'inout' arguments are printed specially.
    if (auto *IOT = Ty->getAs<InOutType>()) {
      addChunkWithTextNoCopy(
          CodeCompletionString::Chunk::ChunkKind::Ampersand, "&");
      Ty = IOT->getObjectType();
    }

    if (Name.empty() && !LocalName.empty()) {
      // Use local (non-API) parameter name if we have nothing else.
      addChunkWithText(
          CodeCompletionString::Chunk::ChunkKind::CallParameterInternalName,
          LocalName.str());
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
    addChunkWithText(CodeCompletionString::Chunk::ChunkKind::CallParameterType,
                     Ty->getString(PO));

    // Look through optional types and type aliases to find out if we have
    // function/closure parameter type that is not an autoclosure.
    Ty = Ty->lookThroughAllAnyOptionalTypes();
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

  void addCallParameter(Identifier Name, Type Ty, bool IsVarArg) {
    addCallParameter(Name, Identifier(), Ty, IsVarArg);
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
        CodeCompletionString::Chunk::ChunkKind::OptionalMethodCallTail, "!");
    getLastChunk().setIsAnnotation();
  }

  void addTypeAnnotation(StringRef Type) {
    addChunkWithText(
        CodeCompletionString::Chunk::ChunkKind::TypeAnnotation, Type);
    getLastChunk().setIsAnnotation();
  }

  void addBraceStmtWithCursor() {
    addChunkWithTextNoCopy(
        CodeCompletionString::Chunk::ChunkKind::BraceStmtWithCursor, " {}");
  }
};

} // namespace ide
} // namespace swift

#endif // SWIFT_LIB_IDE_CODE_COMPLETION_RESULT_BUILDER_H

