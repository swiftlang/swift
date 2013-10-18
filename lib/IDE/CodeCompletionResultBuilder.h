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
  CodeCompletionContext &Context;
  CodeCompletionResult::ResultKind Kind;
  SemanticContextKind SemanticContext;
  const Decl *AssociatedDecl = nullptr;
  unsigned CurrentNestingLevel = 0;
  SmallVector<CodeCompletionString::Chunk, 4> Chunks;
  bool HasLeadingDot = false;

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
  CodeCompletionResultBuilder(CodeCompletionContext &Context,
                              CodeCompletionResult::ResultKind Kind,
                              SemanticContextKind SemanticContext)
      : Context(Context), Kind(Kind), SemanticContext(SemanticContext) {
  }

  ~CodeCompletionResultBuilder() {
    finishResult();
  }

  void setAssociatedDecl(const Decl *D) {
    assert(Kind == CodeCompletionResult::ResultKind::Declaration);
    AssociatedDecl = D;
  }

  void addTextChunk(StringRef Text) {
    addChunkWithText(CodeCompletionString::Chunk::ChunkKind::Text, Text);
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
    HasLeadingDot = true;
    addDot();
  }

  void addDot() {
    addChunkWithTextNoCopy(CodeCompletionString::Chunk::ChunkKind::Dot, ".");
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


  void addCallParameter(Identifier Name, StringRef Type) {
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
    addChunkWithText(
        CodeCompletionString::Chunk::ChunkKind::CallParameterType, Type);
    CurrentNestingLevel--;
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
        "?|!");
    getLastChunk().setIsAnnotation();
  }

  void addTypeAnnotation(StringRef Type) {
    addChunkWithText(
        CodeCompletionString::Chunk::ChunkKind::TypeAnnotation, Type);
    getLastChunk().setIsAnnotation();
  }
};

} // namespace ide
} // namespace swift

#endif // SWIFT_LIB_IDE_CODE_COMPLETION_RESULT_BUILDER_H

