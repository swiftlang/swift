//===--- CodeCompletionStringBuilder.h ----------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IDE_CODECOMPLETIONSTRINGBUILDER_H
#define SWIFT_IDE_CODECOMPLETIONSTRINGBUILDER_H

#include "swift/AST/Types.h"
#include "swift/IDE/CodeCompletionString.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/Allocator.h"

namespace swift {
namespace ide {

class CodeCompletionStringPrinter;

/// For printing in code completion strings, replace archetypes with
/// protocol compositions.
///
/// FIXME: Perhaps this should be an option in PrintOptions instead.
Type eraseArchetypes(Type type, GenericSignature genericSig);

bool hasInterestingDefaultValue(const ParamDecl *param);

class CodeCompletionStringBuilder {
  friend CodeCompletionStringPrinter;

  llvm::BumpPtrAllocator &Allocator;
  unsigned CurrentNestingLevel = 0;
  SmallVector<CodeCompletionString::Chunk, 4> Chunks;

  bool AnnotateResults;

public:
  CodeCompletionStringBuilder(llvm::BumpPtrAllocator &Allocator,
                              bool AnnotateResults)
      : Allocator(Allocator), AnnotateResults(AnnotateResults) {}

private:
  void addChunkWithText(CodeCompletionString::Chunk::ChunkKind Kind,
                        StringRef Text);

  void addChunkWithTextNoCopy(CodeCompletionString::Chunk::ChunkKind Kind,
                              StringRef Text) {
    Chunks.push_back(CodeCompletionString::Chunk::createWithText(
        Kind, CurrentNestingLevel, Text));
  }

  void addSimpleChunk(CodeCompletionString::Chunk::ChunkKind Kind) {
    Chunks.push_back(
        CodeCompletionString::Chunk::createSimple(Kind, CurrentNestingLevel));
  }

  CodeCompletionString::Chunk &getLastChunk() { return Chunks.back(); }

public:
  void withNestedGroup(CodeCompletionString::Chunk::ChunkKind Kind,
                       llvm::function_ref<void()> body);

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
    case AccessLevel::Package:
      addChunkWithTextNoCopy(
          CodeCompletionString::Chunk::ChunkKind::AccessControlKeyword,
          "package ");
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

  void addRequiredKeyword() {
    addChunkWithTextNoCopy(
        CodeCompletionString::Chunk::ChunkKind::AccessControlKeyword,
        "required ");
  }

  void addOverrideKeyword() {
    addChunkWithTextNoCopy(
        CodeCompletionString::Chunk::ChunkKind::OverrideKeyword, "override ");
  }

  void addDeclIntroducer(StringRef Text) {
    addChunkWithText(CodeCompletionString::Chunk::ChunkKind::DeclIntroducer,
                     Text);
  }

  void addBaseName(StringRef Text) {
    addChunkWithText(CodeCompletionString::Chunk::ChunkKind::BaseName, Text);
  }

  void addKeyword(StringRef Text) {
    addChunkWithText(CodeCompletionString::Chunk::ChunkKind::Keyword, Text);
  }

  void addTextChunk(StringRef Text) {
    addChunkWithText(CodeCompletionString::Chunk::ChunkKind::Text, Text);
  }

  void addAnnotatedTextChunk(StringRef Text) {
    addTextChunk(Text);
    getLastChunk().setIsAnnotation();
  }

  void addAnnotatedThrows() {
    addThrows();
    getLastChunk().setIsAnnotation();
  }

  void addThrows() {
    addChunkWithTextNoCopy(
        CodeCompletionString::Chunk::ChunkKind::EffectsSpecifierKeyword,
        " throws");
  }

  void addAnnotatedAsync() {
    addAsync();
    getLastChunk().setIsAnnotation();
  }

  void addAsync() {
    addChunkWithTextNoCopy(
        CodeCompletionString::Chunk::ChunkKind::EffectsSpecifierKeyword,
        " async");
  }

  void addAnnotatedRethrows() {
    addRethrows();
    getLastChunk().setIsAnnotation();
  }

  void addRethrows() {
    addChunkWithTextNoCopy(
        CodeCompletionString::Chunk::ChunkKind::EffectsSpecifierKeyword,
        " rethrows");
  }

  void addAnnotatedLeftParen() {
    addLeftParen();
    getLastChunk().setIsAnnotation();
  }

  void addLeftParen() {
    addChunkWithTextNoCopy(CodeCompletionString::Chunk::ChunkKind::LeftParen,
                           "(");
  }

  void addAnnotatedRightParen() {
    addRightParen();
    getLastChunk().setIsAnnotation();
  }

  void addRightParen() {
    addChunkWithTextNoCopy(CodeCompletionString::Chunk::ChunkKind::RightParen,
                           ")");
  }

  void addAnnotatedLeftBracket() {
    addLeftBracket();
    getLastChunk().setIsAnnotation();
  }

  void addLeftBracket() {
    addChunkWithTextNoCopy(CodeCompletionString::Chunk::ChunkKind::LeftBracket,
                           "[");
  }

  void addAnnotatedRightBracket() {
    addRightBracket();
    getLastChunk().setIsAnnotation();
  }

  void addRightBracket() {
    addChunkWithTextNoCopy(CodeCompletionString::Chunk::ChunkKind::RightBracket,
                           "]");
  }

  void addLeftAngle() {
    addChunkWithTextNoCopy(CodeCompletionString::Chunk::ChunkKind::LeftAngle,
                           "<");
  }

  void addRightAngle() {
    addChunkWithTextNoCopy(CodeCompletionString::Chunk::ChunkKind::RightAngle,
                           ">");
  }

  void addLeadingDot() { addDot(); }

  void addDot() {
    addChunkWithTextNoCopy(CodeCompletionString::Chunk::ChunkKind::Dot, ".");
  }

  void addEllipsis() {
    addChunkWithTextNoCopy(CodeCompletionString::Chunk::ChunkKind::Ellipsis,
                           "...");
  }

  void addComma() {
    addChunkWithTextNoCopy(CodeCompletionString::Chunk::ChunkKind::Comma, ", ");
  }

  void addExclamationMark() {
    addChunkWithTextNoCopy(
        CodeCompletionString::Chunk::ChunkKind::ExclamationMark, "!");
  }

  void addQuestionMark() {
    addChunkWithTextNoCopy(CodeCompletionString::Chunk::ChunkKind::QuestionMark,
                           "?");
  }

  void addEqual() {
    addChunkWithTextNoCopy(CodeCompletionString::Chunk::ChunkKind::Equal, "=");
  }

  void addDeclAttrParamKeyword(StringRef Name, ArrayRef<StringRef> Parameters,
                               StringRef Annotation, bool NeedSpecify) {
    addChunkWithText(
        CodeCompletionString::Chunk::ChunkKind::DeclAttrParamKeyword, Name);
    if (!Parameters.empty()) {
      addLeftParen();
      for (auto Parameter : Parameters) {
        addSimpleNamedParameter(Parameter);
      }
      addRightParen();
    }
    if (NeedSpecify)
      addChunkWithText(
          CodeCompletionString::Chunk::ChunkKind::DeclAttrParamColon, ": ");
    if (!Annotation.empty())
      addTypeAnnotation(Annotation);
  }

  void addDeclAttrKeyword(StringRef Name, StringRef Annotation) {
    addChunkWithText(CodeCompletionString::Chunk::ChunkKind::DeclAttrKeyword,
                     Name);
    if (!Annotation.empty())
      addTypeAnnotation(Annotation);
  }

  void addAttributeKeyword(StringRef Name, StringRef Annotation) {
    addChunkWithText(CodeCompletionString::Chunk::ChunkKind::Attribute, Name);
    if (!Annotation.empty())
      addTypeAnnotation(Annotation);
  }

  StringRef escapeWithBackticks(StringRef Word,
                                llvm::SmallString<16> &Escaped) {
    Escaped.append("`");
    Escaped.append(Word);
    Escaped.append("`");
    return Escaped;
  }

  StringRef escapeKeyword(StringRef Word, bool escapeAllKeywords,
                          llvm::SmallString<16> &EscapedKeyword);

  void addCallParameterColon() {
    addChunkWithText(CodeCompletionString::Chunk::ChunkKind::CallArgumentColon,
                     ": ");
  }

  void addSimpleNamedParameter(StringRef name) {
    withNestedGroup(
        CodeCompletionString::Chunk::ChunkKind::CallArgumentBegin, [&] {
          // Use internal, since we don't want the name to be outside the
          // placeholder.
          addChunkWithText(
              CodeCompletionString::Chunk::ChunkKind::CallArgumentInternalName,
              name);
        });
  }

  void addSimpleTypedParameter(StringRef Annotation, bool IsVarArg = false) {
    withNestedGroup(
        CodeCompletionString::Chunk::ChunkKind::CallArgumentBegin, [&] {
          addChunkWithText(
              CodeCompletionString::Chunk::ChunkKind::CallArgumentType,
              Annotation);
          if (IsVarArg)
            addEllipsis();
        });
  }

  void addCallArgument(Identifier Name, Identifier LocalName, Type Ty,
                       Type ContextTy, bool IsVarArg, bool IsInOut, bool IsIUO,
                       bool IsAutoClosure, bool IsLabeledTrailingClosure,
                       bool IsForOperator, bool HasDefault);

  void addCallArgument(Identifier Name, Type Ty, Type ContextTy = Type(),
                       bool IsForOperator = false) {
    addCallArgument(Name, Identifier(), Ty, ContextTy,
                    /*IsVarArg=*/false, /*IsInOut=*/false, /*IsIUO=*/false,
                    /*IsAutoClosure=*/false,
                    /*IsLabeledTrailingClosure=*/false, IsForOperator,
                    /*HasDefault=*/false);
  }

  void addGenericParameter(StringRef Name) {
    withNestedGroup(
        CodeCompletionString::Chunk::ChunkKind::GenericParameterBegin, [&] {
          addChunkWithText(
              CodeCompletionString::Chunk::ChunkKind::GenericParameterName,
              Name);
        });
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
    addChunkWithText(CodeCompletionString::Chunk::ChunkKind::TypeAnnotation,
                     Type);
    getLastChunk().setIsAnnotation();
  }

  void addTypeAnnotation(Type T, const PrintOptions &PO,
                         NonRecursivePrintOptions nrOptions = std::nullopt,
                         StringRef suffix = "");

  void addBraceStmtWithCursor(StringRef Description = "") {
    addChunkWithText(
        CodeCompletionString::Chunk::ChunkKind::BraceStmtWithCursor,
        Description);
  }

  void addWhitespace(StringRef space) {
    addChunkWithText(CodeCompletionString::Chunk::ChunkKind::Whitespace, space);
  }

  void addAnnotatedWhitespace(StringRef space) {
    addWhitespace(space);
    getLastChunk().setIsAnnotation();
  }

  /// Adds a \c DeclBaseName and escapes identifiers with backticks if necessary
  void addValueBaseName(DeclBaseName Name, bool IsMember);

  /// Build argument patterns for calling. Returns \c true if any content was
  /// added to \p Builder. If \p declParams is non-empty, the size must match
  /// with \p typeParams.
  bool addCallArgumentPatterns(ArrayRef<AnyFunctionType::Param> typeParams,
                               ArrayRef<const ParamDecl *> declParams,
                               const DeclContext *DC,
                               GenericSignature genericSig,
                               bool includeDefaultArgs = true);

  /// Build argument patterns for calling. Returns \c true if any content was
  /// added to \p Builder. If \p Params is non-nullptr, \F
  bool addCallArgumentPatterns(const AnyFunctionType *AFT,
                               const ParameterList *Params,
                               const DeclContext *DC,
                               GenericSignature genericSig,
                               bool includeDefaultArgs = true);

  void addTypeAnnotation(Type T, const DeclContext *DC,
                         GenericSignature genericSig = GenericSignature());

  void addTypeAnnotationForImplicitlyUnwrappedOptional(
      Type T, const DeclContext *DC,
      GenericSignature genericSig = GenericSignature(),
      bool dynamicOrOptional = false);

  void addEffectsSpecifiers(const AnyFunctionType *AFT,
                            const AbstractFunctionDecl *AFD,
                            bool forceAsync = false);

  CodeCompletionString *createCompletionString() {
    return CodeCompletionString::create(Allocator, Chunks);
  }
};

} // end namespace ide
} // end namespace swift

#endif // SWIFT_IDE_CODECOMPLETIONSTRINGBUILDER_H
