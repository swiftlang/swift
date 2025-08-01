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

#include "swift/AST/Types.h"
#include "swift/Basic/LLVM.h"
#include "swift/IDE/CodeCompletionResult.h"
#include "swift/IDE/CodeCompletionResultSink.h"
#include "swift/IDE/CodeCompletionStringBuilder.h"

namespace clang {
class Module;
} // namespace clang

namespace swift {
class Decl;
class ModuleDecl;

namespace ide {

class CodeCompletionResultBuilder {
  CodeCompletionResultSink &Sink;
  CodeCompletionStringBuilder StringBuilder;
  CodeCompletionResultKind Kind;
  SemanticContextKind SemanticContext;
  CodeCompletionFlair Flair;
  unsigned NumBytesToErase = 0;
  const Decl *AssociatedDecl = nullptr;
  bool HasAsyncAlternative = false;
  std::optional<CodeCompletionLiteralKind> LiteralKind;
  CodeCompletionKeywordKind KeywordKind = CodeCompletionKeywordKind::None;
  llvm::PointerUnion<const ModuleDecl *, const clang::Module *>
      CurrentModule;
  bool Cancelled = false;
  ContextFreeNotRecommendedReason ContextFreeNotRecReason =
      ContextFreeNotRecommendedReason::None;
  ContextualNotRecommendedReason ContextualNotRecReason =
      ContextualNotRecommendedReason::None;
  StringRef BriefDocComment;

  /// The result type that this completion item produces.
  CodeCompletionResultType ResultType = CodeCompletionResultType::unknown();

  /// The context in which this completion item is used. Used to compute the
  /// type relation to \c ResultType.
  const ExpectedTypeContext *TypeContext = nullptr;
  const DeclContext *DC = nullptr;
  bool CanCurrDeclContextHandleAsync = false;

  CodeCompletionResult *takeResult();
  void finishResult();

public:
  CodeCompletionResultBuilder(CodeCompletionResultSink &Sink,
                              CodeCompletionResultKind Kind,
                              SemanticContextKind SemanticContext,
                              const DeclContext *CurrDeclContext = nullptr)
      : Sink(Sink),
        StringBuilder(*Sink.Allocator, CurrDeclContext, Sink.annotateResult,
                      Sink.addCallWithNoDefaultArgs),
        Kind(Kind), SemanticContext(SemanticContext) {}

  ~CodeCompletionResultBuilder() {
    finishResult();
  }

  void cancel() {
    Cancelled = true;
  }

  /// Annotated results are requested by the client.
  ///
  /// This affects the structure of the CodeCompletionString.
  bool shouldAnnotateResults() {
    return Sink.annotateResult;
  }

  void setNumBytesToErase(unsigned N) {
    NumBytesToErase = N;
  }

  void setAssociatedDecl(const Decl *D);

  void setHasAsyncAlternative(bool HasAsyncAlternative) {
    this->HasAsyncAlternative = HasAsyncAlternative;
  }

  void setLiteralKind(CodeCompletionLiteralKind kind) { LiteralKind = kind; }
  void setKeywordKind(CodeCompletionKeywordKind kind) { KeywordKind = kind; }
  void setContextFreeNotRecommended(ContextFreeNotRecommendedReason Reason) {
    ContextFreeNotRecReason = Reason;
  }
  void setContextualNotRecommended(ContextualNotRecommendedReason Reason) {
    ContextualNotRecReason = Reason;
  }

  void addFlair(CodeCompletionFlair Options) {
    Flair |= Options;
  }

  /// Indicate that the code completion item does not produce something with a
  /// sensible result type, like a keyword or a method override suggestion.
  void setResultTypeNotApplicable() {
    ResultType = CodeCompletionResultType::notApplicable();
  }

  /// Set the result type of this code completion item and the context that the
  /// item may be used in.
  /// This is not a single unique type because for code completion we consider
  /// e.g. \c Int as producing both an \c Int metatype and an \c Int instance
  /// type.
  void setResultTypes(ArrayRef<Type> ResultTypes) {
    this->ResultType = CodeCompletionResultType(ResultTypes);
  }

  /// Set context in which this code completion item occurs. Used to compute the
  /// item's type relation.
  void setTypeContext(const ExpectedTypeContext &TypeContext,
                      const DeclContext *DC) {
    this->TypeContext = &TypeContext;
    this->DC = DC;
  }

  void setCanCurrDeclContextHandleAsync(bool CanCurrDeclContextHandleAsync) {
    this->CanCurrDeclContextHandleAsync = CanCurrDeclContextHandleAsync;
  }

  void setBriefDocComment(StringRef comment) { BriefDocComment = comment; }

  // CodeCompletionStringBuilder methods
  CodeCompletionStringBuilder &getStringBuilder() { return StringBuilder; }

  void withNestedGroup(CodeCompletionString::Chunk::ChunkKind Kind,
                       llvm::function_ref<void()> body) {
    StringBuilder.withNestedGroup(Kind, body);
  }

  void addAccessControlKeyword(AccessLevel Access) {
    StringBuilder.addAccessControlKeyword(Access);
  }

  void addRequiredKeyword() { StringBuilder.addRequiredKeyword(); }

  void addOverrideKeyword() { StringBuilder.addOverrideKeyword(); }

  void addDeclIntroducer(StringRef Text) {
    StringBuilder.addDeclIntroducer(Text);
  }

  void addBaseName(StringRef Text) { StringBuilder.addBaseName(Text); }

  void addKeyword(StringRef Text) { StringBuilder.addKeyword(Text); }

  void addTextChunk(StringRef Text) { StringBuilder.addTextChunk(Text); }

  void addAnnotatedTextChunk(StringRef Text) {
    StringBuilder.addAnnotatedTextChunk(Text);
  }

  void addAnnotatedThrows() { StringBuilder.addAnnotatedThrows(); }

  void addThrows() { StringBuilder.addThrows(); }

  void addAnnotatedAsync() { StringBuilder.addAnnotatedAsync(); }

  void addAsync() { StringBuilder.addAsync(); }

  void addAnnotatedRethrows() { StringBuilder.addAnnotatedRethrows(); }

  void addRethrows() { StringBuilder.addRethrows(); }

  void addAnnotatedLeftParen() { StringBuilder.addAnnotatedLeftParen(); }

  void addLeftParen() { StringBuilder.addLeftParen(); }

  void addAnnotatedRightParen() { StringBuilder.addAnnotatedRightParen(); }

  void addRightParen() { StringBuilder.addRightParen(); }

  void addAnnotatedLeftBracket() { StringBuilder.addAnnotatedLeftBracket(); }

  void addLeftBracket() { StringBuilder.addLeftBracket(); }

  void addAnnotatedRightBracket() { StringBuilder.addAnnotatedRightBracket(); }

  void addRightBracket() { StringBuilder.addRightBracket(); }

  void addLeftAngle() { StringBuilder.addLeftAngle(); }

  void addRightAngle() { StringBuilder.addRightAngle(); }

  void addLeadingDot() { StringBuilder.addLeadingDot(); }

  void addDot() { StringBuilder.addDot(); }

  void addEllipsis() { StringBuilder.addEllipsis(); }

  void addComma() { StringBuilder.addComma(); }

  void addExclamationMark() { StringBuilder.addExclamationMark(); }

  void addQuestionMark() { StringBuilder.addQuestionMark(); }

  void addEqual() { StringBuilder.addEqual(); }

  void addDeclAttrParamKeyword(StringRef Name, ArrayRef<StringRef> Parameters,
                               StringRef Annotation, bool NeedSpecify) {
    StringBuilder.addDeclAttrParamKeyword(Name, Parameters, Annotation,
                                          NeedSpecify);
  }

  void addDeclAttrKeyword(StringRef Name, StringRef Annotation) {
    StringBuilder.addDeclAttrKeyword(Name, Annotation);
  }

  void addAttributeKeyword(StringRef Name, StringRef Annotation) {
    StringBuilder.addAttributeKeyword(Name, Annotation);
  }

  StringRef escapeWithBackticks(StringRef Word,
                                llvm::SmallString<16> &Escaped) {
    return StringBuilder.escapeWithBackticks(Word, Escaped);
  }

  StringRef escapeKeyword(StringRef Word, bool escapeAllKeywords,
                          llvm::SmallString<16> &EscapedKeyword) {
    return StringBuilder.escapeKeyword(Word, escapeAllKeywords, EscapedKeyword);
  }

  void addCallParameterColon() { StringBuilder.addCallParameterColon(); }

  void addSimpleNamedParameter(StringRef name) {
    StringBuilder.addSimpleNamedParameter(name);
  }

  void addSimpleTypedParameter(StringRef Annotation, bool IsVarArg = false) {
    StringBuilder.addSimpleTypedParameter(Annotation, IsVarArg);
  }

  void addCallArgument(Identifier Name, Identifier LocalName, Type Ty,
                       Type ContextTy, bool IsVarArg, bool IsInOut, bool IsIUO,
                       bool IsAutoClosure, bool IsLabeledTrailingClosure,
                       bool IsForOperator, bool HasDefault) {
    StringBuilder.addCallArgument(
        Name, LocalName, Ty, ContextTy, IsVarArg, IsInOut, IsIUO, IsAutoClosure,
        IsLabeledTrailingClosure, IsForOperator, HasDefault);
  }

  void addCallArgument(Identifier Name, Type Ty, Type ContextTy = Type(),
                       bool IsForOperator = false) {
    StringBuilder.addCallArgument(Name, Ty, ContextTy, IsForOperator);
  }

  void addGenericParameter(StringRef Name) {
    StringBuilder.addGenericParameter(Name);
  }

  void addDynamicLookupMethodCallTail() {
    StringBuilder.addDynamicLookupMethodCallTail();
  }

  void addOptionalMethodCallTail() {
    StringBuilder.addOptionalMethodCallTail();
  }

  void addTypeAnnotation(StringRef Type) {
    StringBuilder.addTypeAnnotation(Type);
  }

  void addTypeAnnotation(Type T, const PrintOptions &PO,
                         NonRecursivePrintOptions nrOptions = std::nullopt,
                         StringRef suffix = "") {
    StringBuilder.addTypeAnnotation(T, PO, nrOptions, suffix);
  }

  void addBraceStmtWithCursor(StringRef Description = "") {
    StringBuilder.addBraceStmtWithCursor(Description);
  }

  void addWhitespace(StringRef space) { StringBuilder.addWhitespace(space); }

  void addAnnotatedWhitespace(StringRef space) {
    StringBuilder.addAnnotatedWhitespace(space);
  }
};

} // namespace ide
} // namespace swift

#endif // SWIFT_LIB_IDE_CODE_COMPLETION_RESULT_BUILDER_H
