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

#include "CodeCompletionStringBuilder.h"
#include "swift/AST/Types.h"
#include "swift/Basic/LLVM.h"
#include "swift/IDE/CodeCompletionResult.h"
#include "swift/IDE/CodeCompletionResultSink.h"

namespace clang {
class Module;
}

namespace swift {
class Decl;
class ModuleDecl;

namespace ide {

class CodeCompletionResultBuilder : public CodeCompletionStringBuilder {
  CodeCompletionResultSink &Sink;
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
                              SemanticContextKind SemanticContext)
      : CodeCompletionStringBuilder(*Sink.Allocator, Sink.annotateResult),
        Sink(Sink), Kind(Kind), SemanticContext(SemanticContext) {}

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

  void setBriefDocComment(StringRef comment) {
    BriefDocComment = comment;
  }
};

} // namespace ide
} // namespace swift

#endif // SWIFT_LIB_IDE_CODE_COMPLETION_RESULT_BUILDER_H
