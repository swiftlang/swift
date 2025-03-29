//===--- CodeCompletionDiagnostics.cpp ------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "CodeCompletionDiagnostics.h"

#include "swift/AST/ASTContext.h"
#include "swift/AST/AvailabilityInference.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsIDE.h"
#include "swift/AST/DiagnosticsSema.h"

using namespace swift;
using namespace ide;

namespace {

CodeCompletionDiagnosticSeverity getSeverity(DiagnosticKind DiagKind) {
  switch (DiagKind) {
  case DiagnosticKind::Error:
    return CodeCompletionDiagnosticSeverity::Error;
    break;
  case DiagnosticKind::Warning:
    return CodeCompletionDiagnosticSeverity::Warning;
    break;
  case DiagnosticKind::Remark:
    return CodeCompletionDiagnosticSeverity::Remark;
    break;
  case DiagnosticKind::Note:
    return CodeCompletionDiagnosticSeverity::Note;
    break;
  }
  llvm_unreachable("unhandled DiagnosticKind");
}

class CodeCompletionDiagnostics {
  const ASTContext &Ctx;
  DiagnosticEngine &Engine;

public:
  CodeCompletionDiagnostics(const ASTContext &Ctx)
      : Ctx(Ctx), Engine(Ctx.Diags) {}

  template <typename... ArgTypes>
  bool
  getDiagnostics(CodeCompletionDiagnosticSeverity &severity,
                 llvm::raw_ostream &Out, Diag<ArgTypes...> ID,
                 typename swift::detail::PassArgument<ArgTypes>::type... VArgs);

  bool getDiagnosticForDeprecated(const ValueDecl *D,
                                  SemanticAvailableAttr Attr,
                                  bool isSoftDeprecated,
                                  CodeCompletionDiagnosticSeverity &severity,
                                  llvm::raw_ostream &Out);

  bool getDiagnosticForDeprecated(const ValueDecl *D,
                                  CodeCompletionDiagnosticSeverity &severity,
                                  llvm::raw_ostream &Out);
};

template <typename... ArgTypes>
bool CodeCompletionDiagnostics::getDiagnostics(
    CodeCompletionDiagnosticSeverity &severity, llvm::raw_ostream &Out,
    Diag<ArgTypes...> ID,
    typename swift::detail::PassArgument<ArgTypes>::type... VArgs) {
  DiagID id = ID.ID;
  std::vector<DiagnosticArgument> DiagArgs{std::move(VArgs)...};
  auto format = Engine.getFormatStringForDiagnostic(id);
  DiagnosticEngine::formatDiagnosticText(Out, format, DiagArgs);
  severity = getSeverity(Engine.declaredDiagnosticKindFor(id));

  return false;
}

bool CodeCompletionDiagnostics::getDiagnosticForDeprecated(
    const ValueDecl *D, SemanticAvailableAttr Attr, bool isSoftDeprecated,
    CodeCompletionDiagnosticSeverity &severity, llvm::raw_ostream &Out) {
  // FIXME: Code completion doesn't offer accessors. It only emits 'VarDecl's.
  // So getter/setter specific availability doesn't work in code completion.

  auto Domain = Attr.getDomain();
  auto DeprecatedRange = Attr.getDeprecatedRange(Ctx).value();
  auto Message = Attr.getMessage();
  auto NewName = Attr.getRename();
  if (!isSoftDeprecated) {
    if (Message.empty() && NewName.empty()) {
      getDiagnostics(severity, Out, diag::availability_deprecated, D,
                     Attr.isPlatformSpecific(), Domain,
                     DeprecatedRange.hasMinimumVersion(), DeprecatedRange,
                     /*message*/ StringRef());
    } else if (!Message.empty()) {
      EncodedDiagnosticMessage EncodedMessage(Message);
      getDiagnostics(severity, Out, diag::availability_deprecated, D,
                     Attr.isPlatformSpecific(), Domain,
                     DeprecatedRange.hasMinimumVersion(), DeprecatedRange,
                     EncodedMessage.Message);
    } else {
      getDiagnostics(severity, Out, diag::availability_deprecated_rename, D,
                     Attr.isPlatformSpecific(), Domain,
                     DeprecatedRange.hasMinimumVersion(), DeprecatedRange,
                     false,
                     /*ReplaceKind*/ 0, NewName);
    }
  } else {
    // '100000' is used as a version number in API that will be deprecated in an
    // upcoming release. This number is to match the 'API_TO_BE_DEPRECATED'
    // macro defined in Darwin platforms.
    bool isDistantFuture = DeprecatedRange.isContainedIn(
        AvailabilityRange(llvm::VersionTuple(100000)));

    if (Message.empty() && NewName.empty()) {
      getDiagnostics(severity, Out, diag::ide_availability_softdeprecated, D,
                     Attr.isPlatformSpecific(), Domain, !isDistantFuture,
                     DeprecatedRange,
                     /*message*/ StringRef());
    } else if (!Message.empty()) {
      EncodedDiagnosticMessage EncodedMessage(Message);
      getDiagnostics(severity, Out, diag::ide_availability_softdeprecated, D,
                     Attr.isPlatformSpecific(), Domain, !isDistantFuture,
                     DeprecatedRange, EncodedMessage.Message);
    } else {
      getDiagnostics(severity, Out,
                     diag::ide_availability_softdeprecated_rename, D,
                     Attr.isPlatformSpecific(), Domain, !isDistantFuture,
                     DeprecatedRange, NewName);
    }
  }
  return false;
}

bool CodeCompletionDiagnostics::getDiagnosticForDeprecated(
    const ValueDecl *D, CodeCompletionDiagnosticSeverity &severity,
    llvm::raw_ostream &Out) {
  if (auto attr = D->getDeprecatedAttr())
    return getDiagnosticForDeprecated(D, *attr, false, severity, Out);

  if (auto attr = D->getSoftDeprecatedAttr())
    return getDiagnosticForDeprecated(D, *attr, true, severity, Out);

  return true;
}

} // namespace

bool swift::ide::getContextFreeCompletionDiagnostics(
    ContextFreeNotRecommendedReason Reason, const ValueDecl *D,
    CodeCompletionDiagnosticSeverity &Severity, llvm::raw_ostream &Out) {
  CodeCompletionDiagnostics Diag(D->getASTContext());
  switch (Reason) {
  case ContextFreeNotRecommendedReason::Deprecated:
  case ContextFreeNotRecommendedReason::SoftDeprecated:
    return Diag.getDiagnosticForDeprecated(D, Severity, Out);
  case ContextFreeNotRecommendedReason::None:
    llvm_unreachable("invalid not recommended reason");
  }
  return true;
}

bool swift::ide::getContextualCompletionDiagnostics(
    ContextualNotRecommendedReason Reason, StringRef NameForDiagnostics,
    CodeCompletionDiagnosticSeverity &Severity, llvm::raw_ostream &Out,
    const ASTContext &Ctx) {
  CodeCompletionDiagnostics Diag(Ctx);
  switch (Reason) {
  case ContextualNotRecommendedReason::RedundantImport:
    return Diag.getDiagnostics(Severity, Out, diag::ide_redundant_import,
                               NameForDiagnostics);
  case ContextualNotRecommendedReason::RedundantImportIndirect:
    return Diag.getDiagnostics(
        Severity, Out, diag::ide_redundant_import_indirect, NameForDiagnostics);
  case ContextualNotRecommendedReason::VariableUsedInOwnDefinition:
    return Diag.getDiagnostics(Severity, Out,
                               diag::ide_recursive_accessor_reference,
                               NameForDiagnostics, /*"getter"*/ 0);
  case ContextualNotRecommendedReason::NonAsyncAlternativeUsedInAsyncContext:
    return Diag.getDiagnostics(
        Severity, Out, diag::ide_has_async_alternative, NameForDiagnostics);
  case ContextualNotRecommendedReason::None:
    llvm_unreachable("invalid not recommended reason");
  }
  return true;
}
