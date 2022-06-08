//===--- CodeCompletionConsumer.cpp ---------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/IDE/CodeCompletionConsumer.h"
#include "swift/IDE/CodeCompletionCache.h"

using namespace swift;
using namespace swift::ide;

static MutableArrayRef<CodeCompletionResult *> copyCodeCompletionResults(
    CodeCompletionResultSink &targetSink, CodeCompletionCache::Value &source,
    bool onlyTypes, bool onlyPrecedenceGroups,
    const ExpectedTypeContext *TypeContext, const DeclContext *DC) {

  // We will be adding foreign results (from another sink) into TargetSink.
  // TargetSink should have an owning pointer to the allocator that keeps the
  // results alive.
  targetSink.ForeignAllocators.push_back(source.Allocator);
  auto startSize = targetSink.Results.size();

  std::function<bool(const ContextFreeCodeCompletionResult *)>
      shouldIncludeResult;
  if (onlyTypes) {
    shouldIncludeResult = [](const ContextFreeCodeCompletionResult *R) -> bool {
      if (R->getKind() != CodeCompletionResultKind::Declaration)
        return false;
      switch (R->getAssociatedDeclKind()) {
      case CodeCompletionDeclKind::Module:
      case CodeCompletionDeclKind::Class:
      case CodeCompletionDeclKind::Actor:
      case CodeCompletionDeclKind::Struct:
      case CodeCompletionDeclKind::Enum:
      case CodeCompletionDeclKind::Protocol:
      case CodeCompletionDeclKind::TypeAlias:
      case CodeCompletionDeclKind::AssociatedType:
      case CodeCompletionDeclKind::GenericTypeParam:
        return true;
      case CodeCompletionDeclKind::PrecedenceGroup:
      case CodeCompletionDeclKind::EnumElement:
      case CodeCompletionDeclKind::Constructor:
      case CodeCompletionDeclKind::Destructor:
      case CodeCompletionDeclKind::Subscript:
      case CodeCompletionDeclKind::StaticMethod:
      case CodeCompletionDeclKind::InstanceMethod:
      case CodeCompletionDeclKind::PrefixOperatorFunction:
      case CodeCompletionDeclKind::PostfixOperatorFunction:
      case CodeCompletionDeclKind::InfixOperatorFunction:
      case CodeCompletionDeclKind::FreeFunction:
      case CodeCompletionDeclKind::StaticVar:
      case CodeCompletionDeclKind::InstanceVar:
      case CodeCompletionDeclKind::LocalVar:
      case CodeCompletionDeclKind::GlobalVar:
        return false;
      }

      llvm_unreachable("Unhandled CodeCompletionDeclKind in switch.");
    };
  } else if (onlyPrecedenceGroups) {
    shouldIncludeResult = [](const ContextFreeCodeCompletionResult *R) -> bool {
      return R->getAssociatedDeclKind() ==
             CodeCompletionDeclKind::PrecedenceGroup;
    };
  } else {
    shouldIncludeResult = [](const ContextFreeCodeCompletionResult *R) -> bool {
      // PrecedenceGroups are only valid in 'onlyPrecedenceGroups'.
      return R->getAssociatedDeclKind() !=
             CodeCompletionDeclKind::PrecedenceGroup;
    };
  }

  USRBasedTypeContext USRTypeContext(TypeContext, source.USRTypeArena);

  for (auto contextFreeResult : source.Results) {
    if (!shouldIncludeResult(contextFreeResult)) {
      continue;
    }
    auto contextualResult = new (*targetSink.Allocator) CodeCompletionResult(
        *contextFreeResult, SemanticContextKind::OtherModule,
        CodeCompletionFlair(),
        /*numBytesToErase=*/0, TypeContext, DC, &USRTypeContext,
        ContextualNotRecommendedReason::None,
        CodeCompletionDiagnosticSeverity::None, /*DiagnosticMessage=*/"");
    targetSink.Results.push_back(contextualResult);
  }

  return llvm::makeMutableArrayRef(targetSink.Results.data() + startSize,
                                   targetSink.Results.size() - startSize);
}

void SimpleCachingCodeCompletionConsumer::handleResultsAndModules(
    CodeCompletionContext &context,
    ArrayRef<RequestedCachedModule> requestedModules,
    const ExpectedTypeContext *TypeContext, const DeclContext *DC) {

  // Use the current SourceFile as the DeclContext so that we can use it to
  // perform qualified lookup, and to get the correct visibility for
  // @testable imports. Also it cannot use 'DC' since it would apply decl
  // context changes to cached results.
  const SourceFile *SF = DC->getParentSourceFile();

  for (auto &R : requestedModules) {
    // FIXME(thread-safety): lock the whole AST context.  We might load a
    // module.
    llvm::Optional<CodeCompletionCache::ValueRefCntPtr> V =
        context.Cache.get(R.Key);
    if (!V.hasValue()) {
      // No cached results found. Fill the cache.
      V = context.Cache.createValue();
      // Temporary sink in which we gather the result. The cache value retains
      // the sink's allocator.
      CodeCompletionResultSink Sink;
      Sink.annotateResult = context.getAnnotateResult();
      Sink.addInitsToTopLevel = context.getAddInitsToTopLevel();
      Sink.enableCallPatternHeuristics = context.getCallPatternHeuristics();
      Sink.includeObjectLiterals = context.includeObjectLiterals();
      Sink.addCallWithNoDefaultArgs = context.addCallWithNoDefaultArgs();
      Sink.setProduceContextFreeResults((*V)->USRTypeArena);
      lookupCodeCompletionResultsFromModule(Sink, R.TheModule, R.Key.AccessPath,
                                            R.Key.ResultsHaveLeadingDot, SF);
      (*V)->Allocator = Sink.Allocator;
      auto &CachedResults = (*V)->Results;
      CachedResults.reserve(Sink.Results.size());
      // Instead of copying the context free results out of the sink's allocator
      // retain the sink's entire allocator (which also includes the contextual
      // properities) and simply store pointers to the context free results that
      // back the contextual results.
      for (auto Result : Sink.Results) {
        assert(
            Result->getContextFreeResult().getResultType().isBackedByUSRs() &&
            "Results stored in the cache should have their result types backed "
            "by a USR because the cache might outlive the ASTContext the "
            "results were created from.");
        CachedResults.push_back(&Result->getContextFreeResult());
      }
      context.Cache.set(R.Key, *V);
    }
    assert(V.hasValue());
    auto newItems =
        copyCodeCompletionResults(context.getResultSink(), **V, R.OnlyTypes,
                                  R.OnlyPrecedenceGroups, TypeContext, DC);
    postProcessCompletionResults(newItems, context.CodeCompletionKind, DC,
                                 &context.getResultSink());
  }

  handleResults(context);
}
