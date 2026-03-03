//===--- TypeCheckCodeCompletion.cpp - Type Checking for Code Completion --===//
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
//
// This file implements various entry points for use by lib/IDE/.
//
//===----------------------------------------------------------------------===//

#include "CodeSynthesis.h"
#include "MiscDiagnostics.h"
#include "TypeCheckObjC.h"
#include "TypeCheckType.h"
#include "TypeChecker.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Attr.h"
#include "swift/AST/DiagnosticSuppression.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/ImportCache.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/Type.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Basic/Statistic.h"
#include "swift/Parse/IDEInspectionCallbacks.h"
#include "swift/Parse/Lexer.h"
#include "swift/Sema/CompletionContextFinder.h"
#include "swift/Sema/ConstraintSystem.h"
#include "swift/Sema/IDETypeChecking.h"
#include "swift/Strings.h"
#include "swift/Subsystems.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/ADT/Twine.h"
#include <algorithm>

using namespace swift;
using namespace constraints;

static bool hasTypeForCompletion(Solution &solution,
                                 CompletionContextFinder &contextAnalyzer) {
  if (contextAnalyzer.hasCompletionExpr()) {
    return solution.hasType(contextAnalyzer.getCompletionExpr());
  } else {
    return solution.hasType(
        contextAnalyzer.getKeyPathContainingCompletionComponent(),
        contextAnalyzer.getKeyPathCompletionComponentIndex());
  }
}

void TypeChecker::filterSolutionsForCodeCompletion(
    SmallVectorImpl<Solution> &solutions,
    CompletionContextFinder &contextAnalyzer) {
  // Ignore solutions that didn't end up involving the completion (e.g. due to
  // a fix to skip over/ignore it).
  llvm::erase_if(solutions, [&](Solution &S) {
    if (hasTypeForCompletion(S, contextAnalyzer))
      return false;
    // FIXME: Technically this should never happen, but it currently does in
    // result builder contexts. Re-evaluate if we can assert here when we have
    // multi-statement closure checking for result builders.
    return true;
  });

  if (solutions.size() <= 1)
    return;

  Score minScore = std::min_element(solutions.begin(), solutions.end(),
                                    [](const Solution &a, const Solution &b) {
    return a.getFixedScore() < b.getFixedScore();
  })->getFixedScore();

  llvm::erase_if(solutions, [&](const Solution &S) {
    return S.getFixedScore().Data[SK_Fix] > minScore.Data[SK_Fix];
  });
}

bool TypeChecker::typeCheckForCodeCompletion(
    SyntacticElementTarget &target,
    llvm::function_ref<void(const Solution &)> callback) {
  auto *DC = target.getDeclContext();
  auto &Context = DC->getASTContext();
  // First of all, let's check whether given target expression
  // does indeed have the code completion location in it.
  {
    auto range = target.getSourceRange();
    if (range.isInvalid() ||
        !containsIDEInspectionTarget(range, Context.SourceMgr))
      return false;
  }

  CompletionContextFinder contextAnalyzer(target);

  // If there was no completion expr (e.g. if the code completion location was
  // among tokens that were skipped over during parser error recovery) bail.
  if (!contextAnalyzer.hasCompletion())
    return false;

  ConstraintSystemOptions options;
  options |= ConstraintSystemFlags::AllowFixes;
  options |= ConstraintSystemFlags::SuppressDiagnostics;
  options |= ConstraintSystemFlags::ForCodeCompletion;

  ConstraintSystem cs(DC, options);

  llvm::SmallVector<Solution, 4> solutions;

  cs.setTargetFor(target.getAsExpr(), target);
  if (!cs.solveForCodeCompletion(target, solutions) || solutions.empty())
    return true;

  // FIXME: instead of filtering, expose the score and viability to clients.
  // Remove solutions that skipped over/ignored the code completion point
  // or that require fixes and have a score that is worse than the best.
  filterSolutionsForCodeCompletion(solutions, contextAnalyzer);

  llvm::for_each(solutions, callback);
  return true;
}

LookupResult
swift::lookupSemanticMember(DeclContext *DC, Type ty, DeclName name) {
  return TypeChecker::lookupMember(DC, ty, DeclNameRef(name), SourceLoc(),
                                   std::nullopt);
}

