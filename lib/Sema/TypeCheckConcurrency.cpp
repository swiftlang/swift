//===--- TypeCheckConcurrency.cpp - Concurrency ---------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements type checking support for Swift's concurrency model.
//
//===----------------------------------------------------------------------===//
#include "TypeChecker.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/TypeCheckRequests.h"

using namespace swift;

/// Check whether the @asyncHandler attribute can be applied to the given
/// function declaration.
///
/// \param diagnose Whether to emit a diagnostic when a problem is encountered.
///
/// \returns \c true if there was a problem with adding the attribute, \c false
/// otherwise.
static bool checkAsyncHandler(FuncDecl *func, bool diagnose) {
  if (!func->getResultInterfaceType()->isVoid()) {
    if (diagnose) {
      func->diagnose(diag::asynchandler_returns_value)
          .highlight(func->getResultTypeSourceRange());
    }

    return true;
  }

  if (func->hasThrows()) {
    if (diagnose) {
      func->diagnose(diag::asynchandler_throws)
          .fixItRemove(func->getThrowsLoc());
    }

    return true;
  }

  if (func->hasAsync()) {
    if (diagnose) {
      func->diagnose(diag::asynchandler_async)
          .fixItRemove(func->getAsyncLoc());
    }

    return true;
  }

  for (auto param : *func->getParameters()) {
    if (param->isInOut()) {
      if (diagnose) {
        param->diagnose(diag::asynchandler_inout_parameter)
            .fixItRemove(param->getSpecifierLoc());
      }

      return true;
    }
  }

  if (func->isMutating()) {
    if (diagnose) {
      auto diag = func->diagnose(diag::asynchandler_mutating);
      if (auto mutatingAttr = func->getAttrs().getAttribute<MutatingAttr>()) {
        diag.fixItRemove(mutatingAttr->getRange());
      }
    }

    return true;
  }

  return false;
}

void swift::addAsyncNotes(FuncDecl *func) {
  func->diagnose(diag::note_add_async_to_function, func->getName());

  if (!checkAsyncHandler(func, /*diagnose=*/false)) {
    func->diagnose(
            diag::note_add_asynchandler_to_function, func->getName())
        .fixItInsert(func->getAttributeInsertionLoc(false), "@asyncHandler ");
  }
}

bool IsAsyncHandlerRequest::evaluate(
    Evaluator &evaluator, FuncDecl *func) const {
  // Check whether the attribute was explicitly specified.
  if (auto attr = func->getAttrs().getAttribute<AsyncHandlerAttr>()) {
    // Check for well-formedness.
    if (checkAsyncHandler(func, /*diagnose=*/true)) {
      attr->setInvalid();
      return false;
    }

    return true;
  }

  if (!func->getASTContext().LangOpts.EnableExperimentalConcurrency)
    return false;

  // Are we in a context where inference is possible?
  auto dc = func->getDeclContext();
  if (!dc->isTypeContext() || !dc->getParentSourceFile() ||
      isa<ProtocolDecl>(dc) || !func->hasBody())
    return false;

  // Is it possible to infer @asyncHandler for this function at all?
  if (checkAsyncHandler(func, /*diagnose=*/false))
    return false;

  // Add an implicit @asyncHandler attribute and return true. We're done.
  auto addImplicitAsyncHandlerAttr = [&] {
    func->getAttrs().add(new (func->getASTContext()) AsyncHandlerAttr(true));
    return true;
  };

  // Check whether any of the conformances in the context of the function
  // implies @asyncHandler.
  {
    auto idc = cast<IterableDeclContext>(dc->getAsDecl());
    auto conformances = evaluateOrDefault(
        dc->getASTContext().evaluator,
        LookupAllConformancesInContextRequest{idc}, { });

    for (auto conformance : conformances) {
      auto protocol = conformance->getProtocol();
      for (auto found : protocol->lookupDirect(func->getName())) {
        if (!isa<ProtocolDecl>(found->getDeclContext()))
          continue;

        auto requirement = dyn_cast<FuncDecl>(found);
        if (!requirement)
          continue;

        if (!requirement->isAsyncHandler())
          continue;

        auto witness = conformance->getWitnessDecl(requirement);
        if (witness != func)
          continue;

        return addImplicitAsyncHandlerAttr();
      }
    }
  }

  // Look through dynamic replacements.
  if (auto replaced = func->getDynamicallyReplacedDecl()) {
    if (auto replacedFunc = dyn_cast<FuncDecl>(replaced))
      if (replacedFunc->isAsyncHandler())
        return addImplicitAsyncHandlerAttr();
  }

  return false;
}
