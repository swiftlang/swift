//===--- IterativeTypeChecker.cpp - Iterative Type Checker ----------------===//
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
//  This file implements the IterativeTypeChecker class, which
//  performs iterative type checking by tracking the set of
//  outstanding type-checking requests and servicing them as needed.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "swift/Sema/IterativeTypeChecker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/Basic/Defer.h"
using namespace swift;

ASTContext &IterativeTypeChecker::getASTContext() const {
  return TC.Context;
}

DiagnosticEngine &IterativeTypeChecker::getDiags() const {
  return getASTContext().Diags;
}

void IterativeTypeChecker::process(
       TypeCheckRequest request,
       UnsatisfiedDependency unsatisfiedDependency) {
  switch (request.getKind()) {
#define TYPE_CHECK_REQUEST(Request,PayloadName)                   \
  case TypeCheckRequest::Request:                                 \
    return process##Request(request.get##PayloadName##Payload(),  \
                            unsatisfiedDependency);

#include "swift/Sema/TypeCheckRequestKinds.def"
  }
}

/// Determine whether the given request has already been satisfied.
bool IterativeTypeChecker::isSatisfied(TypeCheckRequest request) {
  switch (request.getKind()) {
#define TYPE_CHECK_REQUEST(Request,PayloadName)                         \
  case TypeCheckRequest::Request:                                       \
    return is##Request##Satisfied(request.get##PayloadName##Payload());

#include "swift/Sema/TypeCheckRequestKinds.def"
  }

  llvm_unreachable("Unhandled TypeCheckRequestKind in switch.");
}

bool IterativeTypeChecker::breakCycle(TypeCheckRequest request) {
  switch (request.getKind()) {
#define TYPE_CHECK_REQUEST(Request,PayloadName)                         \
  case TypeCheckRequest::Request:                                       \
    return breakCycleFor##Request(request.get##PayloadName##Payload());

#include "swift/Sema/TypeCheckRequestKinds.def"
  }

  llvm_unreachable("Unhandled TypeCheckRequestKind in switch.");
}

void IterativeTypeChecker::satisfy(TypeCheckRequest request) {
  // If the request has already been satisfied, we're done.
  if (isSatisfied(request)) return;

  // Check for circular dependencies in our requests.
  // FIXME: This stack operation is painfully inefficient.
  auto existingRequest = std::find(ActiveRequests.rbegin(),
                                   ActiveRequests.rend(),
                                   request);
  if (existingRequest != ActiveRequests.rend()) {
    auto first = existingRequest.base();
    --first;
    diagnoseCircularReference(llvm::makeArrayRef(&*first,
                                                 &*ActiveRequests.end()));
    return;
  }

  // Add this request to the stack of active requests.
  ActiveRequests.push_back(request);
  SWIFT_DEFER { ActiveRequests.pop_back(); };

  while (true) {
    // Process this requirement, enumerating dependencies if anything else needs
    // to be handled first.
    SmallVector<TypeCheckRequest, 4> unsatisfied;
    process(request, [&](TypeCheckRequest dependency) -> bool {
      if (isSatisfied(dependency)) return false;

      // Record the unsatisfied dependency.
      unsatisfied.push_back(dependency);
      return true;
    });

    // If there were no unsatisfied dependencies, we're done.
    if (unsatisfied.empty()) {
      assert(isSatisfied(request));
      break;
    }

    // Recurse to satisfy any unsatisfied dependencies.
    // FIXME: Don't recurse in the iterative type checker, silly!
    for (auto dependency : unsatisfied) {
      satisfy(dependency);
    }
  }
}

static bool isSelfRedefinedTypeAliasDecl(const TypeCheckRequest &Request) {
  if (Request.getKind() == TypeCheckRequest::Kind::ResolveTypeDecl) {
    if (auto TAD = dyn_cast<TypeAliasDecl>(Request.getAnchor())) {
      SourceRange SR = TAD->getUnderlyingTypeLoc().getSourceRange();
      SourceManager &SM = TAD->getASTContext().SourceMgr;
      CharSourceRange CR = CharSourceRange(SM, SR.Start,
                                           Lexer::getLocForEndOfToken(SM,
                                                                      SR.End));
      return CR.str() == TAD->getNameStr();
    }
  }
  return false;
}

//===----------------------------------------------------------------------===//
// Diagnostics
//===----------------------------------------------------------------------===//
void IterativeTypeChecker::diagnoseCircularReference(
       ArrayRef<TypeCheckRequest> requests) {
  bool isFirst = true;
  for (const auto &request : requests) {
    if (isSelfRedefinedTypeAliasDecl(request)) {
      diagnose(request.getLoc(), diag::redundant_type_alias_define).
        fixItRemove(request.getAnchor()->getSourceRange());
    } else {
      diagnose(request.getLoc(),
               isFirst ? diag::circular_reference :
                         diag::circular_reference_through);
    }

    isFirst = false;
  }

  // Now try to break the cycle.
  bool brokeCycle = false;
  for (const auto &request : reverse(requests)) {
    brokeCycle |= breakCycle(request);
  }

  assert(brokeCycle && "Will the cycle be unbroken?");
  (void) brokeCycle;
}
