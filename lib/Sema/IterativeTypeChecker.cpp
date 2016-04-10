//===--- IterativeTypeChecker.cpp - Iterative Type Checker ----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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
}

bool IterativeTypeChecker::breakCycle(TypeCheckRequest request) {
  switch (request.getKind()) {
#define TYPE_CHECK_REQUEST(Request,PayloadName)                         \
  case TypeCheckRequest::Request:                                       \
    return breakCycleFor##Request(request.get##PayloadName##Payload());

#include "swift/Sema/TypeCheckRequestKinds.def"
  }  
}

void IterativeTypeChecker::satisfy(TypeCheckRequest request) {
  auto startingSize = ActiveRequests.size();
  
  auto addToActiveRequests = [&](TypeCheckRequest request) {
    // Check for circular dependencies in our requests.
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
  };
  
  addToActiveRequests(request);
  SWIFT_DEFER { ActiveRequests.pop_back(); };

  while (ActiveRequests.size() != startingSize) {
    request = ActiveRequests.back();
    // If the request has already been satisfied, we're done.
    if (isSatisfied(request)) {
      ActiveRequests.pop_back();
      continue;
    }
    
    // Process this requirement, enumerating dependencies if anything else needs
    // to be handled first.
    process(request, [&](TypeCheckRequest dependency) -> bool {
      if (isSatisfied(dependency)) return false;
      // Record the unsatisfied dependency.
      addToActiveRequests(dependency);
      return true;
    });
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
#ifndef NDEBUG
  bool brokeCycle = false;
#endif
  for (const auto &request : reverse(requests)) {
    if (breakCycle(request)) {
#ifndef NDEBUG
      brokeCycle = true;
#endif
      break;
    }
  }

  assert(brokeCycle && "Will the cycle be unbroken?");
}
