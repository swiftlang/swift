//===--- IterativeTypeChecker.cpp - Iterative Type Checker ----------------===//
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
//
//  This file implements the IterativeTypeChecker class, which
//  performs iterative type checking by tracking the set of
//  outstanding type-checking requests and servicing them as needed.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "swift/Sema/IterativeTypeChecker.h"
#include "swift/AST/Decl.h"
using namespace swift;

ASTContext &IterativeTypeChecker::getASTContext() const {
  return TC.Context;
}

DiagnosticEngine &IterativeTypeChecker::getDiags() const {
  return getASTContext().Diags;
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

void IterativeTypeChecker::enumerateDependenciesOf(
       TypeCheckRequest request,
       llvm::function_ref<void(TypeCheckRequest)> fn) {
  switch (request.getKind()) {
#define TYPE_CHECK_REQUEST(Request,PayloadName)         \
  case TypeCheckRequest::Request:                       \
    return enumerateDependenciesOf##Request(            \
             request.get##PayloadName##Payload(),       \
             fn);

#include "swift/Sema/TypeCheckRequestKinds.def"
  }
}

void IterativeTypeChecker::satisfy(TypeCheckRequest request) {
  // If the request has already been satisfied, we're done.
  if (isSatisfied(request)) return;

  // Make sure all of the dependencies have been satisfied before continuing.
  while (true) {
    // Enumerate all of the dependencies of this request and capture
    // those that have not been satisfied.
    SmallVector<TypeCheckRequest, 4> unsatisfied;
    enumerateDependenciesOf(request, [&](TypeCheckRequest dependency) {
      // If the dependency has already been satisfied, there's nothing to do.
      if (isSatisfied(dependency)) return;

      // Record the unsatisfied dependency.
      unsatisfied.push_back(dependency);
    });

    // If all dependencies were satisfied, we're done.
    if (unsatisfied.empty()) break;

    // Recurse to satisfy any unsatisfied dependencies.
    // FIXME: Don't recurse in the iterative type checker, silly!
    for (auto dependency: unsatisfied) {
      satisfy(dependency);
    }
  }

  // Satisfy this request.
  switch (request.getKind()) {
#define TYPE_CHECK_REQUEST(Request,PayloadName)                         \
  case TypeCheckRequest::Request:                                       \
    return satisfy##Request(request.get##PayloadName##Payload());

#include "swift/Sema/TypeCheckRequestKinds.def"
  }
}
