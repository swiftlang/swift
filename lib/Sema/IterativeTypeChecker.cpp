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

#include "swift/Sema/IterativeTypeChecker.h"
#include "swift/AST/Decl.h"
using namespace swift;

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

  switch (request.getKind()) {
#define TYPE_CHECK_REQUEST(Request,PayloadName)                         \
  case TypeCheckRequest::Request:                                       \
    return satisfy##Request(request.get##PayloadName##Payload());

#include "swift/Sema/TypeCheckRequestKinds.def"
  }
}
