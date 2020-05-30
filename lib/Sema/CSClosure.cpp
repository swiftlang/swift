//===--- CSClosure.cpp - Closures -----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements constraint generation and solution application for
// closures. It provides part of the implementation of the ConstraintSystem
// class.
//
//===----------------------------------------------------------------------===//

#include "ConstraintSystem.h"
using namespace swift;
using namespace swift::constraints;

bool ConstraintSystem::generateConstraints(
    ClosureExpr *closure, Type resultType) {
  assert(closure->hasSingleExpressionBody());
  auto closureBody = generateConstraints(
      closure->getSingleExpressionBody(), closure, /*isInputExpression=*/false);
  if (!closureBody)
    return true;

  bool hasReturn = hasExplicitResult(closure);
  addConstraint(
      ConstraintKind::Conversion, getType(closureBody),
      resultType,
      getConstraintLocator(closure, LocatorPathElt::ClosureBody(hasReturn)));
  return false;
}
