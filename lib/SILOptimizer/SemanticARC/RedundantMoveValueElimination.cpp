//===--- RedundantMoveValueElimination.cpp - Delete spurious move_values --===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// A move_value ends an owned lifetime and begins an owned lifetime.
//
// The new lifetime may have the same characteristics as the original lifetime
// with regards to
// - lexicality
// - escaping
//
// If it has the same characteristics, there is no reason to have two separate
// lifetimes--they are redundant.  This optimization deletes such redundant
// move_values.
//===----------------------------------------------------------------------===//

#include "SemanticARC/SemanticARCOpts.h"
#include "SemanticARCOptVisitor.h"
#include "swift/SIL/LinearLifetimeChecker.h"

using namespace swift;
using namespace semanticarc;

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

bool SemanticARCOptVisitor::visitMoveValueInst(MoveValueInst *mvi) {
  if (!ctx.shouldPerform(ARCTransformKind::RedundantMoveValueElim))
    return false;

  if (!isRedundantMoveValue(mvi))
    return false;

  // Both characteristics match.
  eraseAndRAUWSingleValueInstruction(mvi, mvi->getOperand());
  return true;
}
