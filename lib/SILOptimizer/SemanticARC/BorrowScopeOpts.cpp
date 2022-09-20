//===--- BorrowScopeOpts.cpp ----------------------------------------------===//
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
///
/// \file
///
/// Optimizations that attempt to simplify and or eliminate borrow scopes. Today
/// we only eliminate scopes, but we could also eliminate redundant scopes by
/// converting struct_extract operations to use destructure operations.
///
//===----------------------------------------------------------------------===//

#include "Context.h"
#include "SemanticARCOptVisitor.h"

using namespace swift;
using namespace swift::semanticarc;

bool SemanticARCOptVisitor::visitBeginBorrowInst(BeginBorrowInst *bbi) {
  // Quickly check if we are supposed to perform this transformation.
  if (!ctx.shouldPerform(ARCTransformKind::RedundantBorrowScopeElimPeephole))
    return false;

  // Non-redundant, lexical borrow scopes must remain in order to ensure that
  // value lifetimes are not observably shortened.
  if (bbi->isLexical() && !isNestedLexicalBeginBorrow(bbi)) {
    return false;
  }

  auto kind = bbi->getOperand()->getOwnershipKind();
  SmallVector<EndBorrowInst *, 16> endBorrows;
  for (auto *op : bbi->getUses()) {
    if (!op->isLifetimeEnding()) {
      // Make sure that this operand can accept our arguments kind.
      if (op->canAcceptKind(kind))
        continue;
      return false;
    }

    // Otherwise, this borrow is being consumed. See if our consuming inst is an
    // end_borrow. If it isn't, then return false, this scope is
    // needed. Otherwise, add the end_borrow to our list of end borrows.
    auto *ebi = dyn_cast<EndBorrowInst>(op->getUser());
    if (!ebi) {
      return false;
    }
    endBorrows.push_back(ebi);
  }

  // At this point, we know that the begin_borrow's operand can be
  // used as an argument to all non-end borrow uses. Eliminate the
  // begin borrow and end borrows.
  while (!endBorrows.empty()) {
    auto *ebi = endBorrows.pop_back_val();
    eraseInstruction(ebi);
  }

  eraseAndRAUWSingleValueInstruction(bbi, bbi->getOperand());
  return true;
}
