//===--- PackExpansionMatcher.cpp - Matching pack expansions --------------===//
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
//
// Utilities for structural matching of sequences of types containing pack
// expansions.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/PackExpansionMatcher.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Type.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/SmallVector.h"

using namespace swift;

static PackType *gatherTupleElements(ArrayRef<TupleTypeElt> &elts,
                                     Identifier name,
                                     ASTContext &ctx) {
  SmallVector<Type, 2> types;

  if (!elts.empty() && elts.front().getName() == name) {
    do {
      types.push_back(elts.front().getType());
      elts = elts.slice(1);
    } while (!elts.empty() && !elts.front().hasName());
  }

  return PackType::get(ctx, types);
}

TuplePackMatcher::TuplePackMatcher(TupleType *lhsTuple, TupleType *rhsTuple)
  : lhsElts(lhsTuple->getElements()),
    rhsElts(rhsTuple->getElements()),
    ctx(lhsTuple->getASTContext()) {}

bool TuplePackMatcher::match() {
  unsigned idx = 0;

  // Iterate over the two tuples in parallel, popping elements from
  // the start.
  while (true) {
    // If both tuples have been exhausted, we're done.
    if (lhsElts.empty() && rhsElts.empty())
      return false;

    if (lhsElts.empty()) {
      assert(!rhsElts.empty());
      return true;
    }

    // A pack expansion type on the left hand side absorbs all elements
    // from the right hand side up to the next mismatched label.
    auto lhsElt = lhsElts.front();
    if (lhsElt.getType()->is<PackExpansionType>()) {
      lhsElts = lhsElts.slice(1);

      assert(lhsElts.empty() || lhsElts.front().hasName() &&
             "Tuple element with pack expansion type cannot be followed "
             "by an unlabeled element");

      auto *rhs = gatherTupleElements(rhsElts, lhsElt.getName(), ctx);
      pairs.emplace_back(lhsElt.getType(), rhs, idx++);
      continue;
    }

    if (rhsElts.empty()) {
      assert(!lhsElts.empty());
      return true;
    }

    // A pack expansion type on the right hand side absorbs all elements
    // from the left hand side up to the next mismatched label.
    auto rhsElt = rhsElts.front();
    if (rhsElt.getType()->is<PackExpansionType>()) {
      rhsElts = rhsElts.slice(1);

      assert(rhsElts.empty() || rhsElts.front().hasName() &&
             "Tuple element with pack expansion type cannot be followed "
             "by an unlabeled element");

      auto *lhs = gatherTupleElements(lhsElts, rhsElt.getName(), ctx);
      pairs.emplace_back(lhs, rhsElt.getType(), idx++);
      continue;
    }

    // Neither side is a pack expansion. We must have an exact match.
    if (lhsElt.getName() != rhsElt.getName())
      return true;

    lhsElts = lhsElts.slice(1);
    rhsElts = rhsElts.slice(1);

    pairs.emplace_back(lhsElt.getType(), rhsElt.getType(), idx++);
  }

  return false;
}