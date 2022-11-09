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
#include <algorithm>

using namespace swift;

static Type createPackBinding(ASTContext &ctx, ArrayRef<Type> types) {
  // If there is only one element and it's a pack expansion type,
  // return the pattern type directly. Because PackType can only appear
  // inside a PackExpansion, PackType(PackExpansionType()) will always
  // simplify to the pattern type.
  if (types.size() == 1) {
    if (types.front()->is<PackExpansionType>()) {
      return types.front();
    }
  }

  auto *packType = PackType::get(ctx, types);
  return PackExpansionType::get(packType, packType);
}

static Type gatherTupleElements(ArrayRef<TupleTypeElt> &elts,
                                Identifier name,
                                ASTContext &ctx) {
  SmallVector<Type, 2> types;

  if (!elts.empty() && elts.front().getName() == name) {
    do {
      types.push_back(elts.front().getType());
      elts = elts.slice(1);
    } while (!elts.empty() && !elts.front().hasName());
  }

  return createPackBinding(ctx, types);
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
    if (auto *lhsExpansionType = lhsElt.getType()->getAs<PackExpansionType>()) {
      lhsElts = lhsElts.slice(1);

      assert(lhsElts.empty() || lhsElts.front().hasName() &&
             "Tuple element with pack expansion type cannot be followed "
             "by an unlabeled element");

      auto rhs = gatherTupleElements(rhsElts, lhsElt.getName(), ctx);
      pairs.emplace_back(lhsExpansionType, rhs, idx++);
      continue;
    }

    if (rhsElts.empty()) {
      assert(!lhsElts.empty());
      return true;
    }

    // A pack expansion type on the right hand side absorbs all elements
    // from the left hand side up to the next mismatched label.
    auto rhsElt = rhsElts.front();
    if (auto *rhsExpansionType = rhsElt.getType()->getAs<PackExpansionType>()) {
      rhsElts = rhsElts.slice(1);

      assert(rhsElts.empty() || rhsElts.front().hasName() &&
             "Tuple element with pack expansion type cannot be followed "
             "by an unlabeled element");

      auto lhs = gatherTupleElements(lhsElts, rhsElt.getName(), ctx);
      pairs.emplace_back(lhs, rhsExpansionType, idx++);
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

ParamPackMatcher::ParamPackMatcher(
    ArrayRef<AnyFunctionType::Param> lhsParams,
    ArrayRef<AnyFunctionType::Param> rhsParams,
    ASTContext &ctx)
  : lhsParams(lhsParams), rhsParams(rhsParams), ctx(ctx) {}

bool ParamPackMatcher::match() {
  unsigned minLength = std::min(lhsParams.size(), rhsParams.size());

  // Consume the longest possible prefix where neither type in
  // the pair is a pack expansion type.
  unsigned prefixLength = 0;
  for (unsigned i = 0; i < minLength; ++i) {
    auto lhsParam = lhsParams[i];
    auto rhsParam = rhsParams[i];

    // FIXME: Check flags

    auto lhsType = lhsParam.getPlainType();
    auto rhsType = rhsParam.getPlainType();

    if (lhsType->is<PackExpansionType>() ||
        rhsType->is<PackExpansionType>()) {
      break;
    }

    // FIXME: Check flags

    pairs.emplace_back(lhsType, rhsType, i);
    ++prefixLength;
  }

  // Consume the longest possible suffix where neither type in
  // the pair is a pack expansion type.
  unsigned suffixLength = 0;
  for (unsigned i = 0; i < minLength - prefixLength; ++i) {
    auto lhsParam = lhsParams[lhsParams.size() - i - 1];
    auto rhsParam = rhsParams[rhsParams.size() - i - 1];

    // FIXME: Check flags

    auto lhsType = lhsParam.getPlainType();
    auto rhsType = rhsParam.getPlainType();

    if (lhsType->is<PackExpansionType>() ||
        rhsType->is<PackExpansionType>()) {
      break;
    }

    pairs.emplace_back(lhsType, rhsType, i);
    ++suffixLength;
  }

  assert(prefixLength + suffixLength <= lhsParams.size());
  assert(prefixLength + suffixLength <= rhsParams.size());

  // Drop the consumed prefix and suffix from each list of types.
  lhsParams = lhsParams.drop_front(prefixLength).drop_back(suffixLength);
  rhsParams = rhsParams.drop_front(prefixLength).drop_back(suffixLength);

  // If nothing remains, we're done.
  if (lhsParams.empty() && rhsParams.empty())
    return false;

  // If the left hand side is a single pack expansion type, bind it
  // to what remains of the right hand side.
  if (lhsParams.size() == 1) {
    auto lhsType = lhsParams[0].getPlainType();
    if (lhsType->is<PackExpansionType>()) {
      SmallVector<Type, 2> rhsTypes;
      for (auto rhsParam : rhsParams) {
        // FIXME: Check rhs flags
        rhsTypes.push_back(rhsParam.getPlainType());
      }
      auto rhs = createPackBinding(ctx, rhsTypes);

      // FIXME: Check lhs flags
      pairs.emplace_back(lhsType, rhs, prefixLength);
      return false;
    }
  }

  // If the right hand side is a single pack expansion type, bind it
  // to what remains of the left hand side.
  if (rhsParams.size() == 1) {
    auto rhsType = rhsParams[0].getPlainType();
    if (rhsType->is<PackExpansionType>()) {
      SmallVector<Type, 2> lhsTypes;
      for (auto lhsParam : lhsParams) {
        // FIXME: Check lhs flags
        lhsTypes.push_back(lhsParam.getPlainType());
      }
      auto lhs = createPackBinding(ctx, lhsTypes);

      // FIXME: Check rhs flags
      pairs.emplace_back(lhs, rhsType, prefixLength);
      return false;
    }
  }

  // Otherwise, all remaining possibilities are invalid:
  // - Neither side has any pack expansions, and they have different lengths.
  // - One side has a pack expansion but the other side is too short, eg
  //   {Int, T..., Float} vs {Int}.
  // - The prefix and suffix are mismatched, so we're left with something
  //   like {T..., Int} vs {Float, U...}.
  return true;
}

PackMatcher::PackMatcher(
    ArrayRef<Type> lhsTypes,
    ArrayRef<Type> rhsTypes,
    ASTContext &ctx)
  : lhsTypes(lhsTypes), rhsTypes(rhsTypes), ctx(ctx) {}

bool PackMatcher::match() {
  unsigned minLength = std::min(lhsTypes.size(), rhsTypes.size());

  // Consume the longest possible prefix where neither type in
  // the pair is a pack expansion type.
  unsigned prefixLength = 0;
  for (unsigned i = 0; i < minLength; ++i) {
    auto lhsType = lhsTypes[i];
    auto rhsType = rhsTypes[i];

    if (lhsType->is<PackExpansionType>() ||
        rhsType->is<PackExpansionType>()) {
      break;
    }

    pairs.emplace_back(lhsType, rhsType, i);
    ++prefixLength;
  }

  // Consume the longest possible suffix where neither type in
  // the pair is a pack expansion type.
  unsigned suffixLength = 0;
  for (unsigned i = 0; i < minLength - prefixLength; ++i) {
    auto lhsType = lhsTypes[lhsTypes.size() - i - 1];
    auto rhsType = rhsTypes[rhsTypes.size() - i - 1];

    if (lhsType->is<PackExpansionType>() ||
        rhsType->is<PackExpansionType>()) {
      break;
    }

    pairs.emplace_back(lhsType, rhsType, i);
    ++suffixLength;
  }

  assert(prefixLength + suffixLength <= lhsTypes.size());
  assert(prefixLength + suffixLength <= rhsTypes.size());

  // Drop the consumed prefix and suffix from each list of types.
  lhsTypes = lhsTypes.drop_front(prefixLength).drop_back(suffixLength);
  rhsTypes = rhsTypes.drop_front(prefixLength).drop_back(suffixLength);

  // If nothing remains, we're done.
  if (lhsTypes.empty() && rhsTypes.empty())
    return false;

  // If the left hand side is a single pack expansion type, bind it
  // to what remains of the right hand side.
  if (lhsTypes.size() == 1) {
    auto lhsType = lhsTypes[0];
    if (lhsType->is<PackExpansionType>()) {
      auto rhs = createPackBinding(ctx, rhsTypes);

      pairs.emplace_back(lhsType, rhs, prefixLength);
      return false;
    }
  }

  // If the right hand side is a single pack expansion type, bind it
  // to what remains of the left hand side.
  if (rhsTypes.size() == 1) {
    auto rhsType = rhsTypes[0];
    if (rhsType->is<PackExpansionType>()) {
      auto lhs = createPackBinding(ctx, lhsTypes);

      pairs.emplace_back(lhs, rhsType, prefixLength);
      return false;
    }
  }

  // Otherwise, all remaining possibilities are invalid:
  // - Neither side has any pack expansions, and they have different lengths.
  // - One side has a pack expansion but the other side is too short, eg
  //   {Int, T..., Float} vs {Int}.
  // - The prefix and suffix are mismatched, so we're left with something
  //   like {T..., Int} vs {Float, U...}.
  return true;
}
