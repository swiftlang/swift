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

static PackExpansionType *createPackBinding(ASTContext &ctx,
                                            ArrayRef<Type> types) {
  // If there is only one element and it's a PackExpansionType,
  // return it directly.
  if (types.size() == 1) {
    if (auto *expansionType = types.front()->getAs<PackExpansionType>()) {
      return expansionType;
    }
  }

  // Otherwise, wrap the elements in PackExpansionType(PackType(...)).
  auto *packType = PackType::get(ctx, types);
  return PackExpansionType::get(packType, packType);
}

static PackExpansionType *gatherTupleElements(ArrayRef<TupleTypeElt> &elts,
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
  unsigned lhsIdx = 0;
  unsigned rhsIdx = 0;

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
      pairs.emplace_back(lhsExpansionType, rhs, lhsIdx++, rhsIdx);
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
      pairs.emplace_back(lhs, rhsExpansionType, lhsIdx, rhsIdx++);
      continue;
    }

    // Neither side is a pack expansion. We must have an exact match.
    if (lhsElt.getName() != rhsElt.getName())
      return true;

    lhsElts = lhsElts.slice(1);
    rhsElts = rhsElts.slice(1);

    pairs.emplace_back(lhsElt.getType(), rhsElt.getType(), lhsIdx++, rhsIdx++);
  }

  return false;
}

TypeListPackMatcher::Element
TypeListPackMatcher::Element::from(const TupleTypeElt &elt) {
  return {elt.getName(), elt.getType()};
}

TypeListPackMatcher::Element
TypeListPackMatcher::Element::from(const AnyFunctionType::Param &param) {
  return {param.getLabel(), param.getPlainType(), param.getParameterFlags()};
}

TypeListPackMatcher::Element TypeListPackMatcher::Element::from(Type type) {
  return {/*label=*/Identifier(), type};
}

TypeListPackMatcher::TypeListPackMatcher(ASTContext &ctx,
                                         ArrayRef<TupleTypeElt> lhsParams,
                                         ArrayRef<TupleTypeElt> rhsParams)
    : ctx(ctx) {
  llvm::transform(lhsParams, std::back_inserter(lhsElements),
                  [&](const auto &elt) { return Element::from(elt); });
  llvm::transform(rhsParams, std::back_inserter(rhsElements),
                  [&](const auto &elt) { return Element::from(elt); });
}

TypeListPackMatcher::TypeListPackMatcher(
    ASTContext &ctx, ArrayRef<AnyFunctionType::Param> lhsParams,
    ArrayRef<AnyFunctionType::Param> rhsParams)
    : ctx(ctx) {
  llvm::transform(lhsParams, std::back_inserter(lhsElements),
                  [&](const auto &elt) {
                    assert(!elt.hasLabel());
                    return Element::from(elt);
                  });
  llvm::transform(rhsParams, std::back_inserter(rhsElements),
                  [&](const auto &elt) {
                    assert(!elt.hasLabel());
                    return Element::from(elt);
                  });
}

TypeListPackMatcher::TypeListPackMatcher(ASTContext &ctx,
                                         ArrayRef<Type> lhsParams,
                                         ArrayRef<Type> rhsParams)
    : ctx(ctx) {
  llvm::transform(lhsParams, std::back_inserter(lhsElements),
                  [&](const auto &elt) { return Element::from(elt); });
  llvm::transform(rhsParams, std::back_inserter(rhsElements),
                  [&](const auto &elt) { return Element::from(elt); });
}

bool TypeListPackMatcher::match() {
  ArrayRef<Element> lhsParams(lhsElements);
  ArrayRef<Element> rhsParams(rhsElements);

  unsigned minLength = std::min(lhsParams.size(), rhsParams.size());

  // Consume the longest possible prefix where neither type in
  // the pair is a pack expansion type.
  unsigned prefixLength = 0;
  for (unsigned i = 0; i < minLength; ++i) {
    unsigned lhsIdx = i;
    unsigned rhsIdx = i;

    auto lhsParam = lhsParams[lhsIdx];
    auto rhsParam = rhsParams[rhsIdx];

    if (lhsParam.getLabel() != rhsParam.getLabel())
      break;

    // FIXME: Check flags

    auto lhsType = lhsParam.getType();
    auto rhsType = rhsParam.getType();

    if (lhsType->is<PackExpansionType>() ||
        rhsType->is<PackExpansionType>()) {
      break;
    }

    // FIXME: Check flags

    pairs.emplace_back(lhsType, rhsType, lhsIdx, rhsIdx);
    ++prefixLength;
  }

  // Consume the longest possible suffix where neither type in
  // the pair is a pack expansion type.
  unsigned suffixLength = 0;
  for (unsigned i = 0; i < minLength - prefixLength; ++i) {
    unsigned lhsIdx = lhsParams.size() - i - 1;
    unsigned rhsIdx = rhsParams.size() - i - 1;

    auto lhsParam = lhsParams[lhsIdx];
    auto rhsParam = rhsParams[rhsIdx];

    // FIXME: Check flags

    if (lhsParam.getLabel() != rhsParam.getLabel())
      break;

    auto lhsType = lhsParam.getType();
    auto rhsType = rhsParam.getType();

    if (lhsType->is<PackExpansionType>() ||
        rhsType->is<PackExpansionType>()) {
      break;
    }

    pairs.emplace_back(lhsType, rhsType, lhsIdx, rhsIdx);
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
    auto lhsType = lhsParams[0].getType();
    if (auto *lhsExpansion = lhsType->getAs<PackExpansionType>()) {
      unsigned lhsIdx = prefixLength;
      unsigned rhsIdx = prefixLength;

      SmallVector<Type, 2> rhsTypes;
      for (auto rhsParam : rhsParams) {
        if (rhsParam.hasLabel())
          return true;

        // FIXME: Check rhs flags
        rhsTypes.push_back(rhsParam.getType());
      }
      auto rhs = createPackBinding(ctx, rhsTypes);

      // FIXME: Check lhs flags
      pairs.emplace_back(lhsExpansion, rhs, lhsIdx, rhsIdx);
      return false;
    }
  }

  // If the right hand side is a single pack expansion type, bind it
  // to what remains of the left hand side.
  if (rhsParams.size() == 1) {
    auto rhsType = rhsParams[0].getType();
    if (auto *rhsExpansion = rhsType->getAs<PackExpansionType>()) {
      unsigned lhsIdx = prefixLength;
      unsigned rhsIdx = prefixLength;

      SmallVector<Type, 2> lhsTypes;
      for (auto lhsParam : lhsParams) {
        if (lhsParam.hasLabel())
          return true;

        // FIXME: Check lhs flags
        lhsTypes.push_back(lhsParam.getType());
      }
      auto lhs = createPackBinding(ctx, lhsTypes);

      // FIXME: Check rhs flags
      pairs.emplace_back(lhs, rhsExpansion, lhsIdx, rhsIdx);
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
