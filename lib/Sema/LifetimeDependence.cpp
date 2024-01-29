//===--- LifetimeDependence.cpp -----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/LifetimeDependence.h"
#include "TypeChecker.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Type.h"
#include "swift/AST/TypeRepr.h"

namespace swift {
std::string LifetimeDependenceInfo::getString() const {
  std::string lifetimeDependenceString;
  auto getOnIndices = [](IndexSubset *bitvector) {
    std::string result;
    bool isFirstSetBit = true;
    for (unsigned i = 0; i < bitvector->getCapacity(); i++) {
      if (bitvector->contains(i)) {
        if (!isFirstSetBit) {
          result += ", ";
        }
        result += std::to_string(i);
        isFirstSetBit = false;
      }
    }
    return result;
  };
  if (inheritLifetimeParamIndices && !inheritLifetimeParamIndices->isEmpty()) {
    lifetimeDependenceString =
        "_inherit(" + getOnIndices(inheritLifetimeParamIndices) + ")";
  }
  if (borrowLifetimeParamIndices && !borrowLifetimeParamIndices->isEmpty()) {
    lifetimeDependenceString +=
        "_borrow(" + getOnIndices(borrowLifetimeParamIndices) + ")";
  }
  if (mutateLifetimeParamIndices && !mutateLifetimeParamIndices->isEmpty()) {
    lifetimeDependenceString +=
        "_mutate(" + getOnIndices(mutateLifetimeParamIndices) + ")";
  }
  return lifetimeDependenceString;
}

void LifetimeDependenceInfo::Profile(llvm::FoldingSetNodeID &ID) const {
  if (inheritLifetimeParamIndices) {
    inheritLifetimeParamIndices->Profile(ID);
  }
  if (borrowLifetimeParamIndices) {
    borrowLifetimeParamIndices->Profile(ID);
  }
  if (mutateLifetimeParamIndices) {
    mutateLifetimeParamIndices->Profile(ID);
  }
}

llvm::Optional<LifetimeDependenceInfo>
LifetimeDependenceInfo::fromTypeRepr(AbstractFunctionDecl *afd, Type resultType,
                                     bool allowIndex) {
  auto *dc = afd->getDeclContext();
  auto &ctx = dc->getASTContext();
  auto &diags = ctx.Diags;
  auto capacity = afd->getParameters()->size() + 1;
  auto lifetimeDependentRepr =
      cast<LifetimeDependentReturnTypeRepr>(afd->getResultTypeRepr());

  SmallBitVector inheritLifetimeParamIndices(capacity);
  SmallBitVector borrowLifetimeParamIndices(capacity);
  SmallBitVector mutateLifetimeParamIndices(capacity);

  auto updateLifetimeDependenceInfo = [&](LifetimeDependenceSpecifier specifier,
                                          unsigned paramIndexToSet,
                                          ValueOwnership ownership) {
    auto loc = specifier.getLoc();
    auto kind = specifier.getLifetimeDependenceKind();

    if (resultType->isEscapable()) {
      diags.diagnose(loc, diag::lifetime_dependence_invalid_return_type);
      return true;
    }

    if (ownership == ValueOwnership::Default) {
      diags.diagnose(loc, diag::lifetime_dependence_missing_ownership_modifier);
      return true;
    }
    if (kind == LifetimeDependenceKind::Borrow &&
        ownership != ValueOwnership::Shared) {
      diags.diagnose(loc, diag::lifetime_dependence_cannot_use_kind, "borrow",
                     getOwnershipSpelling(ownership));
      return true;
    }
    if (kind == LifetimeDependenceKind::Mutate &&
        ownership != ValueOwnership::InOut) {
      diags.diagnose(loc, diag::lifetime_dependence_cannot_use_kind, "mutate",
                     getOwnershipSpelling(ownership));
      return true;
    }
    if (kind == LifetimeDependenceKind::Consume &&
        ownership != ValueOwnership::Owned) {
      diags.diagnose(loc, diag::lifetime_dependence_cannot_use_kind, "consume",
                     getOwnershipSpelling(ownership));
      return true;
    }
    if (inheritLifetimeParamIndices.test(paramIndexToSet) ||
        borrowLifetimeParamIndices.test(paramIndexToSet)) {
      diags.diagnose(loc, diag::lifetime_dependence_duplicate_param_id);
      return true;
    }
    if (kind == LifetimeDependenceKind::Copy ||
        kind == LifetimeDependenceKind::Consume) {
      inheritLifetimeParamIndices.set(paramIndexToSet);
    } else if (kind == LifetimeDependenceKind::Borrow) {
      borrowLifetimeParamIndices.set(paramIndexToSet);
    } else {
      assert(kind == LifetimeDependenceKind::Mutate);
      mutateLifetimeParamIndices.set(paramIndexToSet);
    }
    return false;
  };

  for (auto specifier : lifetimeDependentRepr->getLifetimeDependencies()) {
    switch (specifier.getSpecifierKind()) {
    case LifetimeDependenceSpecifier::SpecifierKind::Named: {
      bool foundParamName = false;
      unsigned paramIndexToSet = 1;
      for (auto *param : *afd->getParameters()) {
        if (param->getParameterName() == specifier.getName()) {
          foundParamName = true;
          if (updateLifetimeDependenceInfo(specifier, paramIndexToSet,
                                           param->getValueOwnership())) {
            return llvm::None;
          }
          break;
        }
        paramIndexToSet++;
      }
      if (!foundParamName) {
        diags.diagnose(specifier.getLoc(),
                       diag::lifetime_dependence_invalid_param_name,
                       specifier.getName());
        return llvm::None;
      }
      break;
    }
    case LifetimeDependenceSpecifier::SpecifierKind::Ordered: {
      auto paramIndex = specifier.getIndex();
      if (paramIndex > afd->getParameters()->size()) {
        diags.diagnose(specifier.getLoc(),
                       diag::lifetime_dependence_invalid_param_index,
                       paramIndex);
        return llvm::None;
      }
      if (updateLifetimeDependenceInfo(
              specifier, /*paramIndexToSet*/ specifier.getIndex() + 1,
              afd->getParameters()->get(paramIndex)->getValueOwnership())) {
        return llvm::None;
      }
      break;
    }
    case LifetimeDependenceSpecifier::SpecifierKind::Self: {
      if (!afd->hasImplicitSelfDecl()) {
        diags.diagnose(specifier.getLoc(),
                       diag::lifetime_dependence_invalid_self);
        return llvm::None;
      }
      if (updateLifetimeDependenceInfo(
              specifier, /*selfIndex*/ 0,
              afd->getImplicitSelfDecl()->getValueOwnership())) {
        return llvm::None;
      }
      break;
    }
    }
  }

  return LifetimeDependenceInfo(
      IndexSubset::get(ctx, inheritLifetimeParamIndices),
      IndexSubset::get(ctx, borrowLifetimeParamIndices),
      IndexSubset::get(ctx, mutateLifetimeParamIndices));
}

llvm::Optional<LifetimeDependenceInfo>
LifetimeDependenceInfo::get(AbstractFunctionDecl *afd, Type resultType,
                            bool allowIndex) {
  auto *returnTypeRepr = afd->getResultTypeRepr();
  if (!returnTypeRepr) {
    return llvm::None;
  }
  if (!isa<LifetimeDependentReturnTypeRepr>(returnTypeRepr)) {
    return llvm::None;
  }
  return LifetimeDependenceInfo::fromTypeRepr(afd, resultType, allowIndex);
}
} // namespace swift
