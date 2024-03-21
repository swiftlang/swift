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

#include "TypeChecker.h"

#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/LifetimeDependence.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Type.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Basic/Defer.h"

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
  if (scopeLifetimeParamIndices && !scopeLifetimeParamIndices->isEmpty()) {
    lifetimeDependenceString +=
        "_scope(" + getOnIndices(scopeLifetimeParamIndices) + ")";
  }
  return lifetimeDependenceString;
}

void LifetimeDependenceInfo::Profile(llvm::FoldingSetNodeID &ID) const {
  if (inheritLifetimeParamIndices) {
    // Copy and Consume are the same, can be unified if we converge on dependsOn
    // syntax
    ID.AddInteger((uint8_t)LifetimeDependenceKind::Copy);
    inheritLifetimeParamIndices->Profile(ID);
  }
  if (scopeLifetimeParamIndices) {
    // Borrow and Mutate are the same, can be unified if we converge on
    // dependsOn syntax
    ID.AddInteger((uint8_t)LifetimeDependenceKind::Borrow);
    scopeLifetimeParamIndices->Profile(ID);
  }
}

LifetimeDependenceInfo LifetimeDependenceInfo::getForParamIndex(
    AbstractFunctionDecl *afd, unsigned index, ValueOwnership ownership) {
  auto *dc = afd->getDeclContext();
  auto &ctx = dc->getASTContext();
  unsigned capacity = afd->getParameters()->size() + 1;
  auto indexSubset = IndexSubset::get(ctx, capacity, {index});
  if (ownership == ValueOwnership::Owned) {
    return LifetimeDependenceInfo{/*inheritLifetimeParamIndices*/ indexSubset,
                                  /*scopeLifetimeParamIndices*/ nullptr};
  }
  assert(ownership == ValueOwnership::Shared ||
         ownership == ValueOwnership::InOut);
  return LifetimeDependenceInfo{/*inheritLifetimeParamIndices*/ nullptr,
                                /*scopeLifetimeParamIndices*/ indexSubset};
}

void LifetimeDependenceInfo::getConcatenatedData(
    SmallVectorImpl<bool> &concatenatedData) const {
  auto pushData = [&](IndexSubset *paramIndices) {
    if (paramIndices == nullptr) {
      return;
    }
    assert(!paramIndices->isEmpty());

    for (unsigned i = 0; i < paramIndices->getCapacity(); i++) {
      if (paramIndices->contains(i)) {
        concatenatedData.push_back(true);
        continue;
      }
      concatenatedData.push_back(false);
    }
  };
  if (hasInheritLifetimeParamIndices()) {
    pushData(inheritLifetimeParamIndices);
  }
  if (hasScopeLifetimeParamIndices()) {
    pushData(scopeLifetimeParamIndices);
  }
}

static bool hasEscapableResultOrYield(AbstractFunctionDecl *afd,
                                      Type resultType) {
  Type resultTypeInContext = afd->mapTypeIntoContext(resultType);
  std::optional<Type> yieldTyInContext;

  if (auto *accessor = dyn_cast<AccessorDecl>(afd)) {
    if (accessor->isCoroutine()) {
      yieldTyInContext = accessor->getStorage()->getValueInterfaceType();
      yieldTyInContext = accessor->mapTypeIntoContext(*yieldTyInContext);
    }
  }
  if (resultTypeInContext->isEscapable() &&
      (!yieldTyInContext.has_value() || (*yieldTyInContext)->isEscapable())) {
    return true;
  }
  return false;
}

std::optional<LifetimeDependenceInfo>
LifetimeDependenceInfo::fromTypeRepr(AbstractFunctionDecl *afd, Type resultType,
                                     bool allowIndex) {
  auto *dc = afd->getDeclContext();
  auto &ctx = dc->getASTContext();
  auto *mod = afd->getModuleContext();
  auto &diags = ctx.Diags;
  auto capacity = afd->getParameters()->size() + 1;
  auto lifetimeDependentRepr =
      cast<LifetimeDependentReturnTypeRepr>(afd->getResultTypeRepr());

  if (hasEscapableResultOrYield(afd, resultType)) {
    diags.diagnose(lifetimeDependentRepr->getLoc(),
                   diag::lifetime_dependence_invalid_return_type);
    return std::nullopt;
  }

  SmallBitVector inheritLifetimeParamIndices(capacity);
  SmallBitVector scopeLifetimeParamIndices(capacity);

  auto updateLifetimeDependenceInfo = [&](LifetimeDependenceSpecifier specifier,
                                          unsigned paramIndexToSet, Type type,
                                          ValueOwnership ownership) {
    auto loc = specifier.getLoc();
    auto kind = specifier.getLifetimeDependenceKind();

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
    if (kind == LifetimeDependenceKind::Consume ||
        kind == LifetimeDependenceKind::Copy) {
      if (type->isEscapable()) {
        diags.diagnose(loc, diag::lifetime_dependence_cannot_use_infer,
                       specifier.getLifetimeDependenceKindString());
        return true;
      }
    }

    // Diagnose when we have lifetime dependence on a type that is
    // BitwiseCopyable & Escapable.
    // ~Escapable types are non-trivial in SIL and we should not raise this
    // error.
    // TODO: Diagnose ~Escapable types are always non-trivial in SIL.
    if (type->isEscapable()) {
      if (ctx.LangOpts.hasFeature(Feature::BitwiseCopyable)) {
        auto *bitwiseCopyableProtocol =
            ctx.getProtocol(KnownProtocolKind::BitwiseCopyable);
        if (bitwiseCopyableProtocol &&
            mod->checkConformance(type, bitwiseCopyableProtocol)) {
          diags.diagnose(loc, diag::lifetime_dependence_on_bitwise_copyable);
          return true;
        }
      }
    }

    if (inheritLifetimeParamIndices.test(paramIndexToSet) ||
        scopeLifetimeParamIndices.test(paramIndexToSet)) {
      diags.diagnose(loc, diag::lifetime_dependence_duplicate_param_id);
      return true;
    }
    if (kind == LifetimeDependenceKind::Copy ||
        kind == LifetimeDependenceKind::Consume) {
      inheritLifetimeParamIndices.set(paramIndexToSet);
    } else {
      assert(kind == LifetimeDependenceKind::Borrow ||
             kind == LifetimeDependenceKind::Mutate);
      scopeLifetimeParamIndices.set(paramIndexToSet);
    }
    return false;
  };

  for (auto specifier : lifetimeDependentRepr->getLifetimeDependencies()) {
    switch (specifier.getSpecifierKind()) {
    case LifetimeDependenceSpecifier::SpecifierKind::Named: {
      bool foundParamName = false;
      unsigned paramIndex = 0;
      for (auto *param : *afd->getParameters()) {
        if (param->getParameterName() == specifier.getName()) {
          foundParamName = true;
          if (updateLifetimeDependenceInfo(
                  specifier, paramIndex + 1,
                  afd->mapTypeIntoContext(
                      param->toFunctionParam().getParameterType()),
                  param->getValueOwnership())) {
            return std::nullopt;
          }
          break;
        }
        paramIndex++;
      }
      if (!foundParamName) {
        diags.diagnose(specifier.getLoc(),
                       diag::lifetime_dependence_invalid_param_name,
                       specifier.getName());
        return std::nullopt;
      }
      break;
    }
    case LifetimeDependenceSpecifier::SpecifierKind::Ordered: {
      auto index = specifier.getIndex();
      if (index > afd->getParameters()->size()) {
        diags.diagnose(specifier.getLoc(),
                       diag::lifetime_dependence_invalid_param_index, index);
        return std::nullopt;
      }
      if (index != 0) {
        auto param = afd->getParameters()->get(index - 1);
        auto ownership = param->getValueOwnership();
        auto type = afd->mapTypeIntoContext(
            param->toFunctionParam().getParameterType());
        if (updateLifetimeDependenceInfo(specifier, index, type, ownership)) {
          return std::nullopt;
        }
        break;
      }
      LLVM_FALLTHROUGH;
    }
    case LifetimeDependenceSpecifier::SpecifierKind::Self: {
      if (!afd->hasImplicitSelfDecl()) {
        diags.diagnose(specifier.getLoc(),
                       diag::lifetime_dependence_invalid_self);
        return std::nullopt;
      }
      if (updateLifetimeDependenceInfo(
              specifier, /*selfIndex*/ 0,
              afd->getImplicitSelfDecl()->getTypeInContext(),
              afd->getImplicitSelfDecl()->getValueOwnership())) {
        return std::nullopt;
      }
      break;
    }
    }
  }

  return LifetimeDependenceInfo(
      inheritLifetimeParamIndices.any()
          ? IndexSubset::get(ctx, inheritLifetimeParamIndices)
          : nullptr,
      scopeLifetimeParamIndices.any()
          ? IndexSubset::get(ctx, scopeLifetimeParamIndices)
          : nullptr,
      /*isExplicit*/ true);
}

// This utility is similar to its overloaded version that builds the
// LifetimeDependenceInfo from the swift decl. Reason for duplicated code is the
// apis on type and ownership is different in SIL compared to Sema.
std::optional<LifetimeDependenceInfo> LifetimeDependenceInfo::fromTypeRepr(
    LifetimeDependentReturnTypeRepr *lifetimeDependentRepr,
    SmallVectorImpl<SILParameterInfo> &params, bool hasSelfParam,
    DeclContext *dc) {
  auto &ctx = dc->getASTContext();
  auto &diags = ctx.Diags;
  auto capacity = hasSelfParam ? params.size() : params.size() + 1;

  SmallBitVector inheritLifetimeParamIndices(capacity);
  SmallBitVector scopeLifetimeParamIndices(capacity);

  auto updateLifetimeDependenceInfo = [&](LifetimeDependenceSpecifier specifier,
                                          unsigned paramIndexToSet,
                                          ParameterConvention paramConvention) {
    auto loc = specifier.getLoc();
    auto kind = specifier.getLifetimeDependenceKind();

    // Once we have dependsOn()/dependsOn(scoped:) syntax, this enum values will
    // be Scope and Inherit.
    if (kind == LifetimeDependenceKind::Borrow &&
        (!isGuaranteedParameter(paramConvention) &&
         !isMutatingParameter(paramConvention))) {
      diags.diagnose(loc, diag::lifetime_dependence_cannot_use_kind, "_scope",
                     getStringForParameterConvention(paramConvention));
      return true;
    }

    if (inheritLifetimeParamIndices.test(paramIndexToSet) ||
        scopeLifetimeParamIndices.test(paramIndexToSet)) {
      diags.diagnose(loc, diag::lifetime_dependence_duplicate_param_id);
      return true;
    }
    if (kind == LifetimeDependenceKind::Copy) {
      inheritLifetimeParamIndices.set(paramIndexToSet);
    } else {
      assert(kind == LifetimeDependenceKind::Borrow);
      scopeLifetimeParamIndices.set(paramIndexToSet);
    }
    return false;
  };

  for (auto specifier : lifetimeDependentRepr->getLifetimeDependencies()) {
    assert(specifier.getSpecifierKind() ==
           LifetimeDependenceSpecifier::SpecifierKind::Ordered);
    auto index = specifier.getIndex();
    if (index > params.size()) {
      diags.diagnose(specifier.getLoc(),
                     diag::lifetime_dependence_invalid_param_index, index);
      return std::nullopt;
    }
    if (index == 0 && !hasSelfParam) {
      diags.diagnose(specifier.getLoc(),
                     diag::lifetime_dependence_invalid_self);
      return std::nullopt;
    }
    auto param = index == 0 ? params.back() : params[index - 1];
    auto paramConvention = param.getConvention();
    if (updateLifetimeDependenceInfo(specifier, index, paramConvention)) {
      return std::nullopt;
    }
  }

  return LifetimeDependenceInfo(
      inheritLifetimeParamIndices.any()
          ? IndexSubset::get(ctx, inheritLifetimeParamIndices)
          : nullptr,
      scopeLifetimeParamIndices.any()
          ? IndexSubset::get(ctx, scopeLifetimeParamIndices)
          : nullptr,
      /*isExplicit*/ true);
}

std::optional<LifetimeDependenceInfo>
LifetimeDependenceInfo::infer(AbstractFunctionDecl *afd, Type resultType) {
  auto *dc = afd->getDeclContext();
  auto &ctx = dc->getASTContext();

  if (!ctx.LangOpts.hasFeature(Feature::NonescapableTypes)) {
    return std::nullopt;
  }

  // Perform lifetime dependence inference under a flag only. Currently all
  // stdlib types can appear is ~Escapable and ~Copyable.
  if (!ctx.LangOpts.EnableExperimentalLifetimeDependenceInference) {
    return std::nullopt;
  }

  auto &diags = ctx.Diags;
  auto returnTypeRepr = afd->getResultTypeRepr();
  auto returnLoc = returnTypeRepr ? returnTypeRepr->getLoc() : afd->getLoc();

  if (hasEscapableResultOrYield(afd, resultType)) {
    return std::nullopt;
  }

  if (afd->getAttrs().hasAttribute<UnsafeNonEscapableResultAttr>()) {
    return std::nullopt;
  }

  if (afd->getKind() != DeclKind::Constructor && afd->hasImplicitSelfDecl()) {
    ValueOwnership ownership = ValueOwnership::Default;
    if (auto *AD = dyn_cast<AccessorDecl>(afd)) {
      if (AD->getAccessorKind() == AccessorKind::Get ||
          AD->getAccessorKind() == AccessorKind::Read) {
        // We don't support "borrowing/consuming" ownership modifiers on
        // getters/_read accessors, they are guaranteed by default for now.
        ownership = ValueOwnership::Shared;
      }
      if (AD->getAccessorKind() == AccessorKind::Modify) {
        ownership = ValueOwnership::InOut;
      }
    } else {
      assert(afd->getKind() == DeclKind::Func);
      ownership = afd->getImplicitSelfDecl()->getValueOwnership();
      if (ownership == ValueOwnership::Default) {
        diags.diagnose(
            returnLoc,
            diag::
                lifetime_dependence_cannot_infer_wo_ownership_modifier_on_method);
        return std::nullopt;
      }
    }
    return LifetimeDependenceInfo::getForParamIndex(afd, /*selfIndex*/ 0,
                                                    ownership);
  }

  auto *cd = dyn_cast<ConstructorDecl>(afd);
  if (cd && cd->isImplicit() && cd->getParameters()->size() == 0) {
    return std::nullopt;
  }

  LifetimeDependenceInfo lifetimeDependenceInfo;
  ParamDecl *candidateParam = nullptr;
  unsigned paramIndex = 0;
  bool hasParamError = false;
  for (auto *param : *afd->getParameters()) {
    SWIFT_DEFER {
      paramIndex++;
    };
    Type paramTypeInContext =
        afd->mapTypeIntoContext(param->getInterfaceType());

    if (paramTypeInContext->hasError()) {
      hasParamError = true;
      continue;
    }
    if (param->getValueOwnership() == ValueOwnership::Default) {
      continue;
    }
    if (param->getValueOwnership() == ValueOwnership::Owned &&
        paramTypeInContext->isEscapable()) {
      continue;
    }

    if (candidateParam) {
      diags.diagnose(
          returnLoc,
          diag::lifetime_dependence_cannot_infer_ambiguous_candidate);
      return std::nullopt;
    }
    candidateParam = param;
    lifetimeDependenceInfo = LifetimeDependenceInfo::getForParamIndex(
        afd, paramIndex + 1, param->getValueOwnership());
  }
  if (cd && cd->isImplicit()) {
    diags.diagnose(cd->getLoc(),
                   diag::lifetime_dependence_cannot_infer_implicit_init);
    return std::nullopt;
  }
  if (!candidateParam && !hasParamError) {
    // Explicitly turn off error messages for builtins, since some of are
    // ~Escapable currently.
    // TODO: rdar://123555720: Remove this check after another round of
    // surveying builtins
    if (auto *fd = dyn_cast<FuncDecl>(afd)) {
      if (fd->isImplicit() && fd->getModuleContext()->isBuiltinModule()) {
        return std::nullopt;
      }
    }
    diags.diagnose(returnLoc,
                   diag::lifetime_dependence_cannot_infer_no_candidates);
    return std::nullopt;
  }
  return lifetimeDependenceInfo;
}

std::optional<LifetimeDependenceInfo>
LifetimeDependenceInfo::get(AbstractFunctionDecl *afd, Type resultType,
                            bool allowIndex) {
  auto *returnTypeRepr = afd->getResultTypeRepr();
  if (isa_and_nonnull<LifetimeDependentReturnTypeRepr>(returnTypeRepr)) {
    return LifetimeDependenceInfo::fromTypeRepr(afd, resultType, allowIndex);
  }
  return LifetimeDependenceInfo::infer(afd, resultType);
}

LifetimeDependenceInfo
LifetimeDependenceInfo::get(ASTContext &ctx,
                            const SmallBitVector &inheritLifetimeIndices,
                            const SmallBitVector &scopeLifetimeIndices) {
  return LifetimeDependenceInfo{
      inheritLifetimeIndices.any()
          ? IndexSubset::get(ctx, inheritLifetimeIndices)
          : nullptr,
      scopeLifetimeIndices.any() ? IndexSubset::get(ctx, scopeLifetimeIndices)
                                 : nullptr};
}

std::optional<LifetimeDependenceKind>
LifetimeDependenceInfo::getLifetimeDependenceOnParam(unsigned paramIndex) {
  if (inheritLifetimeParamIndices) {
    if (inheritLifetimeParamIndices->contains(paramIndex)) {
      // Can arbitarily return copy or consume here.
      // If we converge on dependsOn(borrowed: paramName)/dependsOn(borrowed:
      // paramName) syntax, this can be a single case value.
      return LifetimeDependenceKind::Copy;
    }
  }
  if (scopeLifetimeParamIndices) {
    if (scopeLifetimeParamIndices->contains(paramIndex)) {
      // Can arbitarily return borrow or mutate here.
      // If we converge on dependsOn(borrowed: paramName)/dependsOn(borrowed:
      // paramName) syntax, this can be a single case value.
      return LifetimeDependenceKind::Borrow;
    }
  }
  return {};
}

} // namespace swift
