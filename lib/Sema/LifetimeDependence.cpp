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
        "_inherit(" + getOnIndices(inheritLifetimeParamIndices) + ") ";
  }
  if (scopeLifetimeParamIndices && !scopeLifetimeParamIndices->isEmpty()) {
    lifetimeDependenceString +=
        "_scope(" + getOnIndices(scopeLifetimeParamIndices) + ") ";
  }
  return lifetimeDependenceString;
}

void LifetimeDependenceInfo::Profile(llvm::FoldingSetNodeID &ID) const {
  if (inheritLifetimeParamIndices) {
    ID.AddInteger((uint8_t)LifetimeDependenceKind::Inherit);
    inheritLifetimeParamIndices->Profile(ID);
  }
  if (scopeLifetimeParamIndices) {
    ID.AddInteger((uint8_t)LifetimeDependenceKind::Scope);
    scopeLifetimeParamIndices->Profile(ID);
  }
}

static LifetimeDependenceKind
getLifetimeDependenceKindFromType(Type sourceType) {
  if (sourceType->isEscapable()) {
    return LifetimeDependenceKind::Scope;
  }
  return LifetimeDependenceKind::Inherit;
}

static ValueOwnership getLoweredOwnership(AbstractFunctionDecl *afd) {
  if (isa<ConstructorDecl>(afd)) {
    return ValueOwnership::Owned;
  }
  if (auto *ad = dyn_cast<AccessorDecl>(afd)) {
    if (ad->getAccessorKind() == AccessorKind::Set ||
        ad->getAccessorKind() == AccessorKind::Modify) {
      return ValueOwnership::InOut;
    }
  }
  return ValueOwnership::Shared;
}

static bool
isLifetimeDependenceCompatibleWithOwnership(LifetimeDependenceKind kind,
                                            ValueOwnership ownership,
                                            AbstractFunctionDecl *afd) {
  if (kind == LifetimeDependenceKind::Inherit) {
    return true;
  }
  assert(kind == LifetimeDependenceKind::Scope);
  auto loweredOwnership = ownership != ValueOwnership::Default
                              ? ownership
                              : getLoweredOwnership(afd);

  if (loweredOwnership == ValueOwnership::InOut ||
      loweredOwnership == ValueOwnership::Shared) {
    return true;
  }
  assert(loweredOwnership == ValueOwnership::Owned);
  return false;
}

LifetimeDependenceInfo LifetimeDependenceInfo::getForParamIndex(
    AbstractFunctionDecl *afd, unsigned index, LifetimeDependenceKind kind) {
  auto *dc = afd->getDeclContext();
  auto &ctx = dc->getASTContext();
  unsigned capacity = afd->getParameters()->size() + 1;
  auto indexSubset = IndexSubset::get(ctx, capacity, {index});
  if (kind == LifetimeDependenceKind::Scope) {
    return LifetimeDependenceInfo{/*inheritLifetimeParamIndices*/ nullptr,
                                  /*scopeLifetimeParamIndices*/ indexSubset};
  }
  return LifetimeDependenceInfo{/*inheritLifetimeParamIndices*/ indexSubset,
                                /*scopeLifetimeParamIndices*/ nullptr};
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

static LifetimeDependenceKind getLifetimeDependenceKindFromDecl(
    ParsedLifetimeDependenceKind parsedLifetimeDependenceKind, Type paramType) {
  if (parsedLifetimeDependenceKind == ParsedLifetimeDependenceKind::Scope) {
    return LifetimeDependenceKind::Scope;
  }
  if (parsedLifetimeDependenceKind == ParsedLifetimeDependenceKind::Inherit) {
    // TODO: assert that this can happen only on deserialized decls
    return LifetimeDependenceKind::Inherit;
  }
  return paramType->isEscapable() ? LifetimeDependenceKind::Scope
                                  : LifetimeDependenceKind::Inherit;
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
                                          unsigned paramIndexToSet,
                                          Type paramType,
                                          ValueOwnership ownership) {
    auto loc = specifier.getLoc();

    // Diagnose when we have lifetime dependence on a type that is
    // BitwiseCopyable & Escapable.
    // ~Escapable types are non-trivial in SIL and we should not raise this
    // error.
    // TODO: Diagnose ~Escapable types are always non-trivial in SIL.
    if (paramType->isEscapable()) {
      if (ctx.LangOpts.hasFeature(Feature::BitwiseCopyable)) {
        auto *bitwiseCopyableProtocol =
            ctx.getProtocol(KnownProtocolKind::BitwiseCopyable);
        if (bitwiseCopyableProtocol &&
            mod->checkConformance(paramType, bitwiseCopyableProtocol)) {
          diags.diagnose(loc, diag::lifetime_dependence_on_bitwise_copyable);
          return true;
        }
      }
    }

    auto parsedLifetimeKind = specifier.getParsedLifetimeDependenceKind();
    auto lifetimeKind =
        getLifetimeDependenceKindFromDecl(parsedLifetimeKind, paramType);
    bool isCompatible = isLifetimeDependenceCompatibleWithOwnership(
        lifetimeKind, ownership, afd);
    if (parsedLifetimeKind == ParsedLifetimeDependenceKind::Scope &&
        !isCompatible) {
      diags.diagnose(
          loc, diag::lifetime_dependence_cannot_use_parsed_scoped_consuming);
      return true;
    }
    if (!isCompatible) {
      assert(lifetimeKind == LifetimeDependenceKind::Scope);
      diags.diagnose(
          loc, diag::lifetime_dependence_cannot_use_inferred_scoped_consuming);
      return true;
    }

    if (inheritLifetimeParamIndices.test(paramIndexToSet) ||
        scopeLifetimeParamIndices.test(paramIndexToSet)) {
      diags.diagnose(loc, diag::lifetime_dependence_duplicate_param_id);
      return true;
    }
    if (lifetimeKind == LifetimeDependenceKind::Inherit) {
      inheritLifetimeParamIndices.set(paramIndexToSet);
    } else {
      assert(lifetimeKind == LifetimeDependenceKind::Scope);
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
          if (updateLifetimeDependenceInfo(
                  specifier, paramIndex + 1,
                  afd->mapTypeIntoContext(
                      param->toFunctionParam().getParameterType()),
                  param->getValueOwnership())) {
            return std::nullopt;
          }
          foundParamName = true;
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
                       diag::lifetime_dependence_invalid_self_in_static);
        return std::nullopt;
      }
      if (isa<ConstructorDecl>(afd)) {
        diags.diagnose(specifier.getLoc(),
                       diag::lifetime_dependence_invalid_self_in_init);
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
    auto kind = specifier.getParsedLifetimeDependenceKind();

    if (kind == ParsedLifetimeDependenceKind::Scope &&
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
    if (kind == ParsedLifetimeDependenceKind::Inherit) {
      inheritLifetimeParamIndices.set(paramIndexToSet);
    } else {
      assert(kind == ParsedLifetimeDependenceKind::Scope);
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
                     diag::lifetime_dependence_invalid_self_in_static);
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

  // Disable inference if requested.
  if (!ctx.LangOpts.EnableExperimentalLifetimeDependenceInference) {
    return std::nullopt;
  }

  if (hasEscapableResultOrYield(afd, resultType)) {
    return std::nullopt;
  }

  if (afd->getAttrs().hasAttribute<UnsafeNonEscapableResultAttr>()) {
    return std::nullopt;
  }

  auto &diags = ctx.Diags;
  auto returnTypeRepr = afd->getResultTypeRepr();
  auto returnLoc = returnTypeRepr ? returnTypeRepr->getLoc()
                                  : afd->getLoc(/* SerializedOK */ false);

  auto *cd = dyn_cast<ConstructorDecl>(afd);
  if (cd && cd->isImplicit()) {
    if (cd->getParameters()->size() == 0) {
      return std::nullopt;
    } else {
      diags.diagnose(cd->getLoc(),
                     diag::lifetime_dependence_cannot_infer_implicit_init);
      return std::nullopt;
    }
  }

  if (afd->getKind() != DeclKind::Constructor && afd->hasImplicitSelfDecl()) {
    Type selfTypeInContext = dc->getSelfTypeInContext();
    auto kind = getLifetimeDependenceKindFromType(selfTypeInContext);
    auto selfOwnership = afd->getImplicitSelfDecl()->getValueOwnership();
    if (!isLifetimeDependenceCompatibleWithOwnership(kind, selfOwnership,
                                                     afd)) {
      diags.diagnose(returnLoc,
                     diag::lifetime_dependence_invalid_self_ownership);
      return std::nullopt;
    }
    return LifetimeDependenceInfo::getForParamIndex(afd, /*selfIndex*/ 0, kind);
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
    auto paramOwnership = param->getValueOwnership();
    if (paramTypeInContext->isEscapable() && paramOwnership == ValueOwnership::Default) {
      continue;
    }

    auto lifetimeKind = getLifetimeDependenceKindFromType(paramTypeInContext);
    if (!isLifetimeDependenceCompatibleWithOwnership(lifetimeKind, paramOwnership,
                                                     afd)) {
      continue;
    }
    if (candidateParam) {
      diags.diagnose(
          returnLoc,
          diag::lifetime_dependence_cannot_infer_ambiguous_candidate);
      return std::nullopt;
    }
    candidateParam = param;
    lifetimeDependenceInfo =
        LifetimeDependenceInfo::getForParamIndex(afd, paramIndex + 1, lifetimeKind);
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
      return LifetimeDependenceKind::Inherit;
    }
  }
  if (scopeLifetimeParamIndices) {
    if (scopeLifetimeParamIndices->contains(paramIndex)) {
      return LifetimeDependenceKind::Scope;
    }
  }
  return {};
}

} // namespace swift
