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
#include "swift/AST/ASTContext.h"
#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Type.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/Range.h"

namespace swift {

std::optional<LifetimeDependenceInfo>
getLifetimeDependenceFor(ArrayRef<LifetimeDependenceInfo> lifetimeDependencies,
                         unsigned index) {
  for (auto dep : lifetimeDependencies) {
    if (dep.getTargetIndex() == index) {
      return dep;
    }
  }
  return std::nullopt;
}

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

// Warning: this is incorrect for Setter 'newValue' parameters. It should only
// be called for a Setter's 'self'.
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

LifetimeDependenceInfo
LifetimeDependenceInfo::getForIndex(AbstractFunctionDecl *afd,
                                    unsigned targetIndex, unsigned sourceIndex,
                                    LifetimeDependenceKind kind) {
  auto *dc = afd->getDeclContext();
  auto &ctx = dc->getASTContext();
  unsigned capacity = afd->hasImplicitSelfDecl()
                          ? (afd->getParameters()->size() + 1)
                          : afd->getParameters()->size();
  auto indexSubset = IndexSubset::get(ctx, capacity, {sourceIndex});
  if (kind == LifetimeDependenceKind::Scope) {
    return LifetimeDependenceInfo{/*inheritLifetimeParamIndices*/ nullptr,
                                  /*scopeLifetimeParamIndices*/ indexSubset,
                                  targetIndex,
                                  /*isImmortal*/ false};
  }
  return LifetimeDependenceInfo{/*inheritLifetimeParamIndices*/ indexSubset,
                                /*scopeLifetimeParamIndices*/ nullptr,
                                targetIndex,
                                /*isImmortal*/ false};
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

static Type getResultOrYield(AbstractFunctionDecl *afd) {
  if (auto *accessor = dyn_cast<AccessorDecl>(afd)) {
    if (accessor->isCoroutine()) {
      auto yieldTyInContext = accessor->mapTypeIntoContext(
          accessor->getStorage()->getValueInterfaceType());
      return yieldTyInContext;
    }
  }
  Type resultType;
  if (auto fn = dyn_cast<FuncDecl>(afd)) {
    resultType = fn->getResultInterfaceType();
  } else {
    auto ctor = cast<ConstructorDecl>(afd);
    resultType = ctor->getResultInterfaceType();
  }
  return afd->mapTypeIntoContext(resultType);
}

static bool hasEscapableResultOrYield(AbstractFunctionDecl *afd) {
  return getResultOrYield(afd)->isEscapable();
}

static LifetimeDependenceKind getLifetimeDependenceKindFromDecl(
    ParsedLifetimeDependenceKind parsedLifetimeDependenceKind, Type paramType) {
  if (parsedLifetimeDependenceKind == ParsedLifetimeDependenceKind::Scope) {
    return LifetimeDependenceKind::Scope;
  }
  if (parsedLifetimeDependenceKind == ParsedLifetimeDependenceKind::Inherit) {
    // TODO: assert that this can happen in SIL tests
    return LifetimeDependenceKind::Inherit;
  }
  return paramType->isEscapable() ? LifetimeDependenceKind::Scope
                                  : LifetimeDependenceKind::Inherit;
}

static bool isBitwiseCopyable(Type type, ASTContext &ctx) {
  auto *bitwiseCopyableProtocol =
      ctx.getProtocol(KnownProtocolKind::BitwiseCopyable);
  if (!bitwiseCopyableProtocol) {
    return false;
  }
  return (bool)checkConformance(type, bitwiseCopyableProtocol);
}

std::optional<LifetimeDependenceInfo> LifetimeDependenceInfo::fromTypeRepr(
    AbstractFunctionDecl *afd, LifetimeDependentTypeRepr *lifetimeDependentRepr,
    unsigned targetIndex) {
  auto *dc = afd->getDeclContext();
  auto &ctx = dc->getASTContext();
  auto &diags = ctx.Diags;
  auto capacity = afd->hasImplicitSelfDecl()
                      ? (afd->getParameters()->size() + 1)
                      : afd->getParameters()->size();

  SmallBitVector inheritLifetimeParamIndices(capacity);
  SmallBitVector scopeLifetimeParamIndices(capacity);

  auto updateLifetimeDependenceInfo = [&](LifetimeDependenceSpecifier specifier,
                                          unsigned paramIndexToSet,
                                          Type paramType,
                                          ValueOwnership ownership) {
    auto loc = specifier.getLoc();
    auto parsedLifetimeKind = specifier.getParsedLifetimeDependenceKind();
    auto lifetimeKind =
        getLifetimeDependenceKindFromDecl(parsedLifetimeKind, paramType);
    bool isCompatible = true;
    // Lifetime dependence always propagates through temporary BitwiseCopyable
    // values, even if the dependence is scoped.
    if (!isBitwiseCopyable(paramType, ctx)) {
      isCompatible = isLifetimeDependenceCompatibleWithOwnership(
        lifetimeKind, ownership, afd);
    }
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
    case LifetimeDependenceSpecifier::SpecifierKind::Immortal: {
      auto immortalParam =
          std::find_if(afd->getParameters()->begin(), afd->getParameters()->end(), [](ParamDecl *param) {
            return strcmp(param->getName().get(), "immortal") == 0;
          });
      if (immortalParam != afd->getParameters()->end()) {
        diags.diagnose(*immortalParam,
                       diag::lifetime_dependence_immortal_conflict_name);
      }

      return LifetimeDependenceInfo(nullptr, nullptr, targetIndex,
                                    /*isImmortal*/ true);
    }
    case LifetimeDependenceSpecifier::SpecifierKind::Named: {
      bool foundParamName = false;
      unsigned paramIndex = 0;
      for (auto *param : *afd->getParameters()) {
        if (param->getParameterName() == specifier.getName()) {
          if (updateLifetimeDependenceInfo(
                  specifier, paramIndex,
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
      if (index >= afd->getParameters()->size()) {
        diags.diagnose(specifier.getLoc(),
                       diag::lifetime_dependence_invalid_param_index, index);
        return std::nullopt;
      }
      auto param = afd->getParameters()->get(index);
      auto ownership = param->getValueOwnership();
      auto type =
          afd->mapTypeIntoContext(param->toFunctionParam().getParameterType());
      if (updateLifetimeDependenceInfo(specifier, index, type, ownership)) {
        return std::nullopt;
      }
      break;
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
              specifier, /* selfIndex */ afd->getParameters()->size(),
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
      targetIndex,
      /*isImmortal*/ false);
}

// This utility is similar to its overloaded version that builds the
// LifetimeDependenceInfo from the swift decl. Reason for duplicated code is the
// apis on type and ownership is different in SIL compared to Sema.
std::optional<LifetimeDependenceInfo> LifetimeDependenceInfo::fromTypeRepr(
    LifetimeDependentTypeRepr *lifetimeDependentRepr, unsigned targetIndex,
    ArrayRef<SILParameterInfo> params, DeclContext *dc) {
  auto &ctx = dc->getASTContext();
  auto &diags = ctx.Diags;
  auto capacity = params.size(); // SIL parameters include self

  SmallBitVector inheritLifetimeParamIndices(capacity);
  SmallBitVector scopeLifetimeParamIndices(capacity);

  auto updateLifetimeDependenceInfo = [&](LifetimeDependenceSpecifier specifier,
                                          unsigned paramIndexToSet,
                                          ParameterConvention paramConvention) {
    auto loc = specifier.getLoc();
    auto kind = specifier.getParsedLifetimeDependenceKind();

    if (kind == ParsedLifetimeDependenceKind::Scope &&
        (!isGuaranteedParameterInCallee(paramConvention) &&
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
    switch (specifier.getSpecifierKind()) {
    case LifetimeDependenceSpecifier::SpecifierKind::Ordered: {
      auto index = specifier.getIndex();
      if (index > capacity) {
        diags.diagnose(specifier.getLoc(),
                       diag::lifetime_dependence_invalid_param_index, index);
        return std::nullopt;
      }
      auto param = params[index];
      auto paramConvention = param.getConvention();
      if (updateLifetimeDependenceInfo(specifier, index, paramConvention)) {
        return std::nullopt;
      }
      break;
    }
    case LifetimeDependenceSpecifier::SpecifierKind::Immortal: {
      return LifetimeDependenceInfo(/*inheritLifetimeParamIndices*/ nullptr,
                                    /*scopeLifetimeParamIndices*/ nullptr,
                                    targetIndex,
                                    /*isImmortal*/ true);
    }
    default:
      llvm_unreachable("SIL can only have ordered or immortal lifetime "
                       "dependence specifier kind");
    }
  }

  return LifetimeDependenceInfo(
      inheritLifetimeParamIndices.any()
          ? IndexSubset::get(ctx, inheritLifetimeParamIndices)
          : nullptr,
      scopeLifetimeParamIndices.any()
          ? IndexSubset::get(ctx, scopeLifetimeParamIndices)
          : nullptr,
      targetIndex,
      /*isImmortal*/ false);
}

std::optional<LifetimeDependenceInfo>
LifetimeDependenceInfo::infer(AbstractFunctionDecl *afd) {
  auto *dc = afd->getDeclContext();
  auto &ctx = dc->getASTContext();

  // Disable inference if requested.
  if (!ctx.LangOpts.EnableExperimentalLifetimeDependenceInference) {
    return std::nullopt;
  }

  if (getResultOrYield(afd)->hasError()) {
    return std::nullopt;
  }

  // Setters infer 'self' dependence on 'newValue'.
  if (auto accessor = dyn_cast<AccessorDecl>(afd)) {
    if (accessor->getAccessorKind() == AccessorKind::Set) {
      return inferSetter(accessor);
    }
  } else if (auto *fd = dyn_cast<FuncDecl>(afd)) {
    // Infer self dependence for a mutating function with no result.
    //
    // FIXME: temporary hack until we have dependsOn(self: param) syntax.
    // Do not apply this to accessors (_modify). _modify is handled below like a
    // mutating method.
    if (fd->isMutating() && fd->getResultInterfaceType()->isVoid()
        && !dc->getSelfTypeInContext()->isEscapable()) {
      return inferMutatingSelf(afd);
    }
  }

  if (hasEscapableResultOrYield(afd)) {
    return std::nullopt;
  }

  if (afd->getAttrs().hasAttribute<UnsafeNonEscapableResultAttr>()) {
    return std::nullopt;
  }

  auto &diags = ctx.Diags;
  auto returnTypeRepr = afd->getResultTypeRepr();
  auto returnLoc = returnTypeRepr ? returnTypeRepr->getLoc() : afd->getLoc();
  unsigned resultIndex = afd->hasImplicitSelfDecl()
                             ? afd->getParameters()->size() + 1
                             : afd->getParameters()->size();

  auto *cd = dyn_cast<ConstructorDecl>(afd);
  if (cd && cd->isImplicit()) {
    if (cd->getParameters()->size() == 0) {
      return std::nullopt;
    }
  }

  if (!cd && afd->hasImplicitSelfDecl()) {
    Type selfTypeInContext = dc->getSelfTypeInContext();
    if (selfTypeInContext->isEscapable()) {
      if (isBitwiseCopyable(selfTypeInContext, ctx)) {
          diags.diagnose(
              returnLoc,
              diag::lifetime_dependence_method_escapable_bitwisecopyable_self);
          return std::nullopt;
      }
    }
    auto kind = getLifetimeDependenceKindFromType(selfTypeInContext);
    auto selfOwnership = afd->getImplicitSelfDecl()->getValueOwnership();
    if (!isLifetimeDependenceCompatibleWithOwnership(kind, selfOwnership,
                                                     afd)) {
      diags.diagnose(returnLoc,
                     diag::lifetime_dependence_invalid_self_ownership);
      return std::nullopt;
    }

    // Infer method dependence: result depends on self.
    //
    // This includes _modify. A _modify's yielded value depends on self. The
    // caller of the _modify ensures that the 'self' depends on any value stored
    // to the yielded address.
    return LifetimeDependenceInfo::getForIndex(
        afd, resultIndex, /*selfIndex */ afd->getParameters()->size(), kind);
  }

  std::optional<unsigned> candidateParamIndex;
  std::optional<LifetimeDependenceKind> candidateLifetimeKind;
  unsigned paramIndex = 0;
  bool hasParamError = false;
  for (auto *param : *afd->getParameters()) {
    SWIFT_DEFER { paramIndex++; };
    Type paramTypeInContext =
        afd->mapTypeIntoContext(param->getInterfaceType());
    if (paramTypeInContext->hasError()) {
      hasParamError = true;
      continue;
    }
    auto paramOwnership = param->getValueOwnership();
    if (paramTypeInContext->isEscapable()) {
      if (isBitwiseCopyable(paramTypeInContext, ctx)) {
        continue;
      }
      if (paramOwnership == ValueOwnership::Default) {
        continue;
      }
    }

    candidateLifetimeKind =
        getLifetimeDependenceKindFromType(paramTypeInContext);
    if (!isLifetimeDependenceCompatibleWithOwnership(*candidateLifetimeKind,
                                                     paramOwnership, afd)) {
      continue;
    }
    if (candidateParamIndex) {
      if (cd && afd->isImplicit()) {
        diags.diagnose(
            returnLoc,
            diag::lifetime_dependence_cannot_infer_ambiguous_candidate,
            "on implicit initializer");
        return std::nullopt;
      }
      diags.diagnose(returnLoc,
                     diag::lifetime_dependence_cannot_infer_ambiguous_candidate,
                     "");
      return std::nullopt;
    }
    candidateParamIndex = paramIndex;
  }

  if (!candidateParamIndex && !hasParamError) {
    if (cd && afd->isImplicit()) {
      diags.diagnose(returnLoc,
                     diag::lifetime_dependence_cannot_infer_no_candidates,
                     " on implicit initializer");
      return std::nullopt;
    }
    diags.diagnose(returnLoc,
                   diag::lifetime_dependence_cannot_infer_no_candidates, "");
    return std::nullopt;
  }

  return LifetimeDependenceInfo::getForIndex(
      afd, resultIndex, *candidateParamIndex, *candidateLifetimeKind);
}

/// Infer LifetimeDependence on a setter where 'self' is nonescapable.
std::optional<LifetimeDependenceInfo> LifetimeDependenceInfo::inferSetter(
  AbstractFunctionDecl *afd) {

  auto *param = afd->getParameters()->get(0);
  Type paramTypeInContext =
    afd->mapTypeIntoContext(param->getInterfaceType());
  if (paramTypeInContext->hasError()) {
    return std::nullopt;
  }
  if (paramTypeInContext->isEscapable()) {
    return std::nullopt;
  }
  auto kind = getLifetimeDependenceKindFromType(paramTypeInContext);
  return LifetimeDependenceInfo::getForIndex(
    afd, /*selfIndex */ afd->getParameters()->size(), 0, kind);
}

/// Infer LifetimeDependenceInfo on a mutating method where 'self' is
/// nonescapable and the result is 'void'. For now, we'll assume that 'self'
/// depends on a single nonescapable argument.
std::optional<LifetimeDependenceInfo> LifetimeDependenceInfo::inferMutatingSelf(
  AbstractFunctionDecl *afd) {
  std::optional<LifetimeDependenceInfo> dep;
  for (unsigned paramIndex : range(afd->getParameters()->size())) {
    auto *param = afd->getParameters()->get(paramIndex);
    Type paramTypeInContext =
      afd->mapTypeIntoContext(param->getInterfaceType());
    if (paramTypeInContext->hasError()) {
      continue;
    }
    if (paramTypeInContext->isEscapable()) {
      continue;
    }
    if (dep) {
      // Don't infer dependence on multiple nonescapable parameters. We may want
      // to do this in the future if dependsOn(self: arg1, arg2) syntax is too
      // cumbersome.
      return std::nullopt;
    }
    int selfIndex = afd->getParameters()->size();
    dep = LifetimeDependenceInfo::getForIndex(
      afd, selfIndex, paramIndex, LifetimeDependenceKind::Inherit);
  }
  return dep;
}

std::optional<llvm::ArrayRef<LifetimeDependenceInfo>>
LifetimeDependenceInfo::get(AbstractFunctionDecl *afd) {
  if (!afd->getASTContext().LangOpts.hasFeature(Feature::NonescapableTypes)) {
    return std::nullopt;
  }
  assert(isa<FuncDecl>(afd) || isa<ConstructorDecl>(afd));
  SmallVector<LifetimeDependenceInfo> lifetimeDependencies;
  auto &diags = afd->getASTContext().Diags;

  auto getExplicitLifetimeDependence =
      [&](TypeRepr *typeRepr, unsigned targetIndex,
          Type targetType) -> std::optional<LifetimeDependenceInfo> {
    auto *lifetimeTypeRepr =
        dyn_cast_or_null<LifetimeDependentTypeRepr>(typeRepr);
    if (!lifetimeTypeRepr) {
      return std::nullopt;
    }
    if (targetType->isEscapable()) {
      diags.diagnose(lifetimeTypeRepr->getLoc(),
                     diag::lifetime_dependence_invalid_type);
      return std::nullopt;
    }
    return LifetimeDependenceInfo::fromTypeRepr(afd, lifetimeTypeRepr,
                                                targetIndex);
  };
  auto getExplicitOrImplicitLifetimeDependence =
      [&](TypeRepr *typeRepr, unsigned targetIndex,
          Type targetType) -> std::optional<LifetimeDependenceInfo> {
    if (isa_and_nonnull<LifetimeDependentTypeRepr>(typeRepr)) {
      return getExplicitLifetimeDependence(typeRepr, targetIndex, targetType);
    }
    return LifetimeDependenceInfo::infer(afd);
  };

  for (unsigned targetIndex : indices(*afd->getParameters())) {
    auto *param = (*afd->getParameters())[targetIndex];
    auto paramType =
        afd->mapTypeIntoContext(param->toFunctionParam().getParameterType());
    if (auto result = getExplicitLifetimeDependence(param->getTypeRepr(),
                                                    targetIndex, paramType)) {
      lifetimeDependencies.push_back(*result);
    }
  }

  auto result = getExplicitOrImplicitLifetimeDependence(
      afd->getResultTypeRepr(),
      afd->hasImplicitSelfDecl() ? afd->getParameters()->size() + 1
                                 : afd->getParameters()->size(),
      getResultOrYield(afd));
  if (result) {
    lifetimeDependencies.push_back(*result);
  }

  return afd->getASTContext().AllocateCopy(lifetimeDependencies);
}

std::optional<llvm::ArrayRef<LifetimeDependenceInfo>>
LifetimeDependenceInfo::get(FunctionTypeRepr *funcRepr,
                            ArrayRef<SILParameterInfo> params,
                            ArrayRef<SILResultInfo> results, DeclContext *dc) {
  if (!dc->getASTContext().LangOpts.hasFeature(Feature::NonescapableTypes)) {
    return std::nullopt;
  }
  SmallVector<LifetimeDependenceInfo, 1> lifetimeDependencies;

  auto getExplicitLifetimeDependence =
      [&](TypeRepr *typeRepr,
          unsigned targetIndex) -> std::optional<LifetimeDependenceInfo> {
    auto *lifetimeTypeRepr =
        dyn_cast_or_null<LifetimeDependentTypeRepr>(typeRepr);
    if (!lifetimeTypeRepr) {
      return std::nullopt;
    }
    return LifetimeDependenceInfo::fromTypeRepr(lifetimeTypeRepr, targetIndex,
                                                params, dc);
  };

  auto argsTypeRepr = funcRepr->getArgsTypeRepr()->getElements();
  for (unsigned targetIndex : indices(argsTypeRepr)) {
    if (auto result = getExplicitLifetimeDependence(
            argsTypeRepr[targetIndex].Type, targetIndex)) {
      lifetimeDependencies.push_back(*result);
    }
  }

  auto result = getExplicitLifetimeDependence(funcRepr->getResultTypeRepr(),
                                              params.size());
  if (result) {
    lifetimeDependencies.push_back(*result);
  }

  return dc->getASTContext().AllocateCopy(lifetimeDependencies);
}

} // namespace swift
