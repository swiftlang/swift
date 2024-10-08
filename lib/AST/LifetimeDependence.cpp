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

LifetimeEntry *
LifetimeEntry::create(const ASTContext &ctx, SourceLoc startLoc,
                      SourceLoc endLoc, ArrayRef<LifetimeDescriptor> sources,
                      std::optional<LifetimeDescriptor> targetDescriptor) {
  unsigned size = totalSizeToAlloc<LifetimeDescriptor>(sources.size());
  void *mem = ctx.Allocate(size, alignof(LifetimeEntry));
  return new (mem) LifetimeEntry(startLoc, endLoc, sources, targetDescriptor);
}

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
  std::string lifetimeDependenceString = "@lifetime(";
  auto getSourceString = [](IndexSubset *bitvector, StringRef kind) {
    std::string result;
    bool isFirstSetBit = true;
    for (unsigned i = 0; i < bitvector->getCapacity(); i++) {
      if (bitvector->contains(i)) {
        if (!isFirstSetBit) {
          result += ", ";
        }
        result += kind;
        result += std::to_string(i);
        isFirstSetBit = false;
      }
    }
    return result;
  };
  if (inheritLifetimeParamIndices) {
    assert(!inheritLifetimeParamIndices->isEmpty());
    lifetimeDependenceString +=
        getSourceString(inheritLifetimeParamIndices, "copy ");
  }
  if (scopeLifetimeParamIndices) {
    assert(!scopeLifetimeParamIndices->isEmpty());
    if (inheritLifetimeParamIndices) {
      lifetimeDependenceString += ", ";
    }
    lifetimeDependenceString +=
        getSourceString(scopeLifetimeParamIndices, "borrow ");
  }
  if (isImmortal()) {
    lifetimeDependenceString += "immortal";
  }
  lifetimeDependenceString += ") ";
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
        isYieldingDefaultMutatingAccessor(ad->getAccessorKind())) {
      return ValueOwnership::InOut;
    }
  }
  return ValueOwnership::Shared;
}

static bool isBitwiseCopyable(Type type, ASTContext &ctx) {
  auto *bitwiseCopyableProtocol =
      ctx.getProtocol(KnownProtocolKind::BitwiseCopyable);
  if (!bitwiseCopyableProtocol) {
    return false;
  }
  return (bool)checkConformance(type, bitwiseCopyableProtocol);
}

static bool
isLifetimeDependenceCompatibleWithOwnership(LifetimeDependenceKind kind,
                                            Type type, ValueOwnership ownership,
                                            AbstractFunctionDecl *afd) {
  auto &ctx = afd->getASTContext();
  if (kind == LifetimeDependenceKind::Inherit) {
    return true;
  }
  // Lifetime dependence always propagates through temporary BitwiseCopyable
  // values, even if the dependence is scoped.
  if (isBitwiseCopyable(type, ctx)) {
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

static std::optional<LifetimeDependenceKind>
getLifetimeDependenceKind(LifetimeDescriptor descriptor,
                          AbstractFunctionDecl *afd, ParamDecl *decl) {
  auto &ctx = afd->getASTContext();
  auto &diags = ctx.Diags;
  auto loc = descriptor.getLoc();

  auto ownership = decl->getValueOwnership();
  auto type = decl->getTypeInContext();

  // For @lifetime attribute, we check if we had a "borrow" modifier, if not
  // we infer inherit dependence.
  auto parsedLifetimeKind = descriptor.getParsedLifetimeDependenceKind();
  if (parsedLifetimeKind == ParsedLifetimeDependenceKind::Scope) {
    bool isCompatible = isLifetimeDependenceCompatibleWithOwnership(
        LifetimeDependenceKind::Scope, type, ownership, afd);
    if (!isCompatible) {
      diags.diagnose(
          loc, diag::lifetime_dependence_cannot_use_parsed_borrow_consuming);
      return std::nullopt;
    }
    return LifetimeDependenceKind::Scope;
  }
  if (type->isEscapable()) {
    diags.diagnose(loc,
                   diag::lifetime_dependence_invalid_inherit_escapable_type);
    return std::nullopt;
  }
  return LifetimeDependenceKind::Inherit;
}

// Finds the ParamDecl* and its index from a LifetimeDescriptor
static std::optional<std::pair<ParamDecl *, unsigned>>
getParamDeclFromDescriptor(AbstractFunctionDecl *afd,
                           LifetimeDescriptor descriptor) {
  auto *dc = afd->getDeclContext();
  auto &ctx = dc->getASTContext();
  auto &diags = ctx.Diags;
  switch (descriptor.getDescriptorKind()) {
  case LifetimeDescriptor::DescriptorKind::Named: {
    unsigned paramIndex = 0;
    ParamDecl *candidateParam = nullptr;
    for (auto *param : *afd->getParameters()) {
      if (param->getParameterName().str() == descriptor.getName()) {
        candidateParam = param;
        break;
      }
      paramIndex++;
    }
    if (!candidateParam) {
      diags.diagnose(descriptor.getLoc(),
                     diag::lifetime_dependence_invalid_param_name,
                     descriptor.getName());
      return std::nullopt;
    }
    return std::make_pair(candidateParam, paramIndex);
  }
  case LifetimeDescriptor::DescriptorKind::Ordered: {
    auto paramIndex = descriptor.getIndex();
    if (paramIndex >= afd->getParameters()->size()) {
      diags.diagnose(descriptor.getLoc(),
                     diag::lifetime_dependence_invalid_param_index, paramIndex);
      return std::nullopt;
    }
    auto candidateParam = afd->getParameters()->get(paramIndex);
    return std::make_pair(candidateParam, paramIndex);
  }
  case LifetimeDescriptor::DescriptorKind::Self: {
    if (!afd->hasImplicitSelfDecl()) {
      diags.diagnose(descriptor.getLoc(),
                     diag::lifetime_dependence_invalid_self_in_static);
      return std::nullopt;
    }
    if (isa<ConstructorDecl>(afd)) {
      diags.diagnose(descriptor.getLoc(),
                     diag::lifetime_dependence_invalid_self_in_init);
      return std::nullopt;
    }
    auto *selfDecl = afd->getImplicitSelfDecl();
    return std::make_pair(selfDecl, afd->getParameters()->size());
  }
  }
}

static std::optional<LifetimeDependenceInfo>
populateLifetimeDependence(AbstractFunctionDecl *afd, LifetimeEntry *entry) {
  auto *dc = afd->getDeclContext();
  auto &ctx = dc->getASTContext();
  auto &diags = ctx.Diags;
  auto capacity = afd->hasImplicitSelfDecl()
                      ? (afd->getParameters()->size() + 1)
                      : afd->getParameters()->size();

  SmallBitVector inheritIndices(capacity);
  SmallBitVector scopeIndices(capacity);

  auto updateLifetimeIndices = [&](LifetimeDescriptor descriptor,
                                   unsigned paramIndexToSet,
                                   LifetimeDependenceKind lifetimeKind) {
    if (inheritIndices.test(paramIndexToSet) ||
        scopeIndices.test(paramIndexToSet)) {
      diags.diagnose(descriptor.getLoc(),
                     diag::lifetime_dependence_duplicate_param_id);
      return true;
    }
    if (lifetimeKind == LifetimeDependenceKind::Inherit) {
      inheritIndices.set(paramIndexToSet);
    } else {
      assert(lifetimeKind == LifetimeDependenceKind::Scope);
      scopeIndices.set(paramIndexToSet);
    }
    return false;
  };

  auto targetDescriptor = entry->getTargetDescriptor();
  unsigned targetIndex;
  if (targetDescriptor.has_value()) {
    auto targetDeclAndIndex =
        getParamDeclFromDescriptor(afd, *targetDescriptor);
    if (!targetDeclAndIndex.has_value()) {
      return std::nullopt;
    }
    targetIndex = targetDeclAndIndex->second;
  } else {
    targetIndex = afd->hasImplicitSelfDecl() ? afd->getParameters()->size() + 1
                                             : afd->getParameters()->size();
  }

  for (auto source : entry->getSources()) {
    if (source.isImmortal()) {
      auto immortalParam =
          std::find_if(afd->getParameters()->begin(),
                       afd->getParameters()->end(), [](ParamDecl *param) {
                         return strcmp(param->getName().get(), "immortal") == 0;
                       });
      if (immortalParam != afd->getParameters()->end()) {
        diags.diagnose(*immortalParam,
                       diag::lifetime_dependence_immortal_conflict_name);
        return std::nullopt;
      }
      return LifetimeDependenceInfo(nullptr, nullptr, targetIndex,
                                    /*isImmortal*/ true);
    }

    auto paramDeclAndIndex = getParamDeclFromDescriptor(afd, source);
    if (!paramDeclAndIndex.has_value()) {
      return std::nullopt;
    }
    auto lifetimeKind =
        getLifetimeDependenceKind(source, afd, paramDeclAndIndex->first);
    if (!lifetimeKind.has_value()) {
      return std::nullopt;
    }
    bool hasError =
        updateLifetimeIndices(source, paramDeclAndIndex->second, *lifetimeKind);
    if (hasError) {
      return std::nullopt;
    }
  }

  return LifetimeDependenceInfo(
      inheritIndices.any() ? IndexSubset::get(ctx, inheritIndices) : nullptr,
      scopeIndices.any() ? IndexSubset::get(ctx, scopeIndices) : nullptr,
      targetIndex, /*isImmortal*/ false);
}

std::optional<ArrayRef<LifetimeDependenceInfo>>
LifetimeDependenceInfo::fromLifetimeAttribute(AbstractFunctionDecl *afd) {
  auto *dc = afd->getDeclContext();
  auto &ctx = dc->getASTContext();
  auto &diags = ctx.Diags;
  
  SmallVector<LifetimeDependenceInfo, 1> lifetimeDependencies;
  llvm::SmallSet<unsigned, 1> lifetimeDependentTargets;
  auto lifetimeAttrs = afd->getAttrs().getAttributes<LifetimeAttr>();
  for (auto attr : lifetimeAttrs) {
    auto lifetimeDependenceInfo =
        populateLifetimeDependence(afd, attr->getLifetimeEntry());
    if (!lifetimeDependenceInfo.has_value()) {
      return std::nullopt;
    }
    auto targetIndex = lifetimeDependenceInfo->getTargetIndex();
    if (lifetimeDependentTargets.contains(targetIndex)) {
      // TODO: Diagnose at the source location of the @lifetime attribute with
      // duplicate target.
      diags.diagnose(afd->getLoc(), diag::lifetime_dependence_duplicate_target);
    }
    lifetimeDependentTargets.insert(targetIndex);
    lifetimeDependencies.push_back(*lifetimeDependenceInfo);
  }

  return afd->getASTContext().AllocateCopy(lifetimeDependencies);
}

// This utility is similar to its overloaded version that builds the
// LifetimeDependenceInfo from the swift decl. Reason for duplicated code is
// the apis on type and ownership is different in SIL compared to Sema.
std::optional<LifetimeDependenceInfo> LifetimeDependenceInfo::fromDependsOn(
    LifetimeDependentTypeRepr *lifetimeDependentRepr, unsigned targetIndex,
    ArrayRef<SILParameterInfo> params, DeclContext *dc) {
  auto &ctx = dc->getASTContext();
  auto &diags = ctx.Diags;
  auto capacity = params.size(); // SIL parameters include self

  SmallBitVector inheritLifetimeParamIndices(capacity);
  SmallBitVector scopeLifetimeParamIndices(capacity);

  auto updateLifetimeDependenceInfo = [&](LifetimeDescriptor descriptor,
                                          unsigned paramIndexToSet,
                                          ParameterConvention paramConvention) {
    auto loc = descriptor.getLoc();
    auto kind = descriptor.getParsedLifetimeDependenceKind();

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

  for (auto source : lifetimeDependentRepr->getLifetimeEntry()->getSources()) {
    switch (source.getDescriptorKind()) {
    case LifetimeDescriptor::DescriptorKind::Ordered: {
      auto index = source.getIndex();
      if (index > capacity) {
        diags.diagnose(source.getLoc(),
                       diag::lifetime_dependence_invalid_param_index, index);
        return std::nullopt;
      }
      auto param = params[index];
      auto paramConvention = param.getConvention();
      if (updateLifetimeDependenceInfo(source, index, paramConvention)) {
        return std::nullopt;
      }
      break;
    }
    case LifetimeDescriptor::DescriptorKind::Named: {
      assert(source.isImmortal());
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
    // Do not apply this to accessors (_modify). _modify is handled below like
    // a mutating method.
    if (fd->isMutating() && fd->getResultInterfaceType()->isVoid() &&
        !dc->getSelfTypeInContext()->isEscapable()) {
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
    if (!isLifetimeDependenceCompatibleWithOwnership(kind, selfTypeInContext,
                                                     selfOwnership, afd)) {
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
    if (!isLifetimeDependenceCompatibleWithOwnership(
            *candidateLifetimeKind, paramTypeInContext, paramOwnership, afd)) {
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

  if (afd->getAttrs().hasAttribute<LifetimeAttr>()) {
    return LifetimeDependenceInfo::fromLifetimeAttribute(afd);
  }

  SmallVector<LifetimeDependenceInfo, 1> lifetimeDependencies;
  auto resultDependence = LifetimeDependenceInfo::infer(afd);
  if (!resultDependence.has_value()) {
    return std::nullopt;
  }
  lifetimeDependencies.push_back(*resultDependence);
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

  auto getLifetimeDependenceFromDependsOnTypeModifier =
      [&](TypeRepr *typeRepr,
          unsigned targetIndex) -> std::optional<LifetimeDependenceInfo> {
    auto *lifetimeTypeRepr =
        dyn_cast_or_null<LifetimeDependentTypeRepr>(typeRepr);
    if (!lifetimeTypeRepr) {
      return std::nullopt;
    }
    return LifetimeDependenceInfo::fromDependsOn(lifetimeTypeRepr, targetIndex,
                                                 params, dc);
  };

  auto argsTypeRepr = funcRepr->getArgsTypeRepr()->getElements();
  for (unsigned targetIndex : indices(argsTypeRepr)) {
    if (auto result = getLifetimeDependenceFromDependsOnTypeModifier(
            argsTypeRepr[targetIndex].Type, targetIndex)) {
      lifetimeDependencies.push_back(*result);
    }
  }

  auto result = getLifetimeDependenceFromDependsOnTypeModifier(
      funcRepr->getResultTypeRepr(), params.size());
  if (result) {
    lifetimeDependencies.push_back(*result);
  }

  return dc->getASTContext().AllocateCopy(lifetimeDependencies);
}

} // namespace swift
