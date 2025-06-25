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
#include "swift/AST/Builtins.h"
#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/Type.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/Range.h"
#include "swift/Basic/SourceManager.h"

namespace swift {

LifetimeEntry *
LifetimeEntry::create(const ASTContext &ctx, SourceLoc startLoc,
                      SourceLoc endLoc, ArrayRef<LifetimeDescriptor> sources,
                      std::optional<LifetimeDescriptor> targetDescriptor) {
  unsigned size = totalSizeToAlloc<LifetimeDescriptor>(sources.size());
  void *mem = ctx.Allocate(size, alignof(LifetimeEntry));
  return new (mem) LifetimeEntry(startLoc, endLoc, sources, targetDescriptor);
}

std::string LifetimeEntry::getString() const {
  std::string result = "(";
  if (targetDescriptor.has_value()) {
    result += targetDescriptor->getString();
    result += ": ";
  }

  bool firstElem = true;
  for (auto source : getSources()) {
    if (!firstElem) {
      result += ", ";
    }
    auto lifetimeKind = source.getParsedLifetimeDependenceKind();
    auto kindString = getNameForParsedLifetimeDependenceKind(lifetimeKind);
    bool printSpace = (lifetimeKind == ParsedLifetimeDependenceKind::Borrow ||
                       lifetimeKind == ParsedLifetimeDependenceKind::Inherit);
    if (!kindString.empty()) {
      result += kindString;
    }
    if (printSpace) {
      result += " ";
    }
    result += source.getString();
    firstElem = false;
  }
  result += ")";
  return result;
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

bool
filterEscapableLifetimeDependencies(GenericSignature sig,
        ArrayRef<LifetimeDependenceInfo> inputs,
        SmallVectorImpl<LifetimeDependenceInfo> &outputs,
        llvm::function_ref<Type (unsigned targetIndex)> getSubstTargetType) {
  bool didRemoveLifetimeDependencies = false;

  for (auto &depInfo : inputs) {
    auto targetIndex = depInfo.getTargetIndex();
    Type substTy = getSubstTargetType(targetIndex);

    // If the type still contains type variables we don't know whether we
    // can drop the dependency.
    if (substTy->hasTypeVariable())
      continue;

    // Drop the dependency if the target type is Escapable.
    if (sig || !substTy->hasTypeParameter()) {
      if (substTy->isEscapable(sig)) {
        didRemoveLifetimeDependencies = true;
        continue;
      }
    }
    
    // Otherwise, keep the dependency.
    outputs.push_back(depInfo);
  }

  return didRemoveLifetimeDependencies;
}

StringRef
getNameForParsedLifetimeDependenceKind(ParsedLifetimeDependenceKind kind) {
  switch (kind) {
  case ParsedLifetimeDependenceKind::Borrow:
    return "borrow";
  case ParsedLifetimeDependenceKind::Inherit:
    return "copy";
  case ParsedLifetimeDependenceKind::Inout:
    return "&";
  default:
    return "";
  }
}

std::string LifetimeDependenceInfo::getString() const {
  std::string lifetimeDependenceString = "@lifetime(";
  auto addressable = getAddressableIndices();
  auto condAddressable = getConditionallyAddressableIndices();
  
  auto getSourceString = [&](IndexSubset *bitvector, StringRef kind) {
    std::string result;
    bool isFirstSetBit = true;
    for (unsigned i = 0; i < bitvector->getCapacity(); i++) {
      if (bitvector->contains(i)) {
        if (!isFirstSetBit) {
          result += ", ";
        }
        result += kind;
        if (addressable && addressable->contains(i)) {
          result += "address ";
        } else if (condAddressable && condAddressable->contains(i)) {
          result += "address_for_deps ";
        }
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
  ID.AddBoolean(addressableParamIndicesAndImmortal.getInt());
  ID.AddInteger(targetIndex);
  if (inheritLifetimeParamIndices) {
    ID.AddInteger((uint8_t)LifetimeDependenceKind::Inherit);
    inheritLifetimeParamIndices->Profile(ID);
  }
  if (scopeLifetimeParamIndices) {
    ID.AddInteger((uint8_t)LifetimeDependenceKind::Scope);
    scopeLifetimeParamIndices->Profile(ID);
  }
  if (addressableParamIndicesAndImmortal.getPointer()) {
    ID.AddBoolean(true);
    addressableParamIndicesAndImmortal.getPointer()->Profile(ID);
  } else {
    ID.AddBoolean(false);  
  }
}

// Warning: this is incorrect for Setter 'newValue' parameters. It should only
// be called for a Setter's 'self'.
static ValueOwnership getLoweredOwnership(AbstractFunctionDecl *afd) {
  if (isa<ConstructorDecl>(afd)) {
    return ValueOwnership::Owned;
  }
  if (auto *ad = dyn_cast<AccessorDecl>(afd)) {
    if (ad->getAccessorKind() == AccessorKind::Set ||
        isYieldingMutableAccessor(ad->getAccessorKind())) {
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

static bool isDiagnosedNonEscapable(Type type) {
  if (type->hasError()) {
    return false;
  }
  // FIXME: This check is temporary until rdar://139976667 is fixed.
  // ModuleType created with ModuleType::get methods are ~Copyable and
  // ~Escapable because the Copyable and Escapable conformance is not added to
  // them by default.
  if (type->is<ModuleType>()) {
    return false;
  }
  return !type->isEscapable();
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
  if (hasAddressableParamIndices()) {
    pushData(addressableParamIndicesAndImmortal.getPointer());  
  }
}

class LifetimeDependenceChecker {
  ValueDecl *decl;

  DeclContext *dc;
  ASTContext &ctx;

  SourceLoc returnLoc;

  // Only initialized when hasImplicitSelfDecl() is true.
  unsigned selfIndex = ~0;

  // 'resultIndex' is a pseudo-parameter-index used by LifetimeDependenceInfo to
  // represent the function result.
  unsigned resultIndex = ~0;

  SmallVector<LifetimeDependenceInfo, 1> lifetimeDependencies;

  // True if lifetime diganostics have already been performed. Avoids redundant
  // diagnostics, and allows bypassing diagnostics for special cases.
  bool performedDiagnostics = false;

public:
  LifetimeDependenceChecker(AbstractFunctionDecl *afd)
      : decl(afd), dc(afd->getDeclContext()), ctx(dc->getASTContext()) {
    auto resultTypeRepr = afd->getResultTypeRepr();
    returnLoc = resultTypeRepr ? resultTypeRepr->getLoc() : afd->getLoc();

    if (afd->hasImplicitSelfDecl()) {
      selfIndex = afd->getParameters()->size();
      resultIndex = selfIndex + 1;
    } else {
      resultIndex = afd->getParameters()->size();
    }
  }

  LifetimeDependenceChecker(EnumElementDecl *eed)
      : decl(eed), dc(eed->getDeclContext()), ctx(dc->getASTContext()) {
    auto *paramList = eed->getParameterList();
    resultIndex = paramList ? eed->getParameterList()->size() + 1 : 1;
  }

  std::optional<llvm::ArrayRef<LifetimeDependenceInfo>>
  currentDependencies() const {
    if (lifetimeDependencies.empty()) {
      return std::nullopt;
    }
    return decl->getASTContext().AllocateCopy(lifetimeDependencies);
  }

  std::optional<llvm::ArrayRef<LifetimeDependenceInfo>> checkFuncDecl() {
    assert(isa<FuncDecl>(decl) || isa<ConstructorDecl>(decl));
    assert(lifetimeDependencies.empty());

    auto *afd = cast<AbstractFunctionDecl>(decl);
    // Handle Builtins first because, even though Builtins require
    // LifetimeDependence, we don't force the experimental feature
    // to be enabled when importing the Builtin module.
    if (afd->isImplicit() && afd->getModuleContext()->isBuiltinModule()) {
      inferBuiltin();
      return currentDependencies();
    }

    if (!ctx.LangOpts.hasFeature(Feature::LifetimeDependence)
        && !ctx.LangOpts.hasFeature(Feature::Lifetimes)
        && !ctx.SourceMgr.isImportMacroGeneratedLoc(returnLoc)) {

      // Infer inout dependencies without requiring a feature flag. On
      // returning, 'lifetimeDependencies' contains any inferred
      // dependencies. This does not issue any diagnostics because any invalid
      // usage should generate a missing feature flag diagnostic instead.
      inferInoutParams();

      diagnoseMissingResultDependencies(
        diag::lifetime_dependence_feature_required_return.ID);
      diagnoseMissingSelfDependencies(
        diag::lifetime_dependence_feature_required_mutating.ID);
      diagnoseMissingInoutDependencies(
        diag::lifetime_dependence_feature_required_inout.ID);

      return currentDependencies();
    }

    if (afd->getAttrs().hasAttribute<LifetimeAttr>()) {
      return checkAttribute();
    }
    // Methods or functions with @_unsafeNonescapableResult do not require
    // lifetime annotation and do not infer any lifetime dependency.
    if (afd->getAttrs().hasAttribute<UnsafeNonEscapableResultAttr>()) {
      return std::nullopt;
    }

    inferOrDiagnose();

    // If precise diagnostics were already issued, bypass
    // diagnoseMissingDependencies to avoid redundant diagnostics.
    if (!performedDiagnostics) {
      diagnoseMissingResultDependencies(
        diag::lifetime_dependence_cannot_infer_return.ID);
      diagnoseMissingSelfDependencies(
        diag::lifetime_dependence_cannot_infer_mutating.ID);
      diagnoseMissingInoutDependencies(
        diag::lifetime_dependence_cannot_infer_inout.ID);
    }
    return currentDependencies();
  }

  std::optional<llvm::ArrayRef<LifetimeDependenceInfo>> checkEnumElementDecl() {
    auto *eed = cast<EnumElementDecl>(decl);
    auto *parentEnum = eed->getParentEnum();
    auto enumType =
        parentEnum->mapTypeIntoContext(parentEnum->getDeclaredInterfaceType());

    // Add early bailout for imported enums.
    if (parentEnum->hasClangNode()) {
      return std::nullopt;
    }

    // Escapable enum, bailout.
    if (!isDiagnosedNonEscapable(enumType)) {
      return std::nullopt;
    }
    auto *params = eed->getParameterList();
    // No payload, bailout.
    if (!params) {
      return std::nullopt;
    }

    auto resultIndex = params->size() + /*selfType*/ 1;
    auto capacity = resultIndex + 1;
    SmallBitVector inheritIndices(capacity);
    SmallVector<LifetimeDependenceInfo, 1> lifetimeDependencies;

    // Add all indices of ~Escapable parameters as lifetime dependence sources.
    for (size_t i = 0; i < params->size(); i++) {
      auto paramType = params->get(i)->getTypeInContext();
      if (!isDiagnosedNonEscapable(paramType)) {
        continue;
      }
      inheritIndices.set(i);
    }
    if (inheritIndices.none()) {
      return std::nullopt;
    }
    auto lifetimeDependenceInfo = LifetimeDependenceInfo(
        IndexSubset::get(eed->getASTContext(), inheritIndices), nullptr,
        resultIndex,
        /*isImmortal*/ false);
    lifetimeDependencies.push_back(lifetimeDependenceInfo);

    return eed->getASTContext().AllocateCopy(lifetimeDependencies);
  }

protected:
  template<typename ...ArgTypes>
  InFlightDiagnostic diagnose(
    SourceLoc Loc, Diag<ArgTypes...> ID,
    typename detail::PassArgument<ArgTypes>::type... Args) {
    performedDiagnostics = true;
    return ctx.Diags.diagnose(Loc, ID, std::move(Args)...);
  }

  template<typename ...ArgTypes>
  InFlightDiagnostic
  diagnose(const Decl *decl, Diag<ArgTypes...> id,
           typename detail::PassArgument<ArgTypes>::type... args) {
    return ctx.Diags.diagnose(decl, Diagnostic(id, std::move(args)...));
  }

  bool isInit() const { return isa<ConstructorDecl>(decl); }

  // For initializers, the implicit self parameter is ignored and instead shows
  // up as the result type.
  //
  // Note: Do not use this to reserve the self parameter index.
  // LifetimeDependenceInfo always reserves an extra formal parameter
  // index for hasImplicitSelfDecl(), even for initializers. During function
  // type lowering, it is mapped to the metatype parameter. Without reserving
  // the extra formal self parameter, a dependency targeting the formal result
  // index would incorrectly target the SIL metatype parameter.
  bool hasImplicitSelfParam() const {
    auto *afd = cast<AbstractFunctionDecl>(decl);
    return !isInit() && afd->hasImplicitSelfDecl();
  }

  // In SIL, implicit initializers and accessors become explicit.
  bool isImplicitOrSIL() const {
    auto *afd = cast<AbstractFunctionDecl>(decl);
    if (afd->isImplicit()) {
      return true;
    }
    // TODO: remove this check once SIL prints @lifetime.
    if (auto *sf = afd->getParentSourceFile()) {
      // The AST printer makes implicit initializers explicit, but does not
      // print the @lifetime annotations. Until that is fixed, avoid
      // diagnosing this as an error.
      if (sf->Kind == SourceFileKind::SIL) {
        return true;
      }
    }
    return false;
  }

  bool isInterfaceFile() const {
    // TODO: remove this check once all compilers that are rev-locked to the
    // stdlib print the 'copy' dependence kind in the interface (Aug '25)
    if (auto *sf = decl->getDeclContext()->getParentSourceFile()) {
      if (sf->Kind == SourceFileKind::Interface) {
        return true;
      }
    }
    return false;
  }

  // Infer ambiguous cases for backward compatibility.
  bool useLazyInference() const {
    return isInterfaceFile()
      || ctx.LangOpts.EnableExperimentalLifetimeDependenceInference;
  }

  std::string diagnosticQualifier() const {
    auto *afd = cast<AbstractFunctionDecl>(decl);
    if (afd->isImplicit()) {
      if (isInit()) {
        return "an implicit initializer";
      }
      if (auto *ad = dyn_cast<AccessorDecl>(afd)) {
        std::string qualifier = "the '";
        qualifier += accessorKindName(ad->getAccessorKind());
        qualifier += "' accessor";
        return qualifier;
      }
    }
    if (afd->hasImplicitSelfDecl()) {
      if (isInit()) {
        return "an initializer";
      }
      if (afd->getImplicitSelfDecl()->isInOut()) {
        return "a mutating method";
      }
      return "a method";
    }
    return "a function";
  }
  
  // Ensure that dependencies exist for any return value or inout parameter that
  // needs one. Always runs before the checker completes if no other diagnostics
  // were issued.
  void diagnoseMissingResultDependencies(DiagID diagID) {
    if (!isDiagnosedNonEscapable(getResultOrYield())) {
      return;
    }
    if (llvm::none_of(lifetimeDependencies,
                      [&](LifetimeDependenceInfo dep) {
                        return dep.getTargetIndex() == resultIndex;
                      })) {
      ctx.Diags.diagnose(returnLoc, diagID,
                         {StringRef(diagnosticQualifier())});
    }
  }

  // Ensure that dependencies exist for any mutating self value. Always runs
  // before the checker completes if no other diagnostics were issued. For
  // initializers, the inout self parameter is actually considered the result
  // type so is not handled here.
  void diagnoseMissingSelfDependencies(DiagID diagID) {
    auto *afd = cast<AbstractFunctionDecl>(decl);
    if (!hasImplicitSelfParam()) {
      return;
    }
    auto *selfDecl = afd->getImplicitSelfDecl();
    if (!selfDecl->isInOut()) {
      return;
    }
    if (!isDiagnosedNonEscapable(dc->getSelfTypeInContext())) {
      return;
    }
    if (llvm::none_of(lifetimeDependencies,
                      [&](LifetimeDependenceInfo dep) {
                        return dep.getTargetIndex() == selfIndex;
                      })) {
      ctx.Diags.diagnose(selfDecl->getLoc(), diagID,
                         {StringRef(diagnosticQualifier())});
    }
  }
  
  void diagnoseMissingInoutDependencies(DiagID diagID) {
    auto *afd = cast<AbstractFunctionDecl>(decl);
    unsigned paramIndex = 0;
    for (auto *param : *afd->getParameters()) {
      SWIFT_DEFER { paramIndex++; };
      if (!param->isInOut()) {
        continue;
      }
      if (!isDiagnosedNonEscapable(
            afd->mapTypeIntoContext(param->getInterfaceType()))) {
        continue;
      }
      if (llvm::none_of(lifetimeDependencies,
                        [&](LifetimeDependenceInfo dep) {
                          return dep.getTargetIndex() == paramIndex;
                        })) {
        ctx.Diags.diagnose(param->getLoc(), diagID,
                           {StringRef(diagnosticQualifier()),
                            param->getName().str()});
        if (diagID == diag::lifetime_dependence_cannot_infer_inout.ID) {
          ctx.Diags.diagnose(
            param->getLoc(),
            diag::lifetime_dependence_cannot_infer_inout_suggest,
            param->getName().str());
        }
      }
    }
  }

  bool isCompatibleWithOwnership(ParsedLifetimeDependenceKind kind, Type type,
                                 ValueOwnership loweredOwnership,
                                 bool isInterfaceFile = false) const {
    if (kind == ParsedLifetimeDependenceKind::Inherit) {
      return true;
    }
    if (kind == ParsedLifetimeDependenceKind::Borrow) {
      // An owned/consumed BitwiseCopyable value can be effectively borrowed
      // because its lifetime can be indefinitely extended.
      if (loweredOwnership == ValueOwnership::Owned
          && isBitwiseCopyable(type, ctx)) {
        return true;
      }
      if (isInterfaceFile) {
        return loweredOwnership == ValueOwnership::Shared ||
               loweredOwnership == ValueOwnership::InOut;
      }
      return loweredOwnership == ValueOwnership::Shared;
    }
    assert(kind == ParsedLifetimeDependenceKind::Inout);
    return loweredOwnership == ValueOwnership::InOut;
  }

  bool isCompatibleWithOwnership(LifetimeDependenceKind kind, Type type,
                                 ValueOwnership ownership) const {
    auto *afd = cast<AbstractFunctionDecl>(decl);
    if (kind == LifetimeDependenceKind::Inherit) {
      return true;
    }
    // Lifetime dependence always propagates through temporary BitwiseCopyable
    // values, even if the dependence is scoped.
    if (isBitwiseCopyable(type, ctx)) {
      return true;
    }
    auto loweredOwnership = ownership != ValueOwnership::Default
                                ? ownership
                                : getLoweredOwnership(afd);

    assert(kind == LifetimeDependenceKind::Scope);
    return loweredOwnership == ValueOwnership::Shared ||
           loweredOwnership == ValueOwnership::InOut;
  }

  struct TargetDeps {
    unsigned targetIndex;
    SmallBitVector inheritIndices;
    SmallBitVector scopeIndices;

    TargetDeps(unsigned targetIndex, unsigned capacity)
      : targetIndex(targetIndex), inheritIndices(capacity),
        scopeIndices(capacity) {}

    TargetDeps &&add(unsigned sourceIndex, LifetimeDependenceKind kind) && {
      switch (kind) {
      case LifetimeDependenceKind::Inherit:
        inheritIndices.set(sourceIndex);
        break;
      case LifetimeDependenceKind::Scope:
        scopeIndices.set(sourceIndex);
        break;
      }
      return std::move(*this);
    }
  };

  TargetDeps createDeps(unsigned targetIndex) {
    auto *afd = cast<AbstractFunctionDecl>(decl);
    unsigned capacity = afd->hasImplicitSelfDecl()
      ? (afd->getParameters()->size() + 1)
      : afd->getParameters()->size();
    return TargetDeps(targetIndex, capacity);
  }

  // Allocate LifetimeDependenceInfo in the ASTContext and push it onto
  // lifetimeDependencies.
  void pushDeps(const TargetDeps &&deps) {
    assert(llvm::none_of(lifetimeDependencies,
                         [&](LifetimeDependenceInfo dep) {
                           return dep.getTargetIndex() == deps.targetIndex;
                         }));
    IndexSubset *inheritIndices = nullptr;
    if (deps.inheritIndices.any()) {
      inheritIndices = IndexSubset::get(ctx, deps.inheritIndices);
    }
    IndexSubset *scopeIndices = nullptr;
    if (deps.scopeIndices.any()) {
      scopeIndices = IndexSubset::get(ctx, deps.scopeIndices);
    }
    lifetimeDependencies.push_back(
      LifetimeDependenceInfo{
        /*inheritLifetimeParamIndices*/ inheritIndices,
        /*scopeLifetimeParamIndices*/ scopeIndices,
        deps.targetIndex,
        /*isImmortal*/ false});
  }

  Type getResultOrYield() const {
    auto *afd = cast<AbstractFunctionDecl>(decl);
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

  std::optional<LifetimeDependenceKind>
  getDependenceKindFromDescriptor(LifetimeDescriptor descriptor,
                                  ParamDecl *paramDecl) {
    auto *afd = cast<AbstractFunctionDecl>(decl);
    auto loc = descriptor.getLoc();
    auto type = paramDecl->getTypeInContext();
    auto parsedLifetimeKind = descriptor.getParsedLifetimeDependenceKind();
    auto ownership = paramDecl->getValueOwnership();
    auto loweredOwnership = ownership != ValueOwnership::Default
                                ? ownership
                                : getLoweredOwnership(afd);

    switch (parsedLifetimeKind) {
    case ParsedLifetimeDependenceKind::Default: {
      if (type->isEscapable()) {
        if (loweredOwnership == ValueOwnership::Shared ||
            loweredOwnership == ValueOwnership::InOut) {
          return LifetimeDependenceKind::Scope;
        }
        diagnose(
            loc,
            diag::lifetime_dependence_cannot_use_default_escapable_consuming,
            getOwnershipSpelling(loweredOwnership));
        return std::nullopt;
      }
      if (useLazyInference()) {
        return LifetimeDependenceKind::Inherit;
      }
      diagnose(loc, diag::lifetime_dependence_cannot_infer_kind,
               diagnosticQualifier(), descriptor.getString());
      return std::nullopt;
    }

    case ParsedLifetimeDependenceKind::Borrow: LLVM_FALLTHROUGH;
    case ParsedLifetimeDependenceKind::Inout: {
      // @lifetime(borrow x) is valid only for borrowing parameters.
      // @lifetime(&x) is valid only for inout parameters.
      auto loweredOwnership = ownership != ValueOwnership::Default
        ? ownership : getLoweredOwnership(afd);
      if (isCompatibleWithOwnership(parsedLifetimeKind, type, loweredOwnership,
                                    isInterfaceFile())) {
        return LifetimeDependenceKind::Scope;
      }
      diagnose(loc,
               diag::lifetime_dependence_parsed_borrow_with_ownership,
               getNameForParsedLifetimeDependenceKind(parsedLifetimeKind),
               getOwnershipSpelling(loweredOwnership));
      switch (loweredOwnership) {
      case ValueOwnership::Shared:
        diagnose(loc,
                 diag::lifetime_dependence_parsed_borrow_with_ownership_fix,
                 "borrow ", descriptor.getString());
        break;
      case ValueOwnership::InOut:
        diagnose(loc,
                 diag::lifetime_dependence_parsed_borrow_with_ownership_fix,
                 "&", descriptor.getString());
        break;
      case ValueOwnership::Owned:
      case ValueOwnership::Default:
        break;
      }
      return std::nullopt;
    }
    case ParsedLifetimeDependenceKind::Inherit: {
      // @lifetime(copy x) is only invalid for Escapable types.
      if (type->isEscapable()) {
        if (loweredOwnership == ValueOwnership::Shared) {
          diagnose(loc, diag::lifetime_dependence_invalid_inherit_escapable_type,
                   "borrow ", descriptor.getString());
        } else if (loweredOwnership == ValueOwnership::InOut) {
          diagnose(loc, diag::lifetime_dependence_invalid_inherit_escapable_type,
                   "&", descriptor.getString());
        } else {
          diagnose(
            loc,
            diag::lifetime_dependence_cannot_use_default_escapable_consuming,
            getOwnershipSpelling(loweredOwnership));
        }
        return std::nullopt;
      }
      return LifetimeDependenceKind::Inherit;
    }
    }
  }

  // Finds the ParamDecl* and its index from a LifetimeDescriptor
  std::optional<std::pair<ParamDecl *, unsigned>>
  getParamDeclFromDescriptor(LifetimeDescriptor descriptor) {
    auto *afd = cast<AbstractFunctionDecl>(decl);
    switch (descriptor.getDescriptorKind()) {
    case LifetimeDescriptor::DescriptorKind::Named: {
      unsigned paramIndex = 0;
      ParamDecl *candidateParam = nullptr;
      for (auto *param : *afd->getParameters()) {
        if (param->getParameterName() == descriptor.getName()) {
          candidateParam = param;
          break;
        }
        paramIndex++;
      }
      if (!candidateParam) {
        diagnose(descriptor.getLoc(),
                 diag::lifetime_dependence_invalid_param_name,
                 descriptor.getName());
        return std::nullopt;
      }
      return std::make_pair(candidateParam, paramIndex);
    }
    case LifetimeDescriptor::DescriptorKind::Ordered: {
      auto paramIndex = descriptor.getIndex();
      if (paramIndex >= afd->getParameters()->size()) {
        diagnose(descriptor.getLoc(),
                 diag::lifetime_dependence_invalid_param_index,
                 paramIndex);
        return std::nullopt;
      }
      auto candidateParam = afd->getParameters()->get(paramIndex);
      return std::make_pair(candidateParam, paramIndex);
    }
    case LifetimeDescriptor::DescriptorKind::Self: {
      if (!hasImplicitSelfParam()) {
        diagnose(descriptor.getLoc(),
                 diag::lifetime_dependence_invalid_self_in_static);
        return std::nullopt;
      }
      if (isa<ConstructorDecl>(afd)) {
        diagnose(descriptor.getLoc(),
                 diag::lifetime_dependence_invalid_self_in_init);
        return std::nullopt;
      }
      auto *selfDecl = afd->getImplicitSelfDecl();
      return std::make_pair(selfDecl, afd->getParameters()->size());
    }
    }
  }

  std::optional<ArrayRef<LifetimeDependenceInfo>> checkAttribute() {
    auto *afd = cast<AbstractFunctionDecl>(decl);
    SmallVector<LifetimeDependenceInfo, 1> lifetimeDependencies;
    llvm::SmallSet<unsigned, 1> lifetimeDependentTargets;
    auto lifetimeAttrs = afd->getAttrs().getAttributes<LifetimeAttr>();
    for (auto attr : lifetimeAttrs) {
      auto lifetimeDependenceInfo =
        checkAttributeEntry(attr->getLifetimeEntry());
      if (!lifetimeDependenceInfo.has_value()) {
        return std::nullopt;
      }
      auto targetIndex = lifetimeDependenceInfo->getTargetIndex();
      if (lifetimeDependentTargets.contains(targetIndex)) {
        // TODO: Diagnose at the source location of the @lifetime attribute with
        // duplicate target.
        diagnose(afd->getLoc(), diag::lifetime_dependence_duplicate_target);
      }
      lifetimeDependentTargets.insert(targetIndex);
      lifetimeDependencies.push_back(*lifetimeDependenceInfo);
    }

    return afd->getASTContext().AllocateCopy(lifetimeDependencies);
  }

  std::optional<LifetimeDependenceInfo>
  checkAttributeEntry(LifetimeEntry *entry) {
    auto *afd = cast<AbstractFunctionDecl>(decl);
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
        diagnose(descriptor.getLoc(),
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
      auto targetDeclAndIndex = getParamDeclFromDescriptor(*targetDescriptor);
      if (!targetDeclAndIndex.has_value()) {
        return std::nullopt;
      }
      // TODO: support dependencies on non-inout parameters.
      if (!targetDeclAndIndex->first->isInOut()) {
        diagnose(targetDeclAndIndex->first,
                 diag::lifetime_parameter_requires_inout,
                 targetDescriptor->getName());
      }
      targetIndex = targetDeclAndIndex->second;
    } else {
      targetIndex = afd->hasImplicitSelfDecl()
        ? afd->getParameters()->size() + 1
        : afd->getParameters()->size();
    }

    for (auto source : entry->getSources()) {
      if (source.isImmortal()) {
        auto immortalParam =
          std::find_if(afd->getParameters()->begin(),
                       afd->getParameters()->end(), [](ParamDecl *param) {
                         return param->getName().nonempty()
                           && strcmp(param->getName().get(), "immortal") == 0;
                       });
        if (immortalParam != afd->getParameters()->end()) {
          diagnose(*immortalParam,
                   diag::lifetime_dependence_immortal_conflict_name);
          return std::nullopt;
        }
        return LifetimeDependenceInfo(nullptr, nullptr, targetIndex,
                                      /*isImmortal*/ true);
      }

      auto paramDeclAndIndex = getParamDeclFromDescriptor(source);
      if (!paramDeclAndIndex.has_value()) {
        return std::nullopt;
      }
      auto lifetimeKind =
        getDependenceKindFromDescriptor(source, paramDeclAndIndex->first);
      if (!lifetimeKind.has_value()) {
        return std::nullopt;
      }
      unsigned sourceIndex = paramDeclAndIndex->second;
      if (lifetimeKind == LifetimeDependenceKind::Scope
          && sourceIndex == targetIndex) {
        diagnose(source.getLoc(),
                 diag::lifetime_dependence_cannot_use_parsed_borrow_inout);
        return std::nullopt;
      }
      bool hasError =
        updateLifetimeIndices(source, sourceIndex, *lifetimeKind);
      if (hasError) {
        return std::nullopt;
      }
    }

    return LifetimeDependenceInfo(
      inheritIndices.any() ? IndexSubset::get(ctx, inheritIndices) : nullptr,
      scopeIndices.any() ? IndexSubset::get(ctx, scopeIndices) : nullptr,
      targetIndex, /*isImmortal*/ false);
  }

  // On returning, 'lifetimeDependencies' contains any inferred dependencies and
  // 'performedDiagnostics' indicates whether any specific diagnostics were
  // issued.
  void inferOrDiagnose() {
    // Infer non-Escapable results.
    if (isDiagnosedNonEscapable(getResultOrYield())) {
      if (hasImplicitSelfParam()) {
        // Methods that return a non-Escapable value.
        inferNonEscapableResultOnSelf();
        return;
      }
      if (isInit() && isImplicitOrSIL()) {
        inferImplicitInit();
        return;
      }
      // Regular functions and initializers that return a non-Escapable value.
      inferNonEscapableResultOnParam();
      return;
    }

    // Infer mutating non-Escapable methods (excluding initializers).
    inferMutatingSelf();

    // Infer inout parameters.
    inferInoutParams();
  }

  /// If the current function is a mutating method and 'self' is non-Escapable,
  /// return 'self's ParamDecl.
  bool isMutatingNonEscapableSelf() {
    auto *afd = cast<AbstractFunctionDecl>(decl);
    if (!hasImplicitSelfParam())
      return false;

    if (!isDiagnosedNonEscapable(dc->getSelfTypeInContext()))
      return false;

    assert(!isInit() && "class initializers have Escapable self");
    auto *selfDecl = afd->getImplicitSelfDecl();
    if (!selfDecl->isInOut())
      return false;

    return true;
  }

  // Infer method dependence of result on self for
  // methods, getters, and _modify accessors.
  void inferNonEscapableResultOnSelf() {
    auto *afd = cast<AbstractFunctionDecl>(decl);
    Type selfTypeInContext = dc->getSelfTypeInContext();
    if (selfTypeInContext->hasError()) {
      return;
    }

    bool nonEscapableSelf = isDiagnosedNonEscapable(selfTypeInContext);
    if (auto accessor = dyn_cast<AccessorDecl>(afd)) {
      if (isImplicitOrSIL() || useLazyInference()) {
        if (nonEscapableSelf && afd->getImplicitSelfDecl()->isInOut()) {
          // Implicit accessors that return or yield a non-Escapable value may
          // infer dependency on both self and result.
          inferMutatingAccessor(accessor);
        }
        // Infer the result dependency on self based on the kind of accessor
        // that is wrapped by this synthesized accessors.
        if (auto dependenceKind =
            getAccessorDependence(accessor, selfTypeInContext)) {
          pushDeps(createDeps(resultIndex).add(selfIndex, *dependenceKind));
        }
        return;
      }
      // Explicit accessors are inferred the same way as regular methods.
    }
    // Do infer the result of a mutating method when 'self' is
    // non-Escapable. The missing dependence on inout 'self' will be diagnosed
    // later anyway, so an explicit annotation will still be needed.
    if (nonEscapableSelf && afd->getImplicitSelfDecl()->isInOut()) {
      return;
    }
    // Methods with parameters only apply to lazy inference.
    if (!useLazyInference() && afd->getParameters()->size() > 0) {
      return;
    }
    if (!useLazyInference() && !isImplicitOrSIL()) {
      // Require explicit @_lifetime(borrow self) for UnsafePointer-like self.
      if (!nonEscapableSelf && isBitwiseCopyable(selfTypeInContext, ctx)) {
        diagnose(returnLoc,
                 diag::lifetime_dependence_cannot_infer_bitwisecopyable,
                 diagnosticQualifier(), "self");
        return;
      }
      // Require explicit @_lifetime(copy or borrow) for non-Escapable self.
      if (nonEscapableSelf) {
        diagnose(returnLoc, diag::lifetime_dependence_cannot_infer_kind,
                 diagnosticQualifier(), "self");
        return;
      }
    }
    // Infer based on ownership if possible for either explicit accessors or
    // methods as long as they pass preceding ambiguity checks.
    auto kind = inferLifetimeDependenceKind(
        selfTypeInContext, afd->getImplicitSelfDecl()->getValueOwnership());
    if (!kind) {
      // Special diagnostic for an attempt to depend on a consuming parameter.
      diagnose(returnLoc,
               diag::lifetime_dependence_cannot_infer_scope_ownership,
               "self", diagnosticQualifier());
      return;
    }
    pushDeps(createDeps(resultIndex).add(selfIndex, *kind));
  }

  // Infer the kind of dependence that makes sense for reading or writing a
  // stored property (for getters or initializers).
  std::optional<LifetimeDependenceKind>
  inferLifetimeDependenceKind(Type sourceType, ValueOwnership ownership) {
    auto *afd = cast<AbstractFunctionDecl>(decl);
    if (!sourceType->isEscapable()) {
      return LifetimeDependenceKind::Inherit;
    }
    // Lifetime dependence always propagates through temporary BitwiseCopyable
    // values, even if the dependence is scoped.
    if (isBitwiseCopyable(sourceType, ctx)) {
      return LifetimeDependenceKind::Scope;
    }
    auto loweredOwnership = ownership != ValueOwnership::Default
                                ? ownership
                                : getLoweredOwnership(afd);
    // It is impossible to depend on a consumed Escapable value (unless it is
    // BitwiseCopyable as checked above).
    if (loweredOwnership == ValueOwnership::Owned) {
      return std::nullopt;
    }
    return LifetimeDependenceKind::Scope;
  }

  // Infer implicit initialization. The dependence kind can be inferred, similar
  // to an implicit setter, because the implementation is simply an assignment
  // to stored property.
  void inferImplicitInit() {
    auto *afd = cast<AbstractFunctionDecl>(decl);
    if (afd->getParameters()->size() == 0) {
      // Empty ~Escapable types can be implicitly initialized without any
      // dependencies. In SIL, implicit initializers become explicit. Set
      // performedDiagnostics here to bypass normal dependence checking without
      // raising an error.
      performedDiagnostics = true;
      return;
    }
    auto targetDeps = createDeps(resultIndex);
    unsigned paramIndex = 0;
    for (auto *param : *afd->getParameters()) {
      SWIFT_DEFER { paramIndex++; };
      Type paramTypeInContext =
        afd->mapTypeIntoContext(param->getInterfaceType());
      if (paramTypeInContext->hasError()) {
        return;
      }
      auto kind = inferLifetimeDependenceKind(paramTypeInContext,
                                              param->getValueOwnership());
      if (!kind) {
        diagnose(returnLoc,
                 diag::lifetime_dependence_cannot_infer_scope_ownership,
                 param->getParameterName().str(), diagnosticQualifier());
        return;
      }
      targetDeps = std::move(targetDeps).add(paramIndex, *kind);
    }
    pushDeps(std::move(targetDeps));
  }

  // Infer result dependence on a function or intitializer parameter.
  //
  // Note: for implicit initializers with parameters, consider inferring
  // Inherit dependency for each non-Escapable parameter. This would be
  // consistent with implicit stored property setters. This isn't done yet
  // because we also need to consider any Escapable parameters: either skip
  // inference if any exist, infer scoped dependency, or infer no
  // dependency. Implicit setters for Escapable properties are not inferred.
  void inferNonEscapableResultOnParam() {
    auto *afd = cast<AbstractFunctionDecl>(decl);
    // This is only called when there is no 'self' argument that can be the
    // source of a dependence.
    assert(!hasImplicitSelfParam());

    if (useLazyInference()) {
      return lazillyInferNonEscapableResultOnParam();
    }

    // Strict inference only handles a single escapable parameter,
    // which is an unambiguous borrow dependence.
    if (afd->getParameters()->size() == 0) {
      diagnose(returnLoc,
               diag::lifetime_dependence_cannot_infer_return_no_param,
               diagnosticQualifier());
      diagnose(returnLoc,
               diag::lifetime_dependence_cannot_infer_return_immortal);
      return;
    }
    if (afd->getParameters()->size() > 1) {
      // The usual diagnostic check is sufficient.
      return;
    }
    // Do not infer non-escapable dependence kind -- it is ambiguous.
    auto *param = afd->getParameters()->get(0);
    Type paramTypeInContext =
      afd->mapTypeIntoContext(param->getInterfaceType());
    if (paramTypeInContext->hasError()) {
      return;
    }
    if (!paramTypeInContext->isEscapable()) {
      diagnose(returnLoc, diag::lifetime_dependence_cannot_infer_kind,
               diagnosticQualifier(), param->getParameterName().str());
      return;
    }
    auto kind = LifetimeDependenceKind::Scope;
    auto paramOwnership = param->getValueOwnership();
    if (!isCompatibleWithOwnership(kind, paramTypeInContext, paramOwnership))
    {
      diagnose(returnLoc,
               diag::lifetime_dependence_cannot_infer_scope_ownership,
               param->getParameterName().str(), diagnosticQualifier());
      return;
    }
    pushDeps(createDeps(resultIndex).add(/*paramIndex*/ 0, kind));
  }

  // Lazy inference for .swiftinterface backward compatibility and
  // experimentation. Inference cases can be added but not removed.
  void lazillyInferNonEscapableResultOnParam() {
    auto *afd = cast<AbstractFunctionDecl>(decl);
    std::optional<unsigned> candidateParamIndex;
    std::optional<LifetimeDependenceKind> candidateLifetimeKind;
    unsigned paramIndex = 0;
    for (auto *param : *afd->getParameters()) {
      SWIFT_DEFER { paramIndex++; };
      Type paramTypeInContext =
        afd->mapTypeIntoContext(param->getInterfaceType());
      if (paramTypeInContext->hasError()) {
        return;
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
          inferLifetimeDependenceKind(paramTypeInContext, paramOwnership);
      if (!candidateLifetimeKind) {
        continue;
      }
      if (candidateParamIndex) {
        diagnose(returnLoc,
                 diag::lifetime_dependence_cannot_infer_ambiguous_candidate,
                 diagnosticQualifier());
        return;
      }
      candidateParamIndex = paramIndex;
    }
    if (!candidateParamIndex) {
      diagnose(returnLoc,
               diag::lifetime_dependence_cannot_infer_no_candidates,
               diagnosticQualifier());
      return;
    }
    pushDeps(createDeps(resultIndex).add(*candidateParamIndex,
                                         *candidateLifetimeKind));
  }

  // Infer a mutating 'self' dependency when 'self' is non-Escapable and the
  // result is 'void'.
  void inferMutatingSelf() {
    auto *afd = cast<AbstractFunctionDecl>(decl);
    if (!isMutatingNonEscapableSelf()) {
      return;
    }
    // Handle implicit setters before diagnosing mutating methods. This
    // does not include global accessors, which have no implicit 'self'.
    if (auto accessor = dyn_cast<AccessorDecl>(afd)) {
      // Explicit setters require explicit lifetime dependencies.
      if (isImplicitOrSIL() || useLazyInference()) {
        inferMutatingAccessor(accessor);
      }
      return;
    }
    if (afd->getParameters()->size() > 0) {
      if (useLazyInference()) {
        // Assume that a mutating method does not depend on its parameters.
        // This is unsafe but needed because some MutableSpan APIs snuck into
        // the standard library interface without specifying dependencies.
        pushDeps(createDeps(selfIndex).add(selfIndex,
                                           LifetimeDependenceKind::Inherit));
      }
      return;
    }
    pushDeps(createDeps(selfIndex).add(selfIndex,
                                       LifetimeDependenceKind::Inherit));
  }

  // Infer dependence for an accessor whose non-escapable result depends on
  // self. This includes _read and _modify.
  void inferAccessor(AccessorDecl *accessor, Type selfTypeInContext) {
    auto *afd = cast<AbstractFunctionDecl>(decl);
    if (!isImplicitOrSIL() && !useLazyInference()) {
      return;
    }
    bool nonEscapableSelf = isDiagnosedNonEscapable(selfTypeInContext);
    if (nonEscapableSelf && afd->getImplicitSelfDecl()->isInOut()) {
      // First, infer the dependency on the inout non-Escapable self. This may
      // result in two inferred dependencies for accessors.
      inferMutatingAccessor(accessor);
    }
    // Infer the result dependency on self based on the kind of accessor that
    // is wrapped by this synthesized accessors.
    if (auto dependenceKind =
        getAccessorDependence(accessor, selfTypeInContext)) {
      pushDeps(createDeps(resultIndex).add(selfIndex, *dependenceKind));
    }
  }

  // Infer a mutating accessor's non-Escapable 'self' dependencies.
  void inferMutatingAccessor(AccessorDecl *accessor) {
    switch (accessor->getAccessorKind()) {
    case AccessorKind::Read:
    case AccessorKind::Read2:
      // An implicit _read/read accessor is generated when a mutating getter is
      // declared. Emit the same lifetime dependencies as an implicit _modify.
    case AccessorKind::Modify:
    case AccessorKind::Modify2:
      // A _modify's yielded value depends on self. The _modify dependency in
      // the opposite direction (self depends on the modified value) is not
      // recorded. The caller of _modify ensures that the modified 'self',
      // passed as 'inout', depends on any value stored to the yielded address.
      //
      // This is required for stored properties because the AST generates a
      // _modify for them even though it won't be emitted.
      pushDeps(createDeps(selfIndex).add(selfIndex,
                                         LifetimeDependenceKind::Inherit));
      break;
    case AccessorKind::Set: {
      const unsigned newValIdx = 0;
      auto *afd = cast<AbstractFunctionDecl>(decl);
      auto *param = afd->getParameters()->get(newValIdx);
      Type paramTypeInContext =
        afd->mapTypeIntoContext(param->getInterfaceType());
      if (paramTypeInContext->hasError()) {
        return;
      }
      auto targetDeps =
        createDeps(selfIndex).add(selfIndex, LifetimeDependenceKind::Inherit);

      // The 'newValue' dependence kind must match the getter's dependence kind
      // because generated the implementation '_modify' accessor composes the
      // getter's result with the setter's 'newValue'. In particular, if the
      // result type is Escapable then the getter does not have any lifetime
      // dependency, so the setter cannot depend on 'newValue'.
      if (!paramTypeInContext->isEscapable()) {
        targetDeps = std::move(targetDeps)
                         .add(newValIdx, LifetimeDependenceKind::Inherit);
      }
      pushDeps(std::move(targetDeps));
      break;
    }
    case AccessorKind::MutableAddress:
      if (useLazyInference()) {
        // Assume that a mutating method does not depend on its parameters.
        // Currently only for backward interface compatibility. Even though this
        // is the only useful dependence (a borrow of self is possible but not
        // useful), explicit annotation is required for now to confirm that the
        // mutated self cannot depend on anything stored at this address.
        pushDeps(createDeps(selfIndex).add(selfIndex,
                                           LifetimeDependenceKind::Inherit));
      }
      break;
    default:
      // Unknown mutating accessor.
      break;
    }
  }

  // Implicit accessors must be consistent with the accessor that they
  // wrap. Otherwise, the sythesized implementation will report a diagnostic
  // error.
  std::optional<LifetimeDependenceKind>
  getAccessorDependence(AccessorDecl *accessor, Type selfTypeInContext) {
    std::optional<AccessorKind> wrappedAccessorKind = std::nullopt;
    switch (accessor->getAccessorKind()) {
    case AccessorKind::Read:
    case AccessorKind::Read2:
    case AccessorKind::Modify:
    case AccessorKind::Modify2:
      // read/modify are syntesized as calls to the getter.
      wrappedAccessorKind = AccessorKind::Get;
      break;
    case AccessorKind::Get:
      // getters are synthesized as access to a stored property.
      break;
    default:
      // Unknown synthesized accessor.
      // Setters are handled in inferMutatingAccessor() because they don't
      // return a value.
      return std::nullopt;
    }
    if (wrappedAccessorKind) {
      auto *var = cast<VarDecl>(accessor->getStorage());
      for (auto *wrappedAccessor : var->getAllAccessors()) {
        if (wrappedAccessor->isImplicit())
          continue;
        if (wrappedAccessor->getAccessorKind() == wrappedAccessorKind) {
          if (auto deps = wrappedAccessor->getLifetimeDependencies()) {
            for (auto &dep : *deps) {
              if (dep.getTargetIndex() != resultIndex)
                continue;
              if (dep.checkInherit(selfIndex))
                return LifetimeDependenceKind::Inherit;
              if (dep.checkScope(selfIndex))
                return LifetimeDependenceKind::Scope;
            }
          }
        }
      }
    }
    auto *afd = cast<AbstractFunctionDecl>(decl);
    // Either a Get or Modify without any wrapped accessor. Handle these like a
    // read of the stored property.
    return inferLifetimeDependenceKind(
      selfTypeInContext, afd->getImplicitSelfDecl()->getValueOwnership());
  }

  // Infer 'inout' parameter dependency when the only parameter is
  // non-Escapable.
  //
  // This supports the common case in which the user of a non-Escapable type,
  // such as MutableSpan, wants to modify the span's contents without modifying
  // the span value itself. It should be possible to use MutableSpan this way
  // without requiring any knowledge of lifetime annotations. The tradeoff is
  // that it makes authoring non-Escapable types less safe. For example, a
  // MutableSpan method could update the underlying unsafe pointer and forget to
  // declare a dependence on the incoming pointer.
  //
  // Disallowing other non-Escapable parameters rules out the easy mistake of
  // programmers attempting to trivially reassign the inout parameter. There's
  // is no way to rule out the possibility that they derive another
  // non-Escapable value from an Escapable parameteter. So they can still write
  // the following and will get a lifetime diagnostic:
  //
  //     func reassign(s: inout MutableSpan<Int>, a: [Int]) {
  //       s = a.mutableSpan
  //     }
  //
  // Do not issue any diagnostics. This inference is triggered even when the
  // feature is disabled!
  void inferInoutParams() {
    auto *afd = cast<AbstractFunctionDecl>(decl);
    if (isMutatingNonEscapableSelf()) {
      return;
    }
    std::optional<unsigned> candidateParamIndex;
    bool hasNonEscapableParameter = false;
    if (hasImplicitSelfParam()
        && isDiagnosedNonEscapable(dc->getSelfTypeInContext())) {
      hasNonEscapableParameter = true;
    }
    for (unsigned paramIndex : range(afd->getParameters()->size())) {
      auto *param = afd->getParameters()->get(paramIndex);
      if (isDiagnosedNonEscapable(
            afd->mapTypeIntoContext(param->getInterfaceType()))) {
        if (param->isInOut()) {
          if (hasNonEscapableParameter)
            return;
          candidateParamIndex = paramIndex;
          continue;
        }
        if (candidateParamIndex)
          return;

        hasNonEscapableParameter = true;
      }
    }
    if (candidateParamIndex) {
      pushDeps(createDeps(*candidateParamIndex).add(
                 *candidateParamIndex, LifetimeDependenceKind::Inherit));
    }
  }

  void inferUnambiguousInoutParams() {
    auto *afd = cast<AbstractFunctionDecl>(decl);
    if (afd->getParameters()->size() != 1) {
      return;
    }
    const unsigned paramIndex = 0;
    auto *param = afd->getParameters()->get(paramIndex);
    if (!param->isInOut()) {
      return;
    }
    if (!isDiagnosedNonEscapable(
          afd->mapTypeIntoContext(param->getInterfaceType()))) {
      return;
    }
    pushDeps(createDeps(paramIndex).add(paramIndex,
                                        LifetimeDependenceKind::Inherit));
  }

  void inferBuiltin() {
    auto *afd = cast<AbstractFunctionDecl>(decl);
    // Normal inout parameter inference works for most generic Builtins.
    inferUnambiguousInoutParams();
    if (!lifetimeDependencies.empty()) {
      return;
    }
    const DeclName &name = afd->getName();
    if (name.isSpecial()) {
      return;
    }
    // TODO: declare lifetime dependencies in Builtins.def. Until then, filter
    // the few that are not covered by general inference rules here. This is
    // safer than using a broader rule for implicit declarations. New Builtins
    // need to be considered as they are defined.
    auto id = name.getBaseIdentifier();
    if (id ==
        ctx.getIdentifier(getBuiltinName(BuiltinValueKind::InjectEnumTag))) {
      // ignore the tag parameter
      const unsigned inoutIdx = 0;
      pushDeps(createDeps(inoutIdx).add(inoutIdx,
                                        LifetimeDependenceKind::Inherit));
    } else if (id ==
        ctx.getIdentifier(
          getBuiltinName(BuiltinValueKind::ConvertUnownedUnsafeToGuaranteed))) {
      const unsigned baseIdx = 0;
      const unsigned inoutIdx = 1;
      pushDeps(createDeps(inoutIdx)
               .add(inoutIdx, LifetimeDependenceKind::Inherit)
               .add(baseIdx, LifetimeDependenceKind::Scope));
    }
  }
};

std::optional<llvm::ArrayRef<LifetimeDependenceInfo>>
LifetimeDependenceInfo::get(ValueDecl *decl) {
  if (auto *afd = dyn_cast<AbstractFunctionDecl>(decl)) {
    return LifetimeDependenceChecker(afd).checkFuncDecl();
  }
  auto *eed = cast<EnumElementDecl>(decl);
  return LifetimeDependenceChecker(eed).checkEnumElementDecl();
}

// This implements the logic for SIL type descriptors similar to source-level
// logic in LifetimeDependenceChecker::checkAttributeEntry(). The SIL context is
// substantially different from Sema.
static std::optional<LifetimeDependenceInfo> checkSILTypeModifiers(
  LifetimeDependentTypeRepr *lifetimeDependentRepr, unsigned targetIndex,
  ArrayRef<SILParameterInfo> params, DeclContext *dc) {
  auto &ctx = dc->getASTContext();
  auto &diags = ctx.Diags;
  auto capacity = params.size(); // SIL parameters include self

  SmallBitVector inheritLifetimeParamIndices(capacity);
  SmallBitVector scopeLifetimeParamIndices(capacity);
  SmallBitVector addressableLifetimeParamIndices(capacity);
  SmallBitVector conditionallyAddressableLifetimeParamIndices(capacity);

  auto updateLifetimeDependenceInfo =
    [&](LifetimeDescriptor descriptor,
        unsigned paramIndexToSet,
        ParameterConvention paramConvention) {
      auto loc = descriptor.getLoc();
      auto kind = descriptor.getParsedLifetimeDependenceKind();

      if (kind == ParsedLifetimeDependenceKind::Borrow &&
          isConsumedParameterInCallee(paramConvention)) {
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
        assert(kind == ParsedLifetimeDependenceKind::Borrow);
        scopeLifetimeParamIndices.set(paramIndexToSet);
      }
      return false;
    };

  for (auto source : lifetimeDependentRepr->getLifetimeEntry()->getSources())
  {
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
      switch (source.isAddressable()) {
      case LifetimeDescriptor::IsNotAddressable:
        break;
      case LifetimeDescriptor::IsConditionallyAddressable:
        conditionallyAddressableLifetimeParamIndices.set(index);
        break;
      case LifetimeDescriptor::IsAddressable:
        addressableLifetimeParamIndices.set(index);
        break;
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
    /*isImmortal*/ false,
    addressableLifetimeParamIndices.any()
    ? IndexSubset::get(ctx, addressableLifetimeParamIndices)
    : nullptr,
    conditionallyAddressableLifetimeParamIndices.any()
    ? IndexSubset::get(ctx, conditionallyAddressableLifetimeParamIndices)
    : nullptr);
}

std::optional<llvm::ArrayRef<LifetimeDependenceInfo>>
LifetimeDependenceInfo::getFromSIL(FunctionTypeRepr *funcRepr,
                                   ArrayRef<SILParameterInfo> params,
                                   ArrayRef<SILResultInfo> results,
                                   DeclContext *dc) {
  SmallVector<LifetimeDependenceInfo, 1> lifetimeDependencies;

  auto getLifetimeDependenceFromTypeModifiers =
      [&](TypeRepr *typeRepr,
          unsigned targetIndex) -> std::optional<LifetimeDependenceInfo> {
    auto *lifetimeTypeRepr =
        dyn_cast_or_null<LifetimeDependentTypeRepr>(typeRepr);
    if (!lifetimeTypeRepr) {
      return std::nullopt;
    }
    return checkSILTypeModifiers(lifetimeTypeRepr, targetIndex, params, dc);
  };

  auto argsTypeRepr = funcRepr->getArgsTypeRepr()->getElements();
  for (unsigned targetIndex : indices(argsTypeRepr)) {
    if (auto result = getLifetimeDependenceFromTypeModifiers(
            argsTypeRepr[targetIndex].Type, targetIndex)) {
      lifetimeDependencies.push_back(*result);
    }
  }

  auto result = getLifetimeDependenceFromTypeModifiers(
    funcRepr->getResultTypeRepr(), params.size());
  if (result) {
    lifetimeDependencies.push_back(*result);
  }

  return dc->getASTContext().AllocateCopy(lifetimeDependencies);
}

void LifetimeDependenceInfo::dump() const {
  llvm::errs() << "target: " << getTargetIndex() << '\n';
  if (isImmortal()) {
    llvm::errs() << "  immortal\n";
  }
  if (auto scoped = getScopeIndices()) {
    llvm::errs() << "  scoped: ";
    scoped->dump();
  }
  if (auto inherited = getInheritIndices()) {
    llvm::errs() << "  inherited: ";
    inherited->dump();
  }
  if (auto addressable = getAddressableIndices()) {
    llvm::errs() << "  addressable: ";
    addressable->dump();
  }
}

} // namespace swift
