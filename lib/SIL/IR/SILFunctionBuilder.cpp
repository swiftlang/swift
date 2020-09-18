//===--- SILFunctionBuilder.cpp -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILFunctionBuilder.h"
#include "swift/AST/Availability.h"
#include "swift/AST/Decl.h"
using namespace swift;

SILFunction *SILFunctionBuilder::getOrCreateFunction(
    SILLocation loc, StringRef name, SILLinkage linkage, CanSILFunctionType type, IsBare_t isBareSILFunction,
    IsTransparent_t isTransparent, IsSerialized_t isSerialized,
    IsDynamicallyReplaceable_t isDynamic, ProfileCounter entryCount,
    IsThunk_t isThunk, SubclassScope subclassScope) {
  assert(!type->isNoEscape() && "Function decls always have escaping types.");
  if (auto fn = mod.lookUpFunction(name)) {
    assert(fn->getLoweredFunctionType() == type);
    assert(stripExternalFromLinkage(fn->getLinkage()) ==
           stripExternalFromLinkage(linkage));
    return fn;
  }

  auto fn = SILFunction::create(mod, linkage, name, type, nullptr, loc,
                                isBareSILFunction, isTransparent, isSerialized,
                                entryCount, isDynamic, IsNotExactSelfClass,
                                isThunk, subclassScope);
  fn->setDebugScope(new (mod) SILDebugScope(loc, fn));
  return fn;
}

void SILFunctionBuilder::addFunctionAttributes(
    SILFunction *F, DeclAttributes &Attrs, SILModule &M,
    llvm::function_ref<SILFunction *(SILLocation loc, SILDeclRef constant)>
        getOrCreateDeclaration,
    SILDeclRef constant) {

  for (auto *A : Attrs.getAttributes<SemanticsAttr>())
    F->addSemanticsAttr(cast<SemanticsAttr>(A)->Value);

  // Propagate @_specialize.
  for (auto *A : Attrs.getAttributes<SpecializeAttr>()) {
    auto *SA = cast<SpecializeAttr>(A);
    auto kind =
        SA->getSpecializationKind() == SpecializeAttr::SpecializationKind::Full
            ? SILSpecializeAttr::SpecializationKind::Full
            : SILSpecializeAttr::SpecializationKind::Partial;
    assert(!constant.isNull());
    SILFunction *targetFunction = nullptr;
    auto *attributedFuncDecl = constant.getDecl();
    auto *targetFunctionDecl = SA->getTargetFunctionDecl(attributedFuncDecl);
    if (targetFunctionDecl) {
      SILDeclRef declRef(targetFunctionDecl, constant.kind, false);
      targetFunction = getOrCreateDeclaration(targetFunctionDecl, declRef);
      F->addSpecializeAttr(
          SILSpecializeAttr::create(M, SA->getSpecializedSignature(),
                                    SA->isExported(), kind, targetFunction));
    } else {
      F->addSpecializeAttr(SILSpecializeAttr::create(
          M, SA->getSpecializedSignature(), SA->isExported(), kind, nullptr));
    }
  }

  if (auto *OA = Attrs.getAttribute<OptimizeAttr>()) {
    F->setOptimizationMode(OA->getMode());
  }

  // @_silgen_name and @_cdecl functions may be called from C code somewhere.
  if (Attrs.hasAttribute<SILGenNameAttr>() || Attrs.hasAttribute<CDeclAttr>())
    F->setHasCReferences(true);

  // Validate `@differentiable` attributes by calling `getParameterIndices`.
  // This is important for:
  // - Skipping invalid `@differentiable` attributes in non-primary files.
  // - Preventing duplicate SIL differentiability witness creation for
  //   `@differentiable` attributes on `AbstractStorageDecl` declarations.
  //   Such `@differentiable` attributes are deleted and recreated on the getter
  //   `AccessorDecl` of the `AbstractStorageDecl`.
  for (auto *A : Attrs.getAttributes<DifferentiableAttr>())
    (void)A->getParameterIndices();

  // Propagate `@noDerivative` as `[_semantics "autodiff.nonvarying"]`.
  //
  // `@noDerivative` implies non-varying semantics for differentiable activity
  // analysis. SIL values produced from references to `@noDerivative`
  // declarations will not be marked as varying; these values do not need a
  // derivative.
  if (Attrs.hasAttribute<NoDerivativeAttr>())
    F->addSemanticsAttr("autodiff.nonvarying");

  // Propagate @_dynamicReplacement(for:).
  if (constant.isNull())
    return;
  auto *decl = constant.getDecl();

  // Only emit replacements for the objc entry point of objc methods.
  // There is one exception: @_dynamicReplacement(for:) of @objc methods in
  // generic classes. In this special case we use native replacement instead of
  // @objc categories.
  if (decl->isObjC() && !decl->isNativeMethodReplacement() &&
      F->getLoweredFunctionType()->getExtInfo().getRepresentation() !=
          SILFunctionTypeRepresentation::ObjCMethod)
    return;

  // Only assign replacements when the thing being replaced is function-like and
  // explicitly declared.  
  auto *origDecl = decl->getDynamicallyReplacedDecl();
  auto *replacedDecl = dyn_cast_or_null<AbstractFunctionDecl>(origDecl);
  if (!replacedDecl)
    return;

  // For @objc method replacement we normally use categories to perform the
  // replacement. Except for methods in generic class where we can't. Instead,
  // we special case this and use the native swift replacement mechanism.
  if (decl->isObjC() && !decl->isNativeMethodReplacement()) {
    F->setObjCReplacement(replacedDecl);
    return;
  }

  if (!constant.canBeDynamicReplacement())
    return;

  SILDeclRef declRef(replacedDecl, constant.kind, false);
  auto *replacedFunc = getOrCreateDeclaration(replacedDecl, declRef);

  assert(replacedFunc->getLoweredFunctionType() ==
             F->getLoweredFunctionType() ||
         replacedFunc->getLoweredFunctionType()->hasOpaqueArchetype());

  F->setDynamicallyReplacedFunction(replacedFunc);
}

SILFunction *SILFunctionBuilder::getOrCreateFunction(
    SILLocation loc, SILDeclRef constant, ForDefinition_t forDefinition,
    llvm::function_ref<SILFunction *(SILLocation loc, SILDeclRef constant)>
        getOrCreateDeclaration,
    ProfileCounter entryCount) {
  auto nameTmp = constant.mangle();
  auto constantType = mod.Types.getConstantFunctionType(
      TypeExpansionContext::minimal(), constant);
  SILLinkage linkage = constant.getLinkage(forDefinition);

  if (auto fn = mod.lookUpFunction(nameTmp)) {
    assert(fn->getLoweredFunctionType() == constantType);
    assert(fn->getLinkage() == linkage ||
           (forDefinition == ForDefinition_t::NotForDefinition &&
            fn->getLinkage() ==
                constant.getLinkage(ForDefinition_t::ForDefinition)));
    if (forDefinition) {
      // In all the cases where getConstantLinkage returns something
      // different for ForDefinition, it returns an available-externally
      // linkage.
      if (isAvailableExternally(fn->getLinkage())) {
        fn->setLinkage(constant.getLinkage(ForDefinition));
      }
    }
    return fn;
  }

  IsTransparent_t IsTrans =
      constant.isTransparent() ? IsTransparent : IsNotTransparent;
  IsSerialized_t IsSer = constant.isSerialized();

  EffectsKind EK = constant.hasEffectsAttribute()
                       ? constant.getEffectsAttribute()
                       : EffectsKind::Unspecified;

  Inline_t inlineStrategy = InlineDefault;
  if (constant.isNoinline())
    inlineStrategy = NoInline;
  else if (constant.isAlwaysInline())
    inlineStrategy = AlwaysInline;

  StringRef name = mod.allocateCopy(nameTmp);
  IsDynamicallyReplaceable_t IsDyn = IsNotDynamic;
  if (constant.isDynamicallyReplaceable()) {
    IsDyn = IsDynamic;
    IsTrans = IsNotTransparent;
  }

  auto *F = SILFunction::create(mod, linkage, name, constantType, nullptr, None,
                                IsNotBare, IsTrans, IsSer, entryCount, IsDyn,
                                IsNotExactSelfClass,
                                IsNotThunk, constant.getSubclassScope(),
                                inlineStrategy, EK);
  F->setDebugScope(new (mod) SILDebugScope(loc, F));

  if (constant.isGlobal())
    F->setSpecialPurpose(SILFunction::Purpose::GlobalInit);

  if (constant.hasDecl()) {
    auto decl = constant.getDecl();

    if (constant.isForeign && decl->hasClangNode())
      F->setClangNodeOwner(decl);

    F->setAvailabilityForLinkage(decl->getAvailabilityForLinkage());
    F->setAlwaysWeakImported(decl->isAlwaysWeakImported());

    if (auto *accessor = dyn_cast<AccessorDecl>(decl)) {
      auto *storage = accessor->getStorage();
      // Add attributes for e.g. computed properties.
      addFunctionAttributes(F, storage->getAttrs(), mod,
                            getOrCreateDeclaration);
                            
      auto *varDecl = dyn_cast<VarDecl>(storage);
      if (varDecl && varDecl->getAttrs().hasAttribute<LazyAttr>() &&
          accessor->getAccessorKind() == AccessorKind::Get) {
        F->setSpecialPurpose(SILFunction::Purpose::LazyPropertyGetter);
        
        // Lazy property getters should not get inlined because they are usually
        // non-tivial functions (otherwise the user would not implement it as
        // lazy property). Inlining such getters would most likely not benefit
        // other optimizations because the top-level switch_enum cannot be
        // constant folded in most cases.
        // Also, not inlining lazy property getters enables optimizing them in
        // CSE.
        F->setInlineStrategy(NoInline);
      }
    }
    addFunctionAttributes(F, decl->getAttrs(), mod, getOrCreateDeclaration,
                          constant);
  }

  return F;
}

SILFunction *SILFunctionBuilder::getOrCreateSharedFunction(
    SILLocation loc, StringRef name, CanSILFunctionType type,
    IsBare_t isBareSILFunction, IsTransparent_t isTransparent,
    IsSerialized_t isSerialized, ProfileCounter entryCount, IsThunk_t isThunk,
    IsDynamicallyReplaceable_t isDynamic) {
  return getOrCreateFunction(loc, name, SILLinkage::Shared, type,
                             isBareSILFunction, isTransparent, isSerialized,
                             isDynamic, entryCount, isThunk,
                             SubclassScope::NotApplicable);
}

SILFunction *SILFunctionBuilder::createFunction(
    SILLinkage linkage, StringRef name, CanSILFunctionType loweredType,
    GenericEnvironment *genericEnv, Optional<SILLocation> loc,
    IsBare_t isBareSILFunction, IsTransparent_t isTrans,
    IsSerialized_t isSerialized, IsDynamicallyReplaceable_t isDynamic,
    ProfileCounter entryCount, IsThunk_t isThunk, SubclassScope subclassScope,
    Inline_t inlineStrategy, EffectsKind EK, SILFunction *InsertBefore,
    const SILDebugScope *DebugScope) {
  return SILFunction::create(mod, linkage, name, loweredType, genericEnv, loc,
                             isBareSILFunction, isTrans, isSerialized,
                             entryCount, isDynamic, IsNotExactSelfClass,
                             isThunk, subclassScope,
                             inlineStrategy, EK, InsertBefore, DebugScope);
}
