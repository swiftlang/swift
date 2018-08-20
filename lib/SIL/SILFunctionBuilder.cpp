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

using namespace swift;

SILFunction *SILFunctionBuilder::getOrCreateFunction(
    SILLocation loc, StringRef name, SILLinkage linkage,
    CanSILFunctionType type, IsBare_t isBareSILFunction,
    IsTransparent_t isTransparent, IsSerialized_t isSerialized,
    ProfileCounter entryCount, IsThunk_t isThunk, SubclassScope subclassScope) {
  assert(!type->isNoEscape() && "Function decls always have escaping types.");
  if (auto fn = mod.lookUpFunction(name)) {
    assert(fn->getLoweredFunctionType() == type);
    assert(stripExternalFromLinkage(fn->getLinkage()) ==
           stripExternalFromLinkage(linkage));
    return fn;
  }

  auto fn = SILFunction::create(mod, linkage, name, type, nullptr, loc,
                                isBareSILFunction, isTransparent, isSerialized,
                                entryCount, isThunk, subclassScope);
  fn->setDebugScope(new (mod) SILDebugScope(loc, fn));
  return fn;
}

static bool verifySILSelfParameterType(SILDeclRef DeclRef, SILFunction *F,
                                       CanSILFunctionType FTy) {
  SILModule &M = F->getModule();
  SILParameterInfo PInfo = FTy->getSelfParameter();
  CanType CTy = PInfo.getType();
  SILType Ty = SILType::getPrimitiveObjectType(CTy);

  // We do not care about trivial parameters (for now). There seem to be
  // cases where we lower them as unowned.
  //
  // *NOTE* We do not run this check when we have a generic type since
  // *generic types do not have type lowering and are always treated as
  // *non-trivial since we do not know the type.
  if (CTy->hasArchetype() || CTy->hasTypeParameter() ||
      M.getTypeLowering(Ty).isTrivial())
    return true;

  // If this function is a constructor or destructor, bail. These have @owned
  // parameters.
  if (DeclRef.isConstructor() || DeclRef.isDestructor())
    return true;

  // Otherwise, if this function type has a guaranteed self parameter type,
  // make sure that we have a +0 self param.
  return !FTy->getExtInfo().hasGuaranteedSelfParam() || PInfo.isGuaranteed() ||
         PInfo.isIndirectMutating();
}

static void addFunctionAttributes(SILFunction *F, DeclAttributes &Attrs,
                                  SILModule &M) {
  for (auto *A : Attrs.getAttributes<SemanticsAttr>())
    F->addSemanticsAttr(cast<SemanticsAttr>(A)->Value);

  // Propagate @_specialize.
  for (auto *A : Attrs.getAttributes<SpecializeAttr>()) {
    auto *SA = cast<SpecializeAttr>(A);
    auto kind =
        SA->getSpecializationKind() == SpecializeAttr::SpecializationKind::Full
            ? SILSpecializeAttr::SpecializationKind::Full
            : SILSpecializeAttr::SpecializationKind::Partial;
    F->addSpecializeAttr(SILSpecializeAttr::create(M, SA->getRequirements(),
                                                   SA->isExported(), kind));
  }

  if (auto *OA = Attrs.getAttribute<OptimizeAttr>()) {
    F->setOptimizationMode(OA->getMode());
  }

  // @_silgen_name and @_cdecl functions may be called from C code somewhere.
  if (Attrs.hasAttribute<SILGenNameAttr>() || Attrs.hasAttribute<CDeclAttr>())
    F->setHasCReferences(true);

  if (Attrs.hasAttribute<WeakLinkedAttr>())
    F->setWeakLinked();
}

SILFunction *
SILFunctionBuilder::getOrCreateFunction(SILLocation loc, SILDeclRef constant,
                                        ForDefinition_t forDefinition,
                                        ProfileCounter entryCount) {
  auto name = constant.mangle();
  auto constantType = mod.Types.getConstantFunctionType(constant);
  SILLinkage linkage = constant.getLinkage(forDefinition);

  if (auto fn = mod.lookUpFunction(name)) {
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

  auto *F =
      SILFunction::create(mod, linkage, name, constantType, nullptr, None,
                          IsNotBare, IsTrans, IsSer, entryCount, IsNotThunk,
                          constant.getSubclassScope(), inlineStrategy, EK);
  F->setDebugScope(new (mod) SILDebugScope(loc, F));

  F->setGlobalInit(constant.isGlobal());
  if (constant.hasDecl()) {
    auto decl = constant.getDecl();

    if (constant.isForeign && decl->hasClangNode())
      F->setClangNodeOwner(decl);

    if (auto *accessor = dyn_cast<AccessorDecl>(decl)) {
      auto *storage = accessor->getStorage();
      // Add attributes for e.g. computed properties.
      addFunctionAttributes(F, storage->getAttrs(), mod);
    }
    addFunctionAttributes(F, decl->getAttrs(), mod);
  }

  // If this function has a self parameter, make sure that it has a +0 calling
  // convention. This cannot be done for general function types, since
  // function_ref's SILFunctionTypes do not have archetypes associated with
  // it.
  CanSILFunctionType FTy = F->getLoweredFunctionType();
  if (FTy->hasSelfParam()) {
    (void)&verifySILSelfParameterType;
    assert(verifySILSelfParameterType(constant, F, FTy) &&
           "Invalid signature for SIL Self parameter type");
  }

  return F;
}

SILFunction *SILFunctionBuilder::getOrCreateSharedFunction(
    SILLocation loc, StringRef name, CanSILFunctionType type,
    IsBare_t isBareSILFunction, IsTransparent_t isTransparent,
    IsSerialized_t isSerialized, ProfileCounter entryCount, IsThunk_t isThunk) {
  return getOrCreateFunction(loc, name, SILLinkage::Shared, type,
                             isBareSILFunction, isTransparent, isSerialized,
                             entryCount, isThunk, SubclassScope::NotApplicable);
}

SILFunction *SILFunctionBuilder::createFunction(
    SILLinkage linkage, StringRef name, CanSILFunctionType loweredType,
    GenericEnvironment *genericEnv, Optional<SILLocation> loc,
    IsBare_t isBareSILFunction, IsTransparent_t isTrans,
    IsSerialized_t isSerialized, ProfileCounter entryCount, IsThunk_t isThunk,
    SubclassScope subclassScope, Inline_t inlineStrategy, EffectsKind EK,
    SILFunction *InsertBefore, const SILDebugScope *DebugScope) {
  return SILFunction::create(mod, linkage, name, loweredType, genericEnv, loc,
                             isBareSILFunction, isTrans, isSerialized,
                             entryCount, isThunk, subclassScope, inlineStrategy,
                             EK, InsertBefore, DebugScope);
}
