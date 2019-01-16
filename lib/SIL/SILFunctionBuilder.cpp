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

// SWIFT_ENABLE_TENSORFLOW
void
SILFunctionBuilder::addFunctionAttributes(SILFunction *F, SILDeclRef constant,
                                          DeclAttributes &Attrs, SILModule &M) {
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

  // SWIFT_ENABLE_TENSORFLOW
  // Propagate @differentiable attributes.
  // Don't propagate @differentiable to:
  // - Non-getter accessors (setters, modifiers, etc).
  // - Default argument generator functions.
  // - Thunks. Those are currently handled in SILGenThunk.cpp.
  auto *decl = constant.getDecl();
  if ((!isa<AccessorDecl>(decl) || dyn_cast<AccessorDecl>(decl)->isGetter()) &&
      constant.kind != SILDeclRef::Kind::DefaultArgGenerator &&
      !constant.autoDiffAssociatedFunctionIdentifier &&
      !constant.isThunk()) {
    for (auto *A : Attrs.getAttributes<DifferentiableAttr>()) {
      auto *DA = cast<DifferentiableAttr>(A);
      // Either only adjoint is specified, or both primal and adjoint are
      // spcified.
      std::string primName, adjName, jvpName, vjpName;
      // Note: always setting `hasPrimitiveAdjoint` to true is a hack.
      // Eventually, `hasPrimitiveAdjoint` should be removed from the attribute.
      bool hasPrimitiveAdjoint = true;
      // Note: the following alternative implementations of `isSameModule` don't
      // work under all scenarios:
      // - `F->isDefinition()`
      // - `forDefinition` (passed from `getOrCreateFunction`)
      bool isSameModule = M.getSwiftModule() == decl->getModuleContext();
      // Get primal/adjoint names only for functions defined in the current
      // module.
      if (isSameModule) {
        if (auto *primFn = DA->getPrimalFunction())
          primName = SILDeclRef(primFn).mangle();
        if (auto *adjFn = DA->getAdjointFunction()) {
          // If the adjoint is specified but the primal is not, then we treat
          // the original as the primal.
          if (primName.empty())
            primName = F->getName();
          adjName = SILDeclRef(adjFn).mangle();
          hasPrimitiveAdjoint = true;
        } else {
          assert(primName.empty() &&
                 "Primal cannot be present if adjoint is not");
        }
      }
      // Get JVP/VJP names. If the functions aren't specified, use the expected
      // mangled name. Differentiation pass ensures that JVP and VJP exist.
      if (auto *jvpFn = DA->getJVPFunction())
        jvpName = SILDeclRef(jvpFn).mangle();
      else if (!isSameModule)
        jvpName = constant.asAutoDiffAssociatedFunction(
            AutoDiffAssociatedFunctionIdentifier::get(
                AutoDiffAssociatedFunctionKind::JVP, /*differentiationOrder*/ 1,
                DA->getParameterIndices(), F->getASTContext())).mangle();
      if (auto *vjpFn = DA->getVJPFunction())
        vjpName = SILDeclRef(vjpFn).mangle();
      else if (!isSameModule)
        vjpName = constant.asAutoDiffAssociatedFunction(
            AutoDiffAssociatedFunctionIdentifier::get(
                AutoDiffAssociatedFunctionKind::VJP, /*differentiationOrder*/ 1,
                DA->getParameterIndices(), F->getASTContext())).mangle();
      // Get lowered argument indices.
      auto paramIndices = DA->getParameterIndices();
      auto loweredIndices = paramIndices->getLowered(
          decl->getInterfaceType()->castTo<AnyFunctionType>());
      SILAutoDiffIndices indices(/*source*/ 0, loweredIndices);
      auto silDiffAttr = SILDifferentiableAttr::create(
          M, indices, DA->getRequirements(),
          M.allocateCopy(primName), M.allocateCopy(adjName),
          /*primitive*/ hasPrimitiveAdjoint,
          M.allocateCopy(jvpName), M.allocateCopy(vjpName));
      F->addDifferentiableAttr(silDiffAttr);
    }
  }
}

SILFunction *
SILFunctionBuilder::getOrCreateFunction(SILLocation loc, SILDeclRef constant,
                                        ForDefinition_t forDefinition,
                                        ProfileCounter entryCount) {
  auto nameTmp = constant.mangle();
  auto constantType = mod.Types.getConstantFunctionType(constant);
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

    if (decl->isWeakImported(/*fromModule=*/nullptr))
      F->setWeakLinked();

    if (auto *accessor = dyn_cast<AccessorDecl>(decl)) {
      auto *storage = accessor->getStorage();
      // SWIFT_ENABLE_TENSORFLOW
      addFunctionAttributes(F, constant, storage->getAttrs(), mod);
    }
    // SWIFT_ENABLE_TENSORFLOW
    addFunctionAttributes(F, constant, decl->getAttrs(), mod);
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
