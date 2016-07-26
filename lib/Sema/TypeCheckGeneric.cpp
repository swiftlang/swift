//===--- TypeCheckGeneric.cpp - Generics ----------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements support for generics.
//
//===----------------------------------------------------------------------===//
#include "TypeChecker.h"
#include "GenericTypeResolver.h"
#include "swift/AST/ArchetypeBuilder.h"
#include "swift/Basic/Defer.h"

using namespace swift;

Type DependentGenericTypeResolver::resolveGenericTypeParamType(
                                     GenericTypeParamType *gp) {
  // Don't resolve generic parameters.
  return gp;
}

Type DependentGenericTypeResolver::resolveDependentMemberType(
                                     Type baseTy,
                                     DeclContext *DC,
                                     SourceRange baseRange,
                                     ComponentIdentTypeRepr *ref) {
  auto archetype = Builder.resolveArchetype(baseTy);
  assert(archetype && "Bad generic context nesting?");
  
  return archetype->getRepresentative()
           ->getNestedType(ref->getIdentifier(), Builder)
           ->getDependentType(Builder, true);
}

Type DependentGenericTypeResolver::resolveSelfAssociatedType(
       Type selfTy,
       DeclContext *DC,
       AssociatedTypeDecl *assocType) {
  auto archetype = Builder.resolveArchetype(selfTy);
  assert(archetype && "Bad generic context nesting?");
  
  return archetype->getRepresentative()
           ->getNestedType(assocType->getName(), Builder)
           ->getDependentType(Builder, true);
}

Type DependentGenericTypeResolver::resolveTypeOfContext(DeclContext *dc) {
  // FIXME: Should be the interface type of the extension.
  return dc->getDeclaredInterfaceType();
}

Type DependentGenericTypeResolver::resolveTypeOfDecl(TypeDecl *decl) {
  return decl->getDeclaredInterfaceType();
}

Type GenericTypeToArchetypeResolver::resolveGenericTypeParamType(
                                       GenericTypeParamType *gp) {
  auto gpDecl = gp->getDecl();
  assert(gpDecl && "Missing generic parameter declaration");

  auto archetype = gpDecl->getArchetype();
  if (!archetype)
    return ErrorType::get(gp->getASTContext());

  return archetype;
}

Type GenericTypeToArchetypeResolver::resolveDependentMemberType(
                                  Type baseTy,
                                  DeclContext *DC,
                                  SourceRange baseRange,
                                  ComponentIdentTypeRepr *ref) {
  llvm_unreachable("Dependent type after archetype substitution");
}

Type GenericTypeToArchetypeResolver::resolveSelfAssociatedType(
       Type selfTy,
       DeclContext *DC,
       AssociatedTypeDecl *assocType) {
  llvm_unreachable("Dependent type after archetype substitution");  
}

Type GenericTypeToArchetypeResolver::resolveTypeOfContext(DeclContext *dc) {
  return dc->getDeclaredTypeInContext();
}

Type GenericTypeToArchetypeResolver::resolveTypeOfDecl(TypeDecl *decl) {
  return decl->getDeclaredType();
}

Type PartialGenericTypeToArchetypeResolver::resolveGenericTypeParamType(
                                              GenericTypeParamType *gp) {
  auto gpDecl = gp->getDecl();
  if (!gpDecl)
    return Type(gp);


  auto archetype = gpDecl->getArchetype();
  if (!archetype)
    return Type(gp);

  return archetype;
}


Type PartialGenericTypeToArchetypeResolver::resolveDependentMemberType(
                                              Type baseTy,
                                              DeclContext *DC,
                                              SourceRange baseRange,
                                              ComponentIdentTypeRepr *ref) {
  // We don't have enough information to find the associated type.
  // FIXME: Nonsense, but we shouldn't need this code anyway.
  return DependentMemberType::get(baseTy, ref->getIdentifier(), TC.Context);
}

Type PartialGenericTypeToArchetypeResolver::resolveSelfAssociatedType(
       Type selfTy,
       DeclContext *DC,
       AssociatedTypeDecl *assocType) {
  // We don't have enough information to find the associated type.
  // FIXME: Nonsense, but we shouldn't need this code anyway.
  return DependentMemberType::get(selfTy, assocType, TC.Context);
}

Type
PartialGenericTypeToArchetypeResolver::resolveTypeOfContext(DeclContext *dc) {
  return dc->getDeclaredTypeInContext();
}

Type
PartialGenericTypeToArchetypeResolver::resolveTypeOfDecl(TypeDecl *decl) {
  return decl->getDeclaredType();
}

Type CompleteGenericTypeResolver::resolveGenericTypeParamType(
                                              GenericTypeParamType *gp) {
  // Retrieve the potential archetype corresponding to this generic type
  // parameter.
  // FIXME: When generic parameters can map down to specific types, do so
  // here.
  auto pa = Builder.resolveArchetype(gp);
  (void)pa;

  return gp;
}


Type CompleteGenericTypeResolver::resolveDependentMemberType(
                                    Type baseTy,
                                    DeclContext *DC,
                                    SourceRange baseRange,
                                    ComponentIdentTypeRepr *ref) {
  // Resolve the base to a potential archetype.
  auto basePA = Builder.resolveArchetype(baseTy);
  assert(basePA && "Missing potential archetype for base");
  basePA = basePA->getRepresentative();

  // Retrieve the potential archetype for the nested type.
  auto nestedPA = basePA->getNestedType(ref->getIdentifier(), Builder);

  // If this potential archetype was renamed due to typo correction,
  // complain and fix it.
  if (nestedPA->wasRenamed()) {
    auto newName = nestedPA->getName();
    TC.diagnose(ref->getIdLoc(), diag::invalid_member_type_suggest,
                baseTy, ref->getIdentifier(), newName)
      .fixItReplace(ref->getIdLoc(), newName.str());
    ref->overwriteIdentifier(newName);

    // Go get the actual nested type.
    nestedPA = basePA->getNestedType(newName, Builder);
    assert(!nestedPA->wasRenamed());
  }

  // If the nested type has been resolved to an associated type, use it.
  if (auto assocType = nestedPA->getResolvedAssociatedType()) {
    return DependentMemberType::get(baseTy, assocType, TC.Context);
  }

  // If the nested type comes from a type alias, use either the alias's
  // concrete type, or resolve its components down to another dependent member.
  if (auto alias = nestedPA->getTypeAliasDecl()) {
    return TC.substMemberTypeWithBase(DC->getParentModule(), alias,
                                      baseTy, true);
  }
  
  Identifier name = ref->getIdentifier();
  SourceLoc nameLoc = ref->getIdLoc();

  // Check whether the name can be found in the superclass.
  // FIXME: The archetype builder should be doing this and mapping down to a
  // concrete type.
  if (auto superclassTy = basePA->getSuperclass()) {
    if (auto lookup = TC.lookupMemberType(DC, superclassTy, name)) {
      if (lookup.isAmbiguous()) {
        TC.diagnoseAmbiguousMemberType(baseTy, baseRange, name, nameLoc,
                                       lookup);
        return ErrorType::get(TC.Context);
      }

      // FIXME: Record (via type sugar) that this was referenced via baseTy.
      return lookup.front().second;
    }
  }

  // Complain that there is no suitable type.
  TC.diagnose(nameLoc, diag::invalid_member_type, name, baseTy)
    .highlight(baseRange);
  return ErrorType::get(TC.Context);
}

Type CompleteGenericTypeResolver::resolveSelfAssociatedType(Type selfTy,
       DeclContext *DC,
       AssociatedTypeDecl *assocType) {
  return Builder.resolveArchetype(selfTy)->getRepresentative()
           ->getNestedType(assocType->getName(), Builder)
           ->getDependentType(Builder, false);
}

Type CompleteGenericTypeResolver::resolveTypeOfContext(DeclContext *dc) {
  // FIXME: Should be the interface type of the extension.
  return dc->getDeclaredInterfaceType();
}

Type CompleteGenericTypeResolver::resolveTypeOfDecl(TypeDecl *decl) {
  return decl->getDeclaredInterfaceType();
}

/// Check the generic parameters in the given generic parameter list (and its
/// parent generic parameter lists) according to the given resolver.
bool TypeChecker::checkGenericParamList(ArchetypeBuilder *builder,
                                        GenericParamList *genericParams,
                                        GenericSignature *parentSig,
                                        bool adoptArchetypes,
                                        GenericTypeResolver *resolver) {
  bool invalid = false;

  // If there is a parent context, add the generic parameters and requirements
  // from that context.
  if (builder)
    builder->addGenericSignature(parentSig, adoptArchetypes);

  // If there aren't any generic parameters at this level, we're done.
  if (!genericParams)
    return false;

  assert(genericParams->size() > 0 && "Parsed an empty generic parameter list?");

  // Determine where and how to perform name lookup for the generic
  // parameter lists and where clause.
  TypeResolutionOptions options;
  DeclContext *lookupDC = genericParams->begin()[0]->getDeclContext();
  if (!lookupDC->isModuleScopeContext()) {
    assert(isa<GenericTypeDecl>(lookupDC) || isa<ExtensionDecl>(lookupDC) ||
           isa<AbstractFunctionDecl>(lookupDC) &&
           "not a proper generic parameter context?");
    options = TR_GenericSignature;
  }    

  // First, set the depth of each generic parameter, and add them to the
  // archetype builder. Do this before checking the inheritance clause,
  // since it may itself be dependent on one of these parameters.
  unsigned depth = genericParams->getDepth();
  for (auto param : *genericParams) {
    param->setDepth(depth);

    if (builder) {
      if (builder->addGenericParameter(param))
        invalid = true;
    }
  }

  // Now, check the inheritance clauses of each parameter.
  for (auto param : *genericParams) {
    checkInheritanceClause(param, resolver);

    if (builder) {
      builder->addGenericParameterRequirements(param);

      // Infer requirements from the inherited types.
      for (const auto &inherited : param->getInherited()) {
        if (builder->inferRequirements(inherited, genericParams))
          invalid = true;
      }
    }
  }

  // Visit each of the requirements, adding them to the builder.
  // Add the requirements clause to the builder, validating the types in
  // the requirements clause along the way.
  for (auto &req : genericParams->getRequirements()) {
    if (req.isInvalid())
      continue;

    switch (req.getKind()) {
    case RequirementReprKind::TypeConstraint: {
      // Validate the types.
      if (validateType(req.getSubjectLoc(), lookupDC, options, resolver)) {
        invalid = true;
        req.setInvalid();
        continue;
      }

      if (validateType(req.getConstraintLoc(), lookupDC, options,
                       resolver)) {
        invalid = true;
        req.setInvalid();
        continue;
      }

      // FIXME: Feels too early to perform this check.
      if (!req.getConstraint()->isExistentialType() &&
          !req.getConstraint()->getClassOrBoundGenericClass()) {
        diagnose(genericParams->getWhereLoc(),
                 diag::requires_conformance_nonprotocol,
                 req.getSubjectLoc(), req.getConstraintLoc());
        req.getConstraintLoc().setInvalidType(Context);
        invalid = true;
        req.setInvalid();
        continue;
      }

      break;
    }

    case RequirementReprKind::SameType:
      if (validateType(req.getFirstTypeLoc(), lookupDC, options,
                       resolver)) {
        invalid = true;
        req.setInvalid();
        continue;
      }

      if (validateType(req.getSecondTypeLoc(), lookupDC, options,
                       resolver)) {
        invalid = true;
        req.setInvalid();
        continue;
      }
      
      break;
    }
    
    if (builder && builder->addRequirement(req)) {
      invalid = true;
      req.setInvalid();
    }
  }

  return invalid;
}

/// Collect all of the generic parameter types at every level in the generic
/// parameter list.
static void collectGenericParamTypes(
              GenericParamList *genericParams,
              GenericSignature *parentSig,
              SmallVectorImpl<GenericTypeParamType *> &allParams) {
  // If the parent context has a generic signature, add its generic parameters.
  if (parentSig) {
    allParams.append(parentSig->getGenericParams().begin(),
                     parentSig->getGenericParams().end());
  }

  if (genericParams) {
    // Add our parameters.
    for (auto param : *genericParams) {
      allParams.push_back(param->getDeclaredType()
                            ->castTo<GenericTypeParamType>());
    }
  }
}

/// Check the signature of a generic function.
static bool checkGenericFuncSignature(TypeChecker &tc,
                                      ArchetypeBuilder *builder,
                                      AbstractFunctionDecl *func,
                                      GenericTypeResolver &resolver) {
  bool badType = false;
  func->setIsBeingTypeChecked();

  // Check the generic parameter list.
  auto genericParams = func->getGenericParams();

  tc.checkGenericParamList(builder, genericParams,
                           func->getDeclContext()->getGenericSignatureOfContext(),
                           false, &resolver);

  // Check the parameter patterns.
  for (auto params : func->getParameterLists()) {
    // Check the pattern.
    if (tc.typeCheckParameterList(params, func, TypeResolutionOptions(),
                                  &resolver))
      badType = true;

    // Infer requirements from the pattern.
    if (builder) {
      builder->inferRequirements(params, genericParams);
    }
  }

  // If there is a declared result type, check that as well.
  if (auto fn = dyn_cast<FuncDecl>(func)) {
    if (!fn->getBodyResultTypeLoc().isNull()) {
      // Check the result type of the function.
      TypeResolutionOptions options;
      if (fn->hasDynamicSelf())
        options |= TR_DynamicSelfResult;

      if (tc.validateType(fn->getBodyResultTypeLoc(), fn, options, &resolver)) {
        badType = true;
      }

      // Infer requirements from it.
      if (builder && fn->getBodyResultTypeLoc().getTypeRepr()) {
        builder->inferRequirements(fn->getBodyResultTypeLoc(), genericParams);
      }
    }
  }

  func->setIsBeingTypeChecked(false);
  return badType;
}

static Type getResultType(TypeChecker &TC, FuncDecl *fn, Type resultType) {
  // Look through optional types.
  OptionalTypeKind optKind;
  if (auto origValueType = resultType->getAnyOptionalObjectType(optKind)) {
    // Get the interface type of the result.
    Type ifaceValueType = getResultType(TC, fn, origValueType);

    // Preserve the optional type's original spelling if the interface
    // type is the same as the original.
    if (origValueType.getPointer() == ifaceValueType.getPointer()) {
      return resultType;
    }

    // Wrap the interface type in the right kind of optional.
    switch (optKind) {
    case OTK_None: llvm_unreachable("impossible");
    case OTK_Optional:
      return OptionalType::get(ifaceValueType);
    case OTK_ImplicitlyUnwrappedOptional:
      return ImplicitlyUnwrappedOptionalType::get(ifaceValueType);
    }
    llvm_unreachable("bad optional kind");
  }

  // Rewrite dynamic self to the appropriate interface type.
  if (resultType->is<DynamicSelfType>()) {
    return fn->getDynamicSelfInterface();
  }

  // Weird hacky special case.
  if (!fn->getBodyResultTypeLoc().hasLocation() &&
      fn->isGenericContext()) {
    // FIXME: This should not be rewritten.  This is only needed in cases where
    // we synthesize a function which returns a generic value.  In that case,
    // the return type is specified in terms of archetypes, but has no TypeLoc
    // in the TypeRepr.  Because of this, Sema isn't able to rebuild it in
    // terms of interface types.  When interface types prevail, this should be
    // removed.  Until then, we hack the mapping here.
    return ArchetypeBuilder::mapTypeOutOfContext(fn, resultType);
  }

  return resultType;
}

void TypeChecker::markInvalidGenericSignature(ValueDecl *VD) {
  GenericParamList *genericParams;
  if (auto *AFD = dyn_cast<AbstractFunctionDecl>(VD))
    genericParams = AFD->getGenericParams();
  else
    genericParams = cast<GenericTypeDecl>(VD)->getGenericParams();

  // If there aren't any generic parameters at this level, we're done.
  if (genericParams == nullptr)
    return;

  DeclContext *DC = VD->getDeclContext();
  ArchetypeBuilder builder = createArchetypeBuilder(DC->getParentModule());

  if (auto sig = DC->getGenericSignatureOfContext())
    builder.addGenericSignature(sig, true);

  // Visit each of the generic parameters.
  for (auto param : *genericParams)
    builder.addGenericParameter(param);

  // Wire up the archetypes.
  for (auto GP : *genericParams)
    GP->setArchetype(builder.getArchetype(GP));

  genericParams->setAllArchetypes(
      Context.AllocateCopy(builder.getAllArchetypes()));
}

bool TypeChecker::validateGenericFuncSignature(AbstractFunctionDecl *func) {
  bool invalid = false;

  // Create the archetype builder.
  ArchetypeBuilder builder = createArchetypeBuilder(func->getParentModule());

  // Type check the function declaration, treating all generic type
  // parameters as dependent, unresolved.
  DependentGenericTypeResolver dependentResolver(builder);
  if (checkGenericFuncSignature(*this, &builder, func, dependentResolver))
    invalid = true;

  // If this triggered a recursive validation, back out: we're done.
  // FIXME: This is an awful hack.
  if (func->hasType())
    return !func->isInvalid();

  // Finalize the generic requirements.
  (void)builder.finalize(func->getLoc());

  // The archetype builder now has all of the requirements, although there might
  // still be errors that have not yet been diagnosed. Revert the generic
  // function signature and type-check it again, completely.
  revertGenericFuncSignature(func);
  CompleteGenericTypeResolver completeResolver(*this, builder);
  if (checkGenericFuncSignature(*this, nullptr, func, completeResolver))
    invalid = true;

  // The generic function signature is complete and well-formed. Determine
  // the type of the generic function.

  // Collect the complete set of generic parameter types.
  SmallVector<GenericTypeParamType *, 4> allGenericParams;
  collectGenericParamTypes(func->getGenericParams(),
                           func->getDeclContext()->getGenericSignatureOfContext(),
                           allGenericParams);

  auto sig = builder.getGenericSignature(allGenericParams);

  // Debugging of the archetype builder and generic signature generation.
  if (sig && Context.LangOpts.DebugGenericSignatures) {
    func->dumpRef(llvm::errs());
    llvm::errs() << "\n";
    builder.dump(llvm::errs());
    llvm::errs() << "Generic signature: ";
    sig->print(llvm::errs());
    llvm::errs() << "\n";
    llvm::errs() << "Canonical generic signature: ";
    sig->getCanonicalSignature()->print(llvm::errs());
    llvm::errs() << "\n";
    llvm::errs() << "Canonical generic signature for mangling: ";
    sig->getCanonicalManglingSignature(*func->getParentModule())
    ->print(llvm::errs());
    llvm::errs() << "\n";
  }

  func->setGenericSignature(sig);

  if (invalid) {
    func->overwriteType(ErrorType::get(Context));
    func->setInterfaceType(ErrorType::get(Context));
    return true;
  }

  configureInterfaceType(func);
  return false;
}

void TypeChecker::configureInterfaceType(AbstractFunctionDecl *func) {
  Type funcTy;
  Type initFuncTy;

  auto *sig = func->getGenericSignature();

  if (auto fn = dyn_cast<FuncDecl>(func)) {
    funcTy = fn->getBodyResultTypeLoc().getType();
    
    if (!funcTy) {
      funcTy = TupleType::getEmpty(Context);
    } else {
      funcTy = getResultType(*this, fn, funcTy);
    }

  } else if (auto ctor = dyn_cast<ConstructorDecl>(func)) {
    auto *dc = ctor->getDeclContext();

    funcTy = dc->getSelfInterfaceType();

    // Adjust result type for failability.
    if (ctor->getFailability() != OTK_None)
      funcTy = OptionalType::get(ctor->getFailability(), funcTy);

    initFuncTy = funcTy;
  } else {
    assert(isa<DestructorDecl>(func));
    funcTy = TupleType::getEmpty(Context);
  }

  auto paramLists = func->getParameterLists();
  SmallVector<ParameterList*, 4> storedParamLists;

  // FIXME: Destructors don't have the '()' pattern in their signature, so
  // paste it here.
  if (isa<DestructorDecl>(func)) {
    assert(paramLists.size() == 1 && "Only the self paramlist");
    storedParamLists.push_back(paramLists[0]);
    storedParamLists.push_back(ParameterList::createEmpty(Context));
    paramLists = storedParamLists;
  }

  bool hasSelf = func->getDeclContext()->isTypeContext();
  for (unsigned i = 0, e = paramLists.size(); i != e; ++i) {
    Type argTy;
    Type initArgTy;

    Type selfTy;
    if (i == e-1 && hasSelf) {
      selfTy = func->computeInterfaceSelfType(/*isInitializingCtor=*/false);
      // Substitute in our own 'self' parameter.

      argTy = selfTy;
      if (initFuncTy) {
        initArgTy = func->computeInterfaceSelfType(/*isInitializingCtor=*/true);
      }
    } else {
      argTy = paramLists[e - i - 1]->getInterfaceType(func);

      if (initFuncTy)
        initArgTy = argTy;
    }

    // 'throws' only applies to the innermost function.
    AnyFunctionType::ExtInfo info;
    if (i == 0 && func->hasThrows())
      info = info.withThrows();

    assert(!argTy->hasArchetype());
    assert(!funcTy->hasArchetype());
    if (initFuncTy)
      assert(!initFuncTy->hasArchetype());

    if (sig && i == e-1) {
      funcTy = GenericFunctionType::get(sig, argTy, funcTy, info);
      if (initFuncTy)
        initFuncTy = GenericFunctionType::get(sig, initArgTy, initFuncTy, info);
    } else {
      funcTy = FunctionType::get(argTy, funcTy, info);
      if (initFuncTy)
        initFuncTy = FunctionType::get(initArgTy, initFuncTy, info);
    }
  }

  // Record the interface type.
  func->setInterfaceType(funcTy);
  if (initFuncTy)
    cast<ConstructorDecl>(func)->setInitializerInterfaceType(initFuncTy);

  if (func->getGenericParams()) {
    // Collect all generic params referenced in parameter types,
    // return type or requirements.
    SmallPtrSet<GenericTypeParamDecl *, 4> referencedGenericParams;

    auto visitorFn = [&referencedGenericParams](Type t) {
      if (auto *paramTy = t->getAs<GenericTypeParamType>())
        referencedGenericParams.insert(paramTy->getDecl());
    };

    funcTy->castTo<AnyFunctionType>()->getInput().visit(visitorFn);
    funcTy->castTo<AnyFunctionType>()->getResult().visit(visitorFn);

    auto requirements = sig->getRequirements();
    for (auto req : requirements) {
      if (req.getKind() == RequirementKind::SameType) {
        // Same type requirements may allow for generic
        // inference, even if this generic parameter
        // is not mentioned in the function signature.
        // TODO: Make the test more precise.
        auto left = req.getFirstType();
        auto right = req.getSecondType();
        // For now consider any references inside requirements
        // as a possibility to infer the generic type.
        left.visit(visitorFn);
        right.visit(visitorFn);
      }
    }

    // Find the depth of the function's own generic parameters.
    unsigned fnGenericParamsDepth = func->getGenericParams()->getDepth();

    // Check that every generic parameter type from the signature is
    // among referencedArchetypes.
    for (auto *genParam : sig->getGenericParams()) {
      auto *paramDecl = genParam->getDecl();
      if (paramDecl->getDepth() != fnGenericParamsDepth)
        continue;
      if (!referencedGenericParams.count(paramDecl)) {
        // Produce an error that this generic parameter cannot be bound.
        diagnose(paramDecl->getLoc(), diag::unreferenced_generic_parameter,
                 paramDecl->getNameStr());
        func->setInvalid();
      }
    }
  }
}

GenericSignature *TypeChecker::validateGenericSignature(
                    GenericParamList *genericParams,
                    DeclContext *dc,
                    GenericSignature *outerSignature,
                    std::function<bool(ArchetypeBuilder &)> inferRequirements,
                    bool &invalid) {
  assert(genericParams && "Missing generic parameters?");

  // Create the archetype builder.
  Module *module = dc->getParentModule();
  ArchetypeBuilder builder = createArchetypeBuilder(module);
  auto *parentSig = (outerSignature
                     ? outerSignature
                     : dc->getGenericSignatureOfContext());

  // Type check the generic parameters, treating all generic type
  // parameters as dependent, unresolved.
  DependentGenericTypeResolver dependentResolver(builder);  
  if (checkGenericParamList(&builder, genericParams, parentSig,
                            false, &dependentResolver)) {
    invalid = true;
  }

  /// Perform any necessary requirement inference.
  if (inferRequirements && inferRequirements(builder)) {
    invalid = true;
  }

  // Finalize the generic requirements.
  (void)builder.finalize(genericParams->getSourceRange().Start);

  // The archetype builder now has all of the requirements, although there might
  // still be errors that have not yet been diagnosed. Revert the signature
  // and type-check it again, completely.
  revertGenericParamList(genericParams);
  CompleteGenericTypeResolver completeResolver(*this, builder);
  if (checkGenericParamList(nullptr, genericParams, parentSig,
                            false, &completeResolver)) {
    invalid = true;
  }

  // The generic signature is complete and well-formed. Gather the
  // generic parameter types at all levels.
  SmallVector<GenericTypeParamType *, 4> allGenericParams;
  collectGenericParamTypes(genericParams, parentSig, allGenericParams);

  // Record the generic type parameter types and the requirements.
  auto sig = builder.getGenericSignature(allGenericParams);

  // Debugging of the archetype builder and generic signature generation.
  if (Context.LangOpts.DebugGenericSignatures) {
    dc->printContext(llvm::errs());
    llvm::errs() << "\n";
    builder.dump(llvm::errs());
    llvm::errs() << "Generic signature: ";
    sig->print(llvm::errs());
    llvm::errs() << "\n";
    llvm::errs() << "Canonical generic signature: ";
    sig->getCanonicalSignature()->print(llvm::errs());
    llvm::errs() << "\n";
    llvm::errs() << "Canonical generic signature for mangling: ";
    sig->getCanonicalManglingSignature(*dc->getParentModule())
      ->print(llvm::errs());
    llvm::errs() << "\n";
  }

  return sig;
}

static void revertDependentTypeLoc(TypeLoc &tl) {
  // If there's no type representation, there's nothing to revert.
  if (!tl.getTypeRepr())
    return;

  // Don't revert an error type; we've already complained.
  if (tl.wasValidated() && tl.isError())
    return;

  // Make sure we validate the type again.
  tl.setType(Type(), /*validated=*/false);
}

/// Finalize the given generic parameter list, assigning archetypes to
/// the generic parameters.
void TypeChecker::finalizeGenericParamList(ArchetypeBuilder &builder,
                                           GenericParamList *genericParams,
                                           DeclContext *dc) {
  Accessibility access;
  if (auto *fd = dyn_cast<FuncDecl>(dc))
    access = fd->getFormalAccess();
  else if (auto *nominal = dyn_cast<NominalTypeDecl>(dc))
    access = nominal->getFormalAccess();
  else
    access = Accessibility::Internal;
  access = std::max(access, Accessibility::Internal);

  // Wire up the archetypes.
  for (auto GP : *genericParams) {
    GP->setArchetype(builder.getArchetype(GP));
    checkInheritanceClause(GP);
    if (!GP->hasAccessibility())
      GP->setAccessibility(access);
  }
  genericParams->setAllArchetypes(
    Context.AllocateCopy(builder.getAllArchetypes()));

#ifndef NDEBUG
  // Record archetype contexts.
  for (auto archetype : genericParams->getAllArchetypes()) {
    if (Context.ArchetypeContexts.count(archetype) == 0)
      Context.ArchetypeContexts[archetype] = dc;
  }
#endif

  // Replace the generic parameters with their archetypes throughout the
  // types in the requirements.
  // FIXME: This should not be necessary at this level; it is a transitional
  // step.
  for (auto &Req : genericParams->getRequirements()) {
    if (Req.isInvalid())
      continue;

    switch (Req.getKind()) {
    case RequirementReprKind::TypeConstraint: {
      revertDependentTypeLoc(Req.getSubjectLoc());
      if (validateType(Req.getSubjectLoc(), dc)) {
        Req.setInvalid();
        continue;
      }

      revertDependentTypeLoc(Req.getConstraintLoc());
      if (validateType(Req.getConstraintLoc(), dc)) {
        Req.setInvalid();
        continue;
      }
      break;
    }

    case RequirementReprKind::SameType:
      revertDependentTypeLoc(Req.getFirstTypeLoc());
      if (validateType(Req.getFirstTypeLoc(), dc)) {
        Req.setInvalid();
        continue;
      }

      revertDependentTypeLoc(Req.getSecondTypeLoc());
      if (validateType(Req.getSecondTypeLoc(), dc)) {
        Req.setInvalid();
        continue;
      }
      break;
    }
  }
}

/// Revert the dependent types within the given generic parameter list.
void TypeChecker::revertGenericParamList(GenericParamList *genericParams) {
  // Revert the inherited clause of the generic parameter list.
  for (auto param : *genericParams) {
    param->setCheckedInheritanceClause(false);
    for (auto &inherited : param->getInherited())
      revertDependentTypeLoc(inherited);
  }

  // Revert the requirements of the generic parameter list.
  for (auto &req : genericParams->getRequirements()) {
    if (req.isInvalid())
      continue;

    switch (req.getKind()) {
    case RequirementReprKind::TypeConstraint: {
      revertDependentTypeLoc(req.getSubjectLoc());
      revertDependentTypeLoc(req.getConstraintLoc());
      break;
    }

    case RequirementReprKind::SameType:
      revertDependentTypeLoc(req.getFirstTypeLoc());
      revertDependentTypeLoc(req.getSecondTypeLoc());
      break;
    }
  }
}

bool TypeChecker::validateGenericTypeSignature(GenericTypeDecl *typeDecl) {
  bool invalid = false;

  if (typeDecl->isValidatingGenericSignature())
    return invalid;

  typeDecl->setIsValidatingGenericSignature();

  SWIFT_DEFER { typeDecl->setIsValidatingGenericSignature(false); };

  auto *gp = typeDecl->getGenericParams();
  auto *dc = typeDecl->getDeclContext();

  auto sig = validateGenericSignature(gp, dc, nullptr, nullptr, invalid);
  assert(sig->getInnermostGenericParams().size()
           == typeDecl->getGenericParams()->size());
  typeDecl->setGenericSignature(sig);

  if (invalid) {
    markInvalidGenericSignature(typeDecl);
    return invalid;
  }

  revertGenericParamList(gp);

  ArchetypeBuilder builder =
    createArchetypeBuilder(typeDecl->getModuleContext());
  auto *parentSig = dc->getGenericSignatureOfContext();
  checkGenericParamList(&builder, gp, parentSig);
  finalizeGenericParamList(builder, gp, typeDecl);

  return invalid;
}

void TypeChecker::revertGenericFuncSignature(AbstractFunctionDecl *func) {
  // Revert the result type.
  if (auto fn = dyn_cast<FuncDecl>(func))
    if (!fn->getBodyResultTypeLoc().isNull())
      revertDependentTypeLoc(fn->getBodyResultTypeLoc());

  // Revert the body parameter types.
  for (auto paramList : func->getParameterLists()) {
    for (auto &param : *paramList) {
      // Clear out the type of the decl.
      if (param->hasType() && !param->isInvalid())
        param->overwriteType(Type());
      revertDependentTypeLoc(param->getTypeLoc());
    }
  }

  // Revert the generic parameter list.
  if (func->getGenericParams())
    revertGenericParamList(func->getGenericParams());

  // Clear out the types.
  if (auto fn = dyn_cast<FuncDecl>(func))
    fn->revertType();
  else
    func->overwriteType(Type());
}

/// Create a text string that describes the bindings of generic parameters that
/// are relevant to the given set of types, e.g., "[with T = Bar, U = Wibble]".
///
/// \param types The types that will be scanned for generic type parameters,
/// which will be used in the resulting type.
///
/// \param genericParams The actual generic parameters, whose names will be used
/// in the resulting text.
///
/// \param substitutions The generic parameter -> generic argument substitutions
/// that will have been applied to these types. These are used to produce the
/// "parameter = argument" bindings in the test.
static std::string gatherGenericParamBindingsText(
                     ArrayRef<Type> types,
                     ArrayRef<GenericTypeParamType *> genericParams,
                     TypeSubstitutionMap &substitutions) {
  llvm::SmallPtrSet<GenericTypeParamType *, 2> knownGenericParams;
  for (auto type : types) {
    type.findIf([&](Type type) -> bool {
      if (auto gp = type->getAs<GenericTypeParamType>()) {
        knownGenericParams.insert(gp->getCanonicalType()
                                    ->castTo<GenericTypeParamType>());
      }
      return false;
    });
  }

  if (knownGenericParams.empty())
    return "";

  SmallString<128> result;
  for (auto gp : genericParams) {
    auto canonGP = gp->getCanonicalType()->castTo<GenericTypeParamType>();
    if (!knownGenericParams.count(canonGP))
      continue;

    if (result.empty())
      result += " [with ";
    else
      result += ", ";
    result += gp->getName().str();
    result += " = ";
    result += substitutions[canonGP].getString();
  }

  result += "]";
  return result.str().str();
}

bool TypeChecker::checkGenericArguments(DeclContext *dc, SourceLoc loc,
                                        SourceLoc noteLoc,
                                        Type owner,
                                        GenericSignature *genericSig,
                                        ArrayRef<Type> genericArgs) {
  // Form the set of generic substitutions required
  TypeSubstitutionMap substitutions;

  auto genericParams = genericSig->getGenericParams();

  unsigned count = 0;

  // If the type is nested inside a generic function, skip
  // substitutions from the outer context.
  unsigned start = (genericParams.size() - genericArgs.size());

  for (auto gp : genericParams) {
    if (count >= start) {
      auto gpTy = gp->getCanonicalType()->castTo<GenericTypeParamType>();
      substitutions[gpTy] = genericArgs[count - start];
    }

    count++;
  }

  // The number of generic type arguments being bound must be equal to the
  // total number of generic parameters in the current generic type context.
  assert(count - start == genericArgs.size());

  // Check each of the requirements.
  Module *module = dc->getParentModule();
  for (const auto &req : genericSig->getRequirements()) {
    Type firstType = req.getFirstType().subst(module, substitutions,
                                              SubstFlags::IgnoreMissing);
    if (firstType.isNull()) {
      // Another requirement will fail later; just continue.
      continue;
    }

    Type secondType = req.getSecondType();
    if (secondType) {
      secondType = secondType.subst(module, substitutions,
                                    SubstFlags::IgnoreMissing);
      if (secondType.isNull()) {
        // Another requirement will fail later; just continue.
        continue;
      }
    }

    switch (req.getKind()) {
    case RequirementKind::Conformance: {
      // Protocol conformance requirements.
      auto proto = secondType->castTo<ProtocolType>();
      // FIXME: This should track whether this should result in a private
      // or non-private dependency.
      // FIXME: Do we really need "used" at this point?
      // FIXME: Poor location information. How much better can we do here?
      if (!conformsToProtocol(firstType, proto->getDecl(), dc,
                              ConformanceCheckFlags::Used, nullptr, loc)) {
        return true;
      }

      continue;
    }

    case RequirementKind::Superclass:
      // Superclass requirements.
      if (!isSubtypeOf(firstType, secondType, dc)) {
        // FIXME: Poor source-location information.
        diagnose(loc, diag::type_does_not_inherit, owner, firstType,
                 secondType);

        diagnose(noteLoc, diag::type_does_not_inherit_requirement,
                 req.getFirstType(), req.getSecondType(),
                 gatherGenericParamBindingsText(
                   {req.getFirstType(), req.getSecondType()},
                   genericParams, substitutions));
        return true;
      }
      continue;

    case RequirementKind::SameType:
      if (!firstType->isEqual(secondType)) {
        // FIXME: Better location info for both diagnostics.
        diagnose(loc, diag::types_not_equal, owner, firstType, secondType);

        diagnose(noteLoc, diag::types_not_equal_requirement,
                 req.getFirstType(), req.getSecondType(),
                 gatherGenericParamBindingsText(
                   {req.getFirstType(), req.getSecondType()},
                   genericParams, substitutions));
        return true;
      }
      continue;
      
    case RequirementKind::WitnessMarker:
      continue;
    }
  }

  return false;
}

Type TypeChecker::getWitnessType(Type type, ProtocolDecl *protocol,
                                 ProtocolConformance *conformance,
                                 Identifier name,
                                 Diag<> brokenProtocolDiag) {
  Type ty = ProtocolConformance::getTypeWitnessByName(type, conformance,
                                                      name, this);
  if (!ty && !conformance->isInvalid())
    diagnose(protocol->getLoc(), brokenProtocolDiag);

  return ty;
}
