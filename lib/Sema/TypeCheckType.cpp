//===--- TypeCheckType.cpp - Type Validation ------------------------------===//
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
// This file implements validation for Swift types, emitting semantic errors as
// appropriate and checking default initializer values.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "GenericTypeResolver.h"

#include "swift/Strings.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/ExprHandle.h"
#include "swift/AST/ForeignErrorConvention.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/TypeLoc.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/StringExtras.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Twine.h"
using namespace swift;

GenericTypeResolver::~GenericTypeResolver() { }

Type TypeChecker::getArraySliceType(SourceLoc loc, Type elementType) {
  if (!Context.getArrayDecl()) {
    diagnose(loc, diag::sugar_type_not_found, 0);
    return Type();
  }

  return ArraySliceType::get(elementType);
}

Type TypeChecker::getDictionaryType(SourceLoc loc, Type keyType, 
                                    Type valueType) {
  if (!Context.getDictionaryDecl()) {
    diagnose(loc, diag::sugar_type_not_found, 3);
    return Type();
  }

    return DictionaryType::get(keyType, valueType);
}

Type TypeChecker::getOptionalType(SourceLoc loc, Type elementType) {
  if (!Context.getOptionalDecl()) {
    diagnose(loc, diag::sugar_type_not_found, 1);
    return Type();
  }

  return OptionalType::get(elementType);
}

Type TypeChecker::getImplicitlyUnwrappedOptionalType(SourceLoc loc, Type elementType) {
  if (!Context.getImplicitlyUnwrappedOptionalDecl()) {
    diagnose(loc, diag::sugar_type_not_found, 2);
    return Type();
  }

  return ImplicitlyUnwrappedOptionalType::get(elementType);
}

static Type getStdlibType(TypeChecker &TC, Type &cached, DeclContext *dc,
                          StringRef name) {
  if (cached.isNull()) {
    Module *stdlib = TC.Context.getStdlibModule();
    LookupTypeResult lookup = TC.lookupMemberType(dc, ModuleType::get(stdlib),
                                                  TC.Context.getIdentifier(
                                                    name));
    if (lookup)
      cached = lookup.back().second;
  }
  return cached;
}

Type TypeChecker::getStringType(DeclContext *dc) {
  return ::getStdlibType(*this, StringType, dc, "String");
}
Type TypeChecker::getInt8Type(DeclContext *dc) {
  return ::getStdlibType(*this, Int8Type, dc, "Int8");
}
Type TypeChecker::getUInt8Type(DeclContext *dc) {
  return ::getStdlibType(*this, UInt8Type, dc, "UInt8");
}

/// Find the standard type of exceptions.
///
/// We call this the "exception type" to try to avoid confusion with
/// the AST's ErrorType node.
Type TypeChecker::getExceptionType(DeclContext *dc, SourceLoc loc) {
  if (NominalTypeDecl *decl = Context.getExceptionTypeDecl())
    return decl->getDeclaredType();

  // Not really sugar, but the actual diagnostic text is fine.
  diagnose(loc, diag::sugar_type_not_found, 4);
  return Type();
}

static Type getObjectiveCClassType(TypeChecker &TC,
                                   Type &cache,
                                   Identifier ModuleName,
                                   Identifier TypeName,
                                   DeclContext *dc) {
  if (cache)
    return cache;

  auto &Context = TC.Context;

  // FIXME: Does not respect visibility of the module.
  Module *module = Context.getLoadedModule(ModuleName);
  if (!module)
    return nullptr;

  NameLookupOptions lookupOptions
    = defaultMemberLookupOptions | NameLookupFlags::KnownPrivate;
  if (auto result = TC.lookupMember(dc, ModuleType::get(module), TypeName,
                                    lookupOptions)) {
    for (auto decl : result) {
      if (auto nominal = dyn_cast<NominalTypeDecl>(decl.Decl)) {
        cache = nominal->getDeclaredType();
        return cache;
      }
    }
  }

  return nullptr;
}

Type TypeChecker::getNSObjectType(DeclContext *dc) {
  return getObjectiveCClassType(*this, NSObjectType, Context.Id_ObjectiveC,
                                Context.Id_NSObject, dc);
}

Type TypeChecker::getNSErrorType(DeclContext *dc) {
  return getObjectiveCClassType(*this, NSObjectType, Context.Id_Foundation,
                                Context.Id_NSError, dc);
}

Type TypeChecker::getBridgedToObjC(const DeclContext *dc, Type type) {
  if (auto bridged = Context.getBridgedToObjC(dc, type, this))
    return *bridged;
  return nullptr;
}

Type
TypeChecker::getDynamicBridgedThroughObjCClass(DeclContext *dc,
                                               Type dynamicType,
                                               Type valueType) {
  // We can only bridge from class or Objective-C existential types.
  if (!dynamicType->isObjCExistentialType() &&
      !dynamicType->getClassOrBoundGenericClass())
    return Type();

  // If the value type cannot be bridged, we're done.
  if (!valueType->isPotentiallyBridgedValueType())
    return Type();

  return getBridgedToObjC(dc, valueType);
}

void TypeChecker::forceExternalDeclMembers(NominalTypeDecl *nominalDecl) {
  // Force any delayed members added to the nominal type declaration.
  if (nominalDecl->hasDelayedMemberDecls()) {
    nominalDecl->forceDelayed();
  }
  
  if (nominalDecl->hasDelayedMembers()) {
    this->handleExternalDecl(nominalDecl);
    nominalDecl->setHasDelayedMembers(false);
  }
}

Type TypeChecker::resolveTypeInContext(
       TypeDecl *typeDecl,
       DeclContext *fromDC,
       TypeResolutionOptions options,
       bool isSpecialized,
       GenericTypeResolver *resolver,
       UnsatisfiedDependency *unsatisfiedDependency) {
  PartialGenericTypeToArchetypeResolver defaultResolver(*this);
  if (!resolver)
    resolver = &defaultResolver;

  // If we have a callback to report dependencies, do so.
  if (unsatisfiedDependency &&
      (*unsatisfiedDependency)(requestResolveTypeDecl(typeDecl)))
    return nullptr;

  // If we found a generic parameter, map to the archetype if there is one.
  if (auto genericParam = dyn_cast<GenericTypeParamDecl>(typeDecl)) {
    return resolver->resolveGenericTypeParamType(
             genericParam->getDeclaredType()->castTo<GenericTypeParamType>());
  }

  // If we are referring to a type within its own context, and we have either
  // a generic type with no generic arguments or a non-generic type, use the
  // type within the context.
  if (auto nominal = dyn_cast<NominalTypeDecl>(typeDecl)) {
    
    forceExternalDeclMembers(nominal);
    
    if (!nominal->getGenericParams() || !isSpecialized) {
      for (DeclContext *dc = fromDC; dc; dc = dc->getParent()) {
        switch (dc->getContextKind()) {
        case DeclContextKind::Module:
        case DeclContextKind::FileUnit:
        case DeclContextKind::TopLevelCodeDecl:
        case DeclContextKind::Initializer:
          break;

        case DeclContextKind::NominalTypeDecl:
          // If this is our nominal type, return its type within its context.
          // FIXME: Just produce the type structure when TR_ResolveStructure.
          if (cast<NominalTypeDecl>(dc) == nominal)
            return resolver->resolveTypeOfContext(nominal);
          continue;
            
        case DeclContextKind::ExtensionDecl:
          // If this is an extension of our nominal type, return the type
          // within the context of its extension.
          // FIXME: Just produce the type structure when TR_ResolveStructure.
          if (cast<ExtensionDecl>(dc)->getExtendedType()->getAnyNominal()
                == nominal)
            return resolver->resolveTypeOfContext(dc);
          continue;

        case DeclContextKind::AbstractClosureExpr:
        case DeclContextKind::AbstractFunctionDecl:
        case DeclContextKind::SubscriptDecl:
          continue;
        case DeclContextKind::SerializedLocal:
          llvm_unreachable("should not be typechecking deserialized things");
        }

        break;
      }
    }
  }

  // If the type declaration itself is in a non-type context, no type
  // substitution is needed.
  DeclContext *ownerDC = typeDecl->getDeclContext();
  if (!ownerDC->isTypeContext()) {
    // FIXME: Just produce the type structure when TR_ResolveStructure.
    return typeDecl->getDeclaredType();
  }

  // Find the nearest enclosing type context around the context from which
  // we started our search.
  while (!fromDC->isTypeContext()) {
    fromDC = fromDC->getParent();
    assert(!fromDC->isModuleContext());
  }

  // If we found an associated type in an inherited protocol, the base
  // for our reference to this associated type is our own 'Self'.
  auto assocType = dyn_cast<AssociatedTypeDecl>(typeDecl);
  if (assocType) {
    // If we found an associated type from within its protocol, resolve it
    // as a dependent member relative to Self if Self is still dependent.
    if (fromDC->isProtocolOrProtocolExtensionContext()) {
      auto selfTy = fromDC->getProtocolSelf()->getDeclaredType()
                      ->castTo<GenericTypeParamType>();
      auto baseTy = resolver->resolveGenericTypeParamType(selfTy);

      if (baseTy->isTypeParameter()) {
        return resolver->resolveSelfAssociatedType(baseTy, fromDC, assocType);
      }
    }

    if (typeDecl->getDeclContext() != fromDC) {
      if (fromDC->isProtocolOrProtocolExtensionContext()) {
        return substMemberTypeWithBase(fromDC->getParentModule(),
                                       typeDecl,
                                       fromDC->getProtocolSelf()
                                         ->getArchetype(),
                                       /*isTypeReference=*/true);
      }
    }
  }

  // Walk up through the type scopes to find the context where the type
  // declaration was found. When we find it, substitute the appropriate base
  // type.
  auto ownerNominal = ownerDC->isNominalTypeOrNominalTypeExtensionContext();
  assert(ownerNominal && "Owner must be a nominal type");
  for (auto parentDC = fromDC; !parentDC->isModuleContext();
       parentDC = parentDC->getParent()) {
    // Skip non-type contexts.
    if (!parentDC->isTypeContext())
      continue;

    // Search the type of this context and its supertypes.
    Type superClassOfFromType;
    int traversedClassHierarchyDepth = 0;
    for (auto fromType = resolver->resolveTypeOfContext(parentDC);
         fromType;
         fromType = superClassOfFromType) {
      // If the nominal type declaration of the context type we're looking at
      // matches the owner's nominal type declaration, this is how we found
      // the member type declaration. Substitute the type we're coming from as
      // the base of the member type to produce the projected type result.
      if (fromType->getAnyNominal() == ownerNominal) {
        // If we are referring into a protocol or extension thereof,
        // the base type is the 'Self'.
        if (ownerDC->isProtocolOrProtocolExtensionContext()) {
          auto selfTy = ownerDC->getProtocolSelf()->getDeclaredType()
                          ->castTo<GenericTypeParamType>();
          fromType = resolver->resolveGenericTypeParamType(selfTy);
        }

        // Perform the substitution.
        return substMemberTypeWithBase(parentDC->getParentModule(), typeDecl,
                                       fromType, /*isTypeReference=*/true);
      }

      ProtocolConformance *conformance = nullptr;
      if (assocType &&
          !options.contains(TR_InheritanceClause) &&
          conformsToProtocol(fromType,
                             cast<ProtocolDecl>(assocType->getDeclContext()),
                             parentDC, ConformanceCheckFlags::Used,
                             &conformance) &&
          conformance) {
        return conformance->getTypeWitness(assocType, this).getReplacement();
      }
      superClassOfFromType = getSuperClassOf(fromType);
      /// FIXME: Avoid the possibility of an infinite loop by fixing the root
      ///        cause instead (incomplete circularity detection).
      assert(fromType.getPointer() != superClassOfFromType.getPointer() &&
             "Infinite loop due to circular class inheritance.");
      assert(traversedClassHierarchyDepth++ <= 16384 &&
             "Infinite loop due to circular class inheritance?");
      (void) traversedClassHierarchyDepth;
    }
  }

  // Substitute in the appropriate type for 'Self'.
  // FIXME: We shouldn't have to guess here; the caller should tell us.
  Type fromType;
  if (fromDC->isProtocolOrProtocolExtensionContext())
    fromType = fromDC->getProtocolSelf()->getArchetype();
  else
    fromType = resolver->resolveTypeOfContext(fromDC);

  // Perform the substitution.
  return substMemberTypeWithBase(fromDC->getParentModule(), typeDecl,
                                 fromType, /*isTypeReference=*/true);
}

/// Apply generic arguments to the given type.
Type TypeChecker::applyGenericArguments(Type type,
                                        SourceLoc loc,
                                        DeclContext *dc,
                                        MutableArrayRef<TypeLoc> genericArgs,
                                        bool isGenericSignature,
                                        GenericTypeResolver *resolver) {
  // Make sure we always have a resolver to use.
  PartialGenericTypeToArchetypeResolver defaultResolver(*this);
  if (!resolver)
    resolver = &defaultResolver;

  auto unbound = type->getAs<UnboundGenericType>();
  if (!unbound) {
    // FIXME: Highlight generic arguments and introduce a Fix-It to remove
    // them.
    if (!type->is<ErrorType>()) {
      diagnose(loc, diag::not_a_generic_type, type);
    }
    
    // Just return the type; this provides better recovery anyway.
    return type;
  }

  // Make sure we have the right number of generic arguments.
  // FIXME: If we have fewer arguments than we need, that might be okay, if
  // we're allowed to deduce the remaining arguments from context.
  auto genericParams = unbound->getDecl()->getGenericParams();
  if (genericParams->size() != genericArgs.size()) {
    // FIXME: Highlight <...>.
    diagnose(loc, diag::type_parameter_count_mismatch,
             unbound->getDecl()->getName(),
             genericParams->size(), genericArgs.size(),
             genericArgs.size() < genericParams->size());
    diagnose(unbound->getDecl(), diag::generic_type_declared_here,
             unbound->getDecl()->getName());
    return nullptr;
  }

  TypeResolutionOptions options;
  if (isGenericSignature)
    options |= TR_GenericSignature;

  // Validate the generic arguments and capture just the types.
  SmallVector<Type, 4> genericArgTypes;
  for (auto &genericArg : genericArgs) {
    // Validate the generic argument.
    if (validateType(genericArg, dc, options, resolver))
      return nullptr;

    genericArgTypes.push_back(genericArg.getType());
  }

  // Form the bound generic type
  BoundGenericType *BGT = BoundGenericType::get(unbound->getDecl(),
                                                unbound->getParent(),
                                                genericArgTypes);
  // Check protocol conformance.
  if (!BGT->hasTypeParameter()) {
    SourceLoc noteLoc = unbound->getDecl()->getLoc();
    if (noteLoc.isInvalid())
      noteLoc = loc;

    // FIXME: Record that we're checking substitutions, so we can't end up
    // with infinite recursion.

    // Collect the complete set of generic arguments.
    SmallVector<Type, 4> scratch;
    ArrayRef<Type> allGenericArgs = BGT->getAllGenericArgs(scratch);

    // Check the generic arguments against the generic signature.
    auto genericSig = unbound->getDecl()->getGenericSignature();
    if (unbound->getDecl()->IsValidatingGenericSignature()) {
      diagnose(loc, diag::recursive_requirement_reference);
      return BGT;
    }
    assert(genericSig != nullptr);
    if (checkGenericArguments(dc, loc, noteLoc, unbound, genericSig,
                              allGenericArgs))
      return nullptr;
  }

  return BGT;
}

static Type applyGenericTypeReprArgs(TypeChecker &TC, Type type, SourceLoc loc,
                                     DeclContext *dc,
                                     ArrayRef<TypeRepr *> genericArgs,
                                     bool isGenericSignature,
                                     GenericTypeResolver *resolver) {
  SmallVector<TypeLoc, 8> args;
  for (auto tyR : genericArgs)
    args.push_back(tyR);
  Type ty = TC.applyGenericArguments(type, loc, dc, args,
                                     isGenericSignature, resolver);
  if (!ty)
    return ErrorType::get(TC.Context);
  return ty;
}


/// \brief Diagnose a use of an unbound generic type.
static void diagnoseUnboundGenericType(TypeChecker &tc, Type ty,SourceLoc loc) {
  tc.diagnose(loc, diag::generic_type_requires_arguments, ty);
  auto unbound = ty->castTo<UnboundGenericType>();
  tc.diagnose(unbound->getDecl()->getLoc(), diag::generic_type_declared_here,
              unbound->getDecl()->getName());
}

/// \brief Returns a valid type or ErrorType in case of an error.
static Type resolveTypeDecl(TypeChecker &TC, TypeDecl *typeDecl, SourceLoc loc,
                            DeclContext *dc,
                            ArrayRef<TypeRepr *> genericArgs,
                            TypeResolutionOptions options,
                            GenericTypeResolver *resolver,
                            UnsatisfiedDependency *unsatisfiedDependency) {
  assert(dc && "No declaration context for type resolution?");

  // If we have a callback to report dependencies, do so.
  if (unsatisfiedDependency) {
    if ((*unsatisfiedDependency)(requestResolveTypeDecl(typeDecl)))
      return nullptr;
  } else {
    // Validate the declaration.
    TC.validateDecl(typeDecl);
  }

  // Resolve the type declaration to a specific type. How this occurs
  // depends on the current context and where the type was found.
  Type type = TC.resolveTypeInContext(typeDecl, dc, options,
                                      !genericArgs.empty(), resolver);

  // FIXME: Defensive check that shouldn't be needed, but prevents a
  // huge number of crashes on ill-formed code.
  if (!type)
    return ErrorType::get(TC.Context);

  if (type->is<UnboundGenericType>() && genericArgs.empty() &&
      !options.contains(TR_AllowUnboundGenerics) &&
      !options.contains(TR_ResolveStructure)) {
    diagnoseUnboundGenericType(TC, type, loc);
    return ErrorType::get(TC.Context);
  }

  // If we found a generic parameter, try to resolve it.
  // FIXME: Jump through hoops to maintain syntactic sugar. We shouldn't care
  // about this, because we shouldn't have to do this at all.
  if (auto genericParam = type->getAs<GenericTypeParamType>()) {
    auto resolvedGP = resolver->resolveGenericTypeParamType(genericParam);
    if (auto substituted = dyn_cast<SubstitutedType>(type.getPointer())) {
      type = SubstitutedType::get(substituted->getOriginal(), resolvedGP,
                                  TC.Context);
    } else {
      type = resolvedGP;
    }
  }

  if (!genericArgs.empty() && !options.contains(TR_ResolveStructure)) {
    // Apply the generic arguments to the type.
    type = applyGenericTypeReprArgs(TC, type, loc, dc, genericArgs,
                                    options.contains(TR_GenericSignature),
                                    resolver);
  }

  assert(type);
  return type;
}

/// Retrieve the nearest enclosing nominal type context.
static NominalTypeDecl *getEnclosingNominalContext(DeclContext *dc) {
  while (dc->isLocalContext())
    dc = dc->getParent();

  if (auto nominal = dc->isNominalTypeOrNominalTypeExtensionContext())
    return nominal;

  return nullptr;
}

/// Diagnose a reference to an unknown type.
///
/// This routine diagnoses a reference to an unknown type, and
/// attempts to fix the reference via various means.
///
/// \param tc The type checker through which we should emit the diagnostic.
/// \param dc The context in which name lookup occurred.
///
/// \returns either the corrected type, if possible, or an error type to
/// that correction failed.
static Type diagnoseUnknownType(TypeChecker &tc, DeclContext *dc,
                                Type parentType,
                                SourceRange parentRange,
                                ComponentIdentTypeRepr *comp,
                                TypeResolutionOptions options,
                                GenericTypeResolver *resolver,
                                UnsatisfiedDependency *unsatisfiedDependency) {
  // Unqualified lookup case.
  if (parentType.isNull()) {
    // Attempt to refer to 'Self' within a non-protocol nominal
    // type. Fix this by replacing 'Self' with the nominal type name.
    NominalTypeDecl *nominal = nullptr;
    if (comp->getIdentifier() == tc.Context.Id_Self &&
        !isa<GenericIdentTypeRepr>(comp) &&
        (nominal = getEnclosingNominalContext(dc))) {
      // Retrieve the nominal type and resolve it within this context.
      assert(!isa<ProtocolDecl>(nominal) && "Cannot be a protocol");
      auto type = resolveTypeDecl(tc, nominal, comp->getIdLoc(), dc, { },
                                  options, resolver, unsatisfiedDependency);
      if (type->is<ErrorType>())
        return type;

      // Produce a Fix-It replacing 'Self' with the nominal type name.
      tc.diagnose(comp->getIdLoc(), diag::self_in_nominal, nominal->getName())
        .fixItReplace(comp->getIdLoc(), nominal->getName().str());
      comp->overwriteIdentifier(nominal->getName());
      comp->setValue(nominal);
      return type;
    }
    
    // Fallback.
    SourceLoc L = comp->getIdLoc();
    SourceRange R = SourceRange(comp->getIdLoc());

    // Check if the unknown type is in the type remappings.
    auto &Remapped = tc.Context.RemappedTypes;
    auto TypeName = comp->getIdentifier().str();
    auto I = Remapped.find(TypeName);
    if (I != Remapped.end()) {
      auto RemappedTy = I->second->getString();
      tc.diagnose(L, diag::use_undeclared_type_did_you_mean,
                  comp->getIdentifier(), RemappedTy)
        .highlight(R)
        .fixItReplace(R, RemappedTy);

      // Replace the computed type with the suggested type.
      comp->overwriteIdentifier(tc.Context.getIdentifier(RemappedTy));

      // HACK: 'NSUInteger' suggests both 'UInt' and 'Int'.
      if (TypeName == "NSUInteger") {
        tc.diagnose(L, diag::note_remapped_type, "UInt")
          .fixItReplace(R, "UInt");
      }

      return I->second;
    }

    tc.diagnose(L, diag::use_undeclared_type,
                comp->getIdentifier())
      .highlight(R);

    return ErrorType::get(tc.Context);
  }

  // Qualified lookup case.
  // FIXME: Typo correction!

  // Lookup into a type.
  if (auto moduleType = parentType->getAs<ModuleType>()) {
    tc.diagnose(comp->getIdLoc(), diag::no_module_type,
                comp->getIdentifier(), moduleType->getModule()->getName());
  } else {
    tc.diagnose(comp->getIdLoc(), diag::invalid_member_type,
                comp->getIdentifier(), parentType)
      .highlight(parentRange);
  }

  return ErrorType::get(tc.Context);
}

/// Resolve the given identifier type representation as an unqualified type,
/// returning the type it references.
///
/// \returns Either the resolved type or a null type, the latter of
/// which indicates that some dependencies were unsatisfied.
static Type
resolveTopLevelIdentTypeComponent(TypeChecker &TC, DeclContext *DC,
                                  ComponentIdentTypeRepr *comp,
                                  TypeResolutionOptions options,
                                  bool diagnoseErrors,
                                  GenericTypeResolver *resolver,
                                  UnsatisfiedDependency *unsatisfiedDependency){
  // Short-circuiting.
  if (comp->isInvalid()) return ErrorType::get(TC.Context);

  // If the component has already been bound to a declaration, handle
  // that now.
  if (ValueDecl *VD = comp->getBoundDecl()) {
    // Diagnose non-type declarations.
    auto typeDecl = dyn_cast<TypeDecl>(VD);
    if (!typeDecl) {
      if (diagnoseErrors) {
        TC.diagnose(comp->getIdLoc(), diag::use_non_type_value, VD->getName());
        TC.diagnose(VD, diag::use_non_type_value_prev, VD->getName());
      }

      comp->setInvalid();
      return ErrorType::get(TC.Context);
    }

    // Retrieve the generic arguments, if there are any.
    ArrayRef<TypeRepr *> genericArgs;
    if (auto genComp = dyn_cast<GenericIdentTypeRepr>(comp))
      genericArgs = genComp->getGenericArgs();

    // Resolve the type declaration within this context.
    return resolveTypeDecl(TC, typeDecl, comp->getIdLoc(), DC,
                           genericArgs, options, resolver,
                           unsatisfiedDependency);
  }

  // Resolve the first component, which is the only one that requires
  // unqualified name lookup.
  DeclContext *lookupDC = DC;

  // Dynamic 'Self' in the result type of a function body.
  if (options.contains(TR_DynamicSelfResult) &&
      comp->getIdentifier() == TC.Context.Id_Self) {
    auto func = cast<FuncDecl>(DC);
    assert(func->hasDynamicSelf() && "Not marked as having dynamic Self?");

    return func->getDynamicSelf();
  }

  // For lookups within the generic signature, look at the generic
  // parameters (only), then move up to the enclosing context.
  if (options.contains(TR_GenericSignature)) {
    GenericParamList *genericParams;
    if (auto *nominal = dyn_cast<NominalTypeDecl>(DC)) {
      genericParams = nominal->getGenericParams();
    } else if (auto *ext = dyn_cast<ExtensionDecl>(DC)) {
      genericParams = ext->getGenericParams();
    } else {
      genericParams = cast<AbstractFunctionDecl>(DC)->getGenericParams();
    }

    if (genericParams) {
      auto matchingParam =
          std::find_if(genericParams->begin(), genericParams->end(),
                       [comp](const GenericTypeParamDecl *param) {
        return param->getFullName().matchesRef(comp->getIdentifier());
      });

      if (matchingParam != genericParams->end()) {
        comp->setValue(*matchingParam);
        return resolveTopLevelIdentTypeComponent(TC, DC, comp, options,
                                                 diagnoseErrors, resolver,
                                                 unsatisfiedDependency);
      }
    }

    // If the lookup occurs from within a trailing 'where' clause of
    // a constrained extension, also look for associated types.
    if (genericParams && genericParams->hasTrailingWhereClause() &&
        isa<ExtensionDecl>(DC) && comp->getIdLoc().isValid() &&
        TC.Context.SourceMgr.rangeContainsTokenLoc(
          genericParams->getTrailingWhereClauseSourceRange(),
          comp->getIdLoc())) {
      // We need to be able to perform qualified lookup into the given
      // declaration context.
      if (unsatisfiedDependency &&
          (*unsatisfiedDependency)(
            requestQualifiedLookupInDeclContext({ DC, comp->getIdentifier(),
                                                  comp->getIdLoc() })))
        return nullptr;

      auto nominal = DC->isNominalTypeOrNominalTypeExtensionContext();
      SmallVector<ValueDecl *, 4> decls;
      if (DC->lookupQualified(nominal->getDeclaredInterfaceType(),
                              comp->getIdentifier(),
                              NL_QualifiedDefault|NL_ProtocolMembers,
                              &TC,
                              decls)) {
        for (const auto decl : decls) {
          // FIXME: Better ambiguity handling.
          if (auto assocType = dyn_cast<AssociatedTypeDecl>(decl)) {
            comp->setValue(assocType);
            return resolveTopLevelIdentTypeComponent(TC, DC, comp, options,
                                                     diagnoseErrors, resolver,
                                                     unsatisfiedDependency);
          }
        }
      }
    }

    if (!DC->isCascadingContextForLookup(/*excludeFunctions*/false))
      options |= TR_KnownNonCascadingDependency;

    // The remaining lookups will be in the parent context.
    lookupDC = DC->getParent();
  }

  // We need to be able to perform unqualified lookup into the given
  // declaration context.
  if (unsatisfiedDependency &&
      (*unsatisfiedDependency)(
        requestUnqualifiedLookupInDeclContext({ lookupDC, 
                                                comp->getIdentifier(),
                                                comp->getIdLoc() })))
    return nullptr;

  NameLookupOptions lookupOptions = defaultUnqualifiedLookupOptions;
  lookupOptions |= NameLookupFlags::OnlyTypes;
  if (options.contains(TR_KnownNonCascadingDependency))
    lookupOptions |= NameLookupFlags::KnownPrivate;
  // FIXME: Eliminate this once we can handle finding protocol members
  // in resolveTypeInContext.
  lookupOptions -= NameLookupFlags::ProtocolMembers;
  LookupResult globals = TC.lookupUnqualified(lookupDC, comp->getIdentifier(),
                                              comp->getIdLoc(), lookupOptions);

  // Process the names we found.
  Type current;
  TypeDecl *currentDecl = nullptr;
  bool isAmbiguous = false;
  for (const auto &result : globals) {
    // Ignore non-type declarations.
    auto typeDecl = dyn_cast<TypeDecl>(result.Decl);
    if (!typeDecl)
      continue;
    
    // If necessary, add delayed members to the declaration.
    if (auto nomDecl = dyn_cast<NominalTypeDecl>(typeDecl)) {
      TC.forceExternalDeclMembers(nomDecl);
    }

    ArrayRef<TypeRepr *> genericArgs;
    if (auto genComp = dyn_cast<GenericIdentTypeRepr>(comp))
      genericArgs = genComp->getGenericArgs();
    Type type = resolveTypeDecl(TC, typeDecl, comp->getIdLoc(),
                                DC, genericArgs, options, resolver,
                                unsatisfiedDependency);
    if (!type || type->is<ErrorType>())
      return type;

    // If this is the first result we found, record it.
    if (current.isNull()) {
      current = type;
      currentDecl = typeDecl;
      continue;
    }

    // Otherwise, check for an ambiguity.
    if (!current->isEqual(type)) {
      isAmbiguous = true;
      break;
    }

    // We have a found multiple type aliases that refer to the same thing.
    // Ignore the duplicate.
  }

  // Complain about any ambiguities we detected.
  // FIXME: We could recover by looking at later components.
  if (isAmbiguous) {
    if (diagnoseErrors) {
      TC.diagnose(comp->getIdLoc(), diag::ambiguous_type_base,
                  comp->getIdentifier())
        .highlight(comp->getIdLoc());
      for (auto result : globals) {
        TC.diagnose(result.Decl, diag::found_candidate);
      }
    }

    comp->setInvalid();
    return ErrorType::get(TC.Context);
  }

  // If we found nothing, complain and give ourselves a chance to recover.
  if (current.isNull()) {
    // If we're not allowed to complain or we couldn't fix the
    // source, bail out.
    if (!diagnoseErrors)
      return ErrorType::get(TC.Context);

    return diagnoseUnknownType(TC, DC, nullptr, SourceRange(), comp, options,
                               resolver, unsatisfiedDependency);
  }

  comp->setValue(currentDecl);
  return current;
}

/// Resolve the given identifier type representation as a qualified
/// lookup within the given parent type, returning the type it
/// references.
static Type resolveNestedIdentTypeComponent(
              TypeChecker &TC, DeclContext *DC,
              Type parentTy,
              SourceRange parentRange,
              ComponentIdentTypeRepr *comp,
              TypeResolutionOptions options,
              bool diagnoseErrors,
              GenericTypeResolver *resolver,
              UnsatisfiedDependency *unsatisfiedDependency) {
  // Short-circuiting.
  if (comp->isInvalid()) return ErrorType::get(TC.Context);

  // If a declaration has already been bound, use it.
  if (ValueDecl *decl = comp->getBoundDecl()) {
    // Make sure we have a type declaration.
    auto typeDecl = dyn_cast<TypeDecl>(decl);
    if (!typeDecl) {
      if (diagnoseErrors) {
        TC.diagnose(comp->getIdLoc(), diag::use_non_type_value,
                    decl->getName());
        TC.diagnose(decl, diag::use_non_type_value_prev,
                    decl->getName());
      }

      comp->setInvalid();
      return ErrorType::get(TC.Context);
    }

    Type memberType;

    if (parentTy->isTypeParameter()) {
      // If the parent is a type parameter, the member is a dependent member.

      // FIXME: We either have an associated type here or a member of
      // some member of the superclass bound (or its superclasses),
      // which should allow us to skip much of the work in
      // resolveDependentMemberType.

      // Try to resolve the dependent member type to a specific associated
      // type.
      memberType = resolver->resolveDependentMemberType(parentTy, DC,
                                                        parentRange, comp);
      assert(memberType && "Received null dependent member type");
    } else if (isa<AssociatedTypeDecl>(typeDecl) &&
               !parentTy->is<ArchetypeType>() &&
               !parentTy->isExistentialType()) {
      auto assocType = cast<AssociatedTypeDecl>(typeDecl);

      // Find the conformance and dig out the type witness.
      ConformanceCheckOptions conformanceOptions;
      if (options.contains(TR_InExpression))
        conformanceOptions |= ConformanceCheckFlags::InExpression;

      auto *protocol = cast<ProtocolDecl>(assocType->getDeclContext());
      ProtocolConformance *conformance = nullptr;
      if (!TC.conformsToProtocol(parentTy, protocol, DC, conformanceOptions,
                                 &conformance) ||
          !conformance) {
        return nullptr;
      }

      // FIXME: Establish that we need a type witness.
      return conformance->getTypeWitness(assocType, &TC).getReplacement();
    } else {
      // Otherwise, simply substitute the parent type into the member.
      memberType = TC.substMemberTypeWithBase(DC->getParentModule(), typeDecl,
                                              parentTy,
                                              /*isTypeReference=*/true);
    }

    // Propagate failure.
    if (!memberType || memberType->is<ErrorType>()) return memberType;

    // If there are generic arguments, apply them now.
    if (auto genComp = dyn_cast<GenericIdentTypeRepr>(comp)) {
      memberType = applyGenericTypeReprArgs(
                     TC, memberType, comp->getIdLoc(), DC,
                     genComp->getGenericArgs(),
                     options.contains(TR_GenericSignature),
                     resolver);

      // Propagate failure.
      if (!memberType || memberType->is<ErrorType>()) return memberType;
    }

    // We're done.
    return memberType;
  }

  // If the parent is a dependent type, the member is a dependent member.
  if (parentTy->isTypeParameter()) {
    // Try to resolve the dependent member type to a specific associated
    // type.
    Type memberType = resolver->resolveDependentMemberType(parentTy, DC,
                                                           parentRange,
                                                           comp);
    assert(memberType && "Received null dependent member type");

    if (isa<GenericIdentTypeRepr>(comp) && !memberType->is<ErrorType>()) {
      // FIXME: Highlight generic arguments and introduce a Fix-It to
      // remove them.
      if (diagnoseErrors)
        TC.diagnose(comp->getIdLoc(), diag::not_a_generic_type, memberType);

      // Drop the arguments.
    }

    // If we know what type declaration we're referencing, store it.
    if (auto typeDecl = memberType->getDirectlyReferencedTypeDecl()) {
      comp->setValue(typeDecl);
    }

    return memberType;
  }

  // Look for member types with the given name.
  bool isKnownNonCascading = options.contains(TR_KnownNonCascadingDependency);
  if (!isKnownNonCascading && options.contains(TR_InExpression)) {
    // Expressions cannot affect a function's signature.
    isKnownNonCascading = isa<AbstractFunctionDecl>(DC);
  }

  // We need to be able to perform qualified lookup into the given type.
  if (unsatisfiedDependency) {
    DeclContext *dc;
    if (auto parentNominal = parentTy->getAnyNominal())
      dc = parentNominal;
    else if (auto parentModule = parentTy->getAs<ModuleType>())
      dc = parentModule->getModule();
    else
      dc = nullptr;

    if (dc &&
        (*unsatisfiedDependency)(
          requestQualifiedLookupInDeclContext({ dc, comp->getIdentifier(),
                                                comp->getIdLoc() })))
      return nullptr;
  }

  NameLookupOptions lookupOptions = defaultMemberLookupOptions;
  if (isKnownNonCascading)
    lookupOptions |= NameLookupFlags::KnownPrivate;
  if (options.contains(TR_ExtensionBinding))
    lookupOptions -= NameLookupFlags::ProtocolMembers;
  auto memberTypes = TC.lookupMemberType(DC, parentTy, comp->getIdentifier(),
                                         lookupOptions);

  // Name lookup was ambiguous. Complain.
  // FIXME: Could try to apply generic arguments first, and see whether
  // that resolves things. But do we really want that to succeed?
  if (memberTypes.size() > 1) {
    if (diagnoseErrors)
      TC.diagnoseAmbiguousMemberType(parentTy, parentRange,
                                     comp->getIdentifier(), comp->getIdLoc(),
                                     memberTypes);
    return ErrorType::get(TC.Context);
  }

  // If we didn't find anything, complain.
  bool recovered = false;
  Type memberType;
  TypeDecl *member = nullptr;
  if (!memberTypes) {
    // If we're not allowed to complain or we couldn't fix the
    // source, bail out.
    if (!diagnoseErrors) {
      return ErrorType::get(TC.Context);
    }

    Type ty = diagnoseUnknownType(TC, DC, parentTy, parentRange, comp, options,
                                  resolver, unsatisfiedDependency);
    if (!ty || ty->is<ErrorType>()) {
      return ErrorType::get(TC.Context);
    }

    recovered = true;
    memberType = ty;
    member = cast_or_null<TypeDecl>(comp->getBoundDecl());
  } else {
    memberType = memberTypes.back().second;
    member = memberTypes.back().first;
  }

  if (parentTy->isExistentialType()) {
    if (diagnoseErrors)
      TC.diagnose(comp->getIdLoc(), diag::assoc_type_outside_of_protocol,
                  comp->getIdentifier());

    return ErrorType::get(TC.Context);
  }

  // If there are generic arguments, apply them now.
  if (auto genComp = dyn_cast<GenericIdentTypeRepr>(comp))
    memberType = applyGenericTypeReprArgs(
      TC, memberType, comp->getIdLoc(), DC, genComp->getGenericArgs(),
      options.contains(TR_GenericSignature), resolver);

  if (member)
    comp->setValue(member);
  return memberType;
}

static Type resolveIdentTypeComponent(
              TypeChecker &TC, DeclContext *DC,
              ArrayRef<ComponentIdentTypeRepr *> components,
              TypeResolutionOptions options,
              bool diagnoseErrors,
              GenericTypeResolver *resolver,
              UnsatisfiedDependency *unsatisfiedDependency) {
  auto comp = components.back();

  // The first component uses unqualified lookup.
  auto parentComps = components.slice(0, components.size()-1);
  if (parentComps.empty()) {
    return resolveTopLevelIdentTypeComponent(TC, DC, comp, options,
                                             diagnoseErrors, resolver,
                                             unsatisfiedDependency);
  }

  // All remaining components use qualified lookup.

  // Resolve the parent type.
  Type parentTy = resolveIdentTypeComponent(TC, DC, parentComps, options,
                                            diagnoseErrors, resolver,
                                            unsatisfiedDependency);
  if (!parentTy || parentTy->is<ErrorType>()) return parentTy;

  // Resolve the nested type.
  SourceRange parentRange(parentComps.front()->getIdLoc(),
                          parentComps.back()->getSourceRange().End);
  return resolveNestedIdentTypeComponent(TC, DC, parentTy,
                                         parentRange, comp,
                                         options, diagnoseErrors,
                                         resolver,
                                         unsatisfiedDependency);
}

// FIXME: Merge this with diagAvailability in MiscDiagnostics.cpp.
static bool checkTypeDeclAvailability(Decl *TypeDecl, IdentTypeRepr *IdType,
                                      SourceLoc Loc, DeclContext *DC,
                                      TypeChecker &TC,
                                      bool AllowPotentiallyUnavailableProtocol) {

  if (auto CI = dyn_cast<ComponentIdentTypeRepr>(IdType)) {
    if (auto Attr = AvailableAttr::isUnavailable(TypeDecl)) {
      switch (Attr->getUnconditionalAvailability()) {
      case UnconditionalAvailabilityKind::None:
      case UnconditionalAvailabilityKind::Deprecated:
        break;

      case UnconditionalAvailabilityKind::Unavailable:
        if (!Attr->Rename.empty()) {
          TC.diagnose(Loc, diag::availability_decl_unavailable_rename,
                      CI->getIdentifier(), Attr->Rename)
            .fixItReplace(Loc, Attr->Rename);
        } else if (Attr->Message.empty()) {
          TC.diagnose(Loc, diag::availability_decl_unavailable,
                      CI->getIdentifier())
            .highlight(Loc);
        } else {
          EncodedDiagnosticMessage EncodedMessage(Attr->Message);
          TC.diagnose(Loc, diag::availability_decl_unavailable_msg,
                      CI->getIdentifier(), EncodedMessage.Message)
            .highlight(Loc);
        }
        break;

      case UnconditionalAvailabilityKind::UnavailableInSwift:
        if (Attr->Message.empty()) {
          TC.diagnose(Loc, diag::availability_decl_unavailable_in_swift,
                      CI->getIdentifier())
            .highlight(Loc);
        } else {
          EncodedDiagnosticMessage EncodedMessage(Attr->Message);
          TC.diagnose(Loc, diag::availability_decl_unavailable_in_swift_msg,
                      CI->getIdentifier(), EncodedMessage.Message)
            .highlight(Loc);
        }
        break;
      }

      auto DLoc = TypeDecl->getLoc();
      if (DLoc.isValid())
        TC.diagnose(DLoc, diag::availability_marked_unavailable,
                    CI->getIdentifier()).highlight(Attr->getRange());
      return true;
    }

    if (auto *Attr = TypeChecker::getDeprecated(TypeDecl)) {
      TC.diagnoseDeprecated(CI->getSourceRange(), DC, Attr,
                            CI->getIdentifier());
    }

    if (AllowPotentiallyUnavailableProtocol && isa<ProtocolDecl>(TypeDecl))
      return false;

    // Check for potential unavailability because of the minimum
    // deployment version.
    // We should probably unify this checking for deployment-version API
    // unavailability with checking for explicitly annotated unavailability.
    Optional<UnavailabilityReason> Unavail =
        TC.checkDeclarationAvailability(TypeDecl, Loc, DC);
    if (Unavail.hasValue()) {
      TC.diagnosePotentialUnavailability(TypeDecl, CI->getIdentifier(),
                                         CI->getSourceRange(), DC,
                                         Unavail.getValue());
    }
  }

  return false;
}


static bool diagnoseAvailability(Type ty, IdentTypeRepr *IdType, SourceLoc Loc,
                                 DeclContext *DC, TypeChecker &TC,
                                 bool AllowPotentiallyUnavailableProtocol) {
  if (auto *NAT = dyn_cast<NameAliasType>(ty.getPointer())) {
    if (checkTypeDeclAvailability(NAT->getDecl(), IdType, Loc, DC, TC,
                                  AllowPotentiallyUnavailableProtocol))
      return true;
  }

  // Look through substituted types to diagnose when the original
  // type is marked unavailable.
  if (auto *ST = dyn_cast<SubstitutedType>(ty.getPointer())) {
    if (diagnoseAvailability(ST->getOriginal(), IdType, Loc, DC, TC,
                             AllowPotentiallyUnavailableProtocol)) {
      return true;
    }
  }

  CanType canTy = ty.getCanonicalTypeOrNull();
  if (canTy.isNull())
    return false;
  if (auto NTD = canTy.getAnyNominal())
    return checkTypeDeclAvailability(NTD, IdType, Loc, DC, TC,
                                     AllowPotentiallyUnavailableProtocol);

  return false;
}

/// \brief Returns a valid type or ErrorType in case of an error.
Type TypeChecker::resolveIdentifierType(
       DeclContext *DC,
       IdentTypeRepr *IdType,
       TypeResolutionOptions options,
       bool diagnoseErrors,
       GenericTypeResolver *resolver,
       UnsatisfiedDependency *unsatisfiedDependency) {
  assert(resolver && "Missing generic type resolver");

  auto ComponentRange = IdType->getComponentRange();
  auto Components = llvm::makeArrayRef(ComponentRange.begin(),
                                       ComponentRange.end());
  Type result = resolveIdentTypeComponent(*this, DC, Components, options, 
                                          diagnoseErrors, resolver,
                                          unsatisfiedDependency);
  if (!result) return nullptr;

  if (auto moduleTy = result->getAs<ModuleType>()) {
    if (diagnoseErrors) {
      auto moduleName = moduleTy->getModule()->getName();
      diagnose(Components.back()->getIdLoc(),
               diag::use_undeclared_type, moduleName);
      diagnose(Components.back()->getIdLoc(),
               diag::note_module_as_type, moduleName);
    }
    Components.back()->setInvalid();
    return ErrorType::get(Context);
  }

  // We allow a type to conform to a protocol that is less available than
  // the type itself. This enables a type to retroactively model or directly
  // conform to a protocol only available on newer OSes and yet still be used on
  // older OSes.
  // To support this, inside inheritance clauses we allow references to
  // protocols that are unavailable in the current type refinement context.
  bool AllowPotentiallyUnavailableProtocol =
      options.contains(TR_InheritanceClause);

  // Check the availability of the type. Skip checking for SIL.
  if (!(options & TR_SILType) && !(options & TR_AllowUnavailable) &&
      diagnoseAvailability(result, IdType,
                           Components.back()->getIdLoc(), DC, *this,
                           AllowPotentiallyUnavailableProtocol)) {
    Components.back()->setInvalid();
    return ErrorType::get(Context);
  }
  
  return result;
}

bool TypeChecker::validateType(TypeLoc &Loc, DeclContext *DC,
                               TypeResolutionOptions options,
                               GenericTypeResolver *resolver,
                               UnsatisfiedDependency *unsatisfiedDependency) {
  // FIXME: Verify that these aren't circular and infinite size.
  
  // If we've already validated this type, don't do so again.
  if (Loc.wasValidated())
    return Loc.isError();

  if (Loc.getType().isNull()) {
    auto type = resolveType(Loc.getTypeRepr(), DC, options, resolver,
                            unsatisfiedDependency);
    if (!type) {
      // If a dependency went unsatisfied, just return false.
      if (unsatisfiedDependency) return false;

      type = ErrorType::get(Context);
    }
    Loc.setType(type, true);
    return Loc.isError();
  }

  Loc.setType(Loc.getType(), true);
  return Loc.isError();
}

namespace {
  const auto DefaultParameterConvention = ParameterConvention::Direct_Unowned;
  const auto DefaultResultConvention = ResultConvention::Unowned;

  class TypeResolver {
    TypeChecker &TC;
    ASTContext &Context;
    DeclContext *DC;
    GenericTypeResolver *Resolver;
    swift::UnsatisfiedDependency *UnsatisfiedDependency;
  public:
    TypeResolver(TypeChecker &tc, DeclContext *DC,
                 GenericTypeResolver *resolver,
                 swift::UnsatisfiedDependency *unsatisfiedDependency)
      : TC(tc), Context(tc.Context), DC(DC), Resolver(resolver),
        UnsatisfiedDependency(unsatisfiedDependency)
    {
      assert(resolver);
    }

    Type resolveType(TypeRepr *repr, TypeResolutionOptions options);

  private:

    Type resolveAttributedType(AttributedTypeRepr *repr,
                               TypeResolutionOptions options);
    Type resolveAttributedType(TypeAttributes &attrs, TypeRepr *repr,
                               TypeResolutionOptions options);
    Type resolveASTFunctionType(FunctionTypeRepr *repr,
                                TypeResolutionOptions options,
                                FunctionType::ExtInfo extInfo
                                  = FunctionType::ExtInfo());
    Type resolveSILFunctionType(FunctionTypeRepr *repr,
                                TypeResolutionOptions options,
                                SILFunctionType::ExtInfo extInfo
                                  = SILFunctionType::ExtInfo(),
                                ParameterConvention calleeConvention
                                  = DefaultParameterConvention);
    SILParameterInfo resolveSILParameter(TypeRepr *repr,
                                         TypeResolutionOptions options);
    bool resolveSILResults(TypeRepr *repr, TypeResolutionOptions options,
                           SmallVectorImpl<SILResultInfo> &results,
                           Optional<SILResultInfo> &errorResult);
    bool resolveSingleSILResult(TypeRepr *repr, TypeResolutionOptions options,
                                SmallVectorImpl<SILResultInfo> &results,
                                Optional<SILResultInfo> &errorResult);
    Type resolveInOutType(InOutTypeRepr *repr,
                          TypeResolutionOptions options);
    Type resolveArrayType(ArrayTypeRepr *repr,
                          TypeResolutionOptions options);
    Type resolveDictionaryType(DictionaryTypeRepr *repr,
                               TypeResolutionOptions options);
    Type resolveOptionalType(OptionalTypeRepr *repr,
                             TypeResolutionOptions options);
    Type resolveImplicitlyUnwrappedOptionalType(ImplicitlyUnwrappedOptionalTypeRepr *repr,
                                      TypeResolutionOptions options);
    Type resolveTupleType(TupleTypeRepr *repr,
                          TypeResolutionOptions options);
    Type resolveProtocolCompositionType(ProtocolCompositionTypeRepr *repr,
                                        TypeResolutionOptions options);
    Type resolveMetatypeType(MetatypeTypeRepr *repr,
                             TypeResolutionOptions options);
    Type resolveProtocolType(ProtocolTypeRepr *repr,
                             TypeResolutionOptions options);

    Type buildMetatypeType(MetatypeTypeRepr *repr,
                           Type instanceType,
                           Optional<MetatypeRepresentation> storedRepr);
    Type buildProtocolType(ProtocolTypeRepr *repr,
                           Type instanceType,
                           Optional<MetatypeRepresentation> storedRepr);
  };
}

Type TypeChecker::resolveType(TypeRepr *TyR, DeclContext *DC,
                              TypeResolutionOptions options,
                              GenericTypeResolver *resolver,
                              UnsatisfiedDependency *unsatisfiedDependency) {
  PrettyStackTraceTypeRepr stackTrace(Context, "resolving", TyR);

  // Make sure we always have a resolver to use.
  PartialGenericTypeToArchetypeResolver defaultResolver(*this);
  if (!resolver)
    resolver = &defaultResolver;

  TypeResolver typeResolver(*this, DC, resolver, unsatisfiedDependency);
  auto result = typeResolver.resolveType(TyR, options);
  
  // If we resolved down to an error, make sure to mark the typeRepr as invalid
  // so we don't produce a redundant diagnostic.
  if (result && result->is<ErrorType>())
    TyR->setInvalid();
  return result;
}

Type TypeResolver::resolveType(TypeRepr *repr, TypeResolutionOptions options) {
  assert(repr && "Cannot validate null TypeReprs!");

  // If we know the type representation is invalid, just return an
  // error type.
  if (repr->isInvalid()) return ErrorType::get(TC.Context);

  // Strip the "is function input" bits unless this is a type that knows about
  // them.
  if (!isa<InOutTypeRepr>(repr) && !isa<TupleTypeRepr>(repr)) {
    options -= TR_ImmediateFunctionInput;
    options -= TR_FunctionInput;
  }

  switch (repr->getKind()) {
  case TypeReprKind::Error:
    return ErrorType::get(Context);

  case TypeReprKind::Attributed:
    return resolveAttributedType(cast<AttributedTypeRepr>(repr), options);
  case TypeReprKind::InOut:
    return resolveInOutType(cast<InOutTypeRepr>(repr), options);

  case TypeReprKind::SimpleIdent:
  case TypeReprKind::GenericIdent:
  case TypeReprKind::CompoundIdent:
    return TC.resolveIdentifierType(DC, cast<IdentTypeRepr>(repr), options,
                                    /*diagnoseErrors*/ true, Resolver,
                                    UnsatisfiedDependency);

  case TypeReprKind::Function:
    if (!(options & TR_SILType))
      return resolveASTFunctionType(cast<FunctionTypeRepr>(repr), options);
    return resolveSILFunctionType(cast<FunctionTypeRepr>(repr), options);

  case TypeReprKind::Array:
    return resolveArrayType(cast<ArrayTypeRepr>(repr), options);

  case TypeReprKind::Dictionary:
    return resolveDictionaryType(cast<DictionaryTypeRepr>(repr), options);

  case TypeReprKind::Optional:
    return resolveOptionalType(cast<OptionalTypeRepr>(repr), options);

  case TypeReprKind::ImplicitlyUnwrappedOptional:
    return resolveImplicitlyUnwrappedOptionalType(
             cast<ImplicitlyUnwrappedOptionalTypeRepr>(repr),
             options);

  case TypeReprKind::Tuple:
    return resolveTupleType(cast<TupleTypeRepr>(repr), options);

  case TypeReprKind::Named:
    llvm_unreachable("NamedTypeRepr only shows up as an element of Tuple");

  case TypeReprKind::ProtocolComposition:
    return resolveProtocolCompositionType(
             cast<ProtocolCompositionTypeRepr>(repr),
             options);

  case TypeReprKind::Metatype:
    return resolveMetatypeType(cast<MetatypeTypeRepr>(repr), options);

  case TypeReprKind::Protocol:
    return resolveProtocolType(cast<ProtocolTypeRepr>(repr), options);

  case TypeReprKind::Fixed:
    return cast<FixedTypeRepr>(repr)->getType();
  }
  llvm_unreachable("all cases should be handled");
}

Type TypeResolver::resolveAttributedType(AttributedTypeRepr *repr,
                                         TypeResolutionOptions options) {
  // Copy the attributes, since we're about to start hacking on them.
  TypeAttributes attrs = repr->getAttrs();
  assert(!attrs.empty());

  return resolveAttributedType(attrs, repr->getTypeRepr(), options);
}

Type TypeResolver::resolveAttributedType(TypeAttributes &attrs,
                                         TypeRepr *repr,
                                         TypeResolutionOptions options) {
  // The type we're working with, in case we want to build it differently
  // based on the attributes we see.
  Type ty;
  
  // In SIL *only*, allow @thin, @thick, or @objc_metatype to apply to
  // a metatype.
  if (attrs.has(TAK_thin) || attrs.has(TAK_thick) || 
      attrs.has(TAK_objc_metatype)) {
    if (auto SF = DC->getParentSourceFile()) {
      if (SF->Kind == SourceFileKind::SIL) {
        TypeRepr *base;
        if (auto metatypeRepr = dyn_cast<MetatypeTypeRepr>(repr)) {
          base = metatypeRepr->getBase();
        } else if (auto protocolRepr = dyn_cast<ProtocolTypeRepr>(repr)) {
          base = protocolRepr->getBase();
        } else {
          base = nullptr;
        }

        if (base) {
          Optional<MetatypeRepresentation> storedRepr;
          // The instance type is not a SIL type. We still want to allow
          // unavailable references, though.
          auto instanceOptions = options - TR_SILType | TR_AllowUnavailable;
          auto instanceTy = resolveType(base, instanceOptions);
          if (!instanceTy || instanceTy->is<ErrorType>())
            return instanceTy;

          // Check for @thin.
          if (attrs.has(TAK_thin)) {
            storedRepr = MetatypeRepresentation::Thin;
            attrs.clearAttribute(TAK_thin);
          }

          // Check for @thick.
          if (attrs.has(TAK_thick)) {
            if (storedRepr)
              TC.diagnose(repr->getStartLoc(), 
                          diag::sil_metatype_multiple_reprs);
              
            storedRepr = MetatypeRepresentation::Thick;
            attrs.clearAttribute(TAK_thick);
          }

          // Check for @objc_metatype.
          if (attrs.has(TAK_objc_metatype)) {
            if (storedRepr)
              TC.diagnose(repr->getStartLoc(), 
                          diag::sil_metatype_multiple_reprs);
              
            storedRepr = MetatypeRepresentation::ObjC;
            attrs.clearAttribute(TAK_objc_metatype);
          }

          if (instanceTy->is<ErrorType>()) {
            ty = instanceTy;
          } else if (auto metatype = dyn_cast<MetatypeTypeRepr>(repr)) {
            ty = buildMetatypeType(metatype, instanceTy, storedRepr);
          } else {
            ty = buildProtocolType(cast<ProtocolTypeRepr>(repr),
                                   instanceTy, storedRepr);
          }
        }
      }
    }
  }

  // Pass down the variable function type attributes to the
  // function-type creator.
  static const TypeAttrKind FunctionAttrs[] = {
    TAK_objc_block, TAK_convention, TAK_thin, TAK_noreturn,
    TAK_callee_owned, TAK_callee_guaranteed, TAK_noescape
  };

  auto checkUnsupportedAttr = [&](TypeAttrKind attr) {
    if (attrs.has(attr)) {
      TC.diagnose(attrs.getLoc(attr), diag::attribute_not_supported);
      attrs.clearAttribute(attr);
    }
  };
  
  // Some function representation attributes are not supported at source level;
  // only SIL knows how to handle them.  Reject them unless this is a SIL input.
  if (!(options & TR_SILType)) {
    for (auto silOnlyAttr : {TAK_callee_owned, TAK_callee_guaranteed}) {
      checkUnsupportedAttr(silOnlyAttr);
    }
  }  

  // Other function representation attributes are not normally supported at
  // source level, but we want to support them there in SIL files.
  auto SF = DC->getParentSourceFile();
  if (!SF || SF->Kind != SourceFileKind::SIL) {
    for (auto silOnlyAttr : {TAK_thin, TAK_thick}) {
      checkUnsupportedAttr(silOnlyAttr);
    }
  }
  
  bool hasFunctionAttr = false;
  for (auto i : FunctionAttrs)
    if (attrs.has(i)) {
      hasFunctionAttr = true;
      break;
    }

  // Function attributes require a syntactic function type.
  FunctionTypeRepr *fnRepr = dyn_cast<FunctionTypeRepr>(repr);
  if (hasFunctionAttr && fnRepr) {

    // Functions cannot be both @thin and @objc_block.
    bool thin = attrs.has(TAK_thin);
    bool block = attrs.has(TAK_objc_block);
    if (thin && block) {
      TC.diagnose(attrs.getLoc(TAK_objc_block),
                  diag::objc_block_cannot_be_thin)
        .highlight(attrs.getLoc(TAK_thin));
      thin = false;
    }
    
    bool isNoEscape = attrs.has(TAK_noescape);

    auto calleeConvention = ParameterConvention::Direct_Unowned;
    if (attrs.has(TAK_callee_owned)) {
      if (attrs.has(TAK_callee_guaranteed)) {
        TC.diagnose(attrs.getLoc(TAK_callee_owned),
                    diag::sil_function_repeat_convention, /*callee*/ 2);
      }
      calleeConvention = ParameterConvention::Direct_Owned;
    } else if (attrs.has(TAK_callee_guaranteed)) {
      calleeConvention = ParameterConvention::Direct_Guaranteed;
    }

    if (options & TR_SILType) {
      SILFunctionType::Representation rep;

      if (attrs.hasConvention()) {
        // SIL exposes a greater number of conventions than Swift source.
        auto parsedRep =
          llvm::StringSwitch<Optional<SILFunctionType::Representation>>
            (attrs.getConvention())
            .Case("thick", SILFunctionType::Representation::Thick)
            .Case("block", SILFunctionType::Representation::Block)
            .Case("thin", SILFunctionType::Representation::Thin)
            .Case("c", SILFunctionType::Representation::CFunctionPointer)
            .Case("method", SILFunctionType::Representation::Method)
            .Case("objc_method", SILFunctionType::Representation::ObjCMethod)
            .Case("witness_method", SILFunctionType::Representation::WitnessMethod)
            .Default(None);
        if (!parsedRep) {
          TC.diagnose(attrs.getLoc(TAK_convention),
                      diag::unsupported_sil_convention, attrs.getConvention());
          rep = SILFunctionType::Representation::Thin;
        } else {
          rep = *parsedRep;
        }
        
        // Don't allow both @convention and the old representation attrs.
        if (attrs.has(TAK_thin)) {
          TC.diagnose(attrs.getLoc(TAK_thin),
                      diag::convention_with_deprecated_representation_attribute,
                      "thin");
        }
        if (attrs.has(TAK_objc_block)) {
          TC.diagnose(attrs.getLoc(TAK_objc_block),
                      diag::convention_with_deprecated_representation_attribute,
                      "objc_block");
        }
      } else {
        // Error on the old @thin, @cc, and @objc_block attributes in SIL mode.
        if (thin) {
          TC.diagnose(attrs.getLoc(TAK_thin),
                      diag::sil_deprecated_convention_attribute,
                      "thin", "thin");
          rep = SILFunctionType::Representation::Block;
        } else if (block) {
          TC.diagnose(attrs.getLoc(TAK_thin),
                      diag::sil_deprecated_convention_attribute,
                      "objc_block", "block");
          rep = SILFunctionType::Representation::Block;
        } else {
          rep = SILFunctionType::Representation::Thick;
        }
      }
      
      // Resolve the function type directly with these attributes.
      SILFunctionType::ExtInfo extInfo(rep,
                                       attrs.has(TAK_noreturn));
      
      ty = resolveSILFunctionType(fnRepr, options, extInfo, calleeConvention);
      if (!ty || ty->is<ErrorType>()) return ty;
    } else {
      FunctionType::Representation rep;
      if (attrs.hasConvention()) {
        auto parsedRep =
          llvm::StringSwitch<Optional<FunctionType::Representation>>
            (attrs.getConvention())
            .Case("swift", FunctionType::Representation::Swift)
            .Case("block", FunctionType::Representation::Block)
            .Case("thin", FunctionType::Representation::Thin)
            .Case("c", FunctionType::Representation::CFunctionPointer)
            .Default(None);
        if (!parsedRep) {
          TC.diagnose(attrs.getLoc(TAK_convention),
                      diag::unsupported_convention, attrs.getConvention());
          rep = FunctionType::Representation::Swift;
        } else {
          rep = *parsedRep;
        }
        
        // Don't allow both @convention and the old representation attrs.
        if (attrs.has(TAK_thin)) {
          TC.diagnose(attrs.getLoc(TAK_thin),
                      diag::convention_with_deprecated_representation_attribute,
                      "thin");
        }
        if (attrs.has(TAK_objc_block)) {
          TC.diagnose(attrs.getLoc(TAK_objc_block),
                      diag::convention_with_deprecated_representation_attribute,
                      "objc_block");
        }
      } else {
        auto fixDeprecatedAttribute = [&](TypeAttrKind kind,
                                          StringRef oldName,
                                          StringRef newName) {
          auto start = attrs.getLoc(kind);
          
          SmallString<32> fixitString;
          {
            llvm::raw_svector_ostream os(fixitString);
            os << "convention(" << newName << ")";
          }
          
          TC.diagnose(start, diag::deprecated_convention_attribute,
                      oldName, newName)
            .highlight(start)
            .fixItReplace(start, fixitString);
        };
      
        // Handle the old attributes.
        if (thin) {
          rep = FunctionType::Representation::Thin;
          fixDeprecatedAttribute(TAK_thin, "thin", "thin");
        } else if (block) {
          rep = FunctionType::Representation::Block;
          fixDeprecatedAttribute(TAK_objc_block, "objc_block", "block");
        } else {
          rep = FunctionType::Representation::Swift;
        }
      }
      
      // Resolve the function type directly with these attributes.
      FunctionType::ExtInfo extInfo(rep,
                                    attrs.has(TAK_noreturn),
                                    /*autoclosure is a decl attr*/false,
                                    isNoEscape,
                                    fnRepr->throws());

      ty = resolveASTFunctionType(fnRepr, options, extInfo);
      if (!ty || ty->is<ErrorType>()) return ty;
    }

    for (auto i : FunctionAttrs)
      attrs.clearAttribute(i);
    attrs.convention = None;
  } else if (hasFunctionAttr) {
    for (auto i : FunctionAttrs) {
      if (attrs.has(i)) {
        TC.diagnose(attrs.getLoc(i), diag::attribute_requires_function_type);
        attrs.clearAttribute(i);
      }
    }
  } 

  // If we didn't build the type differently above, build it normally now.
  if (!ty) ty = resolveType(repr, options);
  if (!ty || ty->is<ErrorType>()) return ty;

  // In SIL, handle @opened (n), which creates an existential archetype.
  if (attrs.has(TAK_opened)) {
    if (!ty->isExistentialType()) {
      TC.diagnose(attrs.getLoc(TAK_opened), diag::opened_non_protocol, ty);
    } else {
      ty = ArchetypeType::getOpened(ty, attrs.OpenedID);
    }
    attrs.clearAttribute(TAK_opened);
  }

  // In SIL files *only*, permit @weak and @unowned to apply directly to types.
  if (attrs.hasOwnership()) {
    if (auto SF = DC->getParentSourceFile()) {
      if (SF->Kind == SourceFileKind::SIL) {
        if (((attrs.has(TAK_sil_weak) || attrs.has(TAK_sil_unmanaged)) &&
             ty->getAnyOptionalObjectType()) ||
            (!attrs.has(TAK_sil_weak) && ty->hasReferenceSemantics())) {
          ty = ReferenceStorageType::get(ty, attrs.getOwnership(), Context);
          attrs.clearOwnership();
        }
      }
    }
  }
  
  // In SIL *only*, allow @block_storage to specify a block storage type.
  if ((options & TR_SILType) && attrs.has(TAK_block_storage)) {
    ty = SILBlockStorageType::get(ty->getCanonicalType());
    attrs.clearAttribute(TAK_block_storage);
  }
  
  // In SIL *only*, allow @box to specify a box type.
  if ((options & TR_SILType) && attrs.has(TAK_box)) {
    ty = SILBoxType::get(ty->getCanonicalType());
    attrs.clearAttribute(TAK_box);
  }
  
  for (unsigned i = 0; i != TypeAttrKind::TAK_Count; ++i)
    if (attrs.has((TypeAttrKind)i))
      TC.diagnose(attrs.getLoc((TypeAttrKind)i),
                  diag::attribute_does_not_apply_to_type);

  return ty;
}

Type TypeResolver::resolveASTFunctionType(FunctionTypeRepr *repr,
                                          TypeResolutionOptions options,
                                          FunctionType::ExtInfo extInfo) {
  Type inputTy = resolveType(repr->getArgsTypeRepr(),
                             options | TR_ImmediateFunctionInput);
  if (!inputTy || inputTy->is<ErrorType>()) return inputTy;

  Type outputTy = resolveType(repr->getResultTypeRepr(), options);
  if (!outputTy || outputTy->is<ErrorType>()) return outputTy;

  extInfo = extInfo.withThrows(repr->throws());
  
  // SIL uses polymorphic function types to resolve overloaded member functions.
  if (auto generics = repr->getGenericParams()) {
    return PolymorphicFunctionType::get(inputTy, outputTy, generics, extInfo);
  }

  auto fnTy = FunctionType::get(inputTy, outputTy, extInfo);
  // If the type is a block or C function pointer, it must be representable in
  // ObjC.
  switch (auto rep = extInfo.getRepresentation()) {
  case AnyFunctionType::Representation::Block:
  case AnyFunctionType::Representation::CFunctionPointer:
    if (!TC.isRepresentableInObjC(DC, fnTy)) {
      StringRef strName =
        rep == AnyFunctionType::Representation::Block ? "block" : "c";
      auto extInfo2 =
        extInfo.withRepresentation(AnyFunctionType::Representation::Swift);
      auto simpleFnTy = FunctionType::get(inputTy, outputTy, extInfo2);
      TC.diagnose(repr->getStartLoc(), diag::objc_convention_invalid,
                  simpleFnTy, strName);
    }
    break;

  case AnyFunctionType::Representation::Thin:
  case AnyFunctionType::Representation::Swift:
    break;
  }
  
  return fnTy;
}

Type TypeResolver::resolveSILFunctionType(FunctionTypeRepr *repr,
                                          TypeResolutionOptions options,
                                          SILFunctionType::ExtInfo extInfo,
                                          ParameterConvention callee) {
  bool hasError = false;

  SmallVector<SILParameterInfo, 4> params;
  if (auto tuple = dyn_cast<TupleTypeRepr>(repr->getArgsTypeRepr())) {
    // SIL functions cannot be variadic.
    if (tuple->hasEllipsis()) {
      TC.diagnose(tuple->getEllipsisLoc(), diag::sil_function_ellipsis);
    }

    for (auto elt : tuple->getElements()) {
      if (auto named = dyn_cast<NamedTypeRepr>(elt)) {
        TC.diagnose(named->getNameLoc(), diag::sil_function_label);
        elt = named->getTypeRepr();
      }

      auto param = resolveSILParameter(elt,options | TR_ImmediateFunctionInput);
      params.push_back(param);
      if (!param.getType()) return nullptr;

      if (param.getType()->is<ErrorType>())
        hasError = true;
    }
  } else {
    SILParameterInfo param = resolveSILParameter(repr->getArgsTypeRepr(),
                                           options | TR_ImmediateFunctionInput);
    params.push_back(param);
    if (!param.getType()) return nullptr;

    if (param.getType()->is<ErrorType>())
      hasError = true;
  }

  SILResultInfo result;
  Optional<SILResultInfo> errorResult;
  {
    // For now, resolveSILResults only returns a single ordinary result.
    // FIXME: Deal with unsatisfied dependencies.
    SmallVector<SILResultInfo, 1> ordinaryResults;
    if (resolveSILResults(repr->getResultTypeRepr(), options,
                          ordinaryResults, errorResult)) {
      hasError = true;
    } else {
      if (ordinaryResults.empty()) {
        result = SILResultInfo(TupleType::getEmpty(TC.Context),
                               ResultConvention::Unowned);
      } else {
        result = ordinaryResults.front();
      }
    }
  }

  if (hasError) {
    return ErrorType::get(Context);
  }

  // FIXME: Remap the parsed context types to interface types.
  GenericSignature *genericSig = nullptr;
  SmallVector<SILParameterInfo, 4> interfaceParams;
  SILResultInfo interfaceResult;
  Optional<SILResultInfo> interfaceErrorResult;
  if (repr->getGenericParams()) {
    llvm::DenseMap<ArchetypeType*, Type> archetypeMap;
    genericSig
      = repr->getGenericParams()->getAsCanonicalGenericSignature(archetypeMap,
                                                                 Context);
    
    auto getArchetypesAsDependentTypes = [&](Type t) -> Type {
      if (!t) return t;
      if (auto arch = t->getAs<ArchetypeType>()) {
        // As a kludge, we allow Self archetypes of protocol_methods to be
        // unapplied.
        if (arch->getSelfProtocol() && !archetypeMap.count(arch))
          return arch;
        return arch->getAsDependentType(archetypeMap);
      }
      return t;
    };
    
    for (auto &param : params) {
      auto transParamType =
        param.getType().transform(getArchetypesAsDependentTypes)
          ->getCanonicalType();
      interfaceParams.push_back(param.getWithType(transParamType));
    }
    auto transResultType =
      result.getType().transform(getArchetypesAsDependentTypes)
        ->getCanonicalType();
    interfaceResult = result.getWithType(transResultType);

    if (errorResult) {
      auto transErrorResultType =
        errorResult->getType().transform(getArchetypesAsDependentTypes)
          ->getCanonicalType();
      interfaceErrorResult =
        errorResult->getWithType(transErrorResultType);
    }
  } else {
    interfaceParams = params;
    interfaceResult = result;
    interfaceErrorResult = errorResult;
  }
  return SILFunctionType::get(genericSig, extInfo,
                              callee,
                              interfaceParams, interfaceResult,
                              interfaceErrorResult,
                              Context);
}

SILParameterInfo TypeResolver::resolveSILParameter(
                                 TypeRepr *repr,
                                 TypeResolutionOptions options) {
  assert((options & TR_FunctionInput) | (options & TR_ImmediateFunctionInput) &&
         "Parameters should be marked as inputs");
  auto convention = DefaultParameterConvention;
  Type type;
  bool hadError = false;

  if (auto attrRepr = dyn_cast<AttributedTypeRepr>(repr)) {
    auto attrs = attrRepr->getAttrs();
    auto checkFor = [&](TypeAttrKind tak, ParameterConvention attrConv) {
      if (!attrs.has(tak)) return;
      if (convention != DefaultParameterConvention) {
        TC.diagnose(attrs.getLoc(tak), diag::sil_function_repeat_convention,
                    /*input*/ 0);
        hadError = true;
      }
      attrs.clearAttribute(tak);
      convention = attrConv;
    };
    checkFor(TypeAttrKind::TAK_in_guaranteed,
             ParameterConvention::Indirect_In_Guaranteed);
    checkFor(TypeAttrKind::TAK_in, ParameterConvention::Indirect_In);
    checkFor(TypeAttrKind::TAK_out, ParameterConvention::Indirect_Out);
    checkFor(TypeAttrKind::TAK_inout, ParameterConvention::Indirect_Inout);
    checkFor(TypeAttrKind::TAK_inout_aliasable,
             ParameterConvention::Indirect_InoutAliasable);
    checkFor(TypeAttrKind::TAK_owned, ParameterConvention::Direct_Owned);
    checkFor(TypeAttrKind::TAK_guaranteed,
             ParameterConvention::Direct_Guaranteed);
    checkFor(TypeAttrKind::TAK_deallocating,
             ParameterConvention::Direct_Deallocating);

    type = resolveAttributedType(attrs, attrRepr->getTypeRepr(), options);
  } else {
    type = resolveType(repr, options);
  }

  if (!type) return SILParameterInfo(CanType(), convention);
  if (hadError) type = ErrorType::get(Context);
  return SILParameterInfo(type->getCanonicalType(), convention);
}

bool TypeResolver::resolveSingleSILResult(TypeRepr *repr,
                                          TypeResolutionOptions options,
                              SmallVectorImpl<SILResultInfo> &ordinaryResults,
                                       Optional<SILResultInfo> &errorResult) {
  Type type;
  auto convention = DefaultResultConvention;
  bool isErrorResult = false;

  if (auto attrRepr = dyn_cast<AttributedTypeRepr>(repr)) {
    // Copy the attributes out; we're going to destructively modify them.
    auto attrs = attrRepr->getAttrs();

    // Recognize @error.
    if (attrs.has(TypeAttrKind::TAK_error)) {
      attrs.clearAttribute(TypeAttrKind::TAK_error);
      isErrorResult = true;

      // Error results are always implicitly @owned.
      convention = ResultConvention::Owned;
    }

    // Recognize result conventions.
    bool hadError = false;
    auto checkFor = [&](TypeAttrKind tak, ResultConvention attrConv) {
      if (!attrs.has(tak)) return;
      if (convention != DefaultResultConvention) {
        TC.diagnose(attrs.getLoc(tak), diag::sil_function_repeat_convention,
                    /*result*/ 1);
        hadError = true;
      }
      attrs.clearAttribute(tak);
      convention = attrConv;
    };
    checkFor(TypeAttrKind::TAK_owned, ResultConvention::Owned);
    checkFor(TypeAttrKind::TAK_unowned_inner_pointer,
             ResultConvention::UnownedInnerPointer);
    checkFor(TypeAttrKind::TAK_autoreleased, ResultConvention::Autoreleased);
    if (hadError) return true;

    type = resolveAttributedType(attrs, attrRepr->getTypeRepr(), options);
  } else {
    type = resolveType(repr, options);
  }

  // Propagate type-resolution errors out.
  if (!type || type->is<ErrorType>()) return true;

  assert(!isErrorResult || convention == ResultConvention::Owned);
  SILResultInfo resolvedResult(type->getCanonicalType(), convention);

  // TODO: we want to generalize this to allow multiple normal results.
  // But for now, just allow one.
  if (!isErrorResult) {
    if (!ordinaryResults.empty()) {
      TC.diagnose(repr->getStartLoc(), diag::sil_function_multiple_results);
      return true;
    }

    ordinaryResults.push_back(resolvedResult);
    return false;
  }

  // Error result types must have pointer-like representation.
  // FIXME: check that here?

  // We don't expect to have a reason to support multiple independent
  // error results.  (Would this be disjunctive or conjunctive?)
  if (errorResult.hasValue()) {
    TC.diagnose(repr->getStartLoc(),
                diag::sil_function_multiple_error_results);
    return true;
  }

  errorResult = resolvedResult;
  return false;
}

static bool hasElementWithSILResultAttribute(TupleTypeRepr *tuple) {
  for (auto elt : tuple->getElements()) {
    if (auto attrRepr = dyn_cast<AttributedTypeRepr>(elt)) {
      const TypeAttributes &attrs = attrRepr->getAttrs();
      if (attrs.has(TypeAttrKind::TAK_owned) ||
          attrs.has(TypeAttrKind::TAK_unowned_inner_pointer) ||
          attrs.has(TypeAttrKind::TAK_autoreleased) ||
          attrs.has(TypeAttrKind::TAK_error)) {
        return true;
      }
    }
  }
  return false;
}

bool TypeResolver::resolveSILResults(TypeRepr *repr,
                                     TypeResolutionOptions options,
                                SmallVectorImpl<SILResultInfo> &ordinaryResults,
                                Optional<SILResultInfo> &errorResult) {

  // When we generalize SIL to handle multiple normal results, we
  // should always split up a tuple (a single level deep only).  Until
  // then, we need to recognize when the tuple elements don't use any
  // SIL result attributes and keep it as a single result.
  if (auto tuple = dyn_cast<TupleTypeRepr>(repr)) {
    if (hasElementWithSILResultAttribute(tuple)) {
      bool hadError = false;
      for (auto elt : tuple->getElements()) {
        if (resolveSingleSILResult(elt, options, ordinaryResults, errorResult))
          hadError = true;
      }
      return hadError;
    }
  }

  return resolveSingleSILResult(repr, options, ordinaryResults, errorResult);
}

Type TypeResolver::resolveInOutType(InOutTypeRepr *repr,
                                    TypeResolutionOptions options) {
  Type ty = resolveType(cast<InOutTypeRepr>(repr)->getBase(), options);
  if (!ty || ty->is<ErrorType>()) return ty;

  if (!(options & TR_FunctionInput) &&
      !(options & TR_ImmediateFunctionInput)) {
    TC.diagnose(repr->getInOutLoc(), diag::inout_only_parameter);
    repr->setInvalid();
    return ErrorType::get(Context);
  }
  
  return InOutType::get(ty);
}


Type TypeResolver::resolveArrayType(ArrayTypeRepr *repr,
                                    TypeResolutionOptions options) {
  // FIXME: diagnose non-materializability of element type!
  Type baseTy = resolveType(repr->getBase(), withoutContext(options));
  if (!baseTy || baseTy->is<ErrorType>()) return baseTy;

  auto sliceTy = TC.getArraySliceType(repr->getBrackets().Start, baseTy);
  if (!sliceTy)
    return ErrorType::get(Context);

  return sliceTy;
}

Type TypeResolver::resolveDictionaryType(DictionaryTypeRepr *repr,
                                         TypeResolutionOptions options) {
  // FIXME: diagnose non-materializability of key/value type?
  Type keyTy = resolveType(repr->getKey(), withoutContext(options));
  if (!keyTy || keyTy->is<ErrorType>()) return keyTy;

  Type valueTy = resolveType(repr->getValue(), withoutContext(options));
  if (!valueTy || valueTy->is<ErrorType>()) return valueTy;
  
  if (auto dictTy = TC.getDictionaryType(repr->getBrackets().Start, keyTy, 
                                         valueTy)) {
    // Check the requirements on the generic arguments.
    auto unboundTy = UnboundGenericType::get(TC.Context.getDictionaryDecl(),
                                             nullptr, TC.Context);
    TypeLoc args[2] = { TypeLoc(repr->getKey()), TypeLoc(repr->getValue()) };

    if (!TC.applyGenericArguments(unboundTy, repr->getStartLoc(), DC, args,
                                  options.contains(TR_GenericSignature),
                                  Resolver)) {
      return ErrorType::get(TC.Context);
    }

    return dictTy;
  }

  return ErrorType::get(Context);
}

Type TypeResolver::resolveOptionalType(OptionalTypeRepr *repr,
                                       TypeResolutionOptions options) {
  // The T in T? is a generic type argument and therefore always an AST type.
  // FIXME: diagnose non-materializability of element type!
  Type baseTy = resolveType(repr->getBase(), withoutContext(options));
  if (!baseTy || baseTy->is<ErrorType>()) return baseTy;

  auto optionalTy = TC.getOptionalType(repr->getQuestionLoc(), baseTy);
  if (!optionalTy) return ErrorType::get(Context);

  return optionalTy;
}

Type TypeResolver::resolveImplicitlyUnwrappedOptionalType(
       ImplicitlyUnwrappedOptionalTypeRepr *repr,
       TypeResolutionOptions options) {
  // The T in T! is a generic type argument and therefore always an AST type.
  // FIXME: diagnose non-materializability of element type!
  Type baseTy = resolveType(repr->getBase(), withoutContext(options));
  if (!baseTy || baseTy->is<ErrorType>()) return baseTy;

  auto uncheckedOptionalTy =
    TC.getImplicitlyUnwrappedOptionalType(repr->getExclamationLoc(), baseTy);
  if (!uncheckedOptionalTy)
    return ErrorType::get(Context);

  return uncheckedOptionalTy;
}

Type TypeResolver::resolveTupleType(TupleTypeRepr *repr,
                                    TypeResolutionOptions options) {
  SmallVector<TupleTypeElt, 8> elements;
  elements.reserve(repr->getElements().size());
  
  // If this is the top level of a function input list, peel off the
  // ImmediateFunctionInput marker and install a FunctionInput one instead.
  auto elementOptions = withoutContext(options);
  if (options & TR_ImmediateFunctionInput)
    elementOptions |= TR_FunctionInput;
  
  for (auto tyR : repr->getElements()) {
    if (NamedTypeRepr *namedTyR = dyn_cast<NamedTypeRepr>(tyR)) {
      Type ty = resolveType(namedTyR->getTypeRepr(), elementOptions);
      if (!ty || ty->is<ErrorType>()) return ty;

      elements.push_back(TupleTypeElt(ty, namedTyR->getName()));
    } else {
      Type ty = resolveType(tyR, elementOptions);
      if (!ty || ty->is<ErrorType>()) return ty;

      elements.push_back(TupleTypeElt(ty));
    }
  }

  // Tuple representations are limited outside of function inputs.
  if (!(options & TR_ImmediateFunctionInput)) {
    bool complained = false;

    // Variadic tuples are not permitted.
    if (repr->hasEllipsis()) {
      TC.diagnose(repr->getEllipsisLoc(), diag::tuple_ellipsis);
      repr->removeEllipsis();
      complained = true;
    } 

    // Single-element labeled tuples are not permitted, either.
    if (elements.size() == 1 && elements[0].hasName() &&
        !(options & TR_EnumCase)) {
      if (!complained) {
        auto named = cast<NamedTypeRepr>(repr->getElement(0));
        TC.diagnose(repr->getElement(0)->getStartLoc(),
                    diag::tuple_single_element)
          .fixItRemoveChars(named->getStartLoc(),
                            named->getTypeRepr()->getStartLoc());
      }

      elements[0] = TupleTypeElt(elements[0].getType());
    }
  }

  if (repr->hasEllipsis()) {
    auto &element = elements[repr->getEllipsisIndex()];
    Type baseTy = element.getType();
    Type fullTy = TC.getArraySliceType(repr->getEllipsisLoc(), baseTy);
    Identifier name = element.getName();
    element = TupleTypeElt(fullTy, name, DefaultArgumentKind::None, true);
  }

  return TupleType::get(elements, Context);
}

Type TypeResolver::resolveProtocolCompositionType(
                                         ProtocolCompositionTypeRepr *repr,
                                         TypeResolutionOptions options) {
  SmallVector<Type, 4> ProtocolTypes;
  for (auto tyR : repr->getProtocols()) {
    Type ty = TC.resolveType(tyR, DC, withoutContext(options), Resolver);
    if (!ty || ty->is<ErrorType>()) return ty;

    if (!ty->isExistentialType()) {
      TC.diagnose(tyR->getStartLoc(), diag::protocol_composition_not_protocol,
                  ty);
      continue;
    }

    ProtocolTypes.push_back(ty);
  }
  return ProtocolCompositionType::get(Context, ProtocolTypes);
}

Type TypeResolver::resolveMetatypeType(MetatypeTypeRepr *repr,
                                       TypeResolutionOptions options) {
  // The instance type of a metatype is always abstract, not SIL-lowered.
  Type ty = resolveType(repr->getBase(), withoutContext(options));
  if (!ty || ty->is<ErrorType>()) return ty;

  Optional<MetatypeRepresentation> storedRepr;
  
  // In SIL mode, a metatype must have a @thin, @thick, or
  // @objc_metatype attribute, so metatypes should have been lowered
  // in resolveAttributedType.
  if (options & TR_SILType) {
    TC.diagnose(repr->getStartLoc(), diag::sil_metatype_without_repr);
    storedRepr = MetatypeRepresentation::Thick;
  }

  return buildMetatypeType(repr, ty, storedRepr);
}

Type TypeResolver::buildMetatypeType(
       MetatypeTypeRepr *repr,
       Type instanceType,
       Optional<MetatypeRepresentation> storedRepr) {
  if (instanceType->isAnyExistentialType()) {
    // TODO: diagnose invalid representations?
    return ExistentialMetatypeType::get(instanceType, storedRepr);
  } else {
    return MetatypeType::get(instanceType, storedRepr);
  }
}

Type TypeResolver::resolveProtocolType(ProtocolTypeRepr *repr,
                                       TypeResolutionOptions options) {
  // The instance type of a metatype is always abstract, not SIL-lowered.
  Type ty = resolveType(repr->getBase(), withoutContext(options));
  if (!ty || ty->is<ErrorType>()) return ty;

  Optional<MetatypeRepresentation> storedRepr;
  
  // In SIL mode, a metatype must have a @thin, @thick, or
  // @objc_metatype attribute, so metatypes should have been lowered
  // in resolveAttributedType.
  if (options & TR_SILType) {
    TC.diagnose(repr->getStartLoc(), diag::sil_metatype_without_repr);
    storedRepr = MetatypeRepresentation::Thick;
  }

  return buildProtocolType(repr, ty, storedRepr);
}

Type TypeResolver::buildProtocolType(
       ProtocolTypeRepr *repr,
       Type instanceType,
       Optional<MetatypeRepresentation> storedRepr) {
  if (!instanceType->isAnyExistentialType()) {
    TC.diagnose(repr->getProtocolLoc(), diag::dot_protocol_on_non_existential,
                instanceType);
    return ErrorType::get(TC.Context);
  }

  return MetatypeType::get(instanceType, storedRepr);
}

Type TypeChecker::substMemberTypeWithBase(Module *module,
                                          const ValueDecl *member,
                                          Type baseTy, bool isTypeReference) {
  Type memberType = isTypeReference
                      ? cast<TypeDecl>(member)->getDeclaredInterfaceType()
                      : member->getInterfaceType();

  if (isTypeReference) {
    // The declared interface type for a generic type will have the type
    // arguments; strip them off.
    if (auto nominalTypeDecl = dyn_cast<NominalTypeDecl>(member)) {
      if (auto boundGenericTy = memberType->getAs<BoundGenericType>()) {
        memberType = UnboundGenericType::get(
                       const_cast<NominalTypeDecl *>(nominalTypeDecl),
                       boundGenericTy->getParent(),
                       Context);
      }
    }
  }

  return baseTy->getTypeOfMember(module, member, this, memberType);
}

Type TypeChecker::getSuperClassOf(Type type) {
  return type->getSuperclass(this);
}

Type TypeChecker::resolveMemberType(DeclContext *dc, Type type,
                                    Identifier name) {
  LookupTypeResult memberTypes = lookupMemberType(dc, type, name);
  if (!memberTypes)
    return Type();

  // FIXME: Detect ambiguities here?
  return memberTypes.back().second;
}

/// Look up and validate a type declared in the standard library.
static CanType lookupUniqueTypeInLibrary(TypeChecker &TC,
                                         Module *stdlib,
                                         Identifier name) {
  SmallVector<ValueDecl *, 4> results;
  stdlib->lookupValue({}, name, NLKind::UnqualifiedLookup, results);

  TypeDecl *type = nullptr;
  for (auto result: results) {
    if (auto foundType = dyn_cast<TypeDecl>(result)) {
      // Fail if we find two types with this name.
      if (type) return CanType();
      type = foundType;
    }
  }

  // Fail if we didn't find a type.
  if (!type) return CanType();

  TC.validateDecl(type);
  if (type->isInvalid()) return CanType();

  return type->getDeclaredType()->getCanonicalType();
}

static void lookupAndAddLibraryTypes(TypeChecker &TC,
                                     Module *Stdlib,
                                     ArrayRef<Identifier> TypeNames,
                                     llvm::DenseSet<CanType> &Types) {
  SmallVector<ValueDecl *, 4> Results;
  for (Identifier Id : TypeNames) {
    Stdlib->lookupValue({}, Id, NLKind::UnqualifiedLookup, Results);
    for (auto *VD : Results) {
      if (auto *TD = dyn_cast<TypeDecl>(VD)) {
        TC.validateDecl(TD);
        Types.insert(TD->getDeclaredType()->getCanonicalType());
      }
    }
    Results.clear();
  }
}

/// Emit an additional diagnostic describing why we are applying @objc to the
/// decl, if this is not obvious from the decl itself.
static void describeObjCReason(TypeChecker &TC, const ValueDecl *VD,
                               ObjCReason Reason) {
  if (Reason == ObjCReason::MemberOfObjCProtocol) {
    TC.diagnose(VD->getLoc(), diag::objc_inferring_on_objc_protocol_member);
  } else if (Reason == ObjCReason::OverridesObjC) {
    unsigned kind = isa<VarDecl>(VD) ? 0
                  : isa<SubscriptDecl>(VD) ? 1
                  : isa<ConstructorDecl>(VD) ? 2
                  : 3;

    auto overridden = VD->getOverriddenDecl();
    if (overridden->getLoc().isValid()) {
      TC.diagnose(overridden->getLoc(), diag::objc_overriding_objc_decl,
                  kind, VD->getOverriddenDecl()->getFullName());
    }
  }
}

static void diagnoseFunctionParamNotRepresentable(
    TypeChecker &TC, const AbstractFunctionDecl *AFD, unsigned NumParams,
    unsigned ParamIndex, const ParamDecl *P, ObjCReason Reason) {
  if (Reason == ObjCReason::DoNotDiagnose)
    return;

  if (NumParams == 1) {
    TC.diagnose(AFD->getLoc(), diag::objc_invalid_on_func_single_param_type,
                getObjCDiagnosticAttrKind(Reason));
  } else {
    TC.diagnose(AFD->getLoc(), diag::objc_invalid_on_func_param_type,
                ParamIndex + 1, getObjCDiagnosticAttrKind(Reason));
  }
  if (P->hasType()) {
    Type ParamTy = P->getType();
    SourceRange SR;
    if (auto typeRepr = P->getTypeLoc().getTypeRepr())
      SR = typeRepr->getSourceRange();
    TC.diagnoseTypeNotRepresentableInObjC(AFD, ParamTy, SR);
  }
  describeObjCReason(TC, AFD, Reason);
}

static bool isParamListRepresentableInObjC(TypeChecker &TC,
                                           const AbstractFunctionDecl *AFD,
                                           const ParameterList *PL,
                                           ObjCReason Reason) {
  // If you change this function, you must add or modify a test in PrintAsObjC.

  bool Diagnose = (Reason != ObjCReason::DoNotDiagnose);

  bool IsObjC = true;
  unsigned NumParams = PL->size();
  for (unsigned ParamIndex = 0; ParamIndex != NumParams; ParamIndex++) {
    auto param = PL->get(ParamIndex);
    
    // Swift Varargs are not representable in Objective-C.
    if (param->isVariadic()) {
      if (Diagnose && Reason != ObjCReason::DoNotDiagnose) {
        TC.diagnose(param->getStartLoc(), diag::objc_invalid_on_func_variadic,
                    getObjCDiagnosticAttrKind(Reason))
          .highlight(param->getSourceRange());
        describeObjCReason(TC, AFD, Reason);
      }
      
      return false;
    }
    
    if (TC.isRepresentableInObjC(AFD, param->getType()))
      continue;
    
    // Permit '()' when this method overrides a method with a
    // foreign error convention that replaces NSErrorPointer with ()
    // and this is the replaced parameter.
    AbstractFunctionDecl *overridden;
    if (param->getType()->isVoid() && AFD->isBodyThrowing() &&
        (overridden = AFD->getOverriddenDecl())) {
      auto foreignError = overridden->getForeignErrorConvention();
      if (foreignError &&
          foreignError->isErrorParameterReplacedWithVoid() &&
          foreignError->getErrorParameterIndex() == ParamIndex) {
        continue;
      }
    }

    IsObjC = false;
    if (!Diagnose) {
      // Save some work and return as soon as possible if we are not
      // producing diagnostics.
      return IsObjC;
    }
    diagnoseFunctionParamNotRepresentable(TC, AFD, NumParams, ParamIndex,
                                          param, Reason);
  }
  return IsObjC;
}

/// Check whether the given declaration occurs within a constrained
/// extension, or an extension of a class with generic ancestry, and
/// therefore is not representable in Objective-C.
static bool checkObjCInExtensionContext(TypeChecker &tc,
                                        const ValueDecl *value,
                                        bool diagnose) {
  auto DC = value->getDeclContext();

  if (auto ED = dyn_cast<ExtensionDecl>(DC)) {
    if (ED->getTrailingWhereClause()) {
      if (diagnose) {
        tc.diagnose(value->getLoc(), diag::objc_in_extension_context);
      }
      return true;
    }

    // Check if any classes in the inheritance hierarchy have generic
    // parameters.
    // FIXME: This is a current limitation, not inherent. We don't have
    // a concrete class to attach Objective-C category metadata to.
    Type extendedTy = ED->getDeclaredTypeInContext();
    while (!extendedTy.isNull()) {
      const ClassDecl *CD = extendedTy->getClassOrBoundGenericClass();
      if (!CD)
        break;

      if (CD->getGenericParams()) {
        if (diagnose) {
          tc.diagnose(value->getLoc(), diag::objc_in_generic_extension);
        }
        return true;
      }

      extendedTy = CD->getSuperclass();
    }
  }

  return false;
}

/// Check whether the given declaration contains its own generic parameters,
/// and therefore is not representable in Objective-C.
static bool checkObjCWithGenericParams(TypeChecker &TC,
                                       const AbstractFunctionDecl *AFD,
                                       ObjCReason Reason) {
  bool Diagnose = (Reason != ObjCReason::DoNotDiagnose);

  if (AFD->getGenericParams()) {
    // Diagnose this problem, if asked to.
    if (Diagnose) {
      TC.diagnose(AFD->getLoc(), diag::objc_invalid_with_generic_params,
                  getObjCDiagnosticAttrKind(Reason));
      describeObjCReason(TC, AFD, Reason);
    }

    return true;
  }

  return false;
}

static bool isForeignClassContext(DeclContext *DC) {
  auto type = DC->getDeclaredTypeInContext();
  if (!type)
    return false;
  auto clas = type->getClassOrBoundGenericClass();
  if (!clas)
    return false;
  return clas->isForeign();
}

/// CF types cannot have @objc methods, because they don't have real class
/// objects.
static bool checkObjCInForeignClassContext(TypeChecker &TC,
                                           const ValueDecl *VD,
                                           ObjCReason Reason) {
  bool Diagnose = (Reason != ObjCReason::DoNotDiagnose);

  if (isForeignClassContext(VD->getDeclContext())) {
    if (Diagnose) {
      TC.diagnose(VD->getLoc(), diag::objc_invalid_on_foreign_class,
                  getObjCDiagnosticAttrKind(Reason));
      describeObjCReason(TC, VD, Reason);
    }
    return true;
  }

  return false;
}

bool TypeChecker::isCIntegerType(const DeclContext *DC, Type T) {
  if (CIntegerTypes.empty())
    fillObjCRepresentableTypeCache(DC);
  return CIntegerTypes.count(T->getCanonicalType());
}

/// Determines whether the given type is bridged to an Objective-C class type.
static bool isBridgedToObjectiveCClass(DeclContext *dc, Type type) {
  // Simple case: bridgeable object types.
  if (type->isBridgeableObjectType())
    return true;

  // Determine whether this type is bridged to Objective-C.
  ASTContext &ctx = type->getASTContext();
  Optional<Type> bridged = ctx.getBridgedToObjC(dc, type,
                                                ctx.getLazyResolver());
  if (!bridged)
    return false;

  // Check whether we're bridging to a class.
  auto classDecl = (*bridged)->getClassOrBoundGenericClass();
  if (!classDecl)
    return false;

  // Allow anything that isn't bridged to NSNumber.
  // FIXME: This feels like a hack, but we don't have the right predicate
  // anywhere.
  return classDecl->getName().str() != "NSNumber";
}

bool TypeChecker::isRepresentableInObjC(
       const AbstractFunctionDecl *AFD,
       ObjCReason Reason,
       Optional<ForeignErrorConvention> &errorConvention) {
  // Clear out the error convention. It will be added later if needed.
  errorConvention = None;

  // If you change this function, you must add or modify a test in PrintAsObjC.

  bool Diagnose = (Reason != ObjCReason::DoNotDiagnose);

  if (checkObjCInForeignClassContext(*this, AFD, Reason))
    return false;
  if (checkObjCWithGenericParams(*this, AFD, Reason))
    return false;
  if (checkObjCInExtensionContext(*this, AFD, Diagnose))
    return false;

  if (auto *FD = dyn_cast<FuncDecl>(AFD)) {
    if (FD->isAccessor()) {
      // Accessors can only be @objc if the storage declaration is.
      auto storage = FD->getAccessorStorageDecl();
      validateDecl(storage);
      if (!storage->isObjC()) {
        if (Diagnose) {
          auto error = FD->isGetter()
                    ? (isa<VarDecl>(storage) 
                         ? diag::objc_getter_for_nonobjc_property
                         : diag::objc_getter_for_nonobjc_subscript)
                    : (isa<VarDecl>(storage)
                         ? diag::objc_setter_for_nonobjc_property
                         : diag::objc_setter_for_nonobjc_subscript);

          diagnose(FD->getLoc(), error);
          describeObjCReason(*this, AFD, Reason);
        }
        return false;
      }
    } else {
      unsigned ExpectedParamPatterns = 1;
      if (FD->getImplicitSelfDecl())
        ExpectedParamPatterns++;
      if (FD->getParameterLists().size() != ExpectedParamPatterns) {
        if (Diagnose) {
          diagnose(AFD->getLoc(), diag::objc_invalid_on_func_curried,
                   getObjCDiagnosticAttrKind(Reason));
          describeObjCReason(*this, AFD, Reason);
        }
        return false;
      }
    }

    // willSet/didSet implementations are never exposed to objc, they are always
    // directly dispatched from the synthesized setter.
    if (FD->isObservingAccessor()) {
      if (Diagnose) {
        diagnose(AFD->getLoc(), diag::objc_observing_accessor);
        describeObjCReason(*this, AFD, Reason);
      }
      return false;
    }
  }

  // As a special case, an initializer with a single, named parameter of type
  // '()' is always representable in Objective-C. This allows us to cope with
  // zero-parameter methods with selectors that are longer than "init". For
  // example, this allows:
  //
  // \code
  // class Foo {
  //   @objc init(malice: ()) { } // selector is "initWithMalice"
  // }
  // \endcode
  bool isSpecialInit = false;
  if (auto init = dyn_cast<ConstructorDecl>(AFD))
    isSpecialInit = init->isObjCZeroParameterWithLongSelector();

  if (!isSpecialInit &&
      !isParamListRepresentableInObjC(*this, AFD, AFD->getParameterList(1),
                                      Reason)) {
    if (!Diagnose) {
      // Return as soon as possible if we are not producing diagnostics.
      return false;
    }
  }

  if (auto FD = dyn_cast<FuncDecl>(AFD)) {
    Type ResultType = FD->getResultType();
    if (!ResultType->isVoid() && !isRepresentableInObjC(FD, ResultType)) {
      if (Diagnose) {
        diagnose(AFD->getLoc(), diag::objc_invalid_on_func_result_type,
                 getObjCDiagnosticAttrKind(Reason));
        SourceRange Range =
            FD->getBodyResultTypeLoc().getTypeRepr()->getSourceRange();
        diagnoseTypeNotRepresentableInObjC(FD, ResultType, Range);
        describeObjCReason(*this, FD, Reason);
      }
      return false;
    }
  }

  // Throwing functions must map to a particular error convention.
  if (AFD->isBodyThrowing()) {
    DeclContext *dc = const_cast<AbstractFunctionDecl *>(AFD);
    SourceLoc throwsLoc;
    Type resultType;

    const ConstructorDecl *ctor = nullptr;
    if (auto func = dyn_cast<FuncDecl>(AFD)) {
      resultType = func->getResultType();
      throwsLoc = func->getThrowsLoc();
    } else {
      ctor = cast<ConstructorDecl>(AFD);
      throwsLoc = ctor->getThrowsLoc();
    }

    ForeignErrorConvention::Kind kind;
    CanType errorResultType;
    Type optOptionalType;
    if (ctor) {
      // Initializers always use the nil result convention.
      kind = ForeignErrorConvention::NilResult;

      // Only non-failing initializers can throw.
      if (ctor->getFailability() != OTK_None) {
        if (Diagnose) {
          diagnose(AFD->getLoc(), diag::objc_invalid_on_failing_init,
                   getObjCDiagnosticAttrKind(Reason))
            .highlight(throwsLoc);
          describeObjCReason(*this, AFD, Reason);
        }

        return false;
      }
    } else if (resultType->isVoid()) {
      // Functions that return nothing (void) can be throwing; they indicate
      // failure with a 'false' result.
      kind = ForeignErrorConvention::ZeroResult;
      errorResultType = Context.getBoolDecl()
                          ->getDeclaredInterfaceType()->getCanonicalType();
    } else if (!resultType->getAnyOptionalObjectType() &&
               isBridgedToObjectiveCClass(dc, resultType)) {
      // Functions that return a (non-optional) type bridged to Objective-C
      // can be throwing; they indicate failure with a nil result.
      kind = ForeignErrorConvention::NilResult;
    } else if ((optOptionalType = resultType->getAnyOptionalObjectType()) &&
               isBridgedToObjectiveCClass(dc, optOptionalType)) {
      // Cannot return an optional bridged type, because 'nil' is reserved
      // to indicate failure. Call this out in a separate diagnostic.
      if (Diagnose) {
        diagnose(AFD->getLoc(),
                 diag::objc_invalid_on_throwing_optional_result,
                 getObjCDiagnosticAttrKind(Reason),
                 resultType)
          .highlight(throwsLoc);
        describeObjCReason(*this, AFD, Reason);
      }
      return false;
    } else {
      // Other result types are not permitted.
      if (Diagnose) {
        diagnose(AFD->getLoc(),
                 diag::objc_invalid_on_throwing_result,
                 getObjCDiagnosticAttrKind(Reason),
                 resultType)
          .highlight(throwsLoc);
        describeObjCReason(*this, AFD, Reason);
      }
      return false;
    }

    // The error type is always AutoreleasingUnsafeMutablePointer<NSError?>.
    Type errorParameterType = getNSErrorType(dc);
    if (errorParameterType) {
      errorParameterType = OptionalType::get(errorParameterType);
      errorParameterType
        = BoundGenericType::get(
            Context.getAutoreleasingUnsafeMutablePointerDecl(),
            nullptr,
            errorParameterType);
    }

    // Determine the parameter index at which the error will go.
    unsigned errorParameterIndex;
    bool foundErrorParameterIndex = false;

    // If there is an explicit @objc attribute with a name, look for
    // the "error" selector piece.
    if (auto objc = AFD->getAttrs().getAttribute<ObjCAttr>()) {
      if (auto objcName = objc->getName()) {
        auto selectorPieces = objcName->getSelectorPieces();
        for (unsigned i = selectorPieces.size(); i > 0; --i) {
          // If the selector piece is "error", this is the location of
          // the error parameter.
          auto piece = selectorPieces[i-1];
          if (piece == Context.Id_error) {
            errorParameterIndex = i-1;
            foundErrorParameterIndex = true;
            break;
          }

          // If the first selector piece ends with "Error", it's here.
          if (i == 1 && camel_case::getLastWord(piece.str()) == "Error") {
            errorParameterIndex = i-1;
            foundErrorParameterIndex = true;
            break;
          }
        }
      }
    }

    // If the selector did not provide an index for the error, find
    // the last parameter that is not a trailing closure.
    if (!foundErrorParameterIndex) {
      auto *paramList = AFD->getParameterList(1);
      errorParameterIndex = paramList->size();
      while (errorParameterIndex > 0) {
        // Skip over trailing closures.
        auto type = paramList->get(errorParameterIndex - 1)->getType();

        // It can't be a trailing closure unless it has a specific form.
        // Only consider the rvalue type.
        type = type->getRValueType();
        
        // Look through one level of optionality.
        if (auto objectType = type->getAnyOptionalObjectType())
          type = objectType;
        
        // Is it a function type?
        if (!type->is<AnyFunctionType>()) break;
        --errorParameterIndex;
      }
    }

    // Form the error convention.
    CanType canErrorParameterType;
    if (errorParameterType)
      canErrorParameterType = errorParameterType->getCanonicalType();
    switch (kind) {
    case ForeignErrorConvention::ZeroResult:
      errorConvention = ForeignErrorConvention::getZeroResult(
                          errorParameterIndex,
                          ForeignErrorConvention::IsNotOwned,
                          ForeignErrorConvention::IsNotReplaced,
                          canErrorParameterType,
                          errorResultType);
      break;

    case ForeignErrorConvention::NonZeroResult:
      errorConvention = ForeignErrorConvention::getNonZeroResult(
                          errorParameterIndex,
                          ForeignErrorConvention::IsNotOwned,
                          ForeignErrorConvention::IsNotReplaced,
                          canErrorParameterType,
                          errorResultType);
      break;

    case ForeignErrorConvention::ZeroPreservedResult:
      errorConvention = ForeignErrorConvention::getZeroPreservedResult(
                          errorParameterIndex,
                          ForeignErrorConvention::IsNotOwned,
                          ForeignErrorConvention::IsNotReplaced,
                          canErrorParameterType);
      break;

    case ForeignErrorConvention::NilResult:
      errorConvention = ForeignErrorConvention::getNilResult(
                          errorParameterIndex,
                          ForeignErrorConvention::IsNotOwned,
                          ForeignErrorConvention::IsNotReplaced,
                          canErrorParameterType);
      break;

    case ForeignErrorConvention::NonNilError:
      errorConvention = ForeignErrorConvention::getNilResult(
                          errorParameterIndex,
                          ForeignErrorConvention::IsNotOwned,
                          ForeignErrorConvention::IsNotReplaced,
                          canErrorParameterType);
      break;
    }
  }

  return true;
}

bool TypeChecker::isRepresentableInObjC(const VarDecl *VD, ObjCReason Reason) {
  // If you change this function, you must add or modify a test in PrintAsObjC.

  if (VD->isInvalid())
    return false;

  Type T = VD->getType();
  if (auto *RST = T->getAs<ReferenceStorageType>()) {
    // In-memory layout of @weak and @unowned does not correspond to anything
    // in Objective-C, but this does not really matter here, since Objective-C
    // uses getters and setters to operate on the property.
    // Because of this, look through @weak and @unowned.
    T = RST->getReferentType();
  }
  bool Result = isRepresentableInObjC(VD->getDeclContext(), T);
  bool Diagnose = (Reason != ObjCReason::DoNotDiagnose);

  if (Result && checkObjCInExtensionContext(*this, VD, Diagnose))
    return false;

  if (checkObjCInForeignClassContext(*this, VD, Reason))
    return false;

  if (!Diagnose || Result)
    return Result;

  SourceRange TypeRange = VD->getTypeSourceRangeForDiagnostics();
  diagnose(VD->getLoc(), diag::objc_invalid_on_var,
           getObjCDiagnosticAttrKind(Reason))
      .highlight(TypeRange);
  diagnoseTypeNotRepresentableInObjC(VD->getDeclContext(), VD->getType(),
                                     TypeRange);
  describeObjCReason(*this, VD, Reason);

  return Result;
}

bool TypeChecker::isRepresentableInObjC(const SubscriptDecl *SD,
                                        ObjCReason Reason) {
  // If you change this function, you must add or modify a test in PrintAsObjC.

  bool Diagnose = (Reason != ObjCReason::DoNotDiagnose);

  if (checkObjCInForeignClassContext(*this, SD, Reason))
    return false;

  // Figure out the type of the indices.
  Type IndicesType = SD->getIndicesType();
  if (auto TupleTy = IndicesType->getAs<TupleType>()) {
    if (TupleTy->getNumElements() == 1 && !TupleTy->getElement(0).isVararg())
      IndicesType = TupleTy->getElementType(0);
  }
  
  if (IndicesType->is<ErrorType>())
    return false;

  bool IndicesResult = isRepresentableInObjC(SD->getDeclContext(), IndicesType);
  bool ElementResult = isRepresentableInObjC(SD->getDeclContext(),
                                             SD->getElementType());
  bool Result = IndicesResult && ElementResult;

  if (Result && checkObjCInExtensionContext(*this, SD, Diagnose))
    return false;

  // Make sure we know how to map the selector appropriately.
  if (Result && SD->getObjCSubscriptKind(this) == ObjCSubscriptKind::None) {
    SourceRange IndexRange = SD->getIndices()->getSourceRange();
    diagnose(SD->getLoc(), diag::objc_invalid_subscript_key_type,
             getObjCDiagnosticAttrKind(Reason), IndicesType)
      .highlight(IndexRange);
    return false;
  }

  if (!Diagnose || Result)
    return Result;

  SourceRange TypeRange;
  if (!IndicesResult)
    TypeRange = SD->getIndices()->getSourceRange();
  else
    TypeRange = SD->getElementTypeLoc().getSourceRange();
  diagnose(SD->getLoc(), diag::objc_invalid_on_subscript,
           getObjCDiagnosticAttrKind(Reason))
    .highlight(TypeRange);

  diagnoseTypeNotRepresentableInObjC(SD->getDeclContext(),
                                     !IndicesResult? IndicesType
                                                   : SD->getElementType(),
                                     TypeRange);
  describeObjCReason(*this, SD, Reason);

  return Result;
}

/// True if T is representable as a non-nullable ObjC pointer type.
static bool isAnyObjCRepresentableObjectType(Type T) {
  // Look through a single level of metatype.
  if (auto MTT = T->getAs<AnyMetatypeType>())
    T = MTT->getInstanceType();

  if (auto dynSelf = T->getAs<DynamicSelfType>())
    T = dynSelf->getSelfType();

  if (auto *CT = T->getAs<ClassType>())
    return CT->getDecl()->isObjC();
  return T->isObjCExistentialType();
}

/// True if T is representable as an ObjC pointer type, nullable or otherwise.
static bool isNullableObjCRepresentableObjectType(Type T) {
  // Look through a single layer of optional type.
  if (auto valueType = T->getAnyOptionalObjectType()) {
    T = valueType;
  }
  return isAnyObjCRepresentableObjectType(T);
}

bool TypeChecker::isTriviallyRepresentableInObjC(const DeclContext *DC,
                                                 Type T) {
  // If you change this function, you must add or modify a test in PrintAsObjC.

  // Look through one level of optional type, but remember that we did.
  bool wasOptional = false;
  if (auto valueType = T->getAnyOptionalObjectType()) {
    T = valueType;
    wasOptional = true;
  }

  // T can be represented in Objective-C if T is an @objc class or protocol.
  if (isAnyObjCRepresentableObjectType(T))
    return true;

  auto NTD = T->getAnyNominal();

  // Unmanaged<T> can be represented in Objective-C if T is an @objc class
  // or protocol.
  if (NTD == Context.getUnmanagedDecl()) {
    auto BGT = T->getAs<BoundGenericType>();
    if (!BGT)
      return false;
    assert(BGT->getGenericArgs().size() == 1);
    return isAnyObjCRepresentableObjectType(BGT->getGenericArgs().front());
  }

  // TODO: maybe Optional<UnsafeMutablePointer<T>> should be okay?
  if (wasOptional)
    return false;

  if (NTD) {
    // If the type was imported from Clang, it is representable in Objective-C.
    if (NTD->hasClangNode())
      return true;
    
    // If the type is @objc, it is representable in Objective-C.
    if (NTD->isObjC())
      return true;

    // Pointers may be representable in ObjC.
    PointerTypeKind PTK;
    if (auto pointerElt = T->getAnyPointerElementType(PTK)) {
      switch (PTK) {
      case PTK_UnsafeMutablePointer:
      case PTK_UnsafePointer: {
        // An UnsafeMutablePointer<T> or UnsafePointer<T> is
        // representable in Objective-C if T is a trivially
        // representable type or Void.
        return pointerElt->isEqual(Context.TheEmptyTupleType)
          || isTriviallyRepresentableInObjC(DC, pointerElt);
      }
      case PTK_AutoreleasingUnsafeMutablePointer: {
        // An AutoreleasingUnsafeMutablePointer<T> is representable in ObjC if T
        // is a (potentially optional) ObjC pointer type.
        return isNullableObjCRepresentableObjectType(pointerElt);
      }
      }
    }

  }

  // If it's a mapped type, it's representable.
  fillObjCRepresentableTypeCache(DC);
  if (ObjCMappedTypes.count(T->getCanonicalType()))
    return true;

  return false;
}

static bool
isObjCRepresentableCollection(TypeChecker &TC, const DeclContext *DC, Type T);

static bool
isElementRepresentableInObjC(TypeChecker &TC, const DeclContext *DC, Type T) {
  // If you change this function, you must add or modify a test in PrintAsObjC.

  // ImplicitlyUnwrappedOptional is marked bridgeable to Objective-C, but
  // it's not something you can put in API signatures.
  if (T->getAnyOptionalObjectType())
    return false;

  if (isAnyObjCRepresentableObjectType(T))
    return true;

  if (isObjCRepresentableCollection(TC, DC, T))
    return true;

  if (auto fnTy = T->getAs<AnyFunctionType>())
    return fnTy->getRepresentation() == FunctionTypeRepresentation::Block;

  if (!T->getAs<BoundGenericType>()) {
    // Don't check this path for collections and other things that are only
    // conditionally bridged to Objective-C.
    ProtocolDecl *bridgingProto =
        TC.getProtocol({}, KnownProtocolKind::ObjectiveCBridgeable);
    if (bridgingProto &&
        TC.conformsToProtocol(T, bridgingProto, const_cast<DeclContext *>(DC),
                              ConformanceCheckOptions())) {
      return true;
    }
  }

  return false;
}

static bool
isObjCRepresentableCollection(TypeChecker &TC, const DeclContext *DC, Type T) {
  // If you change this function, you must add or modify a test in PrintAsObjC.

  auto boundGeneric = T->getAs<BoundGenericType>();
  if (!boundGeneric)
    return false;

  // Array<T> is representable when T is bridged to Objective-C.
  if (auto arrayDecl = TC.Context.getArrayDecl()) {
    if (boundGeneric->getDecl() == arrayDecl) {
      auto elementType = boundGeneric->getGenericArgs()[0];
      return isElementRepresentableInObjC(TC, DC, elementType);
    }
  }

  // Dictionary<K, V> is representable when K and V are bridged to Objective-C.
  if (auto dictDecl = TC.Context.getDictionaryDecl()) {
    if (boundGeneric->getDecl() == dictDecl) {
      // The key type must be bridged to Objective-C.
      auto keyType = boundGeneric->getGenericArgs()[0];
      if (!isElementRepresentableInObjC(TC, DC, keyType))
        return false;

      // The value type must be bridged to Objective-C.
      auto valueType = boundGeneric->getGenericArgs()[1];
      if (!isElementRepresentableInObjC(TC, DC, valueType))
        return false;

      return true;
    }
  }

  // Set<T> is representable when T is bridged to Objective-C.
  if (auto setDecl = TC.Context.getSetDecl()) {
    if (boundGeneric->getDecl() == setDecl) {
      auto elementType = boundGeneric->getGenericArgs()[0];
      return isElementRepresentableInObjC(TC, DC, elementType);
    }
  }

  return false;
}

bool TypeChecker::isRepresentableInObjC(const DeclContext *DC, Type T) {
  // If you change this function, you must add or modify a test in PrintAsObjC.

  if (isTriviallyRepresentableInObjC(DC, T))
    return true;

  // Look through one level of optional type, but remember that we did.
  bool wasOptional = false;
  if (auto valueType = T->getAnyOptionalObjectType()) {
    T = valueType;
    wasOptional = true;
  }

  if (auto FT = T->getAs<FunctionType>()) {
    switch (FT->getRepresentation()) {
    case AnyFunctionType::Representation::Thin:
      return false;
    case AnyFunctionType::Representation::Swift:
    case AnyFunctionType::Representation::Block:
    case AnyFunctionType::Representation::CFunctionPointer:
      break;
    }
    
    Type Input = FT->getInput();
    if (auto InputTuple = Input->getAs<TupleType>()) {
      for (auto &Elt : InputTuple->getElements()) {
        if (Elt.isVararg())
          return false;
        if (!isRepresentableInObjC(DC, Elt.getType()))
          return false;
      }
    } else if (!isRepresentableInObjC(DC, Input)) {
      return false;
    }

    Type Result = FT->getResult();
    if (!Result->isVoid() && !isRepresentableInObjC(DC, Result))
      return false;

    if (FT->getExtInfo().throws())
      return false;

    return true;
  }

  if (isObjCRepresentableCollection(*this, DC, T))
    return true;

  // Check to see if this is a bridged type.  Note that some bridged
  // types are representable, but their optional type is not.
  fillObjCRepresentableTypeCache(DC);
  auto iter = ObjCRepresentableTypes.find(T->getCanonicalType());
  if (iter != ObjCRepresentableTypes.end()) {
    if (wasOptional && !iter->second)
      return false;
    return true;
  }

  return false;
}

void TypeChecker::diagnoseTypeNotRepresentableInObjC(const DeclContext *DC,
                                                     Type T,
                                                     SourceRange TypeRange) {
  // Special diagnostic for tuples.
  if (T->is<TupleType>()) {
    if (T->isVoid())
      diagnose(TypeRange.Start, diag::not_objc_empty_tuple)
          .highlight(TypeRange);
    else
      diagnose(TypeRange.Start, diag::not_objc_tuple)
          .highlight(TypeRange);
    return;
  }

  // Special diagnostic for classes.
  if (auto *CT = T->getAs<ClassType>()) {
    if (!CT->getDecl()->isObjC())
      diagnose(TypeRange.Start, diag::not_objc_swift_class)
          .highlight(TypeRange);
    return;
  }

  // Special diagnostic for structs.
  if (T->is<StructType>()) {
    diagnose(TypeRange.Start, diag::not_objc_swift_struct)
        .highlight(TypeRange);
    return;
  }

  // Special diagnostic for enums.
  if (T->is<EnumType>()) {
    diagnose(TypeRange.Start, diag::not_objc_swift_enum)
        .highlight(TypeRange);
    return;
  }

  // Special diagnostic for protocols and protocol compositions.
  SmallVector<ProtocolDecl *, 4> Protocols;
  if (T->isExistentialType(Protocols)) {
    if (Protocols.empty()) {
      // protocol<> is not @objc.
      diagnose(TypeRange.Start, diag::not_objc_empty_protocol_composition);
      return;
    }
    // Find a protocol that is not @objc.
    for (auto PD : Protocols) {
      if (!PD->isObjC()) {
        diagnose(TypeRange.Start, diag::not_objc_protocol,
                 PD->getDeclaredType());
        return;
      }
    }
    return;
  }

  if (T->is<ArchetypeType>()) {
    diagnose(TypeRange.Start, diag::not_objc_generic_type_param)
        .highlight(TypeRange);
    return;
  }

  if (auto fnTy = T->getAs<FunctionType>()) {
    if (fnTy->getExtInfo().throws() ) {
      diagnose(TypeRange.Start, diag::not_objc_function_type_throwing)
        .highlight(TypeRange);
      return;
    }

    diagnose(TypeRange.Start, diag::not_objc_function_type_param)
      .highlight(TypeRange);
    return;
  }
}

static void
lookupAndAddRepresentableType(TypeChecker &TC,
                              Module *stdlib,
                              Identifier nativeName,
                              bool isOptionalRepresentable,
                              llvm::DenseMap<CanType, bool> &types) {
  auto nativeType = lookupUniqueTypeInLibrary(TC, stdlib, nativeName);
  if (!nativeType) return;

  types.insert({nativeType, isOptionalRepresentable});
}

void TypeChecker::fillObjCRepresentableTypeCache(const DeclContext *DC) {
  if (!ObjCMappedTypes.empty())
    return;

  SmallVector<Identifier, 32> StdlibTypeNames;

  StdlibTypeNames.push_back(Context.getIdentifier("COpaquePointer"));
#define MAP_BUILTIN_TYPE(CLANG_BUILTIN_KIND, SWIFT_TYPE_NAME) \
  StdlibTypeNames.push_back(Context.getIdentifier(#SWIFT_TYPE_NAME));
#include "swift/ClangImporter/BuiltinMappedTypes.def"

  Module *Stdlib = getStdlibModule(DC);
  lookupAndAddLibraryTypes(*this, Stdlib, StdlibTypeNames, ObjCMappedTypes);

  StdlibTypeNames.clear();
#define MAP_BUILTIN_TYPE(_, __)
#define MAP_BUILTIN_INTEGER_TYPE(CLANG_BUILTIN_KIND, SWIFT_TYPE_NAME) \
  StdlibTypeNames.push_back(Context.getIdentifier(#SWIFT_TYPE_NAME));
#include "swift/ClangImporter/BuiltinMappedTypes.def"
  lookupAndAddLibraryTypes(*this, Stdlib, StdlibTypeNames, CIntegerTypes);

#define BRIDGE_TYPE(BRIDGED_MODULE, BRIDGED_TYPE,                          \
                    NATIVE_MODULE, NATIVE_TYPE, OPTIONAL_IS_BRIDGED)       \
  if (Context.getIdentifier(#NATIVE_MODULE) == Context.StdlibModuleName) { \
    lookupAndAddRepresentableType(*this, Stdlib,                           \
                                  Context.getIdentifier(#NATIVE_TYPE),     \
                                  OPTIONAL_IS_BRIDGED,                     \
                                  ObjCRepresentableTypes);                 \
  }
#include "swift/SIL/BridgedTypes.def"

  Identifier ID_Darwin = Context.Id_Darwin;
  if (auto DarwinModule = Context.getLoadedModule(ID_Darwin)) {
    StdlibTypeNames.clear();
    StdlibTypeNames.push_back(Context.getIdentifier("DarwinBoolean"));
    lookupAndAddLibraryTypes(*this, DarwinModule, StdlibTypeNames,
                             ObjCMappedTypes);
  }

  Identifier ID_ObjectiveC = Context.Id_ObjectiveC;
  if (auto ObjCModule = Context.getLoadedModule(ID_ObjectiveC)) {
    StdlibTypeNames.clear();
    StdlibTypeNames.push_back(Context.getIdentifier("Selector"));
    StdlibTypeNames.push_back(Context.getIdentifier("ObjCBool"));
    StdlibTypeNames.push_back(Context.getIdentifier("NSZone"));
    lookupAndAddLibraryTypes(*this, ObjCModule, StdlibTypeNames,
                             ObjCMappedTypes);
  }

  Identifier ID_CoreGraphics = Context.getIdentifier("CoreGraphics");
  if (auto CoreGraphicsModule = Context.getLoadedModule(ID_CoreGraphics)) {
    StdlibTypeNames.clear();
    StdlibTypeNames.push_back(Context.getIdentifier("CGFloat"));
    lookupAndAddLibraryTypes(*this, CoreGraphicsModule, StdlibTypeNames,
                             ObjCMappedTypes);
  }

  Identifier ID_Foundation = Context.Id_Foundation;
  if (auto FoundationModule = Context.getLoadedModule(ID_Foundation)) {
    StdlibTypeNames.clear();
    StdlibTypeNames.push_back(Context.getIdentifier("NSErrorPointer"));
    lookupAndAddLibraryTypes(*this, FoundationModule, StdlibTypeNames,
                             ObjCMappedTypes);
  }
  
  // Pull SIMD types of size 2...4 from the SIMD module, if it exists.
  Identifier ID_SIMD = Context.Id_simd;
  if (auto SIMDModule = Context.getLoadedModule(ID_SIMD)) {
    StdlibTypeNames.clear();
#define MAP_SIMD_TYPE(BASENAME, __)                                      \
    {                                                                    \
      char name[] = #BASENAME "0";                                       \
      for (unsigned i = 2; i <= SWIFT_MAX_IMPORTED_SIMD_ELEMENTS; ++i) { \
        *(std::end(name) - 2) = '0' + i;                                 \
        StdlibTypeNames.push_back(Context.getIdentifier(name));          \
      }                                                                  \
    }
#include "swift/ClangImporter/SIMDMappedTypes.def"
    lookupAndAddLibraryTypes(*this, SIMDModule, StdlibTypeNames,
                             ObjCMappedTypes);
  }
}

namespace {

class UnsupportedProtocolVisitor
  : public TypeReprVisitor<UnsupportedProtocolVisitor>, public ASTWalker
{
  TypeChecker &TC;
  SmallPtrSet<ProtocolDecl *, 4> Diagnosed;

public:
  UnsupportedProtocolVisitor(TypeChecker &tc) : TC(tc) { }

  SmallPtrSet<ProtocolDecl *, 4> &getDiagnosedProtocols() { return Diagnosed; }

  bool walkToTypeReprPre(TypeRepr *T) {
    visit(T);
    return true;
  }

  void visitIdentTypeRepr(IdentTypeRepr *T) {
    auto comp = T->getComponentRange().back();
    if (auto proto = dyn_cast_or_null<ProtocolDecl>(comp->getBoundDecl())) {
      if (!proto->existentialTypeSupported(&TC)) {
        TC.diagnose(comp->getIdLoc(), diag::unsupported_existential_type,
                    proto->getName());
        Diagnosed.insert(proto);
      }

      return;
    }
  }
};

}

void TypeChecker::checkUnsupportedProtocolType(Decl *decl) {
  if (!decl || decl->isInvalid())
    return;

  // Global type aliases are okay.
  if (isa<TypeAliasDecl>(decl) &&
      decl->getDeclContext()->isModuleScopeContext())
    return;

  // Non-typealias type declarations are okay.
  if (isa<TypeDecl>(decl) && !isa<TypeAliasDecl>(decl))
    return;

  // Extensions are okay.
  if (isa<ExtensionDecl>(decl))
    return;

  UnsupportedProtocolVisitor visitor(*this);
  decl->walk(visitor);
  if (auto valueDecl = dyn_cast<ValueDecl>(decl)) {
    if (auto type = valueDecl->getType()) {
      type.findIf([&](Type type) -> bool {
        SmallVector<ProtocolDecl*, 2> protocols;
        if (type->isExistentialType(protocols)) {
          for (auto *proto : protocols) {
            if (proto->existentialTypeSupported(this))
              continue;

            if (visitor.getDiagnosedProtocols().insert(proto).second) {
              diagnose(valueDecl->getLoc(),
                       diag::unsupported_existential_type,
                       proto->getName());
            }
          }
        }

        return false;
      });
    }
  }
}

void TypeChecker::checkUnsupportedProtocolType(Stmt *stmt) {
  if (!stmt)
    return;

  UnsupportedProtocolVisitor visitor(*this);
  stmt->walk(visitor);
}
