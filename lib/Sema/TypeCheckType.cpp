//===--- TypeCheckType.cpp - Type Validation ------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
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
#include "swift/AST/ASTWalker.h"
#include "swift/AST/ExprHandle.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/TypeLoc.h"
#include "swift/Basic/SourceManager.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/SmallString.h"
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

Type TypeChecker::getOptionalType(SourceLoc loc, Type elementType) {
  if (!Context.getOptionalDecl()) {
    diagnose(loc, diag::sugar_type_not_found, 1);
    return Type();
  }

  return OptionalType::get(elementType);
}

Type TypeChecker::resolveTypeInContext(TypeDecl *typeDecl,
                                       DeclContext *fromDC,
                                       bool isSpecialized,
                                       GenericTypeResolver *resolver) {
  PartialGenericTypeToArchetypeResolver defaultResolver(*this);
  if (!resolver)
    resolver = &defaultResolver;

  // If we found a generic parameter, map to the archetype if there is one.
  if (auto genericParam = dyn_cast<GenericTypeParamDecl>(typeDecl)) {
    return resolver->resolveGenericTypeParamType(
             genericParam->getDeclaredType()->castTo<GenericTypeParamType>());
  }

  // If we're referring to a generic type and no generic arguments have been
  // provided, and we are in the context of that generic type or one of its
  // extensions, imply the generic arguments
  if (auto nominal = dyn_cast<NominalTypeDecl>(typeDecl)) {
    
    // Force any delayed members added to the nominal type declaration.
    if (nominal->hasDelayedProtocolDecls() ||
        nominal->hasDelayedMemberDecls()) {
      nominal->forceDelayed();
    }
    
    if (nominal->hasDelayedMembers()) {
      // Because this is a component reference, we need to resolve the
      // external decl that represents the LHS.
      handleExternalDecl(nominal);
      
      // The nominal type declaration is now fully realized.
      nominal->setHasDelayedMembers(false);
    }
    
    if (nominal->getGenericParams() && !isSpecialized) {
      for (DeclContext *dc = fromDC; dc; dc = dc->getParent()) {
        switch (dc->getContextKind()) {
        case DeclContextKind::Module:
        case DeclContextKind::FileUnit:
        case DeclContextKind::TopLevelCodeDecl:
        case DeclContextKind::Initializer:
          break;

        case DeclContextKind::NominalTypeDecl:
          // If this is our nominal type, return its type within its context.
          if (cast<NominalTypeDecl>(dc) == nominal)
            return resolver->resolveTypeOfContext(nominal);
          continue;
            
        case DeclContextKind::ExtensionDecl:
          // If this is an extension of our nominal type, return the type
          // within the context of its extension.
          if (cast<ExtensionDecl>(dc)->getExtendedType()->getAnyNominal()
                == nominal)
            return resolver->resolveTypeOfContext(dc);
          continue;

        case DeclContextKind::AbstractClosureExpr:
        case DeclContextKind::AbstractFunctionDecl:
          continue;
        }

        break;
      }
    }
  }

  // If the type declaration itself is in a non-type context, no type
  // substitution is needed.
  DeclContext *ownerDC = typeDecl->getDeclContext();
  if (!ownerDC->isTypeContext()) {
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
  if (isa<AssociatedTypeDecl>(typeDecl)) {
    // If we found an associated type from within its protocol, resolve it
    // as a dependent member relative to Self if Self is still dependent.
    if (auto proto = dyn_cast<ProtocolDecl>(fromDC)) {
      auto selfTy
        = proto->getSelf()->getDeclaredType()->castTo<GenericTypeParamType>();
      auto baseTy = resolver->resolveGenericTypeParamType(selfTy);

      if (baseTy->isDependentType()) {
        return resolver->resolveDependentMemberType(baseTy, fromDC,
                                                    SourceRange(),
                                                    typeDecl->getName(),
                                                    SourceLoc());
      }
    }

    if (typeDecl->getDeclContext() != fromDC) {
      if (auto fromProto = dyn_cast<ProtocolDecl>(fromDC)) {
        return substMemberTypeWithBase(fromDC->getParentModule(),
                                       typeDecl->getDeclaredType(), typeDecl,
                                       fromProto->getSelf()->getArchetype());
      }
    }
  }

  // Walk up through the type scopes to find the context where the type
  // declaration was found. When we find it, substitute the appropriate base
  // type.
  Type ownerType = resolver->resolveTypeOfContext(ownerDC);
  auto ownerNominal = ownerType->getAnyNominal();
  assert(ownerNominal && "Owner must be a nominal type");
  for (; !fromDC->isModuleContext(); fromDC = fromDC->getParent()) {
    // Skip non-type contexts.
    if (!fromDC->isTypeContext())
      continue;

    // Search the type of this context and its supertypes.
    for (auto fromType = resolver->resolveTypeOfContext(fromDC);
         fromType;
         fromType = getSuperClassOf(fromType)) {
      // If the nominal type declaration of the context type we're looking at
      // matches the owner's nominal type declaration, this is how we found
      // the member type declaration. Substitute the type we're coming from as
      // the base of the member type to produce the projected type result.
      if (fromType->getAnyNominal() == ownerNominal) {
        return substMemberTypeWithBase(fromDC->getParentModule(),
                                       typeDecl->getDeclaredType(), typeDecl,
                                       fromType);
      }
    }
  }

  llvm_unreachable("Shouldn't have found this type");
}

/// Apply generic arguments to the given type.
Type TypeChecker::applyGenericArguments(Type type,
                                        SourceLoc loc,
                                        DeclContext *dc,
                                        MutableArrayRef<TypeLoc> genericArgs,
                                        GenericTypeResolver *resolver) {
  // Make sure we always have a resolver to use.
  PartialGenericTypeToArchetypeResolver defaultResolver(*this);
  if (!resolver)
    resolver = &defaultResolver;

  auto unbound = type->getAs<UnboundGenericType>();
  if (!unbound) {
    // FIXME: Highlight generic arguments and introduce a Fix-It to remove
    // them.
    diagnose(loc, diag::not_a_generic_type, type);

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

  // Validate the generic arguments and capture just the types.
  SmallVector<Type, 4> genericArgTypes;
  for (auto &genericArg : genericArgs) {
    // Validate the generic argument.
    if (validateType(genericArg, dc, None, resolver))
      return nullptr;

    genericArgTypes.push_back(genericArg.getType());
  }

  // Form the bound generic type
  BoundGenericType *BGT = BoundGenericType::get(unbound->getDecl(),
                                                unbound->getParent(),
                                                genericArgTypes);
  // Check protocol conformance.
  if (!BGT->isDependentType()) {
    // FIXME: Record that we're checking substitutions, so we can't end up
    // with infinite recursion.
    TypeSubstitutionMap Substitutions;
    ConformanceMap Conformance;
    unsigned Index = 0;
    for (Type Arg : BGT->getGenericArgs()) {
      auto GP = genericParams->getParams()[Index++];
      auto Archetype = GP.getAsTypeParam()->getArchetype();
      Substitutions[Archetype] = Arg;
    }

    if (checkSubstitutions(Substitutions, Conformance, dc, loc))
      return nullptr;
  }

  return BGT;
}

static Type applyGenericTypeReprArgs(TypeChecker &TC, Type type, SourceLoc loc,
                                     DeclContext *dc,
                                     ArrayRef<TypeRepr *> genericArgs,
                                     GenericTypeResolver *resolver) {
  SmallVector<TypeLoc, 8> args;
  for (auto tyR : genericArgs)
    args.push_back(tyR);
  Type ty = TC.applyGenericArguments(type, loc, dc, args, resolver);
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
                            bool allowUnboundGenerics,
                            GenericTypeResolver *resolver) {
  TC.validateDecl(typeDecl);

  Type type;
  if (dc) {
    // Resolve the type declaration to a specific type. How this occurs
    // depends on the current context and where the type was found.
    type = TC.resolveTypeInContext(typeDecl, dc, !genericArgs.empty(),
                                   resolver);
  } else {
    type = typeDecl->getDeclaredType();
  }

  if (type->is<UnboundGenericType>() &&
      genericArgs.empty() && !allowUnboundGenerics) {
    diagnoseUnboundGenericType(TC, type, loc);
    return ErrorType::get(TC.Context);
  }

  // If we found a generic parameter, try to resolve it.
  if (auto genericParam = type->getAs<GenericTypeParamType>()) {
    type = resolver->resolveGenericTypeParamType(genericParam);
  }

  if (!genericArgs.empty()) {
    // Apply the generic arguments to the type.
    type = applyGenericTypeReprArgs(TC, type, loc, dc, genericArgs, resolver);
  }

  assert(type);
  return type;
}

/// Retrieve the nearest enclosing nominal type context.
static NominalTypeDecl *getEnclosingNominalContext(DeclContext *dc) {
  while (dc->isLocalContext())
    dc = dc->getParent();

  if (dc->isTypeContext())
    return dc->getDeclaredTypeOfContext()->getAnyNominal();

  return nullptr;
}

/// Diagnose a reference to an unknown type.
///
/// This routine diagnoses a reference to an unknown type, and
/// attempts to fix the reference via various means.
///
/// \param tc The type checker through which we should emit the diagnostic.
/// \param dc The context in which name lookup occurred.
/// \param components The components that refer to the type, where the last
/// component refers to the type that could not be found.
///
/// \returns true if we could not fix the type reference, false if
/// typo correction (or some other mechanism) was able to fix the
/// reference.
static bool diagnoseUnknownType(TypeChecker &tc, DeclContext *dc,
                                ArrayRef<ComponentIdentTypeRepr *> components,
                                GenericTypeResolver *resolver) {
  auto comp = components.back();

  // Unqualified lookup case.
  if (components.size() == 1) {
    // Attempt to refer to 'Self' within a non-protocol nominal
    // type. Fix this by replacing 'Self' with the nominal type name.
    NominalTypeDecl *nominal = nullptr;
    if (comp->getIdentifier() == tc.Context.Id_Self &&
        !isa<GenericIdentTypeRepr>(comp) &&
        (nominal = getEnclosingNominalContext(dc))) {
      // Retrieve the nominal type and resolve it within this context.
      assert(!isa<ProtocolDecl>(nominal) && "Cannot be a protocol");
      auto type = resolveTypeDecl(tc, nominal, comp->getIdLoc(), dc, { },
                                  /*allowUnboundGenerics=*/false, resolver);
      if (type->is<ErrorType>())
        return true;

      // Produce a Fix-It replacing 'Self' with the nominal type name.
      tc.diagnose(comp->getIdLoc(), diag::self_in_nominal, nominal->getName())
        .fixItReplace(comp->getIdLoc(), nominal->getName().str());
      comp->overwriteIdentifier(nominal->getName());
      comp->setValue(type);
      return false;
    }
    
    // Fallback.
    SourceLoc L = comp->getIdLoc();
    SourceRange R = SourceRange(comp->getIdLoc(),
                                components.back()->getIdLoc());

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
      comp->setValue(I->second);

      // HACK: 'NSUInteger' suggests both 'UInt' and 'Int'.
      if (TypeName == "NSUInteger") {
        tc.diagnose(L, diag::note_remapped_type, "UInt")
          .fixItReplace(R, "UInt");
      }
    }
    else {
      tc.diagnose(L, diag::use_undeclared_type,
                  comp->getIdentifier())
      .highlight(R);
    }

    return true;
  }

  // Qualified lookup case.
  auto parentComponents = components.slice(0, components.size()-1);
  auto parentRange = SourceRange(parentComponents.front()->getStartLoc(),
                                 parentComponents.back()->getEndLoc());

  // Lookup into a type.
  if (auto parentType = parentComponents.back()->getBoundType()) {
    tc.diagnose(comp->getIdLoc(), diag::invalid_member_type,
                comp->getIdentifier(), parentType)
      .highlight(parentRange);

    return true;
  }

  /// Lookup into a module.
  auto module = parentComponents.back()->getBoundModule();
  assert(module && "Unresolved parent component?");
  tc.diagnose(comp->getIdLoc(), diag::no_module_type,
              comp->getIdentifier(), module->Name);
  return true;
}

static llvm::PointerUnion<Type, Module *>
resolveIdentTypeComponent(TypeChecker &TC, DeclContext *DC,
                          ArrayRef<ComponentIdentTypeRepr *> components,
                          TypeResolutionOptions options,
                          bool diagnoseErrors,
                          GenericTypeResolver *resolver) {
  auto &comp = components.back();
  if (!comp->isBound()) {
    auto parentComps = components.slice(0, components.size()-1);
    if (parentComps.empty()) {
      // Resolve the first component, which is the only one that requires
      // unqualified name lookup.
      UnqualifiedLookup Globals(comp->getIdentifier(), DC, &TC,comp->getIdLoc(),
                                /*TypeLookup*/true);

      // Process the names we found.
      llvm::PointerUnion<Type, Module *> current;
      bool isAmbiguous = false;
      for (const auto &result : Globals.Results) {
        // If we found a module, record it.
        if (result.Kind == UnqualifiedLookupResult::ModuleName) {
          // If we already found a name of some sort, it's ambiguous.
          if (!current.isNull()) {
            isAmbiguous = true;
            break;
          }

          // Save this result.
          current = result.getNamedModule();
          comp->setValue(result.getNamedModule());
          continue;
        }

        // Ignore non-type declarations.
        auto typeDecl = dyn_cast<TypeDecl>(result.getValueDecl());
        if (!typeDecl)
          continue;
        
        // If necessary, add delayed members to the declaration.
        if (auto nomDecl = dyn_cast<NominalTypeDecl>(typeDecl)) {
          if (nomDecl->hasDelayedProtocolDecls() ||
              nomDecl->hasDelayedMemberDecls()) {
            nomDecl->forceDelayed();
          }
          
          if (nomDecl->hasDelayedMembers()) {
            TC.handleExternalDecl(nomDecl);
            
            nomDecl->setHasDelayedMembers(false);
          }
        }

        ArrayRef<TypeRepr *> genericArgs;
        if (auto genComp = dyn_cast<GenericIdentTypeRepr>(comp))
          genericArgs = genComp->getGenericArgs();
        Type type = resolveTypeDecl(TC, typeDecl, comp->getIdLoc(),
                                    DC, genericArgs,
                                    options.contains(TR_AllowUnboundGenerics),
                                    resolver);
        if (type->is<ErrorType>()) {
          comp->setValue(type);
          return type;
        }

        // If this is the first result we found, record it.
        if (current.isNull()) {
          current = type;
          comp->setValue(type);
          continue;
        }

        // Otherwise, check for an ambiguity.
        if (current.is<Module *>() || !current.get<Type>()->isEqual(type)) {
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
            .highlight(SourceRange(comp->getIdLoc(),
                                   components.back()->getIdLoc()));
          for (auto Result : Globals.Results) {
            if (Globals.Results[0].hasValueDecl())
              TC.diagnose(Result.getValueDecl(), diag::found_candidate);
            else
              TC.diagnose(comp->getIdLoc(), diag::found_candidate);
          }
        }
        Type ty = ErrorType::get(TC.Context);
        comp->setValue(ty);
        return ty;
      }

      // If we found nothing, complain and give ourselves a chance to recover.
      if (current.isNull()) {
        // If we're not allowed to complain or we couldn't fix the
        // source, bail out.
        if (!diagnoseErrors || 
            diagnoseUnknownType(TC, DC, components, resolver)) {
          Type ty = ErrorType::get(TC.Context);
          comp->setValue(ty);
          return ty;
        }
      }
    } else {
      llvm::PointerUnion<Type, Module *>
        parent = resolveIdentTypeComponent(TC, DC, parentComps, options,
                                           diagnoseErrors, resolver);
      // If the last resolved component is a type, perform member type lookup.
      if (parent.is<Type>()) {
        // FIXME: Want the end of the back range.
        SourceRange parentRange(parentComps.front()->getIdLoc(),
                                parentComps.back()->getIdLoc());

        auto parentTy = parent.get<Type>();
        if (parentTy->is<ErrorType>())
          return parent.get<Type>();
        
        // If the parent is a dependent type, the member is a dependent member.
        if (parentTy->is<DependentMemberType>() ||
            parentTy->is<GenericTypeParamType>()) {
          
          // Try to resolve the dependent member type to a specific associated
          // type.
          Type memberType = resolver->resolveDependentMemberType(
                                        parentTy, DC,
                                        parentRange,
                                        comp->getIdentifier(),
                                        comp->getIdLoc());
          
          
          assert(memberType && "Received null dependent member type");

          if (isa<GenericIdentTypeRepr>(comp) && !memberType->is<ErrorType>()) {
            // FIXME: Highlight generic arguments and introduce a Fix-It to
            // remove them.
            if (diagnoseErrors)
              TC.diagnose(comp->getIdLoc(), diag::not_a_generic_type, memberType);

            // Drop the arguments.
          }

          comp->setValue(memberType);
          return memberType;
        }
        
        // Look for member types with the given name.
        auto memberTypes = TC.lookupMemberType(parentTy, comp->getIdentifier(),
                                               DC);

        // Name lookup was ambiguous. Complain.
        // FIXME: Could try to apply generic arguments first, and see whether
        // that resolves things. But do we really want that to succeed?
        if (memberTypes.size() > 1) {
          if (diagnoseErrors)
            TC.diagnoseAmbiguousMemberType(parent.get<Type>(),
                                           parentRange,
                                           comp->getIdentifier(),
                                           comp->getIdLoc(),
                                           memberTypes);
          Type ty = ErrorType::get(TC.Context);
          comp->setValue(ty);
          return ty;
        }

        // If we didn't find anything, complain.
        bool recovered = false;
        if (!memberTypes) {
          // If we're not allowed to complain or we couldn't fix the
          // source, bail out.
          if (!diagnoseErrors || 
              diagnoseUnknownType(TC, DC, components, resolver)) {
            Type ty = ErrorType::get(TC.Context);
            comp->setValue(ty);
            return ty;
          }

          recovered = true;
        }

        if (parentTy->isExistentialType()) {
          TC.diagnose(comp->getIdLoc(), diag::assoc_type_outside_of_protocol,
                      comp->getIdentifier());
          Type ty = ErrorType::get(TC.Context);
          comp->setValue(ty);
          return ty;
        }

        auto memberType = recovered? comp->getBoundType() 
                                   : memberTypes.back().second;

        // If there are generic arguments, apply them now.
        if (auto genComp = dyn_cast<GenericIdentTypeRepr>(comp))
          memberType = applyGenericTypeReprArgs(TC, memberType,
                                                genComp->getIdLoc(),
                                                DC, genComp->getGenericArgs(),
                                                resolver);

        comp->setValue(memberType);
        return memberType;
      }

      // Lookup into a module.
      auto module = parent.get<Module *>();
      LookupTypeResult foundModuleTypes =
        TC.lookupMemberType(ModuleType::get(module), comp->getIdentifier(), DC);

      // If lookup was ambiguous, complain.
      if (foundModuleTypes.isAmbiguous()) {
        if (diagnoseErrors) {
          TC.diagnose(comp->getIdLoc(), diag::ambiguous_module_type,
                      comp->getIdentifier(), module->Name);
          for (auto foundType : foundModuleTypes) {
            // Only consider type declarations.
            auto typeDecl = foundType.first;
            if (!typeDecl)
              continue;

            TC.diagnose(typeDecl, diag::found_candidate_type,
                        typeDecl->getDeclaredType());
          }
        }
        Type ty = ErrorType::get(TC.Context);
        comp->setValue(ty);
        return ty;
      }

      // If we didn't find a type, complain.
      bool recovered = false;
      if (!foundModuleTypes) {
        if (!diagnoseErrors || 
            diagnoseUnknownType(TC, DC, components, resolver)) {
          Type ty = ErrorType::get(TC.Context);
          comp->setValue(ty);
          return ty;
        }

        recovered = true;
      }

      Type foundType = recovered? comp->getBoundType()
                                : foundModuleTypes[0].second;

      // If there are generic arguments, apply them now.
      if (auto genComp = dyn_cast<GenericIdentTypeRepr>(comp)) {
        foundType = applyGenericTypeReprArgs(TC, foundType, genComp->getIdLoc(),
                                             DC, genComp->getGenericArgs(),
                                             resolver);
      }

      comp->setValue(foundType);
    }
  }

  assert(comp->isBound());
  if (Type ty = comp->getBoundType())
    return ty;
  if (Module *mod = comp->getBoundModule())
    return mod;

  ValueDecl *VD = comp->getBoundDecl();
  auto typeDecl = dyn_cast<TypeDecl>(VD);
  if (!typeDecl) {
    if (diagnoseErrors) {
      TC.diagnose(comp->getIdLoc(), diag::use_non_type_value, VD->getName());
      TC.diagnose(VD, diag::use_non_type_value_prev, VD->getName());
    }
    Type ty = ErrorType::get(TC.Context);
    comp->setValue(ty);
    return ty;
  }

  ArrayRef<TypeRepr *> genericArgs;
  if (auto genComp = dyn_cast<GenericIdentTypeRepr>(comp))
    genericArgs = genComp->getGenericArgs();

  Type type = resolveTypeDecl(TC, typeDecl, comp->getIdLoc(), DC,
                              genericArgs, 
                              options.contains(TR_AllowUnboundGenerics),
                              resolver);
  comp->setValue(type);
  return type;
}

static bool diagnoseAvailability(DeclContext *DC, Type ty,
                                 IdentTypeRepr *IdType,
                                 SourceLoc Loc,
                                 TypeChecker &TC) {
  CanType canTy = ty.getCanonicalTypeOrNull();
  if (canTy.isNull())
    return false;
  auto D = canTy.getAnyNominal();
  if (!D)
    return false;
  for (auto Attr : D->getAttrs()) {
    if (auto AvAttr = dyn_cast<AvailabilityAttr>(Attr)) {
      // FIXME: unify this checking with declarations.
      if (AvAttr->hasPlatform())
        continue;
      if (AvAttr->IsUnvailable) {
        if (auto CI = dyn_cast<ComponentIdentTypeRepr>(IdType)) {
          StringRef Message = AvAttr->Message;
          if (Message.empty()) {
            TC.diagnose(Loc, diag::availability_decl_unavailable,
                        CI->getIdentifier())
              .highlight(SourceRange(Loc, Loc));
          }
          else {
            TC.diagnose(Loc, diag::availability_decl_unavailable_msg,
                        CI->getIdentifier(), Message)
            .highlight(SourceRange(Loc, Loc));
          }
          return true;
        }
      }
    }
  }
  return false;
}

/// \brief Returns a valid type or ErrorType in case of an error.
Type TypeChecker::resolveIdentifierType(DeclContext *DC,
                                        IdentTypeRepr *IdType,
                                        TypeResolutionOptions options,
                                        bool diagnoseErrors,
                                        GenericTypeResolver *resolver) {
  assert(resolver && "Missing generic type resolver");

  auto ComponentRange = IdType->getComponentRange();
  auto Components = llvm::makeArrayRef(ComponentRange.begin(),
                                       ComponentRange.end());
  llvm::PointerUnion<Type, Module *>
    result = resolveIdentTypeComponent(*this, DC, Components, options, 
                                       diagnoseErrors, resolver);
  if (auto mod = result.dyn_cast<Module*>()) {
    if (diagnoseErrors)
      diagnose(Components.back()->getIdLoc(),
               diag::use_module_as_type, mod->Name);
    Type ty = ErrorType::get(Context);
    Components.back()->setValue(ty);
    return ty;
  }

  // Check the availability of the type.
  if (diagnoseAvailability(DC, result.get<Type>(), IdType,
      Components.back()->getIdLoc(), *this)) {
    Type ty = ErrorType::get(Context);
    Components.back()->setValue(ty);
    return ty;
  }

  return result.get<Type>();
}

bool TypeChecker::validateType(TypeLoc &Loc, DeclContext *DC,
                               TypeResolutionOptions options,
                               GenericTypeResolver *resolver) {
  // FIXME: Verify that these aren't circular and infinite size.
  
  // If we've already validated this type, don't do so again.
  if (Loc.wasValidated())
    return Loc.isError();

  if (Loc.getType().isNull()) {
    Loc.setType(resolveType(Loc.getTypeRepr(), DC, options, resolver),
                true);
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

  public:
    TypeResolver(TypeChecker &tc, DeclContext *DC,
                 GenericTypeResolver *resolver)
      : TC(tc), Context(tc.Context), DC(DC), Resolver(resolver) {
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
                                FunctionType::ExtInfo extInfo
                                  = FunctionType::ExtInfo(),
                                ParameterConvention calleeConvention
                                  = DefaultParameterConvention);
    SILParameterInfo resolveSILParameter(TypeRepr *repr,
                                         TypeResolutionOptions options);
    SILResultInfo resolveSILResult(TypeRepr *repr,
                                   TypeResolutionOptions options);
    Type resolveInOutType(InOutTypeRepr *repr,
                          TypeResolutionOptions options);
    Type resolveArrayType(ArrayTypeRepr *repr,
                          TypeResolutionOptions options);
    Type resolveOptionalType(OptionalTypeRepr *repr,
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
                              GenericTypeResolver *resolver) {
  PrettyStackTraceTypeRepr stackTrace(Context, "resolving", TyR);

  // Make sure we always have a resolver to use.
  PartialGenericTypeToArchetypeResolver defaultResolver(*this);
  if (!resolver)
    resolver = &defaultResolver;

  TypeResolver typeResolver(*this, DC, resolver);
  return typeResolver.resolveType(TyR, options);
}

Type TypeResolver::resolveType(TypeRepr *repr, TypeResolutionOptions options) {
  assert(repr && "Cannot validate null TypeReprs!");
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
                                    /*diagnoseErrors*/ true, Resolver);

  case TypeReprKind::Function:
    if (!(options & TR_SILType))
      return resolveASTFunctionType(cast<FunctionTypeRepr>(repr), options);
    return resolveSILFunctionType(cast<FunctionTypeRepr>(repr), options);

  case TypeReprKind::Array:
    return resolveArrayType(cast<ArrayTypeRepr>(repr), options);

  case TypeReprKind::Optional:
    return resolveOptionalType(cast<OptionalTypeRepr>(repr), options);

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
          auto instanceTy = resolveType(base, options);

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

  // @unchecked should only annotate a ? representation.  Remember
  // that we saw this and drill into the OptionalTypeRepr.
  bool isUncheckedOptional = false;
  if (attrs.has(TAK_unchecked)) {
    attrs.clearAttribute(TAK_unchecked);
    if (auto optionalRepr = dyn_cast<OptionalTypeRepr>(repr)) {
      repr = optionalRepr->getBase();
    } else {
      SourceRange range = repr->getSourceRange();
      auto diagnostic = TC.diagnose(range.Start,
                                    diag::unchecked_not_optional_type);
      diagnostic.highlight(range);

      // Add fix-its to add the '?', possibly with parens.
      if (isa<ErrorTypeRepr>(repr)) {
        // suppress fix-it
      } else if (repr->isSimple()) {
        diagnostic.fixItInsert(range.End, "?");
      } else {
        diagnostic.fixItInsert(range.Start, "(");
        diagnostic.fixItInsert(range.End, ")?");
      }
    }
    isUncheckedOptional = true;
  }

  // Pass down the variable function type attributes to the
  // function-type creator.
  static const TypeAttrKind FunctionAttrs[] = {
    TAK_auto_closure, TAK_objc_block, TAK_cc, TAK_thin, TAK_noreturn,
    TAK_callee_owned, TAK_callee_guaranteed
  };

  bool hasFunctionAttr = false;
  for (auto i : FunctionAttrs)
    if (attrs.has(i)) {
      hasFunctionAttr = true;
      break;
    }

  // Function attributes require a syntactic function type.
  FunctionTypeRepr *fnRepr = dyn_cast<FunctionTypeRepr>(repr);
  if (hasFunctionAttr && fnRepr) {
    // auto_closures must take () syntactically.
    if (attrs.has(TAK_auto_closure)) {
      auto input = fnRepr->getArgsTypeRepr();
      auto inputTuple = dyn_cast<TupleTypeRepr>(input);
      if (inputTuple == 0 || !inputTuple->getElements().empty()) {
        TC.diagnose(attrs.getLoc(TAK_auto_closure),
                    diag::autoclosure_function_input_nonunit)
          .highlight(input->getSourceRange());
      }
    }

    // Resolve the function type directly with these attributes.
    FunctionType::ExtInfo extInfo(attrs.hasCC()
                                    ? attrs.getAbstractCC()
                                    : AbstractCC::Freestanding,
                                  attrs.has(TAK_thin),
                                  attrs.has(TAK_noreturn),
                                  attrs.has(TAK_auto_closure),
                                  attrs.has(TAK_objc_block));

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
      ty = resolveSILFunctionType(fnRepr, options, extInfo, calleeConvention);
    } else {
      ty = resolveASTFunctionType(fnRepr, options, extInfo);
    }

    for (auto i : FunctionAttrs)
      attrs.clearAttribute(i);
    attrs.cc = Nothing;

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
  if (ty->is<ErrorType>())
    return ty;

  // Apply @unchecked first.
  if (isUncheckedOptional) {
    ty = UncheckedOptionalType::get(ty);
  }

  // In SIL, handle @sil_self, which extracts the Self type of a protocol.
  if (attrs.has(TAK_sil_self)) {
    if (auto protoTy = ty->getAs<ProtocolType>()) {
      ty = protoTy->getDecl()->getSelf()->getArchetype();
    } else {
      TC.diagnose(attrs.getLoc(TAK_sil_self), diag::sil_self_non_protocol, ty)
        .highlight(repr->getSourceRange());
    }
    attrs.clearAttribute(TAK_sil_self);
  }

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
  if (attrs.hasOwnership() && ty->hasReferenceSemantics()) {
    if (auto SF = DC->getParentSourceFile()) {
      if (SF->Kind == SourceFileKind::SIL) {
        ty = ReferenceStorageType::get(ty, attrs.getOwnership(), Context);
        attrs.clearOwnership();
      }
    }
  }

  // Diagnose @local_storage in nested positions.
  if (attrs.has(TAK_local_storage)) {
    assert(DC->getParentSourceFile()->Kind == SourceFileKind::SIL);
    TC.diagnose(attrs.getLoc(TAK_local_storage),diag::sil_local_storage_nested);
    attrs.clearAttribute(TAK_local_storage);
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
  // Generic types are only first-class in SIL.
  if (auto generics = repr->getGenericParams()) {
    TC.diagnose(generics->getSourceRange().Start,
                diag::first_class_generic_function);
  }

  Type inputTy = resolveType(repr->getArgsTypeRepr(),
                             options | TR_ImmediateFunctionInput);
  if (inputTy->is<ErrorType>())
    return inputTy;
  Type outputTy = resolveType(repr->getResultTypeRepr(),
                              options | TR_FunctionResult);
  if (outputTy->is<ErrorType>())
    return outputTy;
  return FunctionType::get(inputTy, outputTy, extInfo);
}

Type TypeResolver::resolveSILFunctionType(FunctionTypeRepr *repr,
                                          TypeResolutionOptions options,
                                          FunctionType::ExtInfo extInfo,
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
      if (param.getType()->is<ErrorType>())
        hasError = true;
    }
  } else {
    SILParameterInfo param = resolveSILParameter(repr->getArgsTypeRepr(),
                                           options | TR_ImmediateFunctionInput);
    params.push_back(param);
    if (param.getType()->is<ErrorType>())
      hasError = true;
  }

  SILResultInfo result = resolveSILResult(repr->getResultTypeRepr(),
                                          options | TR_FunctionResult);
  if (result.getType()->is<ErrorType>())
    hasError = true;

  if (hasError)
    return ErrorType::get(Context);

  // FIXME: Remap the parsed context types to interface types.
  GenericSignature *genericSig = nullptr;
  SmallVector<SILParameterInfo, 4> interfaceParams;
  SILResultInfo interfaceResult;
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
      CanType interfaceTy = param.getType()
        .transform(getArchetypesAsDependentTypes)
        ->getCanonicalType();
      interfaceParams.push_back(SILParameterInfo(interfaceTy,
                                                 param.getConvention()));
    }
    CanType resultTy
      = result.getType().transform(getArchetypesAsDependentTypes)
                        ->getCanonicalType();
    interfaceResult = SILResultInfo(resultTy, result.getConvention());
  } else {
    interfaceParams = params;
    interfaceResult = result;
  }
  return SILFunctionType::get(genericSig, extInfo,
                              callee,
                              interfaceParams, interfaceResult,
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
    checkFor(TypeAttrKind::TAK_in, ParameterConvention::Indirect_In);
    checkFor(TypeAttrKind::TAK_out, ParameterConvention::Indirect_Out);
    checkFor(TypeAttrKind::TAK_inout, ParameterConvention::Indirect_Inout);
    checkFor(TypeAttrKind::TAK_owned, ParameterConvention::Direct_Owned);
    checkFor(TypeAttrKind::TAK_guaranteed,
             ParameterConvention::Direct_Guaranteed);

    type = resolveAttributedType(attrs, attrRepr->getTypeRepr(), options);
  } else {
    type = resolveType(repr, options);
  }

  if (hadError) type = ErrorType::get(Context);
  return SILParameterInfo(type->getCanonicalType(), convention);
}

SILResultInfo TypeResolver::resolveSILResult(TypeRepr *repr,
                                             TypeResolutionOptions options) {
  assert(options & TR_FunctionResult && "Should be marked as a result");
  auto convention = DefaultResultConvention;
  Type type;
  bool hadError = false;

  if (auto attrRepr = dyn_cast<AttributedTypeRepr>(repr)) {
    auto attrs = attrRepr->getAttrs();
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
    checkFor(TypeAttrKind::TAK_autoreleased, ResultConvention::Autoreleased);

    type = resolveAttributedType(attrs, attrRepr->getTypeRepr(), options);
  } else {
    type = resolveType(repr, options);
  }

  if (hadError) type = ErrorType::get(Context);
  return SILResultInfo(type->getCanonicalType(), convention);
}

Type TypeResolver::resolveInOutType(InOutTypeRepr *repr,
                                    TypeResolutionOptions options) {
  Type ty = resolveType(cast<InOutTypeRepr>(repr)->getBase(), options);
  if (ty->is<ErrorType>())
    return ty;

  if (!(options & TR_FunctionInput) &&
      !(options & TR_ImmediateFunctionInput)) {
    TC.diagnose(repr->getInOutLoc(), diag::inout_only_parameter);
    return ty;
  }
  
  return InOutType::get(ty);
}


Type TypeResolver::resolveArrayType(ArrayTypeRepr *repr,
                                    TypeResolutionOptions options) {
  // FIXME: diagnose non-materializability of element type!
  Type baseTy = resolveType(repr->getBase(), withoutContext(options));
  if (baseTy->is<ErrorType>())
    return baseTy;
  
  if (ExprHandle *sizeEx = repr->getSize()) {
    // FIXME: We don't support fixed-length arrays yet.
    // FIXME: We need to check Size! (It also has to be convertible to int).
    TC.diagnose(repr->getBrackets().Start, diag::unsupported_fixed_length_array)
      .highlight(sizeEx->getExpr()->getSourceRange());
    return ErrorType::get(Context);
  }

  auto sliceTy = TC.getArraySliceType(repr->getBrackets().Start, baseTy);
  if (!sliceTy)
    return ErrorType::get(Context);

  return sliceTy;
}

Type TypeResolver::resolveOptionalType(OptionalTypeRepr *repr,
                                       TypeResolutionOptions options) {
  // The T in T? is a generic type argument and therefore always an AST type.
  // FIXME: diagnose non-materializability of element type!
  Type baseTy = resolveType(repr->getBase(), withoutContext(options));
  if (baseTy->is<ErrorType>())
    return baseTy;

  auto optionalTy = TC.getOptionalType(repr->getQuestionLoc(), baseTy);
  if (!optionalTy)
    return ErrorType::get(Context);

  return optionalTy;
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
      if (ty->is<ErrorType>())
        return ty;
      elements.push_back(TupleTypeElt(ty, namedTyR->getName()));
    } else {
      Type ty = resolveType(tyR, elementOptions);
      if (ty->is<ErrorType>())
        return ty;
      elements.push_back(TupleTypeElt(ty));
    }
  }

  if (repr->hasEllipsis()) {
    Type baseTy = elements.back().getType();
    Type fullTy = TC.getArraySliceType(repr->getEllipsisLoc(), baseTy);
    Identifier name = elements.back().getName();
    // FIXME: Where are we rejecting default arguments for variadic
    // parameters?
    elements.back() = TupleTypeElt(fullTy, name, DefaultArgumentKind::None,
                                   true);
  }

  return TupleType::get(elements, Context);
}

Type TypeResolver::resolveProtocolCompositionType(
                                         ProtocolCompositionTypeRepr *repr,
                                         TypeResolutionOptions options) {
  SmallVector<Type, 4> ProtocolTypes;
  for (auto tyR : repr->getProtocols()) {
    Type ty = TC.resolveType(tyR, DC, withoutContext(options), Resolver);
    if (ty->is<ErrorType>())
      return ty;
    if (!ty->isExistentialType()) {
      TC.diagnose(tyR->getStartLoc(), diag::protocol_composition_not_protocol,
                  ty);
      continue;
    }

    // The special AnyObject protocol can't be part of a protocol
    // composition.
    if (auto protoTy = ty->getAs<ProtocolType>()){
      if (protoTy->getDecl()->isSpecificProtocol(
              KnownProtocolKind::AnyObject)) {
        TC.diagnose(tyR->getStartLoc(),
                    diag::protocol_composition_dynamic_lookup);
        continue;
      }
    }

    ProtocolTypes.push_back(ty);
  }
  return ProtocolCompositionType::get(Context, ProtocolTypes);
}

Type TypeResolver::resolveMetatypeType(MetatypeTypeRepr *repr,
                                       TypeResolutionOptions options) {
  // The instance type of a metatype is always abstract, not SIL-lowered.
  Type ty = resolveType(repr->getBase(), withoutContext(options));
  if (ty->is<ErrorType>())
    return ty;

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

Type TypeResolver::buildMetatypeType(MetatypeTypeRepr *repr,
                                     Type instanceType,
                                     Optional<MetatypeRepresentation> storedRepr) {
  if (instanceType->isExistentialType()) {
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
  if (ty->is<ErrorType>())
    return ty;

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

Type TypeResolver::buildProtocolType(ProtocolTypeRepr *repr,
                                     Type instanceType,
                                     Optional<MetatypeRepresentation> storedRepr) {
  if (!instanceType->isExistentialType()) {
    TC.diagnose(repr->getProtocolLoc(), diag::dot_protocol_on_non_existential);
    return ErrorType::get(TC.Context);
  }

  return MetatypeType::get(instanceType, storedRepr);
}


Type TypeChecker::substType(Module *module, Type type,
                            TypeSubstitutionMap &Substitutions,
                            bool IgnoreMissing) {
  return type.subst(module, Substitutions, IgnoreMissing, this);
}

Type TypeChecker::substMemberTypeWithBase(Module *module, Type T,
                                          ValueDecl *Member, Type BaseTy) {
  if (!BaseTy)
    return T;

  return BaseTy->getTypeOfMember(module, Member, this, T);
}

Type TypeChecker::getSuperClassOf(Type type) {
  return type->getSuperclass(this);
}

Type TypeChecker::resolveMemberType(DeclContext *dc, Type type,
                                    Identifier name) {
  LookupTypeResult memberTypes = lookupMemberType(type, name, dc);
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

static void lookupLibraryTypes(TypeChecker &TC,
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
  if (Reason == ObjCReason::MemberOfObjCProtocol)
    TC.diagnose(VD->getLoc(), diag::objc_inferring_on_objc_protocol_member);
}

static bool isClassOrObjCProtocol(Type T) {
  if (auto *CT = T->getAs<ClassType>())
    return CT->getDecl()->isObjC();

  SmallVector<ProtocolDecl *, 4> Protocols;
  if (T->isExistentialType(Protocols)) {
    if (Protocols.empty()) {
      // protocol<> is not @objc.
      return false;
    }
    // Check that all protocols are @objc.
    for (auto PD : Protocols) {
      if (!PD->isObjC())
        return false;
    }
    return true;
  }

  return false;
}

static Type getFunctionParamType(const Pattern *P) {
  if (auto *TP = dyn_cast<TypedPattern>(P))
    return TP->getType();
  return {};
}

static SourceRange getFunctionParamTypeSourceRange(const Pattern *P) {
  if (auto *TP = dyn_cast<TypedPattern>(P))
    return TP->getTypeLoc().getTypeRepr()->getSourceRange();
  return {};
}

static bool isParamRepresentableInObjC(TypeChecker &TC,
                                       const DeclContext *DC,
                                       const Pattern *P) {
  auto *TP = dyn_cast<TypedPattern>(P);
  if (!TP)
    return false;
  if (!TC.isRepresentableInObjC(DC, TP->getType()))
    return false;
  auto *SubPattern = TP->getSubPattern();
  return isa<NamedPattern>(SubPattern) || isa<AnyPattern>(SubPattern);
}

static void diagnoseFunctionParamNotRepresentable(
    TypeChecker &TC, const AbstractFunctionDecl *AFD, unsigned NumParams,
    unsigned ParamIndex, const Pattern *P, ObjCReason Reason) {
  if (Reason == ObjCReason::DontDiagnose)
    return;

  if (NumParams == 1) {
    TC.diagnose(AFD->getLoc(), diag::objc_invalid_on_func_single_param_type);
  } else {
    TC.diagnose(AFD->getLoc(), diag::objc_invalid_on_func_param_type,
                ParamIndex + 1);
  }
  if (Type ParamTy = getFunctionParamType(P)) {
    SourceRange SR = getFunctionParamTypeSourceRange(P);
    TC.diagnoseTypeNotRepresentableInObjC(AFD, ParamTy, SR);
  }
  describeObjCReason(TC, AFD, Reason);
}

static bool isParamPatternRepresentableInObjC(TypeChecker &TC,
                                              const AbstractFunctionDecl *AFD,
                                              const Pattern *P,
                                              ObjCReason Reason) {
  bool Diagnose = (Reason != ObjCReason::DontDiagnose);
  if (auto *TP = dyn_cast<TuplePattern>(P)) {
    auto Fields = TP->getFields();
    unsigned NumParams = Fields.size();

    if (NumParams == 0)
      return true;

    // Setters on subscripts are allowed to have two arguments, the index and
    // the set value.
    bool isOK = false;
    if (auto *FD = dyn_cast<FuncDecl>(AFD))
      if (NumParams == 2 && FD->getAccessorKind() == AccessorKind::IsSetter &&
          isa<SubscriptDecl>(FD->getAccessorStorageDecl()))
      isOK = true;
    
    if (!isOK && NumParams != 1 && !AFD->hasSelectorStyleSignature()) {
      // If the function has two or more parameters, it should have a
      // selector-style declaration.
      if (Diagnose)
        TC.diagnose(AFD->getLoc(), diag::objc_invalid_on_tuple_style);
      return false;
    }

    bool IsObjC = true;
    for (unsigned ParamIndex = 0; ParamIndex != NumParams; ParamIndex++) {
      auto &TupleElt = Fields[ParamIndex];
      if (!isParamRepresentableInObjC(TC, AFD, TupleElt.getPattern())) {
        IsObjC = false;
        if (!Diagnose) {
          // Save some work and return as soon as possible if we are not
          // producing diagnostics.
          return IsObjC;
        }
        diagnoseFunctionParamNotRepresentable(TC, AFD, NumParams, ParamIndex,
                                              TupleElt.getPattern(), Reason);
      }
    }
    return IsObjC;
  }
  auto *PP = cast<ParenPattern>(P);
  if (!isParamRepresentableInObjC(TC, AFD, PP->getSubPattern())) {
    diagnoseFunctionParamNotRepresentable(TC, AFD, 1, 1, PP->getSubPattern(),
                                          Reason);
    return false;
  }
  return true;
}

/// Check whether the given declaration occurs within a generic context
/// and, therefore, is not representable in Objective-C.
static bool checkObjCInGenericContext(TypeChecker &tc,
                                      const ValueDecl *value,
                                      bool diagnose) {
  // Non-generic contexts are okay.
  auto dc = value->getDeclContext();
  if (!dc->isGenericContext())
    return false;

  // Protocol contexts are okay.
  if (isa<ProtocolDecl>(dc))
    return false;

  // Diagnose this problem, if asked to.
  if (diagnose) {
    int kind = isa<SubscriptDecl>(value)? 3
             : isa<VarDecl>(value)? 2
             : isa<ConstructorDecl>(value)? 1
             : 0;
    tc.diagnose(value->getLoc(), diag::objc_in_generic_context, kind);
  }

  return true;
}

bool TypeChecker::isRepresentableInObjC(const AbstractFunctionDecl *AFD,
                                        ObjCReason Reason) {
  bool Diagnose = (Reason != ObjCReason::DontDiagnose);
  if (auto *FD = dyn_cast<FuncDecl>(AFD)) {
    if (!FD->isGetterOrSetter()) {
      unsigned ExpectedParamPatterns = 1;
      if (FD->getImplicitSelfDecl())
        ExpectedParamPatterns++;
      if (FD->getBodyParamPatterns().size() != ExpectedParamPatterns) {
        if (Diagnose) {
          diagnose(AFD->getLoc(), diag::objc_invalid_on_func_curried);
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

  if (!isParamPatternRepresentableInObjC(*this, AFD,
                                         AFD->getBodyParamPatterns()[1],
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
        diagnose(AFD->getLoc(), diag::objc_invalid_on_func_result_type);
        SourceRange Range =
            FD->getBodyResultTypeLoc().getTypeRepr()->getSourceRange();
        diagnoseTypeNotRepresentableInObjC(FD, ResultType, Range);
        describeObjCReason(*this, FD, Reason);
      }
      return false;
    }
  }

  if (checkObjCInGenericContext(*this, AFD, Diagnose))
    return false;

  return true;
}

bool TypeChecker::isRepresentableInObjC(const VarDecl *VD, ObjCReason Reason) {
  Type T = VD->getType();
  if (auto *RST = T->getAs<ReferenceStorageType>()) {
    // In-memory layout of @weak and @unowned does not correspond to anything
    // in Objective-C, but this does not really matter here, since Objective-C
    // uses getters and setters to operate on the property.
    // Because of this, look through @weak and @unowned.
    T = RST->getReferentType();
  }
  bool Result = isRepresentableInObjC(VD->getDeclContext(), T);
  bool Diagnose = (Reason != ObjCReason::DontDiagnose);

  if (Result && checkObjCInGenericContext(*this, VD, Diagnose))
    return false;

  if (!Diagnose || Result)
    return Result;

  enum {
    DiagnoseAsAtObjC = 0,
    DiagnoseAsIBOutlet = 1
  };

  unsigned AttrKind = (Reason == ObjCReason::ExplicitlyIBOutlet)
                          ? DiagnoseAsIBOutlet
                          : DiagnoseAsAtObjC;
  SourceRange TypeRange = VD->getTypeSourceRangeForDiagnostics();
  diagnose(VD->getLoc(), diag::objc_invalid_on_var, AttrKind)
      .highlight(TypeRange);
  diagnoseTypeNotRepresentableInObjC(VD->getDeclContext(), VD->getType(),
                                     TypeRange);
  describeObjCReason(*this, VD, Reason);

  return Result;
}

bool TypeChecker::isRepresentableInObjC(const SubscriptDecl *SD,
                                        ObjCReason Reason) {
  bool Diagnose = (Reason != ObjCReason::DontDiagnose);

  // Figure out the type of the indices.
  Type IndicesType = SD->getIndices()->getType();
  if (auto TupleTy = IndicesType->getAs<TupleType>()) {
    if (TupleTy->getNumElements() == 1 && !TupleTy->getFields()[0].isVararg())
      IndicesType = TupleTy->getElementType(0);
  }

  bool IndicesResult = isRepresentableInObjC(SD->getDeclContext(), IndicesType);
  bool ElementResult = isRepresentableInObjC(SD->getDeclContext(),
                                             SD->getElementType());
  bool Result = IndicesResult && ElementResult;

  if (Result && checkObjCInGenericContext(*this, SD, Diagnose))
    return false;

  // Make sure we know how to map the selector appropriately.
  if (Result && SD->getObjCSubscriptKind() == ObjCSubscriptKind::None) {
    SourceRange IndexRange = SD->getIndices()->getSourceRange();
    diagnose(SD->getLoc(), diag::objc_invalid_subscript_key_type)
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
  diagnose(SD->getLoc(), diag::objc_invalid_on_subscript)
    .highlight(TypeRange);

  diagnoseTypeNotRepresentableInObjC(SD->getDeclContext(),
                                     !IndicesResult? IndicesType
                                                   : SD->getElementType(),
                                     TypeRange);
  describeObjCReason(*this, SD, Reason);

  return Result;
}

/// True if T is representable as a non-nullable ObjC pointer type.
static bool isObjCPointerType(Type T) {
  // FIXME: Return true for closures, and for anything bridged to a class type.

  // Look through a single level of metatype.
  if (auto MTT = T->getAs<AnyMetatypeType>())
    T = MTT->getInstanceType();

  if (isClassOrObjCProtocol(T))
    return true;

  if (T->is<DynamicSelfType>())
    return true;

  return false;
}

/// True if T is representable as an ObjC pointer type, nullable or otherwise.
static bool isObjCPointerOrOptionalType(Type T) {
  // Look through a single layer of optional type.
  if (auto valueType = T->getAnyOptionalObjectType()) {
    T = valueType;
  }
  return isObjCPointerType(T);
}

bool TypeChecker::isTriviallyRepresentableInObjC(const DeclContext *DC,
                                                 Type T) {
  // Look through one level of optional type, but remember that we did.
  bool wasOptional = false;
  if (auto valueType = T->getAnyOptionalObjectType()) {
    T = valueType;
    wasOptional = true;
  }

  if (isObjCPointerType(T))
    return true;

  // TODO: maybe Optional<UnsafePointer<T>> should be okay?
  if (wasOptional) return false;

  if (auto NTD = T->getAnyNominal()) {
    // If the type was imported from Clang, it is representable in Objective-C.
    if (NTD->hasClangNode())
      return true;

    // An UnsafePointer<T> is representable in Objective-C if T is a
    // trivially representable type.
    if (NTD == getUnsafePointerDecl(DC)) {
      T = T->castTo<BoundGenericType>()->getGenericArgs()[0];
      return isTriviallyRepresentableInObjC(DC, T);
    }
    // A CConstPointer<T> is representable in ObjC if T
    // is a trivially representable type.
    if (NTD == getCConstPointerDecl(DC)) {
      T = T->castTo<BoundGenericType>()->getGenericArgs()[0];
      return isTriviallyRepresentableInObjC(DC, T);
    }
    
    // A CMutablePointer<T> is representable in ObjC if T
    // is a trivially representable type and *not* an ObjC pointer type.
    if (NTD == getCMutablePointerDecl(DC)) {
      T = T->castTo<BoundGenericType>()->getGenericArgs()[0];
      return isTriviallyRepresentableInObjC(DC, T)
        && !isObjCPointerType(T);
    }
    
    // An ObjCMutablePointer<T> is representable in ObjC if T
    // is a (potentially optional) ObjC pointer type.
    if (NTD == getObjCMutablePointerDecl(DC)) {
      T = T->castTo<BoundGenericType>()->getGenericArgs()[0];
      return isObjCPointerOrOptionalType(T);
    }
    
    // TODO: C*VoidPointer
  }

  // If it's a mapped type, it's representable.
  fillObjCRepresentableTypeCache(DC);
  if (ObjCMappedTypes.count(T->getCanonicalType()))
    return true;

  return false;
}

bool TypeChecker::isRepresentableInObjC(const DeclContext *DC, Type T) {
  if (isTriviallyRepresentableInObjC(DC, T))
    return true;

  // Look through one level of optional type, but remember that we did.
  bool wasOptional = false;
  if (auto valueType = T->getAnyOptionalObjectType()) {
    T = valueType;
    wasOptional = true;
  }

  if (auto FT = T->getAs<FunctionType>()) {
    if (!FT->isBlock())
      return false;
    Type Input = FT->getInput();
    if (auto InputTuple = Input->getAs<TupleType>()) {
      for (auto &Elt : InputTuple->getFields()) {
        if (!isRepresentableInObjC(DC, Elt.getType()))
          return false;
      }
    } else if (!isRepresentableInObjC(DC, Input)) {
      return false;
    }

    Type Result = FT->getResult();
    if (!Result->isVoid() && !isRepresentableInObjC(DC, Result))
      return false;

    return true;
  }

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

  if (T->is<FunctionType>()) {
    diagnose(TypeRange.Start, diag::not_objc_function_type_param)
      .highlight(TypeRange);
    return;
  }
}

static void lookupRepresentableType(TypeChecker &TC,
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
  lookupLibraryTypes(*this, Stdlib, StdlibTypeNames, ObjCMappedTypes);

#define BRIDGE_TYPE(BRIDGED_MODULE, BRIDGED_TYPE,                          \
                    NATIVE_MODULE, NATIVE_TYPE, OPTIONAL_IS_BRIDGED)       \
  if (Context.getIdentifier(#NATIVE_MODULE) == Context.StdlibModuleName) { \
    lookupRepresentableType(*this, Stdlib,                                 \
                            Context.getIdentifier(#NATIVE_TYPE),           \
                            OPTIONAL_IS_BRIDGED,                           \
                            ObjCRepresentableTypes);                       \
  }
#include "swift/SIL/BridgedTypes.def"

  Identifier ID_ObjectiveC = Context.getIdentifier(OBJC_MODULE_NAME);
  if (auto ObjCModule = Context.getLoadedModule(ID_ObjectiveC)) {
    StdlibTypeNames.clear();
    StdlibTypeNames.push_back(Context.getIdentifier("Selector"));
    StdlibTypeNames.push_back(Context.getIdentifier("ObjCBool"));
    lookupLibraryTypes(*this, ObjCModule, StdlibTypeNames, ObjCMappedTypes);
  }
}

