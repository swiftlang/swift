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

#include "swift/AST/ASTWalker.h"
#include "swift/AST/ExprHandle.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/TypeLoc.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Twine.h"
using namespace swift;

GenericTypeResolver::~GenericTypeResolver() { }

Type TypeChecker::getArraySliceType(SourceLoc loc, Type elementType) {
  if (!Context.getSliceDecl()) {
    diagnose(loc, diag::sugar_type_not_found, 0);
    return Type();
  }

  return ArraySliceType::get(elementType, Context);
}

Type TypeChecker::getOptionalType(SourceLoc loc, Type elementType) {
  if (!Context.getOptionalDecl()) {
    diagnose(loc, diag::sugar_type_not_found, 1);
    return Type();
  }

  return OptionalType::get(elementType, Context);
}

Type TypeChecker::resolveTypeInContext(TypeDecl *typeDecl,
                                       DeclContext *fromDC,
                                       bool isSpecialized) {
  // If we found a generic parameter, map to the archetype if there is one.
  if (auto genericParam = dyn_cast<GenericTypeParamDecl>(typeDecl)) {
    if (auto archetype = genericParam->getArchetype()) {
      return archetype;
    }

    return genericParam->getDeclaredType();
  }

  // If we're referring to a generic type and no generic arguments have been
  // provided, and we are in the context of that generic type or one of its
  // extensions, imply the generic arguments
  if (auto nominal = dyn_cast<NominalTypeDecl>(typeDecl)) {
    if (nominal->getGenericParams() && !isSpecialized) {
      for (DeclContext *dc = fromDC; dc; dc = dc->getParent()) {
        switch (dc->getContextKind()) {
        case DeclContextKind::Module:
        case DeclContextKind::TopLevelCodeDecl:
          break;

        case DeclContextKind::NominalTypeDecl:
          // If this is our nominal type, return its type within its context.
          if (cast<NominalTypeDecl>(dc) == nominal)
            return nominal->getDeclaredTypeInContext();
          continue;
            
        case DeclContextKind::ExtensionDecl:
          // If this is an extension of our nominal type, return the type
          // within the context of its extension.
          if (cast<ExtensionDecl>(dc)->getExtendedType()->getAnyNominal()
                == nominal)
            return dc->getDeclaredTypeInContext();
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
  if (isa<AssociatedTypeDecl>(typeDecl) &&
      typeDecl->getDeclContext() != fromDC) {
    if (auto fromProto = dyn_cast<ProtocolDecl>(fromDC)) {
      return substMemberTypeWithBase(typeDecl->getDeclaredType(), typeDecl,
                                     fromProto->getSelf()->getDeclaredType());
    }
  }

  // Walk up through the type scopes to find the context where the type
  // declaration was found. When we find it, substitute the appropriate base
  // type.
  Type ownerType = ownerDC->getDeclaredTypeInContext();
  auto ownerNominal = ownerType->getAnyNominal();
  assert(ownerNominal && "Owner must be a nominal type");
  for (; !fromDC->isModuleContext(); fromDC = fromDC->getParent()) {
    // Skip non-type contexts.
    if (!fromDC->isTypeContext())
      continue;

    // Search the type of this context and its supertypes.
    for (auto fromType = fromDC->getDeclaredTypeInContext();
         fromType;
         fromType = getSuperClassOf(fromType)) {
      // If the nominal type declaration of the context type we're looking at
      // matches the owner's nominal type declaration, this is how we found
      // the member type declaration. Substitute the type we're coming from as
      // the base of the member type to produce the projected type result.
      if (fromType->getAnyNominal() == ownerNominal) {
        return substMemberTypeWithBase(typeDecl->getDeclaredType(), typeDecl,
                                       fromType);
      }
    }
  }

  llvm_unreachable("Shouldn't have found this type");
}

/// Apply generic arguments to the given type.
Type TypeChecker::applyGenericArguments(Type type,
                                        SourceLoc loc,
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
    if (validateType(genericArg, /*allowUnboundGenerics=*/false,
                     resolver))
      return nullptr;

    genericArgTypes.push_back(genericArg.getType());
  }

  // Form the bound generic type
  BoundGenericType *BGT = BoundGenericType::get(unbound->getDecl(),
                                                unbound->getParent(),
                                                genericArgTypes);
  // Check protocol conformance.
  // FIXME: Should be able to check when there are type variables?
  if (!BGT->isDependentType() && !BGT->hasTypeVariable()) {
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

    if (checkSubstitutions(Substitutions, Conformance, loc))
      return nullptr;
  }

  return BGT;
}

static Type applyGenericTypeReprArgs(TypeChecker &TC, Type type, SourceLoc loc,
                                     MutableArrayRef<TypeRepr *> genericArgs,
                                     GenericTypeResolver *resolver) {
  SmallVector<TypeLoc, 8> args;
  for (auto tyR : genericArgs)
    args.push_back(tyR);
  Type ty = TC.applyGenericArguments(type, loc, args, resolver);
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
                            MutableArrayRef<TypeRepr *> genericArgs,
                            bool allowUnboundGenerics,
                            GenericTypeResolver *resolver) {
  TC.validateDecl(typeDecl);

  Type type;
  if (dc) {
    // Resolve the type declaration to a specific type. How this occurs
    // depends on the current context and where the type was found.
    type = TC.resolveTypeInContext(typeDecl, dc, !genericArgs.empty());
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
    type = applyGenericTypeReprArgs(TC, type, loc, genericArgs, resolver);
  }

  assert(type);
  return type;
}

static llvm::PointerUnion<Type, Module *>
resolveIdentTypeComponent(TypeChecker &TC,
                          MutableArrayRef<IdentTypeRepr::Component> components,
                          bool allowUnboundGenerics,
                          GenericTypeResolver *resolver) {
  auto &comp = components.back();
  if (!comp.isBound()) {
    auto parentComps = components.slice(0, components.size()-1);
    if (parentComps.empty()) {
      // Resolve the first component, which is the only one that requires
      // unqualified name lookup.
      DeclContext *dc = comp.getUnboundContext();
      assert(dc);

      // Perform an unqualified lookup.
      UnqualifiedLookup Globals(comp.getIdentifier(), dc, &TC, comp.getIdLoc(),
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
          comp.setValue(result.getNamedModule());
          continue;
        }

        // Ignore non-type declarations.
        auto typeDecl = dyn_cast<TypeDecl>(result.getValueDecl());
        if (!typeDecl)
          continue;

        Type type = resolveTypeDecl(TC, typeDecl, comp.getIdLoc(),
                                    dc, comp.getGenericArgs(),
                                    allowUnboundGenerics,
                                    resolver);
        if (type->is<ErrorType>()) {
          comp.setValue(type);
          return type;
        }

        // If this is the first result we found, record it.
        if (current.isNull()) {
          current = type;
          comp.setValue(type);
          continue;
        }

        // Otherwise, check for an ambiguity.
        if (current.is<Module *>() || !current.get<Type>()->isEqual(type)) {
          isAmbiguous = true;
          break;
        }

        // We have a found multiple type aliases that refer to the sam thing.
        // Ignore the duplicate.
      }

      // If we found nothing, complain and fail.
      if (current.isNull()) {
        TC.diagnose(comp.getIdLoc(), components.size() == 1 ?
                    diag::use_undeclared_type : diag::unknown_name_in_type,
                    comp.getIdentifier())
          .highlight(SourceRange(comp.getIdLoc(),
                                 components.back().getIdLoc()));
        Type ty = ErrorType::get(TC.Context);
        comp.setValue(ty);
        return ty;
      }

      // Complain about any ambiguities we detected.
      // FIXME: We could recover by looking at later components.
      if (isAmbiguous) {
        TC.diagnose(comp.getIdLoc(), diag::ambiguous_type_base,
                    comp.getIdentifier())
          .highlight(SourceRange(comp.getIdLoc(),
                                 components.back().getIdLoc()));
        for (auto Result : Globals.Results) {
          if (Globals.Results[0].hasValueDecl())
            TC.diagnose(Result.getValueDecl(), diag::found_candidate);
          else
            TC.diagnose(comp.getIdLoc(), diag::found_candidate);
        }
        Type ty = ErrorType::get(TC.Context);
        comp.setValue(ty);
        return ty;
      }

    } else {
      llvm::PointerUnion<Type, Module *>
        parent = resolveIdentTypeComponent(TC, parentComps,
                                           allowUnboundGenerics,
                                           resolver);
      // If the last resolved component is a type, perform member type lookup.
      if (parent.is<Type>()) {
        // FIXME: Want the end of the back range.
        SourceRange parentRange(parentComps.front().getIdLoc(),
                                parentComps.back().getIdLoc());

        auto parentTy = parent.get<Type>();
        if (parentTy->is<ErrorType>())
          return parent.get<Type>();

        // If the parent is a dependent type, the member is a dependent member.
        if (parentTy->isDependentType()) {
          // Try to resolve the dependent member type to a specific associated
          // type.
          Type memberType = resolver->resolveDependentMemberType(
                                        parentTy,
                                        parentRange,
                                        comp.getIdentifier(),
                                        comp.getIdLoc());
          assert(memberType && "Received null dependent member type");

          if (!comp.getGenericArgs().empty() && !memberType->is<ErrorType>()) {
            // FIXME: Highlight generic arguments and introduce a Fix-It to
            // remove them.
            TC.diagnose(comp.getIdLoc(), diag::not_a_generic_type, memberType);

            // Drop the arguments.
          }

          comp.setValue(memberType);
          return memberType;
        }

        // Look for member types with the given name.
        auto memberTypes = TC.lookupMemberType(parentTy, comp.getIdentifier());

        // If we didn't find anything, complain.
        // FIXME: Typo correction!
        if (!memberTypes) {
          TC.diagnose(comp.getIdLoc(), diag::invalid_member_type,
                      comp.getIdentifier(), parent.get<Type>())
            .highlight(parentRange);
          Type ty = ErrorType::get(TC.Context);
          comp.setValue(ty);
          return ty;
        }

        // Name lookup was ambiguous. Complain.
        // FIXME: Could try to apply generic arguments first, and see whether
        // that resolves things. But do we really want that to succeed?
        if (memberTypes.size() > 1) {
          TC.diagnoseAmbiguousMemberType(parent.get<Type>(),
                                         parentRange,
                                         comp.getIdentifier(),
                                         comp.getIdLoc(),
                                         memberTypes);
          Type ty = ErrorType::get(TC.Context);
          comp.setValue(ty);
          return ty;
        }

        auto memberType = memberTypes.back().second;

        // If there are generic arguments, apply them now.
        if (!comp.getGenericArgs().empty())
          memberType = applyGenericTypeReprArgs(TC, memberType, comp.getIdLoc(),
                                                comp.getGenericArgs(),
                                                resolver);

        comp.setValue(memberType);
        return memberType;
      }

      // Lookup into a module.
      auto module = parent.get<Module *>();
      LookupTypeResult foundModuleTypes =
        TC.lookupMemberType(ModuleType::get(module), comp.getIdentifier());

      // If we didn't find a type, complain.
      if (!foundModuleTypes) {
        // FIXME: Fully-qualified module name?
        TC.diagnose(comp.getIdLoc(), diag::no_module_type, comp.getIdentifier(),
                    module->Name);
        Type ty = ErrorType::get(TC.Context);
        comp.setValue(ty);
        return ty;
      }

      // If lookup was ambiguous, complain.
      if (foundModuleTypes.isAmbiguous()) {
        TC.diagnose(comp.getIdLoc(), diag::ambiguous_module_type,
                    comp.getIdentifier(), module->Name);
        for (auto foundType : foundModuleTypes) {
          // Only consider type declarations.
          auto typeDecl = foundType.first;
          if (!typeDecl)
            continue;

          TC.diagnose(typeDecl, diag::found_candidate_type,
                      typeDecl->getDeclaredType());
        }
        Type ty = ErrorType::get(TC.Context);
        comp.setValue(ty);
        return ty;
      }
      Type foundType = foundModuleTypes[0].second;

      // If there are generic arguments, apply them now.
      if (!comp.getGenericArgs().empty()) {
        foundType = applyGenericTypeReprArgs(TC, foundType, comp.getIdLoc(),
                                          comp.getGenericArgs(), resolver);
      }

      comp.setValue(foundType);
    }
  }

  assert(comp.isBound());
  if (Type ty = comp.getBoundType())
    return ty;
  if (Module *mod = comp.getBoundModule())
    return mod;

  ValueDecl *VD = comp.getBoundDecl();
  auto typeDecl = dyn_cast<TypeDecl>(VD);
  if (!typeDecl) {
    TC.diagnose(comp.getIdLoc(), diag::use_non_type_value, VD->getName());
    TC.diagnose(VD, diag::use_non_type_value_prev, VD->getName());
    Type ty = ErrorType::get(TC.Context);
    comp.setValue(ty);
    return ty;
  }

  Type type = resolveTypeDecl(TC, typeDecl, comp.getIdLoc(), nullptr,
                              comp.getGenericArgs(), allowUnboundGenerics,
                              resolver);
  comp.setValue(type);
  return type;
}

/// \brief Returns a valid type or ErrorType in case of an error.
static Type resolveIdentifierType(TypeChecker &TC, IdentTypeRepr* IdType,
                                  bool allowUnboundGenerics,
                                  GenericTypeResolver *resolver) {
  assert(resolver && "Missing generic type resolver");

  llvm::PointerUnion<Type, Module *>
    result = resolveIdentTypeComponent(TC, IdType->Components,
                                       allowUnboundGenerics,
                                       resolver);
  if (auto mod = result.dyn_cast<Module*>()) {
    TC.diagnose(IdType->Components.back().getIdLoc(),
                diag::use_module_as_type, mod->Name);
    Type ty = ErrorType::get(TC.Context);
    IdType->Components.back().setValue(ty);
    return ty;
  }

  return result.get<Type>();
}

bool TypeChecker::validateType(TypeLoc &Loc, bool allowUnboundGenerics,
                               GenericTypeResolver *resolver) {
  // FIXME: Verify that these aren't circular and infinite size.
  
  // If we've already validated this type, don't do so again.
  if (Loc.wasValidated())
    return Loc.isError();

  if (Loc.getType().isNull()) {
    Loc.setType(resolveType(Loc.getTypeRepr(), allowUnboundGenerics, resolver),
                true);
    return Loc.isError();
  }

  Loc.setType(Loc.getType(), true);
  return Loc.isError();
}

Type TypeChecker::resolveType(TypeRepr *TyR, bool allowUnboundGenerics,
                              GenericTypeResolver *resolver) {
  PrettyStackTraceTypeRepr stackTrace(Context, "resolving", TyR);

  // Make sure we always have a resolver to use.
  PartialGenericTypeToArchetypeResolver defaultResolver(*this);
  if (!resolver)
    resolver = &defaultResolver;

  assert(TyR && "Cannot validate null TypeReprs!");
  switch (TyR->getKind()) {
  case TypeReprKind::Error:
    return ErrorType::get(Context);

  case TypeReprKind::Attributed: {
    Type Ty;
    auto AttrTyR = cast<AttributedTypeRepr>(TyR);
    DeclAttributes attrs = AttrTyR->getAttrs();
    assert(!attrs.empty());
    Ty = resolveType(AttrTyR->getTypeRepr(),
                     /*FIXME:allowUnboundGenerics=*/false,
                     resolver);
    if (Ty->is<ErrorType>())
      return Ty;

    if (attrs.isInOut()) {
      LValueType::Qual quals;
      Ty = LValueType::get(Ty, quals, Context);
      attrs.InOut = false; // so that the empty() check below works
    }

    // Handle the auto_closure, cc, and objc_block attributes for function types.
    if (attrs.isAutoClosure() || attrs.hasCC() || attrs.isObjCBlock() ||
        attrs.isThin() || attrs.isNoReturn() || 
        attrs.getKernelOrShaderKind() != KernelOrShaderKind::Default) {
      FunctionType *FT = dyn_cast<FunctionType>(Ty.getPointer());
      TupleType *InputTy = 0;
      if (FT) InputTy = dyn_cast<TupleType>(FT->getInput().getPointer());
      if (FT == 0) {
        // auto_closures and objc_blocks require a syntactic function type.
        if (attrs.isAutoClosure())
          diagnose(attrs.AtLoc, diag::attribute_requires_function_type,
                   "auto_closure");
        if (attrs.isObjCBlock())
          diagnose(attrs.AtLoc, diag::attribute_requires_function_type,
                   "objc_block");
        if (attrs.hasCC())
          diagnose(attrs.AtLoc, diag::attribute_requires_function_type,
                   "cc");
        if (attrs.isThin())
          diagnose(attrs.AtLoc, diag::attribute_requires_function_type,
                   "thin");
        if (attrs.isNoReturn())
          diagnose(attrs.AtLoc, diag::attribute_requires_function_type,
                   "noreturn");
        if (attrs.isKernel())
          diagnose(attrs.AtLoc, diag::attribute_requires_function_type,
                   "kernel");
        if (attrs.isVertex())
          diagnose(attrs.AtLoc, diag::attribute_requires_function_type,
                   "vertex");
        if (attrs.isFragment())
          diagnose(attrs.AtLoc, diag::attribute_requires_function_type,
                   "fragment");
      } else if (attrs.isAutoClosure() &&
                 (InputTy == 0 || !InputTy->getFields().empty())) {
        // auto_closures must take () syntactically.
        diagnose(attrs.AtLoc, diag::autoclosure_function_input_nonunit,
                 FT->getInput());
      } else {
        // Otherwise, we're ok, rebuild type, adding the AutoClosure and ObjcBlock
        // bit.
        auto Info = FunctionType::ExtInfo(attrs.hasCC()
                                          ? attrs.getAbstractCC()
                                          : AbstractCC::Freestanding,
                                          attrs.isThin(),
                                          attrs.isNoReturn(),
                                          attrs.isAutoClosure(),
                                          attrs.isObjCBlock());
        Ty = FunctionType::get(FT->getInput(), FT->getResult(),
                               Info,
                               Context);
      }
      attrs.AutoClosure = false;
      attrs.ObjCBlock = false;
      attrs.Thin = false;
      attrs.NoReturn = false;
      attrs.cc = Nothing;
      attrs.KernelOrShader = KernelOrShaderKind::Default;
    }

    // In SIL translation units *only*, permit [weak] and [unowned] to
    // apply directly to types.
    if (attrs.hasOwnership() && TU.Kind == TranslationUnit::SIL &&
        Ty->hasReferenceSemantics()) {
      Ty = ReferenceStorageType::get(Ty, attrs.getOwnership(), Context);
      attrs.clearOwnership();
    }

    // Diagnose [local_storage] in nested positions.
    if (attrs.isLocalStorage()) {
      assert(TU.Kind == TranslationUnit::SIL);
      diagnose(attrs.AtLoc, diag::sil_local_storage_nested);
      attrs.LocalStorage = false;
    }

    // FIXME: this is lame.
    if (!attrs.empty())
      diagnose(attrs.AtLoc, diag::attribute_does_not_apply_to_type);

    return Ty;
  }

  case TypeReprKind::Ident:
    return resolveIdentifierType(*this, cast<IdentTypeRepr>(TyR),
                                 allowUnboundGenerics,
                                 resolver);

  case TypeReprKind::Function: {
    auto FnTyR = cast<FunctionTypeRepr>(TyR);
    Type inputTy = resolveType(FnTyR->getArgsTypeRepr(),
                               /*FIXME:allowUnboundGenerics=*/false,
                               resolver);
    if (inputTy->is<ErrorType>())
      return inputTy;
    Type outputTy = resolveType(FnTyR->getResultTypeRepr(),
                                /*FIXME:allowUnboundGenerics=*/false,
                                resolver);
    if (outputTy->is<ErrorType>())
      return outputTy;
    return FunctionType::get(inputTy, outputTy, Context);
  }

  case TypeReprKind::Array: {
    // FIXME: diagnose non-materializability of element type!
    auto ArrTyR = cast<ArrayTypeRepr>(TyR);
    Type baseTy = resolveType(ArrTyR->getBase(),
                              /*FIXME:allowUnboundGenerics=*/false,
                              resolver);
    if (baseTy->is<ErrorType>())
      return baseTy;

    if (ExprHandle *sizeEx = ArrTyR->getSize()) {
      // FIXME: We don't support fixed-length arrays yet.
      // FIXME: We need to check Size! (It also has to be convertible to int).
      diagnose(ArrTyR->getBrackets().Start, diag::unsupported_fixed_length_array)
        .highlight(sizeEx->getExpr()->getSourceRange());
      return ErrorType::get(Context);
    }

    auto sliceTy = getArraySliceType(ArrTyR->getBrackets().Start, baseTy);
    if (!sliceTy)
      return ErrorType::get(Context);

    return sliceTy;
  }

  case TypeReprKind::Optional: {
    // FIXME: diagnose non-materializability of element type!
    auto optTyR = cast<OptionalTypeRepr>(TyR);
    Type baseTy = resolveType(optTyR->getBase(),
                              /*FIXME:allowUnboundGenerics=*/false,
                              resolver);
    if (baseTy->is<ErrorType>())
      return baseTy;

    auto optionalTy = getOptionalType(optTyR->getQuestionLoc(), baseTy);
    if (!optionalTy)
      return ErrorType::get(Context);

    return optionalTy;
  }

  case TypeReprKind::Tuple: {
    auto TupTyR = cast<TupleTypeRepr>(TyR);
    SmallVector<TupleTypeElt, 8> Elements;
    for (auto tyR : TupTyR->getElements()) {
      if (NamedTypeRepr *namedTyR = dyn_cast<NamedTypeRepr>(tyR)) {
        Type ty = resolveType(namedTyR->getTypeRepr(),
                              /*FIXME:allowUnboundGenerics=*/false,
                              resolver);
        if (ty->is<ErrorType>())
          return ty;
        Elements.push_back(TupleTypeElt(ty, namedTyR->getName()));
      } else {
        Type ty = resolveType(tyR,
                              /*FIXME:allowUnboundGenerics=*/false,
                              resolver);
        if (ty->is<ErrorType>())
          return ty;
        Elements.push_back(TupleTypeElt(ty));
      }
    }

    if (TupTyR->hasEllipsis()) {
      Type BaseTy = Elements.back().getType();
      Type FullTy = getArraySliceType(TupTyR->getEllipsisLoc(), BaseTy);
      Identifier Name = Elements.back().getName();
      // FIXME: Where are we rejecting default arguments for variadic
      // parameters?
      Elements.back() = TupleTypeElt(FullTy, Name, DefaultArgumentKind::None,
                                     true);
    }

    return TupleType::get(Elements, Context);
  }

  case TypeReprKind::Named:
    llvm_unreachable("NamedTypeRepr only shows up as an element of Tuple");

  case TypeReprKind::ProtocolComposition: {
    auto ProtTyR = cast<ProtocolCompositionTypeRepr>(TyR);
    SmallVector<Type, 4> ProtocolTypes;
    for (auto tyR : ProtTyR->getProtocols()) {
      Type ty = resolveType(tyR, /*FIXME:allowUnboundGenerics=*/false,
                            resolver);
      if (ty->is<ErrorType>())
        return ty;
      if (!ty->isExistentialType()) {
        diagnose(tyR->getStartLoc(), diag::protocol_composition_not_protocol,
                 ty);
        continue;
      }

      // The special DynamicLookup protocol can't be part of a protocol
      // composition.
      if (auto protoTy = ty->getAs<ProtocolType>()){
        if (protoTy->getDecl()->isSpecificProtocol(
              KnownProtocolKind::DynamicLookup)) {
          diagnose(tyR->getStartLoc(),
                   diag::protocol_composition_dynamic_lookup);
          continue;
        }
      }

      ProtocolTypes.push_back(ty);
    }
    return ProtocolCompositionType::get(Context, ProtocolTypes);
  }

  case TypeReprKind::MetaType: {
    Type ty = resolveType(cast<MetaTypeTypeRepr>(TyR)->getBase(),
                          /*FIXME:allowUnboundGenerics=*/false,
                          resolver);
    if (ty->is<ErrorType>())
      return ty;
    return MetaTypeType::get(ty, Context);
  }
  }

  llvm_unreachable("all cases should be handled");
}

Type TypeChecker::transformType(Type type,
                                const std::function<Type(Type)> &fn) {
  return type.transform(Context, fn);
}

Type TypeChecker::substType(Type type, TypeSubstitutionMap &Substitutions,
                            bool IgnoreMissing) {
  return type.subst(&TU, Substitutions, IgnoreMissing, this);
}

Type TypeChecker::substMemberTypeWithBase(Type T, ValueDecl *Member,
                                          Type BaseTy) {
  if (!BaseTy)
    return T;

  return BaseTy->getTypeOfMember(&TU, Member, this, T);
}

Type TypeChecker::getSuperClassOf(Type type) {
  return type->getSuperclass(this);
}

Type TypeChecker::resolveMemberType(Type type, Identifier name) {
  LookupTypeResult memberTypes = lookupMemberType(type, name);
  if (!memberTypes)
    return Type();


  // FIXME: Detect ambiguities here?
  return memberTypes.back().second;
}

