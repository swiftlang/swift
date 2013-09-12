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
#include "swift/AST/ASTWalker.h"
#include "swift/AST/ExprHandle.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/TypeLoc.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Twine.h"
using namespace swift;

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

        case DeclContextKind::FuncExpr:
        case DeclContextKind::PipeClosureExpr:
        case DeclContextKind::ClosureExpr:
        case DeclContextKind::ConstructorDecl:
        case DeclContextKind::DestructorDecl:
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
                                        MutableArrayRef<TypeLoc> genericArgs) {
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
    if (validateType(genericArg))
      return nullptr;

    genericArgTypes.push_back(genericArg.getType());
  }

  // Form the bound generic type
  BoundGenericType *BGT = BoundGenericType::get(unbound->getDecl(),
                                                unbound->getParent(),
                                                genericArgTypes);
  // Check protocol conformance.
  // FIXME: Should be able to check even when there are type variables
  // present?
  if (!BGT->hasSubstitutions() && !BGT->hasTypeVariable() &&
      !BGT->isDependentType()) {
    // FIXME: Record that we're checking substitutions, so we can't end up
    // with infinite recursion.
    TypeSubstitutionMap Substitutions;
    ConformanceMap Conformance;
    auto genericParams = BGT->getDecl()->getGenericParams();
    unsigned Index = 0;
    for (Type Arg : BGT->getGenericArgs()) {
      auto GP = genericParams->getParams()[Index++];
      auto Archetype = GP.getAsTypeParam()->getArchetype();
      Substitutions[Archetype] = Arg;
    }

    if (checkSubstitutions(Substitutions, Conformance, loc, &Substitutions))
      return nullptr;
    else {
      // Record these substitutions.
      BGT->setSubstitutions(encodeSubstitutions(genericParams, Substitutions,
                                                Conformance, true));
    }
  }

  return BGT;
}

static Type applyGenericTypeReprArgs(TypeChecker &TC, Type type, SourceLoc loc,
                                     MutableArrayRef<TypeRepr *> genericArgs) {
  SmallVector<TypeLoc, 8> args;
  for (auto tyR : genericArgs)
    args.push_back(tyR);
  Type ty = TC.applyGenericArguments(type, loc, args);
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
                            bool allowUnboundGenerics) {
  TC.validateTypeDecl(typeDecl);

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

  // If we found a generic parameter, map to the archetype if there is one.
  if (auto genericParam = type->getAs<GenericTypeParamType>()) {
    if (auto archetype = genericParam->getDecl()->getArchetype()) {
      type = archetype;
    }
  }

  if (!genericArgs.empty()) {
    // Apply the generic arguments to the type.
    type = applyGenericTypeReprArgs(TC, type, loc, genericArgs);
  }

  assert(type);
  return type;
}

static llvm::PointerUnion<Type, Module *>
resolveIdentTypeComponent(TypeChecker &TC,
                          MutableArrayRef<IdentTypeRepr::Component> components,
                          bool allowUnboundGenerics) {
  auto &comp = components.back();
  if (!comp.isBound()) {
    auto parentComps = components.slice(0, components.size()-1);
    if (parentComps.empty()) {
      // Resolve the first component, which is the only one that requires
      // unqualified name lookup.
      DeclContext *dc = comp.getUnboundContext();
      assert(dc);

      // Perform an unqualified lookup.
      UnqualifiedLookup Globals(comp.getIdentifier(), dc, comp.getIdLoc(),
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
                                    allowUnboundGenerics);
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
                                           allowUnboundGenerics);
      // If the last resolved component is a type, perform member type lookup.
      if (parent.is<Type>()) {
        auto parentTy = parent.get<Type>();
        if (parentTy->is<ErrorType>())
          return parent.get<Type>();

        // If the parent is a dependent type, the member is a dependent member.
        if (parentTy->isDependentType()) {
          // Form a dependent member type.
          Type memberType = DependentMemberType::get(parentTy,
                                                     comp.getIdentifier(),
                                                     TC.Context);

          if (!comp.getGenericArgs().empty()) {
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
          // FIXME: Highlight base range.
          TC.diagnose(comp.getIdLoc(), diag::invalid_member_type,
                      comp.getIdentifier(), parent.get<Type>());
          Type ty = ErrorType::get(TC.Context);
          comp.setValue(ty);
          return ty;
        }

        // Name lookup was ambiguous. Complain.
        // FIXME: Could try to apply generic arguments first, and see whether
        // that resolves things. But do we really want that to succeed?
        if (memberTypes.size() > 1) {
          TC.diagnose(comp.getIdLoc(), diag::ambiguous_member_type,
                      comp.getIdentifier(), parent.get<Type>());
          for (const auto &member : memberTypes) {
            TC.diagnose(member.first, diag::found_candidate_type,
                        member.second);
          }
          Type ty = ErrorType::get(TC.Context);
          comp.setValue(ty);
          return ty;
        }

        auto memberType = memberTypes.back().second;

        // If there are generic arguments, apply them now.
        if (!comp.getGenericArgs().empty())
          memberType = applyGenericTypeReprArgs(TC, memberType, comp.getIdLoc(),
                                                comp.getGenericArgs());

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
                                          comp.getGenericArgs());
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
                              comp.getGenericArgs(), allowUnboundGenerics);
  comp.setValue(type);
  return type;
}

/// \brief Returns a valid type or ErrorType in case of an error.
static Type resolveIdentifierType(TypeChecker &TC, IdentTypeRepr* IdType,
                                  bool allowUnboundGenerics) {
  llvm::PointerUnion<Type, Module *>
    result = resolveIdentTypeComponent(TC, IdType->Components,
                                       allowUnboundGenerics);
  if (auto mod = result.dyn_cast<Module*>()) {
    TC.diagnose(IdType->Components.back().getIdLoc(),
                diag::use_module_as_type, mod->Name);
    Type ty = ErrorType::get(TC.Context);
    IdType->Components.back().setValue(ty);
    return ty;
  }

  return result.get<Type>();
}

bool TypeChecker::validateType(TypeLoc &Loc, bool allowUnboundGenerics) {
  // FIXME: Verify that these aren't circular and infinite size.
  
  // If we've already validated this type, don't do so again.
  if (Loc.wasValidated())
    return Loc.isError();

  if (Loc.getType().isNull()) {
    Loc.setType(resolveType(Loc.getTypeRepr(), allowUnboundGenerics), true);
    return Loc.isError();
  }

  Type ty = Loc.getType();
  if (validateTypeSimple(ty))
    ty = ErrorType::get(Context);

  Loc.setType(ty, true);
  return Loc.isError();
}

Type TypeChecker::resolveType(TypeRepr *TyR, bool allowUnboundGenerics) {
  PrettyStackTraceTypeRepr stackTrace(Context, "resolving", TyR);

  assert(TyR && "Cannot validate null TypeReprs!");
  switch (TyR->getKind()) {
  case TypeReprKind::Error:
    return ErrorType::get(Context);

  case TypeReprKind::Attributed: {
    Type Ty;
    auto AttrTyR = cast<AttributedTypeRepr>(TyR);
    DeclAttributes attrs = AttrTyR->getAttrs();
    assert(!attrs.empty());
    Ty = resolveType(AttrTyR->getTypeRepr());
    if (Ty->is<ErrorType>())
      return Ty;

    if (attrs.isByref()) {
      LValueType::Qual quals;
      Ty = LValueType::get(Ty, quals, Context);
      attrs.Byref = false; // so that the empty() check below works
    }

    // Handle the auto_closure, cc, and objc_block attributes for function types.
    if (attrs.isAutoClosure() || attrs.hasCC() || attrs.isObjCBlock() ||
        attrs.isThin() || attrs.isNoReturn()) {
      FunctionType *FT = dyn_cast<FunctionType>(Ty.getPointer());
      TupleType *InputTy = 0;
      if (FT) InputTy = dyn_cast<TupleType>(FT->getInput().getPointer());
      if (FT == 0) {
        // auto_closures and objc_blocks require a syntactic function type.
        if (attrs.isAutoClosure())
          diagnose(attrs.LSquareLoc, diag::attribute_requires_function_type,
                   "auto_closure");
        if (attrs.isObjCBlock())
          diagnose(attrs.LSquareLoc, diag::attribute_requires_function_type,
                   "objc_block");
        if (attrs.hasCC())
          diagnose(attrs.LSquareLoc, diag::attribute_requires_function_type,
                   "cc");
        if (attrs.isThin())
          diagnose(attrs.LSquareLoc, diag::attribute_requires_function_type,
                   "thin");
        if (attrs.isNoReturn())
          diagnose(attrs.LSquareLoc, diag::attribute_requires_function_type,
                   "noreturn");
      } else if (attrs.isAutoClosure() &&
                 (InputTy == 0 || !InputTy->getFields().empty())) {
        // auto_closures must take () syntactically.
        diagnose(attrs.LSquareLoc, diag::autoclosure_function_input_nonunit,
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
      diagnose(attrs.LSquareLoc, diag::sil_local_storage_nested);
      attrs.LocalStorage = false;
    }

    // FIXME: this is lame.
    if (!attrs.empty())
      diagnose(attrs.LSquareLoc, diag::attribute_does_not_apply_to_type);

    return Ty;
  }

  case TypeReprKind::Ident:
    return resolveIdentifierType(*this, cast<IdentTypeRepr>(TyR),
                                 allowUnboundGenerics);

  case TypeReprKind::Function: {
    auto FnTyR = cast<FunctionTypeRepr>(TyR);
    Type inputTy = resolveType(FnTyR->getArgsTypeRepr());
    if (inputTy->is<ErrorType>())
      return inputTy;
    Type outputTy = resolveType(FnTyR->getResultTypeRepr());
    if (outputTy->is<ErrorType>())
      return outputTy;
    return FunctionType::get(inputTy, outputTy, Context);
  }

  case TypeReprKind::Array: {
    // FIXME: diagnose non-materializability of element type!
    auto ArrTyR = cast<ArrayTypeRepr>(TyR);
    Type baseTy = resolveType(ArrTyR->getBase());
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
    Type baseTy = resolveType(optTyR->getBase());
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
        Type ty = resolveType(namedTyR->getTypeRepr());
        if (ty->is<ErrorType>())
          return ty;
        Elements.push_back(TupleTypeElt(ty, namedTyR->getName()));
      } else {
        Type ty = resolveType(tyR);
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
      Type ty = resolveType(tyR);
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
    Type ty = resolveType(cast<MetaTypeTypeRepr>(TyR)->getBase());
    if (ty->is<ErrorType>())
      return ty;
    return MetaTypeType::get(ty, Context);
  }
  }

  llvm_unreachable("all cases should be handled");
}

bool TypeChecker::validateTypeSimple(Type InTy) {
  assert(InTy && "Cannot validate null types!");

  TypeBase *T = InTy.getPointer();
  // FIXME: Verify that these aren't circular and infinite size.

  switch (T->getKind()) {
  case TypeKind::Error:
  case TypeKind::BuiltinFloat:
  case TypeKind::BuiltinInteger:
  case TypeKind::BuiltinRawPointer:
  case TypeKind::BuiltinObjectPointer:
  case TypeKind::BuiltinObjCPointer:
  case TypeKind::BuiltinVector:
  case TypeKind::Union:
  case TypeKind::Struct:
  case TypeKind::Class:
  case TypeKind::Archetype:
  case TypeKind::AssociatedType:
  case TypeKind::GenericTypeParam:
  case TypeKind::UnboundGeneric:
    // These types are already canonical anyway.
    return false;

  case TypeKind::Module:
  case TypeKind::Protocol:
  case TypeKind::TypeVariable:
  case TypeKind::DependentMember:
    // Nothing to validate.
    return false;

#define ARTIFICIAL_TYPE(Id, Parent) \
  case TypeKind::Id:
#define TYPE(Id, Parent)
#include "swift/AST/TypeNodes.def"
    llvm_unreachable("should not need to validate an artificial type");

  case TypeKind::Substituted:
    return validateTypeSimple(cast<SubstitutedType>(T)->getReplacementType());

  case TypeKind::NameAlias:
    return validateTypeSimple(
                        cast<NameAliasType>(T)->getDecl()->getUnderlyingType());

  case TypeKind::Paren:
    return validateTypeSimple(cast<ParenType>(T)->getUnderlyingType());

  case TypeKind::Tuple: {
    TupleType *TT = cast<TupleType>(T);

    // Okay, we found an uncanonicalized tuple type, which might have default
    // values.  If so, we'll potentially have to update it.
    for (unsigned i = 0, e = TT->getFields().size(); i != e; ++i) {
      // The element has *at least* a type or an initializer, so we start by
      // verifying each individually.
      Type EltTy = TT->getFields()[i].getType();
      if (validateTypeSimple(EltTy))
        return true;
    }
    break;
  }

  case TypeKind::LValue:
    return validateTypeSimple(cast<LValueType>(T)->getObjectType());

  case TypeKind::Function:
  case TypeKind::PolymorphicFunction: {
    AnyFunctionType *FT = cast<AnyFunctionType>(T);
    if (validateTypeSimple(FT->getInput()))
      return true;
    if (validateTypeSimple(FT->getResult()))
      return true;
    break;
  }
  case TypeKind::Array: {
    ArrayType *AT = cast<ArrayType>(T);
    if (validateTypeSimple(AT->getBaseType()))
      return true;
    break;
  }
  case TypeKind::ArraySlice: {
    ArraySliceType *AT = cast<ArraySliceType>(T);
    if (validateTypeSimple(AT->getBaseType()))
      return true;
    if (validateTypeSimple(AT->getImplementationType()))
      return true;
    break;
  }
  case TypeKind::Optional: {
    OptionalType *OT = cast<OptionalType>(T);
    if (validateTypeSimple(OT->getBaseType()))
      return true;
    if (validateTypeSimple(OT->getImplementationType()))
      return true;
    break;
  }

  case TypeKind::ProtocolComposition: {
    ProtocolCompositionType *PC = cast<ProtocolCompositionType>(T);
    for (auto Proto : PC->getProtocols()) {
      if (validateTypeSimple(Proto))
        return true;
    }
    break;
  }

  case TypeKind::MetaType: {
    MetaTypeType *Meta = cast<MetaTypeType>(T);
    if (validateTypeSimple(Meta->getInstanceType()))
      return true;
    break;
  }

  case TypeKind::BoundGenericClass:
  case TypeKind::BoundGenericUnion:
  case TypeKind::BoundGenericStruct: {
    BoundGenericType *BGT = cast<BoundGenericType>(T);
    unsigned Index = 0;
    for (Type Arg : BGT->getGenericArgs()) {
      if (validateTypeSimple(Arg))
        return true;
    }

    // Check protocol conformance.
    // FIXME: Should be able to check even when there are type variables
    // present?
    if (!BGT->hasSubstitutions() && !BGT->hasTypeVariable()) {
      // FIXME: Record that we're checking substitutions, so we can't end up
      // with infinite recursion.
      TypeSubstitutionMap Substitutions;
      ConformanceMap Conformance;
      auto genericParams = BGT->getDecl()->getGenericParams();
      for (Type Arg : BGT->getGenericArgs()) {
        auto GP = genericParams->getParams()[Index++];
        auto Archetype = GP.getAsTypeParam()->getArchetype();
        Substitutions[Archetype] = Arg;
      }

      if (checkSubstitutions(Substitutions, Conformance, SourceLoc(),
                             &Substitutions))
        return true;
      // Record these substitutions.
      BGT->setSubstitutions(encodeSubstitutions(genericParams, Substitutions,
                                                Conformance, true));
    }
    break;
  }
  }

  return false;
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

  // Look through the metatype.
  if (auto MetaBase = BaseTy->getAs<MetaTypeType>())
    BaseTy = MetaBase->getInstanceType();

  if (auto BGT = BaseTy->getRValueType()->getAs<BoundGenericType>()) {
    // FIXME: Cache this?
    TypeSubstitutionMap Substitutions;
    auto Params = BGT->getDecl()->getGenericParams()->getParams();
    auto Args = BGT->getGenericArgs();
    for (unsigned i = 0, e = BGT->getGenericArgs().size(); i != e; ++i) {
      auto ParamTy = Params[i].getAsTypeParam()->getArchetype();
      Substitutions[ParamTy] = Args[i];
    }

    return substType(T, Substitutions);
  }

  auto BaseArchetype = BaseTy->getRValueType()->getAs<ArchetypeType>();
  if (!BaseArchetype)
    return T;

  auto ProtoType = Member->getDeclContext()->getDeclaredTypeOfContext()
                     ->getAs<ProtocolType>();
  if (!ProtoType)
    return T;

  auto Proto = ProtoType->getDecl();
  auto SelfDecl = Proto->getSelf();
  TypeSubstitutionMap Substitutions;
  Substitutions[SelfDecl->getArchetype()] = BaseArchetype;
  return substType(T, Substitutions);
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

