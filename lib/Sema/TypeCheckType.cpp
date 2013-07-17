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
#include "swift/AST/TypeLoc.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Twine.h"
using namespace swift;

/// Given an array slice type with a valid base type and no
/// implementation type, find its canonicalization.
static bool buildArraySliceType(TypeChecker &TC, ArraySliceType *sliceTy,
                                SourceLoc loc) {
  if (sliceTy->hasImplementationType())
    return false;
  
  Type baseTy = sliceTy->getBaseType();

  // Hack for array slices: first, try to look up Slice<T>.
  UnqualifiedLookup genericSliceLookup(TC.Context.getIdentifier("Slice"),
                                       &TC.TU);
  if (TypeDecl *sliceTypeDecl = genericSliceLookup.getSingleTypeResult()) {
    if (auto sliceTypeNTD = dyn_cast<NominalTypeDecl>(sliceTypeDecl)) {
      if (auto Params = sliceTypeNTD->getGenericParams()) {
        if (Params->size() == 1) {
          Type implTy = BoundGenericType::get(sliceTypeNTD, Type(), baseTy);
          sliceTy->setImplementationType(implTy);
          return false;
        }
      }
    }
  }

  TC.diagnose(loc, diag::slice_type_not_found);
  return true;
}

Type TypeChecker::getArraySliceType(SourceLoc loc, Type elementType,
                                    bool canonicalize) {
  ArraySliceType *sliceTy = ArraySliceType::get(elementType, Context);
  if (sliceTy->hasCanonicalTypeComputed()) return sliceTy;
  if (buildArraySliceType(*this, sliceTy, loc)) return Type();
  if (canonicalize) {
    sliceTy->getCanonicalType();
    validateTypeSimple(sliceTy->getImplementationType());
  }
  return sliceTy;
}

Type TypeChecker::resolveTypeInContext(TypeDecl *typeDecl,
                                       DeclContext *fromDC,
                                       bool isSpecialized) {
  // If we're referring to a generic type and no generic arguments have been
  // provided, and we are in the context of that generic type or one of its
  // extensions, imply the generic arguments
  if (auto nominal = dyn_cast<NominalTypeDecl>(typeDecl)) {
    if (nominal->getGenericParams() && !isSpecialized) {
      for (DeclContext *dc = fromDC; dc; dc = dc->getParent()) {
        switch (dc->getContextKind()) {
        case DeclContextKind::BuiltinModule:
        case DeclContextKind::ClangModule:
        case DeclContextKind::SerializedModule:
        case DeclContextKind::TopLevelCodeDecl:
        case DeclContextKind::TranslationUnit:
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

        case DeclContextKind::CapturingExpr:
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
  // for our reference to this associated type is our own 'This'.
  if (isa<TypeAliasDecl>(typeDecl) &&
      isa<ProtocolDecl>(ownerDC) &&
      typeDecl->getDeclContext() != fromDC) {
    if (auto fromProto = dyn_cast<ProtocolDecl>(fromDC)) {
      return substMemberTypeWithBase(typeDecl->getDeclaredType(), typeDecl,
                                     fromProto->getThis()->getDeclaredType());
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
  auto genericParams = unbound->getDecl()->getGenericParams();
  if (genericParams->size() != genericArgs.size()) {
    // FIXME: Show the type name here.
    // FIXME: Point at the actual declaration of the underlying type.
    diagnose(loc, diag::type_parameter_count_mismatch,
             genericArgs.size(), genericParams->size());
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
  if (!BGT->hasSubstitutions() && !BGT->hasTypeVariable()) {
    // FIXME: Record that we're checking substitutions, so we can't end up
    // with infinite recursion.
    TypeSubstitutionMap Substitutions;
    ConformanceMap Conformance;
    auto genericParams = BGT->getDecl()->getGenericParams();
    unsigned Index = 0;
    for (Type Arg : BGT->getGenericArgs()) {
      auto GP = genericParams->getParams()[Index++];
      auto Archetype = GP.getAsTypeParam()->getDeclaredType()
                         ->getAs<ArchetypeType>();
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

/// \brief Find a type member as a qualified member of a module.
LookupTypeResult TypeChecker::lookupMemberType(Module *module, Identifier name){
  LookupTypeResult result;
  SmallVector<ValueDecl*, 4> decls;
  // FIXME: The use of AccessPathTy() is weird here.
  module->lookupValue(Module::AccessPathTy(), name,
                      NLKind::QualifiedLookup, decls);
  
  for (auto decl : decls) {
    // Only consider type declarations.
    auto typeDecl = dyn_cast<TypeDecl>(decl);
    if (!typeDecl)
      continue;
    
    auto type = typeDecl->getDeclaredType();
    result.addResult({typeDecl, type});
  }
  return result;
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
        if (parent.get<Type>()->is<ErrorType>())
          return parent.get<Type>();

        // Look for member types with the given name.
        auto memberTypes = TC.lookupMemberType(parent.get<Type>(),
                                               comp.getIdentifier());

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
      LookupTypeResult foundModuleTypes = TC.lookupMemberType(module,
                                                          comp.getIdentifier());

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
    return !Loc.isError();

  if (Loc.getType().isNull()) {
    Loc.setType(resolveType(Loc.getTypeRepr(), allowUnboundGenerics), true);
    return !Loc.isError();
  }

  Type ty = Loc.getType();
  if (validateTypeSimple(ty))
    ty = ErrorType::get(Context);

  Loc.setType(ty, true);
  return !Loc.isError();
}

void TypeChecker::validateTypeDecl(TypeDecl *D) {
  if (TypeAliasDecl *TAD = dyn_cast<TypeAliasDecl>(D)) {
    // TypeAliasDecls may not have a type reference.
    if (TAD->getUnderlyingTypeLoc().getTypeRepr())
      validateType(TAD->getUnderlyingTypeLoc());
  }
}

Type TypeChecker::resolveType(TypeRepr *TyR, bool allowUnboundGenerics) {
  assert(TyR && "Cannot validate null TypeReprs!");
  switch (TyR->getKind()) {
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
        attrs.isThin()) {
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
      } else if (attrs.isAutoClosure() &&
                 (InputTy == 0 || !InputTy->getFields().empty())) {
        // auto_closures must take () syntactically.
        diagnose(attrs.LSquareLoc, diag::autoclosure_function_input_nonunit,
                 FT->getInput());
      } else {
        // Otherwise, we're ok, rebuild type, adding the AutoClosure and ObjcBlock
        // bit.
        Ty = FunctionType::get(FT->getInput(), FT->getResult(),
                               attrs.isAutoClosure(),
                               attrs.isObjCBlock(),
                               attrs.isThin(),
                               attrs.hasCC()
                                ? attrs.getAbstractCC()
                                : AbstractCC::Freestanding,
                               Context);
      }
      attrs.AutoClosure = false;
      attrs.ObjCBlock = false;
      attrs.Thin = false;
      attrs.cc = Nothing;
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

    auto sliceTy = ArraySliceType::get(baseTy, Context);
    if (buildArraySliceType(*this, sliceTy, ArrTyR->getBrackets().Start))
      return ErrorType::get(Context);

    return sliceTy;
  }

  case TypeReprKind::Tuple: {
    auto TupTyR = cast<TupleTypeRepr>(TyR);
    SmallVector<TupleTypeElt, 8> Elements;
    for (auto tyR : TupTyR->getElements()) {
      if (NamedTypeRepr *namedTyR = dyn_cast<NamedTypeRepr>(tyR)) {
        Type ty = resolveType(namedTyR->getTypeRepr());
        if (ty->is<ErrorType>())
          return ty;
        Elements.push_back(TupleTypeElt(ty, namedTyR->getName(), nullptr));
      } else {
        Type ty = resolveType(tyR);
        if (ty->is<ErrorType>())
          return ty;
        Elements.push_back(TupleTypeElt(ty));
      }
    }

    if (TupTyR->hasEllipsis()) {
      Type BaseTy = Elements.back().getType();
      Type FullTy = ArraySliceType::get(BaseTy, Context);
      Identifier Name = Elements.back().getName();
      ExprHandle *Init = Elements.back().getInit();
      Elements.back() = TupleTypeElt(FullTy, Name, Init, true);
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
        return ErrorType::get(Context);
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
  case TypeKind::BuiltinOpaquePointer:
  case TypeKind::BuiltinObjectPointer:
  case TypeKind::BuiltinObjCPointer:
  case TypeKind::BuiltinVector:
  case TypeKind::OneOf:
  case TypeKind::Struct:
  case TypeKind::Class:
  case TypeKind::Archetype:
  case TypeKind::UnboundGeneric:
    // These types are already canonical anyway.
    return false;

  case TypeKind::Module:
  case TypeKind::Protocol:
  case TypeKind::TypeVariable:
    // Nothing to validate.
    return false;

  case TypeKind::ReferenceStorage:
    llvm_unreachable("reference storage type in typechecker");

  case TypeKind::Substituted:
    return validateTypeSimple(cast<SubstitutedType>(T)->getReplacementType());

  case TypeKind::NameAlias:
    return validateTypeSimple(
                        cast<NameAliasType>(T)->getDecl()->getUnderlyingType());

  case TypeKind::Identifier:
    llvm_unreachable("identifier in validateTypeSimple");

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
    if (!AT->hasImplementationType()) {
      buildArraySliceType(*this, AT, SourceLoc());
    }
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
  case TypeKind::BoundGenericOneOf:
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
        auto Archetype = GP.getAsTypeParam()->getDeclaredType()
                           ->getAs<ArchetypeType>();
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
  // Try transforming this type.
  Type transformed = fn(type);
  if (!transformed)
    return nullptr;

  // If the client changed the type, we're done.
  if (transformed.getPointer() != type.getPointer())
    return transformed;

  // Recursive into children of this type.
  TypeBase *base = type.getPointer();
  switch (type->getKind()) {
#define ALWAYS_CANONICAL_TYPE(Id, Parent) \
  case TypeKind::Id:                      \
    return type;
#define UNCHECKED_TYPE(Id, Parent) ALWAYS_CANONICAL_TYPE(Id, Parent)
#define TYPE(Id, Parent)
#include "swift/AST/TypeNodes.def"
  case TypeKind::OneOf:
  case TypeKind::Struct:
  case TypeKind::Class:
  case TypeKind::Protocol: {
    auto nominalTy = cast<NominalType>(base);
    if (auto parentTy = nominalTy->getParent()) {
      parentTy = transformType(parentTy, fn);
      if (!parentTy)
        return Type();
      
      if (parentTy.getPointer() != nominalTy->getParent().getPointer())
        return NominalType::get(nominalTy->getDecl(), parentTy, Context);
    }
    return type;
  }

  case TypeKind::ReferenceStorage:
    llvm_unreachable("reference storage type in typechecker");

  case TypeKind::UnboundGeneric: {
    auto unbound = cast<UnboundGenericType>(base);
    Type substParentTy;
    if (auto parentTy = unbound->getParent()) {
      substParentTy = transformType(parentTy, fn);
      if (!substParentTy)
        return Type();

      if (substParentTy.getPointer() == parentTy.getPointer())
        return type;
    } else {
      // Substitutions only affect the parent.
      return type;
    }

    return UnboundGenericType::get(unbound->getDecl(), substParentTy, Context);
  }

  case TypeKind::BoundGenericClass:
  case TypeKind::BoundGenericOneOf:
  case TypeKind::BoundGenericStruct: {
    auto BGT = cast<BoundGenericType>(base);
    SmallVector<Type, 4> SubstArgs;
    bool AnyChanged = false;
    Type substParentTy;
    if (auto parentTy = BGT->getParent()) {
      substParentTy = transformType(parentTy, fn);
      if (!substParentTy)
        return Type();

      if (substParentTy.getPointer() != parentTy.getPointer())
        AnyChanged = true;
    }

    for (Type Arg : BGT->getGenericArgs()) {
      Type SubstArg = transformType(Arg, fn);
      if (!SubstArg)
        return Type();
      SubstArgs.push_back(SubstArg);
      if (SubstArg.getPointer() != Arg.getPointer())
        AnyChanged = true;
    }

    if (!AnyChanged)
      return type;

    return BoundGenericType::get(BGT->getDecl(), substParentTy, SubstArgs);
  }

  case TypeKind::MetaType: {
    auto Meta = cast<MetaTypeType>(base);
    auto UnderlyingTy = transformType(Meta->getInstanceType(),
                                  fn);
    if (!UnderlyingTy)
      return Type();

    if (UnderlyingTy.getPointer() == Meta->getInstanceType().getPointer())
      return type;

    return MetaTypeType::get(UnderlyingTy, Context);
  }

  case TypeKind::NameAlias: {
    auto Alias = cast<NameAliasType>(base);
    auto UnderlyingTy = transformType(Alias->getDecl()->getUnderlyingType(),
                                  fn);
    if (!UnderlyingTy)
      return Type();

    if (UnderlyingTy.getPointer()
          == Alias->getDecl()->getUnderlyingType().getPointer())
      return type;

    return SubstitutedType::get(type, UnderlyingTy, Context);
  }

  case TypeKind::Identifier: {
    auto Id = cast<IdentifierType>(base);
    if (!Id->isMapped())
      return type;

    auto MappedTy = transformType(Id->getMappedType(), fn);
    if (!MappedTy)
      return Type();

    if (MappedTy.getPointer() == Id->getMappedType().getPointer())
      return type;

    return SubstitutedType::get(type, MappedTy, Context);
  }

  case TypeKind::Paren: {
    auto paren = cast<ParenType>(base);
    Type underlying = transformType(paren->getUnderlyingType(), fn);
    if (!underlying)
      return Type();

    if (underlying.getPointer() == paren->getUnderlyingType().getPointer())
      return type;

    return ParenType::get(Context, underlying);
  }

  case TypeKind::Tuple: {
    auto Tuple = cast<TupleType>(base);
    bool AnyChanged = false;
    SmallVector<TupleTypeElt, 4> Elements;
    unsigned Index = 0;
    for (auto Elt : Tuple->getFields()) {
      Type EltTy = transformType(Elt.getType(), fn);
      if (!EltTy)
        return Type();

      // FIXME: Substitute into default arguments.

      // If nothing has changd, just keep going.
      if (!AnyChanged && !Elt.hasInit() &&
          EltTy.getPointer() == Elt.getType().getPointer()) {
        ++Index;
        continue;
      }

      // If this is the first change we've seen, copy all of the previous
      // elements.
      if (!AnyChanged) {
        // Copy all of the previous elements.
        for (unsigned I = 0; I != Index; ++I) {
          const TupleTypeElt &FromElt =Tuple->getFields()[I];
          Elements.push_back(TupleTypeElt(FromElt.getType(), FromElt.getName(),
                                          FromElt.getInit(),
                                          FromElt.isVararg()));
        }

        AnyChanged = true;
      }

      // Add the new tuple element, with the new type, no initializer,
      Elements.push_back(TupleTypeElt(EltTy, Elt.getName(), Elt.getInit(),
                                      Elt.isVararg()));
      ++Index;
    }

    if (!AnyChanged)
      return type;

    return TupleType::get(Elements, Context);
  }

  case TypeKind::Substituted: {
    auto SubstAT = cast<SubstitutedType>(base);
    auto Subst = transformType(SubstAT->getReplacementType(), fn);
    if (!Subst)
      return Type();

    if (Subst.getPointer() == SubstAT->getReplacementType().getPointer())
      return type;

    return SubstitutedType::get(SubstAT->getOriginal(), Subst, Context);
  }

  case TypeKind::Function:
  case TypeKind::PolymorphicFunction: {
    auto Function = cast<AnyFunctionType>(base);
    auto InputTy = transformType(Function->getInput(), fn);
    if (!InputTy)
      return Type();
    auto ResultTy = transformType(Function->getResult(), fn);
    if (!ResultTy)
      return Type();

    if (InputTy.getPointer() == Function->getInput().getPointer() &&
        ResultTy.getPointer() == Function->getResult().getPointer())
      return type;

    // Function types always parenthesized input types, but some clients
    // (e.g., the type-checker) occasionally perform transformations that
    // don't abide this. Fix up the input type appropriately.
    if (!InputTy->hasTypeVariable() &&
        !isa<ParenType>(InputTy.getPointer()) &&
        !isa<TupleType>(InputTy.getPointer())) {
      InputTy = ParenType::get(Context, InputTy);
    }

    if (auto polyFn = dyn_cast<PolymorphicFunctionType>(base)) {
      return PolymorphicFunctionType::get(InputTy, ResultTy,
                                          &polyFn->getGenericParams(),
                                          Context);
    } else {
      auto fn = cast<FunctionType>(base);
      return FunctionType::get(InputTy, ResultTy,
                               fn->isAutoClosure(),
                               fn->isBlock(),
                               fn->isThin(),
                               Context);
    }
  }

  case TypeKind::Array: {
    auto Array = cast<ArrayType>(base);
    auto BaseTy = transformType(Array->getBaseType(), fn);
    if (!BaseTy)
      return Type();

    if (BaseTy.getPointer() == Array->getBaseType().getPointer())
      return type;

    return ArrayType::get(BaseTy, Array->getSize(), Context);
  }

  case TypeKind::ArraySlice: {
    auto Slice = cast<ArraySliceType>(base);
    auto BaseTy = transformType(Slice->getBaseType(), fn);
    if (!BaseTy)
      return Type();

    if (BaseTy.getPointer() == Slice->getBaseType().getPointer())
      return type;

    return getArraySliceType(SourceLoc(), BaseTy);
  }

  case TypeKind::LValue: {
    auto LValue = cast<LValueType>(base);
    auto ObjectTy = transformType(LValue->getObjectType(), fn);
    if (!ObjectTy)
      return Type();

    if (ObjectTy.getPointer() == LValue->getObjectType().getPointer())
      return type;

    return LValueType::get(ObjectTy, LValue->getQualifiers(), Context);
  }

  case TypeKind::ProtocolComposition: {
    auto PC = cast<ProtocolCompositionType>(base);
    SmallVector<Type, 4> Protocols;
    bool AnyChanged = false;
    unsigned Index = 0;
    for (auto Proto : PC->getProtocols()) {
      auto SubstProto = transformType(Proto, fn);
      if (!SubstProto)
        return Type();
      
      if (AnyChanged) {
        Protocols.push_back(SubstProto);
        ++Index;
        continue;
      }
      
      if (SubstProto.getPointer() != Proto.getPointer()) {
        AnyChanged = true;
        Protocols.append(Protocols.begin(), Protocols.begin() + Index);
        Protocols.push_back(SubstProto);
      }
      
      ++Index;
    }
    
    if (!AnyChanged)
      return type;
    
    return ProtocolCompositionType::get(Context, Protocols);
  }
  }
  
  llvm_unreachable("Unhandled type in transformation");
}

Type TypeChecker::substType(Type origType, TypeSubstitutionMap &Substitutions,
                            bool IgnoreMissing) {
  return transformType(origType,
                       [&](Type type) -> Type {
    auto substOrig = dyn_cast<SubstitutableType>(type.getPointer());
    if (!substOrig)
      return type;

    TypeSubstitutionMap::const_iterator Known = Substitutions.find(substOrig);
    if (Known != Substitutions.end() && Known->second)
      return SubstitutedType::get(substOrig, Known->second, Context);

    auto parent = substOrig->getParent();
    if (!parent)
      return type;

    // Substitute into the parent type.
    Type SubstParent = substType(parent, Substitutions);
    if (!SubstParent)
      return Type();

    // If the parent didn't change, we won't change.
    if (SubstParent.getPointer() == parent)
      return type;

    // If the parent is an archetype, extract the child archetype with the
    // given name.
    if (auto ArchetypeParent = SubstParent->getAs<ArchetypeType>()) {
      return ArchetypeParent->getNestedType(substOrig->getName());
    }
     
    // Retrieve the type with the given name.

    // Tuples don't have member types.
    // FIXME: Feels like a hack.
     if (SubstParent->is<TupleType>()) {
       assert(IgnoreMissing && "Expect member type within tuple type");
       return type;
     }

    // FIXME: Shouldn't we be using protocol-conformance information here?
    LookupTypeResult MemberTypes
      = lookupMemberType(SubstParent, substOrig->getName());
    if (!MemberTypes && IgnoreMissing)
      return type;

    // FIXME: Detect ambiguities here?
    return MemberTypes.back().second;
  });
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
      Type ParamTy = Params[i].getAsTypeParam()->getUnderlyingType();
      Substitutions[ParamTy->castTo<ArchetypeType>()] = Args[i];
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
  auto ThisDecl = Proto->getThis();
  TypeSubstitutionMap Substitutions;
  Substitutions[ThisDecl->getDeclaredType()->castTo<ArchetypeType>()]
    = BaseArchetype;
  return substType(T, Substitutions);
}

Type TypeChecker::getSuperClassOf(Type type) {
  Type baseTy;
  Type specializedTy;
  if (auto classTy = type->getAs<ClassType>()) {
    baseTy = classTy->getDecl()->getBaseClass();
    if (auto parentTy = classTy->getParent()) {
      if (parentTy->isSpecialized())
        specializedTy = parentTy;
    }
  } else if (auto boundTy = type->getAs<BoundGenericType>()) {
    if (auto classDecl = dyn_cast<ClassDecl>(boundTy->getDecl())) {
      baseTy = classDecl->getBaseClass();
      specializedTy = type;
    }
  } else if (auto archetypeTy = type->getAs<ArchetypeType>()) {
    baseTy = archetypeTy->getSuperclass();
  } else {
    // No other types have base classes.
    return nullptr;
  }

  if (!specializedTy || !baseTy)
    return baseTy;

  // If the type is specialized, we need to gather all of the substitutions.
  // We've already dealt with the top level, but continue gathering
  // specializations from the parent types.
  TypeSubstitutionMap substitutions;
  while (specializedTy) {
    if (auto nominalTy = specializedTy->getAs<NominalType>()) {
      specializedTy = nominalTy->getParent();
      continue;
    }

    // Introduce substitutions for each of the generic parameters/arguments.
    auto boundTy = specializedTy->castTo<BoundGenericType>();
    auto gp = boundTy->getDecl()->getGenericParams()->getParams();
    for (unsigned i = 0, n = boundTy->getGenericArgs().size(); i != n; ++i) {
      auto archetype
        = gp[i].getAsTypeParam()->getDeclaredType()->castTo<ArchetypeType>();
      substitutions[archetype] = boundTy->getGenericArgs()[i];
    }

    specializedTy = boundTy->getParent();
  }

  // Perform substitutions into the base type.
  return substType(baseTy, substitutions);
}
