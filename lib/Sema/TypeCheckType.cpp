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

Type TypeChecker::getArraySliceType(SourceLoc loc, Type elementType) {
  ArraySliceType *sliceTy = ArraySliceType::get(elementType, Context);
  if (sliceTy->hasCanonicalTypeComputed()) return sliceTy;
  if (buildArraySliceType(*this, sliceTy, loc)) return Type();
  sliceTy->getCanonicalType();
  validateTypeSimple(sliceTy->getImplementationType());
  return sliceTy;
}

/// Resolve a reference to the given type declaration within a particular
/// context.
///
/// This routine aids unqualified name lookup for types by performing the
/// resolution necessary to rectify the declaration found by name lookup with
/// the declaration context from which name lookup started.
static Type resolveTypeInContext(TypeChecker &tc, TypeDecl *typeDecl,
                                 DeclContext *fromDC) {
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
  // FIXME: We'll need to turn this into a more general 'implicit base'
  // projection, so that any time we find something in an outer context
  // we make sure to map to that context.
  if (isa<TypeAliasDecl>(typeDecl) &&
      isa<ProtocolDecl>(ownerDC) &&
      typeDecl->getDeclContext() != fromDC) {
    if (auto fromProto = dyn_cast<ProtocolDecl>(fromDC)) {
      return tc.substMemberTypeWithBase(typeDecl->getDeclaredType(), typeDecl,
                                        fromProto->getThis()->getDeclaredType());
    }
  }

  return typeDecl->getDeclaredType();
}

/// Apply generic arguments to the given type.
static Type applyGenericArguments(TypeChecker &TC, Type type, SourceLoc loc,
                                  ArrayRef<TypeLoc> genericArgs) {
  auto unbound = type->getAs<UnboundGenericType>();
  if (!unbound) {
    // FIXME: Highlight generic arguments and introduce a Fix-It to remove
    // them.
    TC.diagnose(loc, diag::not_a_generic_type, type);

    // Just return the type; this provides better recovery anyway.
    return type;
  }

  // Make sure we have the right number of generic arguments.
  auto genericParams = unbound->getDecl()->getGenericParams();
  if (genericParams->size() != genericArgs.size()) {
    // FIXME: Show the type name here.
    // FIXME: Point at the actual declaration of the underlying type.
    TC.diagnose(loc, diag::type_parameter_count_mismatch,
                genericArgs.size(), genericParams->size());
    return nullptr;
  }

  // Validate the generic arguments and capture just the types.
  SmallVector<Type, 4> genericArgTypes;
  for (auto &genericArg : genericArgs) {
    // Validate the generic argument.
    // FIXME: Totally broken. We need GenericArgs to be mutable!
    TypeLoc genericArgCopy = genericArg;
    if (TC.validateType(genericArgCopy)) {
      genericArgCopy.setInvalidType(TC.Context);
      return nullptr;
    }

    genericArgTypes.push_back(genericArgCopy.getType());
  }

  // Form the bound generic type
  type = BoundGenericType::get(unbound->getDecl(), unbound->getParent(),
                               genericArgTypes);

  // FIXME: Total hack. We should do the checking of the arguments right
  // here.
  TypeLoc tl{type, loc};
  if (TC.validateType(tl)) {
    return nullptr;
  }

  return type;
}

/// \brief Validate the given identifier type.
static bool validateIdentifierType(TypeChecker &TC, IdentifierType* IdType,
                                   TypeLoc &Loc) {
  MutableArrayRef<IdentifierType::Component> Components = IdType->Components;

  // The currently-resolved result, which captures the result of name lookup
  // for all components that have been considered thus far.
  llvm::PointerUnion<Type, Module *> current;

  if (Components[0].Value.is<DeclContext *>()) {
    // Resolve the first component, which is the only one that requires
    // unqualified name lookup.
    DeclContext *dc = Components[0].Value.get<DeclContext*>();
    assert(dc);

    // Perform an unqualified lookup.
    UnqualifiedLookup Globals(Components[0].Id, dc, Components[0].Loc,
                              /*TypeLookup*/true);

    // Process the names we found.
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
        Components[0].setValue(result.getNamedModule());
        continue;
      }

      // Ignore non-type declarations.
      auto typeDecl = dyn_cast<TypeDecl>(result.getValueDecl());
      if (!typeDecl)
        continue;

      // Resolve the type declaration to a specific type. How this occurs
      // depends on the current context and where the type was found.
      auto type = resolveTypeInContext(TC, typeDecl, dc);
      if (!type)
        continue;

      // FIXME: Egregious hack. We should be type-checking the typeDecl we found,
      // above.
      {
        TypeLoc tempLoc(type, SourceRange(typeDecl->getStartLoc()));
        if (TC.validateType(tempLoc)) {
          return true;
        }
        type = tempLoc.getType();
      }

      // If this is the first result we found, record it.
      if (current.isNull()) {
        current = type;
        Components[0].setValue(type);
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
      TC.diagnose(Components[0].Loc, Components.size() == 1 ?
                  diag::use_undeclared_type : diag::unknown_name_in_type,
                  Components[0].Id)
        .highlight(SourceRange(Components[0].Loc, Components.back().Loc));
      return true;
    }

    // Complain about any ambiguities we detected.
    // FIXME: We could recover by looking at later components.
    if (isAmbiguous) {
      TC.diagnose(Components[0].Loc, diag::ambiguous_type_base, Components[0].Id)
        .highlight(SourceRange(Components[0].Loc, Components.back().Loc));
      for (auto Result : Globals.Results) {
        if (Globals.Results[0].hasValueDecl())
          TC.diagnose(Result.getValueDecl(), diag::found_candidate);
        else
          TC.diagnose(Components[0].Loc, diag::found_candidate);
      }
      return true;
    }
  } else {
    auto typeDecl = cast<TypeDecl>(Components[0].Value.get<ValueDecl *>());
    auto type = typeDecl->getDeclaredType();

    // FIXME: Egregious hack. We should be type-checking the typeDecl we found,
    // above.
    {
      TypeLoc tempLoc(type, SourceRange(typeDecl->getStartLoc()));
      if (TC.validateType(tempLoc)) {
        return true;
      }
      type = tempLoc.getType();
    }

    // If this is the first result we found, record it.
    Components[0].setValue(type);
    current = type;
  }

  // If the first component has generic arguments, apply them.
  if (!Components[0].GenericArgs.empty()) {
    if (current.is<Module *>()) {
      // FIXME: Diagnose this and drop the arguments.
    } else {
      // Apply the generic arguments to the type.
      auto type = applyGenericArguments(TC, current.get<Type>(),
                                        Components[0].Loc,
                                        Components[0].GenericArgs);
      if (!type)
        return true;

      current = type;
      Components[0].setValue(type);
    }
  }

  // Resolve the remaining components.
  for (auto &comp : Components.slice(1)) {
    // If the last resolved component is a type, perform member type lookup.
    if (current.is<Type>()) {
      // Look for member types with the given name.
      auto memberTypes = TC.lookupMemberType(current.get<Type>(), comp.Id);

      // If we didn't find anything, complain.
      // FIXME: Typo correction!
      if (!memberTypes) {
        // FIXME: Highlight base range.
        TC.diagnose(comp.Loc, diag::invalid_member_type, comp.Id,
                    current.get<Type>());
        return true;
      }

      // Name lookup was ambiguous. Complain.
      // FIXME: Could try to apply generic arguments first, and see whether
      // that resolves things. But do we really want that to succeed?
      if (memberTypes.size() > 1) {
        TC.diagnose(comp.Loc, diag::ambiguous_member_type, comp.Id,
                    current.get<Type>());
        for (const auto &member : memberTypes) {
          TC.diagnose(member.first, diag::found_candidate_type,
                      member.second);
        }
        return true;
      }

      auto memberType = memberTypes.back().second;

      // If there are generic arguments, apply them now.
      if (!comp.GenericArgs.empty()) {
        memberType = applyGenericArguments(TC, memberType, comp.Loc,
                                           comp.GenericArgs);
        if (!memberType)
          return true;
      }

      // Update our position with the type we just determined.
      current = memberType;
      comp.setValue(memberType);
      continue;
    }

    // Lookup into a module.
    // FIXME: The use of AccessPathTy() is weird here.
    auto module = current.get<Module *>();
    SmallVector<ValueDecl*, 4> decls;
    module->lookupValue(Module::AccessPathTy(), comp.Id,
                        NLKind::QualifiedLookup, decls);

    bool isAmbiguous = false;
    Type foundType;
    for (auto decl : decls) {
      // Only consider type declarations.
      auto typeDecl = dyn_cast<TypeDecl>(decl);
      if (!typeDecl)
        continue;

      auto type = typeDecl->getDeclaredType();
      if (!foundType) {
        foundType = type;
        continue;
      }

      if (!foundType->isEqual(type)) {
        isAmbiguous = true;
        break;
      }
    }

    // If we didn't find a type, complain.
    if (!foundType) {
      // FIXME: Fully-qualified module name?
      TC.diagnose(comp.Loc, diag::no_module_type, comp.Id, module->Name);
      return true;
    }

    // If lookup was ambiguous, complain.
    if (isAmbiguous) {
      TC.diagnose(comp.Loc, diag::ambiguous_module_type, comp.Id, module->Name);
      for (auto decl : decls) {
        // Only consider type declarations.
        auto typeDecl = dyn_cast<TypeDecl>(decl);
        if (!typeDecl)
          continue;

        TC.diagnose(typeDecl, diag::found_candidate_type,
                    typeDecl->getDeclaredType());
      }
      return true;
    }

    // If there are generic arguments, apply them now.
    if (!comp.GenericArgs.empty()) {
      foundType = applyGenericArguments(TC, foundType, comp.Loc,
                                        comp.GenericArgs);
      if (!foundType)
        return true;
    }

    // Update our position with the type we just determined.
    current = foundType;
    comp.setValue(foundType);
  }

  return false;
}

/// validateType - Types can contain expressions (in the default values for
/// tuple elements), and thus need semantic analysis to ensure that these
/// expressions are valid and that they have the appropriate conversions etc.
///
/// This returns true if the type is invalid.
bool TypeChecker::validateType(TypeLoc &Loc) {
  Type InTy = Loc.getType();
  assert(InTy && "Cannot validate null types!");

  TypeBase *T = InTy.getPointer();
  // FIXME: Verify that these aren't circular and infinite size.
  
  // If we've already validated this type, don't do so again.
  if (T->wasValidated()) return !T->isValid();

  bool IsInvalid = false;
  
  // \brief RAII object that sets the validation pass on the type when it
  // goes out of scope.
  class SetValidation {
    Type T;
    bool &IsInvalid;

  public:
    SetValidation(Type T, bool &IsInvalid) : T(T), IsInvalid(IsInvalid) { }
    ~SetValidation() {
      T->setValidated(!IsInvalid);
    }
  } setValidation(T, IsInvalid);

  switch (T->getKind()) {
  case TypeKind::Error:
    // Error already diagnosed.
    return IsInvalid = true;
  case TypeKind::BuiltinFloat:
  case TypeKind::BuiltinInteger:
  case TypeKind::BuiltinRawPointer:
  case TypeKind::BuiltinOpaquePointer:
  case TypeKind::BuiltinObjectPointer:
  case TypeKind::BuiltinObjCPointer:
  case TypeKind::BuiltinVector:
  case TypeKind::UnstructuredUnresolved:
  case TypeKind::OneOf:
  case TypeKind::Struct:
  case TypeKind::Class:
  case TypeKind::Archetype:
  case TypeKind::UnboundGeneric:
    // These types are already canonical anyway.
    return IsInvalid = false;
      
  case TypeKind::Module:
  case TypeKind::Protocol:
  case TypeKind::TypeVariable:
    // Nothing to validate.
    break;

  case TypeKind::Substituted: {
    TypeLoc TL(cast<SubstitutedType>(T)->getReplacementType(),
               Loc.getSourceRange());
    return IsInvalid = validateType(TL);
  }

  case TypeKind::NameAlias: {
    TypeAliasDecl *D = cast<NameAliasType>(T)->getDecl();
    IsInvalid = validateType(D->getUnderlyingTypeLoc());
    if (IsInvalid)
      D->getUnderlyingTypeLoc().setInvalidType(Context);
    break;
  }
  case TypeKind::Identifier: {
    IdentifierType *DNT = cast<IdentifierType>(T);
    if (DNT->isMapped()) {
      // FIXME: Refactor this to avoid fake TypeLoc
      TypeLoc TempLoc = TypeLoc::withoutLoc(DNT->getMappedType());
      IsInvalid = validateType(TempLoc);
      break;
    }
    if (!IsInvalid && validateIdentifierType(*this, DNT, Loc))
      IsInvalid = true;
    break;
  }
  case TypeKind::Paren: {
    // FIXME: Extract real typeloc info.
    TypeLoc TempLoc{ cast<ParenType>(T)->getUnderlyingType(),
                     Loc.getSourceRange() };
    return IsInvalid = validateType(TempLoc);
  }
  case TypeKind::Tuple: {
    TupleType *TT = cast<TupleType>(T);
    
    // Okay, we found an uncanonicalized tuple type, which might have default
    // values.  If so, we'll potentially have to update it.
    for (unsigned i = 0, e = TT->getFields().size(); i != e; ++i) {
      // The element has *at least* a type or an initializer, so we start by
      // verifying each individually.
      Type EltTy = TT->getFields()[i].getType();
      // FIXME: Extract real typeloc info
      TypeLoc TempLoc{ EltTy, Loc.getSourceRange() };
      if (EltTy && validateType(TempLoc)) {
        IsInvalid = true;
        break;
      }

      ExprHandle *EltInit = TT->getFields()[i].getInit();
      if (EltInit == 0 || EltInit->alreadyChecked()) continue;

      Expr *initExpr = EltInit->getExpr();
      // FIXME: Should pass the DeclContext in which the tuple type appears.
      if (typeCheckExpression(initExpr, &TU, EltTy)) {
        diagnose(initExpr->getLoc(), diag::while_converting_default_tuple_value,
                 EltTy);
        IsInvalid = true;
        break;
      }

      if (!EltTy)
        initExpr = coerceToMaterializable(initExpr);

      struct CheckForLocalRef : public ASTWalker {
        TypeChecker &TC;

        CheckForLocalRef(TypeChecker &TC) : TC(TC) {}

        std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
          if (DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(E)) {
            if (DRE->getDecl()->getDeclContext()->isLocalContext()) {
              TC.diagnose(E->getLoc(), diag::tuple_default_local_ref);
            }
          }
          return { true, E };
        }
      };

      initExpr->walk(CheckForLocalRef(*this));

      // If both a type and an initializer are specified, make sure the
      // initializer's type agrees with the (redundant) type.
      assert(EltTy.isNull() || EltTy->isEqual(initExpr->getType()));
      EltTy = initExpr->getType();

      EltInit->setExpr(initExpr, true);
      TT->updateInitializedElementType(i, EltTy);
    }
    break;
  }

  case TypeKind::LValue: {
    // FIXME: Extract real typeloc info.
    TypeLoc TempLoc{ cast<LValueType>(T)->getObjectType(),
                     Loc.getSourceRange() };
    IsInvalid = validateType(TempLoc);
    // FIXME: diagnose non-materializability of object type!
    break;
  }
      
  case TypeKind::Function:
  case TypeKind::PolymorphicFunction: {
    AnyFunctionType *FT = cast<AnyFunctionType>(T);
    // FIXME: Extract real typeloc info.
    TypeLoc TempLoc{ FT->getInput(), Loc.getSourceRange() };
    IsInvalid = validateType(TempLoc);
    if (!IsInvalid) {
      TempLoc = TypeLoc{ FT->getResult(), Loc.getSourceRange() };
      IsInvalid = validateType(TempLoc);
    }
    // FIXME: diagnose non-materializability of result type!
    break;
  }
  case TypeKind::Array: {
    ArrayType *AT = cast<ArrayType>(T);
    // FIXME: Extract real typeloc info.
    TypeLoc TempLoc{ AT->getBaseType(), Loc.getSourceRange() };
    IsInvalid = validateType(TempLoc);
    // FIXME: diagnose non-materializability of element type!
    // FIXME: We need to check AT->Size! (It also has to be convertible to int).
    break;
  }
  case TypeKind::ArraySlice: {
    ArraySliceType *AT = cast<ArraySliceType>(T);
    // FIXME: Extract real typeloc info.
    TypeLoc TempLoc{ AT->getBaseType(), Loc.getSourceRange() };
    IsInvalid = validateType(TempLoc);
    // FIXME: diagnose non-materializability of element type?
    if (!IsInvalid && !AT->hasImplementationType()) {
      IsInvalid = buildArraySliceType(*this, AT, Loc.getSourceRange().Start);
    }
    break;
  }
      
  case TypeKind::ProtocolComposition: {
    ProtocolCompositionType *PC = cast<ProtocolCompositionType>(T);
    for (auto Proto : PC->getProtocols()) {
      // FIXME: Extract real typeloc info.
      TypeLoc TempLoc{ Proto, Loc.getSourceRange() };
      if (validateType(TempLoc))
        IsInvalid = true;
      else if (!Proto->isExistentialType()) {
        SourceLoc DiagLoc = Loc.getSourceRange().Start;
        diagnose(DiagLoc, diag::protocol_composition_not_protocol,
                 Proto);
        IsInvalid = true;
      }
    }
    break;
  }

  case TypeKind::MetaType: {
    MetaTypeType *Meta = cast<MetaTypeType>(T);
    // FIXME: Extract real typeloc info?  Should we be validating this type
    // in the first place?
    TypeLoc TempLoc{ Meta->getInstanceType(), Loc.getSourceRange() };
    IsInvalid = validateType(TempLoc);
    break;
  }

  case TypeKind::BoundGenericClass:
  case TypeKind::BoundGenericOneOf:
  case TypeKind::BoundGenericStruct: {
    BoundGenericType *BGT = cast<BoundGenericType>(T);
    unsigned Index = 0;
    for (Type Arg : BGT->getGenericArgs()) {
      // FIXME: Extract real typeloc info?  Should we be validating this type
      // in the first place?
      TypeLoc TempLoc = TypeLoc::withoutLoc(Arg);
      if (validateType(TempLoc)) {
        IsInvalid = true;
        break;
      }
    }

    // Check protocol conformance.
    // FIXME: Should be able to check even when there are type variables
    // present?
    if (!IsInvalid && !BGT->hasSubstitutions() && !BGT->hasTypeVariable()) {
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

      if (checkSubstitutions(Substitutions, Conformance,
                             Loc.getSourceRange().Start, &Substitutions))
        IsInvalid = true;
      else {
        // Record these substitutions.
        BGT->setSubstitutions(encodeSubstitutions(genericParams, Substitutions,
                                                  Conformance, true));
      }
    }
    break;
  }
  }

  // If we determined that this type is invalid, erase it in the caller.
  if (IsInvalid)
    return true;

  // FIXME: This isn't good enough: top-level stuff can have these as well and
  // their types need to be resolved at the end of name binding.  Perhaps we
  // should require them to have explicit types even if they have values and 
  // let the value mismatch be detected at typechecking time? 
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
                                          FromElt.getVarargBaseTy()));
        }

        AnyChanged = true;
      }

      // Substitute into the base type of a variadic tuple.
      Type VarargBaseTy;
      if (Elt.isVararg()) {
        VarargBaseTy = transformType(Elt.getVarargBaseTy(), fn);
        if (!VarargBaseTy)
          return Type();
      }

      // Add the new tuple element, with the new type, no initializer,
      Elements.push_back(TupleTypeElt(EltTy, Elt.getName(), Elt.getInit(),
                                      VarargBaseTy));
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

    T = substType(T, Substitutions);
    validateTypeSimple(T);
    return T;
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
  T = substType(T, Substitutions);
  validateTypeSimple(T);
  return T;
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
