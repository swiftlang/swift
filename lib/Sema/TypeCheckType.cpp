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

  // As a fallback until IRGen for generic types works, try looking for
  // SliceX, where the base type is the nominal type X.
  Identifier name;
  if (NominalType *Nominal = baseTy->getAs<NominalType>()) {
    name = Nominal->getDecl()->getName();
  } else {
    TC.diagnose(loc, diag::base_of_array_slice_not_nominal, baseTy);
    return true;
  }

  llvm::SmallString<32> nameBuffer("Slice");
  nameBuffer.append(name.str());

  UnqualifiedLookup sliceLookup(TC.Context.getIdentifier(nameBuffer), &TC.TU);
  TypeDecl *implementationType = sliceLookup.getSingleTypeResult();
  if (!implementationType) {
    TC.diagnose(loc, diag::slice_type_not_found, nameBuffer);
    return true;
  }
  sliceTy->setImplementationType(implementationType->getDeclaredType());
  return false;
}

Type TypeChecker::getArraySliceType(SourceLoc loc, Type elementType) {
  ArraySliceType *sliceTy = ArraySliceType::get(elementType, Context);
  if (sliceTy->hasCanonicalTypeComputed()) return sliceTy;
  if (buildArraySliceType(*this, sliceTy, loc)) return Type();
  sliceTy->getCanonicalType();
  validateTypeSimple(sliceTy->getImplementationType());
  return sliceTy;
}

/// \brief Determine whether all of the lookup results are associated types.
static bool
allAssociatedTypes(SmallVectorImpl<UnqualifiedLookupResult> &Results) {
  for (auto &Result : Results) {
    if (Result.hasValueDecl() &&
        isa<TypeAliasDecl>(Result.getValueDecl()) &&
        isa<ProtocolDecl>(Result.getValueDecl()->getDeclContext()))
      continue;

    return false;
  }

  return true;
}

/// \brief Determine whether all of the lookup results are associated types.
static bool
allAssociatedTypes(SmallVectorImpl<ValueDecl *> &Results) {
  for (auto VD : Results) {
    if (isa<TypeAliasDecl>(VD) &&
        isa<ProtocolDecl>(VD->getDeclContext()))
      continue;

    return false;
  }

  return true;
}

/// validateType - Types can contain expressions (in the default values for
/// tuple elements), and thus need semantic analysis to ensure that these
/// expressions are valid and that they have the appropriate conversions etc.
///
/// This returns true if the type is invalid.
bool TypeChecker::validateType(TypeLoc &Loc, bool isFirstPass) {
  Type InTy = Loc.getType();
  assert(InTy && "Cannot validate null types!");

  TypeBase *T = InTy.getPointer();
  // FIXME: Verify that these aren't circular and infinite size.
  
  // If we've already validated this type, don't do so again.
  if (T->getValidated()) return false;

  // \brief RAII object that sets the validation pass on the type when it
  // goes out of scope.
  class SetValidation {
    Type T;

  public:
    SetValidation(Type T) : T(T) { }
    ~SetValidation() {
      T->setValidated();
    }
  } setValidation(T);

  bool IsInvalid = false;
  switch (T->getKind()) {
  case TypeKind::Error:
    // Error already diagnosed.
    return true;
  case TypeKind::BuiltinFloat:
  case TypeKind::BuiltinInteger:
  case TypeKind::BuiltinRawPointer:
  case TypeKind::BuiltinOpaquePointer:
  case TypeKind::BuiltinObjectPointer:
  case TypeKind::BuiltinObjCPointer:
  case TypeKind::UnstructuredUnresolved:
  case TypeKind::OneOf:
  case TypeKind::Struct:
  case TypeKind::Class:
  case TypeKind::Archetype:
  case TypeKind::UnboundGeneric:
    // These types are already canonical anyway.
    return false;
      
  case TypeKind::Module:
  case TypeKind::Protocol:
  case TypeKind::DeducibleGenericParam:
  case TypeKind::TypeVariable:
    // Nothing to validate.
    break;

  case TypeKind::Substituted: {
    TypeLoc TL(cast<SubstitutedType>(T)->getReplacementType(),
               Loc.getSourceRange());
    return validateType(TL, isFirstPass);
  }

  case TypeKind::NameAlias: {
    TypeAliasDecl *D = cast<NameAliasType>(T)->getDecl();
    IsInvalid = validateType(D->getUnderlyingTypeLoc(), isFirstPass);
    if (IsInvalid)
      D->getUnderlyingTypeLoc().setInvalidType(Context);
    break;
  }
  case TypeKind::Identifier: {
    IdentifierType *DNT = cast<IdentifierType>(T);
    if (DNT->Components.back().Value.is<Type>()) {
      // FIXME: Refactor this to avoid fake TypeLoc
      TypeLoc TempLoc = TypeLoc::withoutLoc(DNT->getMappedType());
      IsInvalid = validateType(TempLoc, isFirstPass);
      break;
    }
    MutableArrayRef<IdentifierType::Component> Components = DNT->Components;

    // Iteratively resolve the components.
    IdentifierType::Component LastOne = Components[0];
    for (auto &C : Components) {
      TypeDecl *TD = nullptr;
      Type BaseTy;
      if (!C.Value.isNull()) {
        // If there is a value, it must be a ValueDecl; resolve it.
        TD = dyn_cast<TypeDecl>(C.Value.get<ValueDecl*>());
      } else if (LastOne.Value.isNull()) {
        // We haven't resolved anything yet; use an unqualified lookup.
        Identifier Name = C.Id;
        SourceLoc Loc = C.Loc;

        // FIXME: This loop is ridiculously inefficient.
        DeclContext *DC = nullptr;
        for (auto IdAndContext : TU.getUnresolvedIdentifierTypes()) {
          if (IdAndContext.first == DNT) {
            DC = IdAndContext.second;
            break;
          }
        }
        assert(DC && "no entry in UnresolvedIdentifierTypes "
                     "for unresolved identifier type!");

        // Perform an unqualified lookup.
        UnqualifiedLookup Globals(Name, DC, SourceLoc(), /*TypeLookup*/true);

        // FIXME: We need to centralize ambiguity checking
        if (Globals.Results.size() > 1 && !allAssociatedTypes(Globals.Results)){
          diagnose(Loc, diag::ambiguous_type_base, Name)
            .highlight(SourceRange(Loc, Components.back().Loc));
          for (auto Result : Globals.Results) {
            if (Globals.Results[0].hasValueDecl())
              diagnose(Result.getValueDecl(), diag::found_candidate);
            else
              diagnose(Loc, diag::found_candidate);
          }
          return true;
        }

        if (Globals.Results.empty()) {
          diagnose(Loc, Components.size() == 1 ? 
                     diag::use_undeclared_type : diag::unknown_name_in_type, Name)
            .highlight(SourceRange(Loc, Components.back().Loc));
          return true;
        }

        switch (Globals.Results[0].Kind) {
        case UnqualifiedLookupResult::ModuleMember:
        case UnqualifiedLookupResult::LocalDecl:
        case UnqualifiedLookupResult::MemberProperty:
        case UnqualifiedLookupResult::MemberFunction:
        case UnqualifiedLookupResult::MetatypeMember:
        case UnqualifiedLookupResult::ExistentialMember:
        case UnqualifiedLookupResult::ArchetypeMember:
          TD = dyn_cast<TypeDecl>(Globals.Results[0].getValueDecl());
          break;
        case UnqualifiedLookupResult::ModuleName:
          C.Value = Globals.Results[0].getNamedModule();
          break;

        case UnqualifiedLookupResult::MetaArchetypeMember:
          // FIXME: This is actually possible in protocols.
          llvm_unreachable("meta-archetype member in unqualified name lookup");
          break;
        }

        // If we found an associated type in an inherited protocol, the base
        // for our reference to this associated type is our own 'This'.
        // FIXME: We'll need to turn this into a more general 'implicit base'
        // projection, so that any time we find something in an outer context
        // we make sure to map to that context.
        if (TD && isa<TypeAliasDecl>(TD) &&
            isa<ProtocolDecl>(TD->getDeclContext()) &&
            TD->getDeclContext() != DC) {
          if (auto Proto = dyn_cast<ProtocolDecl>(DC)) {
            BaseTy = Proto->getThis()->getDeclaredType();
          }
        }
      } else if (auto M = LastOne.Value.dyn_cast<Module*>()) {
        // Lookup into a named module.
        SmallVector<ValueDecl*, 8> Decls;
        M->lookupValue(Module::AccessPathTy(), C.Id, 
                       NLKind::QualifiedLookup, Decls);
        if (Decls.size() == 1 ||
            (Decls.size() > 1 && !allAssociatedTypes(Decls)))
          TD = dyn_cast<TypeDecl>(Decls.front());
        // FIXME: Diagnostic if not found or ambiguous?
      } else if (auto T = LastOne.Value.dyn_cast<Type>()) {
        // Lookup into a type.
        BaseTy = T;
        MemberLookup ML(T, C.Id, TU, /*TypeLookup*/true);
        if (ML.Results.size() == 1)
          TD = dyn_cast<TypeDecl>(ML.Results.back().D);
        // FIXME: Diagnostic if not found or ambiguous?
      } else {
        diagnose(C.Loc, diag::unknown_dotted_type_base, LastOne.Id)
          .highlight(SourceRange(Components[0].Loc, Components.back().Loc));
        return true;
      }

      if (TD) {
        Type Ty;
        if (!C.GenericArgs.empty()) {
          if (auto NTD = dyn_cast<NominalTypeDecl>(TD)) {
            if (auto Params = NTD->getGenericParams()) {
              if (Params->size() == C.GenericArgs.size()) {
                SmallVector<Type, 4> GenericArgTypes;
                for (TypeLoc T : C.GenericArgs)
                  GenericArgTypes.push_back(T.getType());
                Ty = BoundGenericType::get(NTD, BaseTy, GenericArgTypes);
              }
                
            }
          }

          // FIXME: Diagnostic if applying the arguments fails?
        } else {
          Ty = TD->getDeclaredType();

          // If we're referencing a nominal type that is in a generic context,
          // we need to consider our base type a well.
          // FIXME: NameAliasTypes need to be substituted through as well.
          // FIXME: Also deal with unbound and bound generic types.
          if (auto nominalD = dyn_cast<NominalTypeDecl>(TD)) {
            if (BaseTy &&
                !nominalD->getGenericParams() &&
                nominalD->getDeclContext()->getGenericParamsOfContext()) {
              Ty = NominalType::get(nominalD, BaseTy, Context);
            }
          }
        }
        if (Ty) {
          // FIXME: Refactor this to avoid fake TypeLoc
          TypeLoc TempLoc{ Ty, Loc.getSourceRange() };
          if (validateType(TempLoc, isFirstPass)) {
            return true;
          }

          if (BaseTy)
            Ty = substMemberTypeWithBase(Ty, TD, BaseTy);
          C.Value = Ty;
        }
      }

      if (C.Value.isNull()) {
        if (LastOne.Value.isNull())
          diagnose(C.Loc, Components.size() == 1 ? 
                   diag::named_definition_isnt_type :
                   diag::dotted_reference_not_type, C.Id)
            .highlight(SourceRange(C.Loc, Components.back().Loc));
        else
          diagnose(C.Loc, diag::invalid_member_type, C.Id, LastOne.Id)
            .highlight(SourceRange(Components[0].Loc, Components.back().Loc));
        return true;
      }

      LastOne = C;
    }
    break;
  }
  case TypeKind::Paren: {
    // FIXME: Extract real typeloc info.
    TypeLoc TempLoc{ cast<ParenType>(T)->getUnderlyingType(),
                     Loc.getSourceRange() };
    return validateType(TempLoc, isFirstPass);
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
      if (EltTy && validateType(TempLoc, isFirstPass)) {
        IsInvalid = true;
        break;
      }

      ExprHandle *EltInit = TT->getFields()[i].getInit();
      if (EltInit == 0) continue;

      if (isFirstPass) {
        if (!EltTy) {
          diagnose(EltInit->getExpr()->getLoc(),
                   diag::tuple_global_missing_type);
          IsInvalid = true;
          break;
        }
        continue;
      } 

      Expr *initExpr = EltInit->getExpr();
      if (typeCheckExpression(initExpr, EltTy)) {
        diagnose(initExpr->getLoc(), diag::while_converting_default_tuple_value,
                 EltTy);
        IsInvalid = true;
        break;
      }

      if (!EltTy)
        initExpr = convertToMaterializable(initExpr);

      struct CheckForLocalRef : public ASTWalker {
        TypeChecker &TC;

        CheckForLocalRef(TypeChecker &TC) : TC(TC) {}

        bool walkToExprPre(Expr *E) {
          if (DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(E)) {
            if (DRE->getDecl()->getDeclContext()->isLocalContext()) {
              TC.diagnose(E->getLoc(), diag::tuple_default_local_ref);
            }
          }
          return true;
        }
      };

      initExpr->walk(CheckForLocalRef(*this));

      // If both a type and an initializer are specified, make sure the
      // initializer's type agrees with the (redundant) type.
      assert(EltTy.isNull() || EltTy->isEqual(initExpr->getType()));
      EltTy = initExpr->getType();

      EltInit->setExpr(initExpr);
      TT->updateInitializedElementType(i, EltTy);
    }
    break;
  }

  case TypeKind::LValue: {
    // FIXME: Extract real typeloc info.
    TypeLoc TempLoc{ cast<LValueType>(T)->getObjectType(),
                     Loc.getSourceRange() };
    IsInvalid = validateType(TempLoc, isFirstPass);
    // FIXME: diagnose non-materializability of object type!
    break;
  }
      
  case TypeKind::Function:
  case TypeKind::PolymorphicFunction: {
    AnyFunctionType *FT = cast<AnyFunctionType>(T);
    // FIXME: Extract real typeloc info.
    TypeLoc TempLoc{ FT->getInput(), Loc.getSourceRange() };
    IsInvalid = validateType(TempLoc, isFirstPass);
    if (!IsInvalid) {
      TempLoc = TypeLoc{ FT->getResult(), Loc.getSourceRange() };
      IsInvalid = validateType(TempLoc, isFirstPass);
    }
    // FIXME: diagnose non-materializability of result type!
    break;
  }
  case TypeKind::Array: {
    ArrayType *AT = cast<ArrayType>(T);
    // FIXME: Extract real typeloc info.
    TypeLoc TempLoc{ AT->getBaseType(), Loc.getSourceRange() };
    IsInvalid = validateType(TempLoc, isFirstPass);
    // FIXME: diagnose non-materializability of element type!
    // FIXME: We need to check AT->Size! (It also has to be convertible to int).
    break;
  }
  case TypeKind::ArraySlice: {
    ArraySliceType *AT = cast<ArraySliceType>(T);
    // FIXME: Extract real typeloc info.
    TypeLoc TempLoc{ AT->getBaseType(), Loc.getSourceRange() };
    IsInvalid = validateType(TempLoc, isFirstPass);
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
      if (validateType(TempLoc, isFirstPass))
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
    IsInvalid = validateType(TempLoc, isFirstPass);
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
      if (validateType(TempLoc, isFirstPass)) {
        IsInvalid = true;
        break;
      }
    }

    // Check protocol conformance.
    if (!IsInvalid && !BGT->hasSubstitutions()) {
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
                                                  Conformance, false, true));
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
  case TypeKind::Class: {
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

Type TypeChecker::substType(Type origType, TypeSubstitutionMap &Substitutions) {
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

    // If the parent is a deducible generic parameter, build a new child
    // generic parameter and record it.
    if (auto DeducibleParent = SubstParent->getAs<DeducibleGenericParamType>()){
      auto OriginalArchetype = dyn_cast<ArchetypeType>(substOrig);
      if (!OriginalArchetype)
        return type;

      auto NewChild
        = DeducibleGenericParamType::getNew(Context, DeducibleParent,
                                            OriginalArchetype);
      Substitutions[substOrig] = NewChild;
      return NewChild;
    }

    // If the parent is an archetype, extract the child archetype with the
    // given name.
    if (auto ArchetypeParent = SubstParent->getAs<ArchetypeType>()) {
      return ArchetypeParent->getNestedType(substOrig->getName());
    }
     
    // Retrieve the type with the given name.
    // FIXME: Shouldn't we be using protocol-conformance information here?
    MemberLookup ML(SubstParent, substOrig->getName(), TU, /*TypeLookup*/true);
    assert(ML.Results.size() && "No type lookup results?");
    TypeDecl *TD = cast<TypeDecl>(ML.Results.back().D);
    return substMemberTypeWithBase(TD->getDeclaredType(), TD, SubstParent);
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
