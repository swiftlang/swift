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
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Twine.h"
using namespace swift;

/// Given an array slice type with a valid base type and no
/// implementation type, find its canonicalization.
static bool buildArraySliceType(TypeChecker &TC, ArraySliceType *sliceTy) {
  Type baseTy = sliceTy->getBaseType();
  SourceLoc loc = sliceTy->getFirstRefLoc();

  // Hack for array slices: first, try to look up Slice<T>.
  UnqualifiedLookup genericSliceLookup(TC.Context.getIdentifier("Slice"),
                                       &TC.TU);
  if (TypeDecl *sliceTypeDecl = genericSliceLookup.getSingleTypeResult()) {
    if (auto sliceTypeNTD = dyn_cast<NominalTypeDecl>(sliceTypeDecl)) {
      if (auto Params = sliceTypeNTD->getGenericParams()) {
        if (Params->size() == 1) {
          Type implTy = BoundGenericType::get(sliceTypeNTD, baseTy);
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
  ArraySliceType *sliceTy = ArraySliceType::get(elementType, loc, Context);
  if (sliceTy->hasCanonicalTypeComputed()) return sliceTy;
  if (buildArraySliceType(*this, sliceTy)) return Type();
  sliceTy->getCanonicalType();
  return sliceTy;
}

/// validateType - Recursively check to see if the type of a decl is valid.  If
/// not, diagnose the problem and collapse it to an ErrorType.
bool TypeChecker::validateType(ValueDecl *VD, bool isFirstPass) {
  if (!validateType(VD->getType(), VD->getLoc(), isFirstPass)) return false;
  
  VD->overwriteType(ErrorType::get(Context));
  return true;
}

/// validateType - Types can contain expressions (in the default values for
/// tuple elements), and thus need semantic analysis to ensure that these
/// expressions are valid and that they have the appropriate conversions etc.
///
/// This returns true if the type is invalid.
bool TypeChecker::validateType(Type InTy, SourceLoc Loc, bool isFirstPass) {
  assert(InTy && "Cannot validate null types!");

  TypeBase *T = InTy.getPointer();
  // FIXME: Verify that these aren't circular and infinite size.
  
  // If a type has a canonical type, then it is known safe.
  if (T->hasCanonicalTypeComputed()) return false;

  bool IsInvalid = false;
  
  switch (T->getKind()) {
  case TypeKind::Error:
    // Error already diagnosed.
    return true;
  case TypeKind::BuiltinFloat:
  case TypeKind::BuiltinInteger:
  case TypeKind::BuiltinRawPointer:
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
  case TypeKind::Substituted:
  case TypeKind::DeducibleGenericParam:
    // Nothing to validate.
    break;

  case TypeKind::NameAlias: {
    TypeAliasDecl *D = cast<NameAliasType>(T)->getDecl();
    IsInvalid = !D->hasUnderlyingType() ||
                validateType(D->getUnderlyingType(), Loc, isFirstPass);
    if (IsInvalid)
      D->overwriteUnderlyingType(ErrorType::get(Context));
    break;
  }
  case TypeKind::Identifier: {
    IdentifierType *DNT = cast<IdentifierType>(T);
    if (DNT->Components.back().Value.is<Type>()) {
      IsInvalid = validateType(DNT->getMappedType(), Loc, isFirstPass);
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
        DeclContext *DC;
        for (auto IdAndContext : TU.getUnresolvedIdentifierTypes()) {
          if (IdAndContext.first == DNT) {
            DC = IdAndContext.second;
            break;
          }
        }

        // Perform an unqualified lookup.
        UnqualifiedLookup Globals(Name, DC, SourceLoc(), /*TypeLookup*/true);

        if (Globals.Results.size() > 1) {
          diagnose(Loc, diag::abiguous_type_base, Name)
            << SourceRange(Loc, Components.back().Loc);
          for (auto Result : Globals.Results) {
            if (Globals.Results[0].hasValueDecl())
              diagnose(Result.getValueDecl()->getStartLoc(), diag::found_candidate);
            else
              diagnose(Loc, diag::found_candidate);
          }
          return true;
        }

        if (Globals.Results.empty()) {
          diagnose(Loc, Components.size() == 1 ? 
                     diag::use_undeclared_type : diag::unknown_name_in_type, Name)
            << SourceRange(Loc, Components.back().Loc);
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
      } else if (auto M = LastOne.Value.dyn_cast<Module*>()) {
        // Lookup into a named module.
        SmallVector<ValueDecl*, 8> Decls;
        M->lookupValue(Module::AccessPathTy(), C.Id, 
                       NLKind::QualifiedLookup, Decls);
        if (Decls.size() == 1)
          TD = dyn_cast<TypeDecl>(Decls.back());
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
          << SourceRange(Components[0].Loc, Components.back().Loc);
        return true;
      }

      if (TD) {
        Type Ty;
        if (!C.GenericArgs.empty()) {
          if (auto NTD = dyn_cast<NominalTypeDecl>(TD)) {
            if (auto Params = NTD->getGenericParams()) {
              if (Params->size() == C.GenericArgs.size())
                Ty = BoundGenericType::get(NTD, C.GenericArgs);
            }
          }

          // FIXME: Diagnostic if applying the arguments fails?
        } else {
          Ty = TD->getDeclaredType();
        }
        if (Ty) {
          if (validateType(Ty, Loc, isFirstPass))
            return true;
          if (BaseTy)
            Ty = substMemberTypeWithBase(Ty, BaseTy);
          C.Value = Ty;
        }
      }

      if (C.Value.isNull()) {
        if (LastOne.Value.isNull())
          diagnose(C.Loc, Components.size() == 1 ? 
                   diag::named_definition_isnt_type :
                   diag::dotted_reference_not_type, C.Id)
            << SourceRange(C.Loc, Components.back().Loc);
        else
          diagnose(C.Loc, diag::invalid_member_type, C.Id, LastOne.Id)
            << SourceRange(Components[0].Loc, Components.back().Loc);
        return true;
      }

      LastOne = C;
    }
    break;
  }
  case TypeKind::Paren:
    return validateType(cast<ParenType>(T)->getUnderlyingType(), Loc,
                        isFirstPass);
  case TypeKind::Tuple: {
    TupleType *TT = cast<TupleType>(T);
    
    // Okay, we found an uncanonicalized tuple type, which might have default
    // values.  If so, we'll potentially have to update it.
    for (unsigned i = 0, e = TT->getFields().size(); i != e; ++i) {
      // The element has *at least* a type or an initializer, so we start by
      // verifying each individually.
      Type EltTy = TT->getFields()[i].getType();
      if (EltTy && validateType(EltTy, Loc, isFirstPass)) {
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

  case TypeKind::LValue:
    // FIXME: diagnose non-materializability of object type!
    IsInvalid = validateType(cast<LValueType>(T)->getObjectType(), Loc,
                             isFirstPass);
    break;
      
  case TypeKind::Function:
  case TypeKind::PolymorphicFunction: {
    AnyFunctionType *FT = cast<AnyFunctionType>(T);
    IsInvalid = validateType(FT->getInput(), Loc, isFirstPass) ||
                validateType(FT->getResult(), Loc, isFirstPass);
    // FIXME: diagnose non-materializability of result type!
    break;
  }
  case TypeKind::Array: {
    ArrayType *AT = cast<ArrayType>(T);
    IsInvalid = validateType(AT->getBaseType(), Loc, isFirstPass);
    // FIXME: diagnose non-materializability of element type!
    // FIXME: We need to check AT->Size! (It also has to be convertible to int).
    break;
  }
  case TypeKind::ArraySlice: {
    ArraySliceType *AT = cast<ArraySliceType>(T);
    IsInvalid = validateType(AT->getBaseType(), Loc, isFirstPass);
    // FIXME: diagnose non-materializability of element type?
    if (!IsInvalid)
      IsInvalid = buildArraySliceType(*this, AT);
    break;
  }
      
  case TypeKind::ProtocolComposition: {
    ProtocolCompositionType *PC = cast<ProtocolCompositionType>(T);
    for (auto Proto : PC->getProtocols()) {
      if (validateType(Proto, Loc, isFirstPass))
        IsInvalid = true;
      else if (!Proto->isExistentialType()) {
        // FIXME: Terrible source-location information.
        diagnose(PC->getFirstLoc(), diag::protocol_composition_not_protocol,
                 Proto);
        IsInvalid = true;
      }
    }
    break;
  }

  case TypeKind::MetaType: {
    MetaTypeType *Meta = cast<MetaTypeType>(T);
    IsInvalid = validateType(Meta->getInstanceType(), Loc,
                             isFirstPass);
    break;
  }

  case TypeKind::BoundGeneric: {
    BoundGenericType *BGT = cast<BoundGenericType>(T);
    unsigned Index = 0;
    for (Type Arg : BGT->getGenericArgs()) {
      if (validateType(Arg, Loc, isFirstPass)) {
        IsInvalid = true;
        break;
      }
    }

    // Check protocol conformance.
    if (!isFirstPass && !IsInvalid && !BGT->hasConformanceInformation()) {
      SmallVector<ProtocolConformance *, 4> Conformances;
      SmallVector<unsigned, 4> Offsets;
      for (Type Arg : BGT->getGenericArgs()) {
        Offsets.push_back(Conformances.size());
        
        auto GP = BGT->getDecl()->getGenericParams()->getParams()[Index++];
        auto Archetype = GP.getAsTypeParam()->getDeclaredType()
                           ->getAs<ArchetypeType>();
        for (auto Proto : Archetype->getConformsTo()) {
          ProtocolConformance *Conformance;
          if (conformsToProtocol(Arg, Proto, &Conformance)) {
            Conformances.push_back(Conformance);
          } else {
            // FIXME: Wretched location information
            diagnose(Loc, diag::invalid_implicit_protocol_conformance,
                     Arg, Proto->getDeclaredType());
            IsInvalid = true;
          }
        }
      }

      if (!IsInvalid) {
        BGT->setConformances(Offsets, Conformances);
      }
    }
    break;
  }
  }

  // If we determined that this type is invalid, erase it in the caller.
  if (IsInvalid)
    return true;

  // Now that we decided that this type is ok, get the canonical type for it so
  // that we never reanalyze it again.
  // If it is ever a performance win to avoid computing canonical types, we can
  // just keep a SmallPtrSet of analyzed Types in TypeChecker.
  InTy->getCanonicalType();
  
  // FIXME: This isn't good enough: top-level stuff can have these as well and
  // their types need to be resolved at the end of name binding.  Perhaps we
  // should require them to have explicit types even if they have values and 
  // let the value mismatch be detected at typechecking time? 
  return false;
}

/// \brief Substitute the given archetypes for their substitution types
/// within the given type.
///
/// \returns The substituted type, or null if the substitution failed.
///
/// FIXME: We probably want to have both silent and loud failure modes. However,
/// the only possible failure now is from array slice types, which occur
/// simply because we don't have Slice<T> yet.
Type TypeChecker::substType(Type T, TypeSubstitutionMap &Substitutions) {
  if (auto Original = dyn_cast<SubstitutableType>(T)) {
    TypeSubstitutionMap::const_iterator Known = Substitutions.find(Original);
    if (Known == Substitutions.end() || !Known->second)
      return T;

    return SubstitutedType::get(Original, Known->second, Context);
  }
  
  switch (T->getKind()) {
#define ALWAYS_CANONICAL_TYPE(Id, Parent) \
  case TypeKind::Id:                    \
    return T;
#define UNCHECKED_TYPE(Id, Parent) ALWAYS_CANONICAL_TYPE(Id, Parent)
#define TYPE(Id, Parent)
#include "swift/AST/TypeNodes.def"

  case TypeKind::BoundGeneric: {
    auto BGT = cast<BoundGenericType>(T);
    SmallVector<Type, 4> SubstArgs;
    bool AnyChanged = false;
    for (Type Arg : BGT->getGenericArgs()) {
      Type SubstArg = substType(Arg, Substitutions);
      if (!SubstArg)
        return Type();
      SubstArgs.push_back(SubstArg);
      if (SubstArg.getPointer() != Arg.getPointer())
        AnyChanged = true;
    }

    if (!AnyChanged)
      return T;

    return BoundGenericType::get(BGT->getDecl(), SubstArgs);
  }

  case TypeKind::MetaType: {
    auto Meta = cast<MetaTypeType>(T);
    auto UnderlyingTy = substType(Meta->getInstanceType(),
                                  Substitutions);
    if (!UnderlyingTy)
      return Type();
    
    if (UnderlyingTy.getPointer() == Meta->getInstanceType().getPointer())
      return T;

    return MetaTypeType::get(UnderlyingTy, Context);
  }

  case TypeKind::NameAlias: {
    auto Alias = cast<NameAliasType>(T);
    auto UnderlyingTy = substType(Alias->getDecl()->getUnderlyingType(),
                                  Substitutions);
    if (!UnderlyingTy)
      return Type();
    
    if (UnderlyingTy.getPointer()
          == Alias->getDecl()->getUnderlyingType().getPointer())
      return T;

    return SubstitutedType::get(T, UnderlyingTy, Context);
  }
      
  case TypeKind::Identifier: {
    auto Id = cast<IdentifierType>(T);
    if (!Id->isMapped())
      return T;
    
    auto MappedTy = substType(Id->getMappedType(), Substitutions);
    if (!MappedTy)
      return Type();
    
    if (MappedTy.getPointer() == Id->getMappedType().getPointer())
      return T;
    
    return SubstitutedType::get(T, MappedTy, Context);
  }
      
  case TypeKind::Paren: {
    auto Paren = cast<ParenType>(T);
    Type Underlying = substType(Paren->getUnderlyingType(), Substitutions);
    if (!Underlying)
      return Type();
    
    if (Underlying.getPointer() == Paren->getUnderlyingType().getPointer())
      return T;
    
    return ParenType::get(Context, Underlying);
  }

  case TypeKind::Tuple: {
    auto Tuple = cast<TupleType>(T);
    bool AnyChanged = false;
    SmallVector<TupleTypeElt, 4> Elements;
    unsigned Index = 0;
    for (auto Elt : Tuple->getFields()) {
      Type EltTy = substType(Elt.getType(), Substitutions);
      if (!EltTy)
        return Type();
            
      // FIXME: Substitute into default arguments rather than simply dropping
      // them.

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
                                          /*Init=*/nullptr,
                                          FromElt.getVarargBaseTy()));
        }
        
        AnyChanged = true;
      }

      // Substitute into the base type of a variadic tuple.
      Type VarargBaseTy;
      if (Elt.isVararg()) {
        VarargBaseTy = substType(Elt.getVarargBaseTy(), Substitutions);
        if (!VarargBaseTy)
          return Type();
      }
      
      // Add the new tuple element, with the new type, no initializer,
      Elements.push_back(TupleTypeElt(EltTy, Elt.getName(), /*Init=*/nullptr,
                                      VarargBaseTy));
      ++Index;
    }
    
    if (!AnyChanged)
      return T;
    
    return TupleType::get(Elements, Context);
  }
      
  case TypeKind::Substituted: {
    auto SubstAT = cast<SubstitutedType>(T);
    auto Subst = substType(SubstAT->getReplacementType(), Substitutions);
    if (!Subst)
      return Type();
    
    if (Subst.getPointer() == SubstAT->getReplacementType().getPointer())
      return T;
    
    return SubstitutedType::get(SubstAT->getOriginal(), Subst, Context);
  }
      
  case TypeKind::Function:
  case TypeKind::PolymorphicFunction: {
    auto Function = cast<AnyFunctionType>(T);
    auto InputTy = substType(Function->getInput(), Substitutions);
    if (!InputTy)
      return Type();
    auto ResultTy = substType(Function->getResult(), Substitutions);
    if (!ResultTy)
      return Type();
    
    if (InputTy.getPointer() == Function->getInput().getPointer() &&
        ResultTy.getPointer() == Function->getResult().getPointer())
      return T;
    
    if (auto polyFn = dyn_cast<PolymorphicFunctionType>(T)) {
      return PolymorphicFunctionType::get(InputTy, ResultTy,
                                          &polyFn->getGenericParams(),
                                          Context);
    } else {
      auto fn = cast<FunctionType>(T);
      return FunctionType::get(InputTy, ResultTy, fn->isAutoClosure(),
                               Context);
    }
  }
      
  case TypeKind::Array: {
    auto Array = cast<ArrayType>(T);
    auto BaseTy = substType(Array->getBaseType(), Substitutions);
    if (!BaseTy)
      return Type();
    
    if (BaseTy.getPointer() == Array->getBaseType().getPointer())
      return T;
    
    return ArrayType::get(BaseTy, Array->getSize(), Context);
  }
      
  case TypeKind::ArraySlice: {
    auto Slice = cast<ArraySliceType>(T);
    auto BaseTy = substType(Slice->getBaseType(), Substitutions);
    if (!BaseTy)
      return Type();
    
    if (BaseTy.getPointer() == Slice->getBaseType().getPointer())
      return T;
    
    return getArraySliceType(Slice->getFirstRefLoc(), BaseTy);
  }
      
  case TypeKind::LValue: {
    auto LValue = cast<LValueType>(T);
    auto ObjectTy = substType(LValue->getObjectType(), Substitutions);
    if (!ObjectTy)
      return Type();
    
    if (ObjectTy.getPointer() == LValue->getObjectType().getPointer())
      return T;
    
    return LValueType::get(ObjectTy, LValue->getQualifiers(), Context);
  }
      
  case TypeKind::ProtocolComposition: {
    auto PC = cast<ProtocolCompositionType>(T);
    SmallVector<Type, 4> Protocols;
    bool AnyChanged = false;
    unsigned Index = 0;
    for (auto Proto : PC->getProtocols()) {
      auto SubstProto = substType(Proto, Substitutions);
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
      return T;
    
    return ProtocolCompositionType::get(Context, PC->getFirstLoc(), Protocols);
  }
  }
  
  llvm_unreachable("Unhandled type in substitution");
}

Type TypeChecker::substMemberTypeWithBase(Type T, Type BaseTy) {
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

  // FIXME: Lame that we need to copy the substitutions here, but the
  // underlying DenseMap might get reallocated.
  TypeSubstitutionMap Substitutions = Context.AssociatedTypeMap[BaseArchetype];
  return substType(T, Substitutions);
}
