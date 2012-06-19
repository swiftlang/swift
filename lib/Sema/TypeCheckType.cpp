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
#include "swift/AST/NameLookup.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Twine.h"
using namespace swift;

/// Given an array slice type with a valid base type and no
/// implementation type, find its canonicalization.
static bool buildArraySliceType(TypeChecker &TC, ArraySliceType *sliceTy) {
  Type baseTy = sliceTy->getBaseType();

  SourceLoc loc = sliceTy->getFirstRefLoc();

  // As our current hack for array slices, require the base type to be
  // a nominal type X, then look for a type SliceX.
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
  if (!validateType(VD->getType(), isFirstPass)) return false;
  
  VD->overwriteType(ErrorType::get(Context));
  return true;
}

/// validateType - Types can contain expressions (in the default values for
/// tuple elements), and thus need semantic analysis to ensure that these
/// expressions are valid and that they have the appropriate conversions etc.
///
/// This returns true if the type is invalid.
bool TypeChecker::validateType(Type InTy, bool isFirstPass) {
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
    // These types are already canonical anyway.
    return false;
      
  case TypeKind::MetaType:
  case TypeKind::Module:
  case TypeKind::Protocol:
  case TypeKind::SubstArchetype:
    // Nothing to validate.
    break;

  case TypeKind::NameAlias: {
    TypeAliasDecl *D = cast<NameAliasType>(T)->getDecl();
    IsInvalid = !D->hasUnderlyingType() ||
                validateType(D->getUnderlyingType(), isFirstPass);
    if (IsInvalid)
      D->overwriteUnderlyingType(ErrorType::get(Context));
    break;
  }
  case TypeKind::Identifier:
    return validateType(cast<IdentifierType>(T)->getMappedType(), isFirstPass);
  case TypeKind::Paren:
    return validateType(cast<ParenType>(T)->getUnderlyingType(), isFirstPass);
  case TypeKind::Tuple: {
    TupleType *TT = cast<TupleType>(T);
    
    // Okay, we found an uncanonicalized tuple type, which might have default
    // values.  If so, we'll potentially have to update it.
    for (unsigned i = 0, e = TT->getFields().size(); i != e; ++i) {
      // The element has *at least* a type or an initializer, so we start by
      // verifying each individually.
      Type EltTy = TT->getFields()[i].getType();
      if (EltTy && validateType(EltTy, isFirstPass)) {
        IsInvalid = true;
        break;
      }

      Expr *EltInit = TT->getFields()[i].getInit();
      if (EltInit == 0) continue;

      if (isFirstPass) {
        if (!EltTy) {
          diagnose(EltInit->getLoc(), diag::tuple_global_missing_type);
          IsInvalid = true;
          break;
        }
        continue;
      } 

      Expr *OldInit = EltInit;
      if (typeCheckExpression(EltInit, EltTy)) {
        diagnose(OldInit->getLoc(),diag::while_converting_default_tuple_value,
                 EltTy);
        IsInvalid = true;
        break;
      }

      if (!EltTy)
        EltInit = convertToMaterializable(EltInit);

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

      EltInit->walk(CheckForLocalRef(*this));

      // If both a type and an initializer are specified, make sure the
      // initializer's type agrees with the (redundant) type.
      assert(EltTy.isNull() || EltTy->isEqual(EltInit->getType()));
      EltTy = EltInit->getType();

      TT->updateInitializedElementType(i, EltTy, EltInit);
    }
    break;
  }

  case TypeKind::LValue:
    // FIXME: diagnose non-materializability of object type!
    IsInvalid = validateType(cast<LValueType>(T)->getObjectType(), isFirstPass);
    break;
      
  case TypeKind::Function: {
    FunctionType *FT = cast<FunctionType>(T);
    IsInvalid = validateType(FT->getInput(), isFirstPass) ||
                validateType(FT->getResult(), isFirstPass);
    // FIXME: diagnose non-materializability of result type!
    break;
  }
  case TypeKind::Array: {
    ArrayType *AT = cast<ArrayType>(T);
    IsInvalid = validateType(AT->getBaseType(), isFirstPass);
    // FIXME: diagnose non-materializability of element type!
    // FIXME: We need to check AT->Size! (It also has to be convertible to int).
    break;
  }
  case TypeKind::ArraySlice: {
    ArraySliceType *AT = cast<ArraySliceType>(T);
    IsInvalid = validateType(AT->getBaseType(), isFirstPass);
    // FIXME: diagnose non-materializability of element type?
    if (!IsInvalid)
      IsInvalid = buildArraySliceType(*this, AT);
    break;
  }
      
  case TypeKind::ProtocolComposition: {
    ProtocolCompositionType *PC = cast<ProtocolCompositionType>(T);
    for (auto Proto : PC->getProtocols()) {
      if (validateType(Proto, isFirstPass))
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

namespace {
  /// \brief Flags that control how subtyping is checked.
  enum SubTypeFlags {
    /// \brief Whether to allow 'non-trivial' subtyping relationships, e.g.,
    /// ones that change the representation of the object.
    ST_AllowNonTrivial = 0x01,
    /// \brief Whether to allow 'non-trivial' subtyping relationships in the
    /// input/result of function types.
    ST_AllowNonTrivialFunction = 0x02
  };
}

/// \brief Helper routine for TypeChecker::isSubtypeOf, that performs the
/// actual 
static bool isSubtypeOf(TypeChecker &TC, Type T1, Type T2, unsigned Flags,
                        bool &Trivial) {
  assert((!(Flags & ST_AllowNonTrivialFunction) ||
          Flags & ST_AllowNonTrivial) &&
         "Non-trivial function input/result without non-trivial subtyping?");
  
  // If the types are equivalent, we're done.
  if (T1->isEqual(T2))
    return true;
  
  // A value of a given type is a subtype of a protocol type if it conforms
  // to that protocol type. This is a non-trivial conversion, because it
  // requires the introduction of a new protocol mapping.
  if (Flags & ST_AllowNonTrivial) {
    SmallVector<ProtocolDecl *, 4> T2Protos;
    if (T2->isExistentialType(T2Protos)) {
      for (auto Proto2 : T2Protos) {
        if (!TC.conformsToProtocol(T1, Proto2)) {
          return false;
        }
      }
      
      Trivial = false;
      return true;
    }
  }
  
  // From here on, we require the types to have equivalent kind.
  if (T1->getCanonicalType()->getKind() !=
      T2->getCanonicalType()->getKind())
    return false;
  
  // An lvalue type is a subtype of another lvalue type if their object types
  // are the same and its qualifiers are a subset of the qualifiers of the
  // other.
  if (auto LV1 = T1->getAs<LValueType>()) {
    auto LV2 = T2->getAs<LValueType>();
    return LV1->getQualifiers() <= LV2->getQualifiers() &&
           LV1->getObjectType()->isEqual(LV2->getObjectType());
  }
  
  // Function types allow covariant result types and contravariant argument
  // types, ignoring labels.
  if (auto Func1 = T1->getAs<FunctionType>()) {
    auto Func2 = T2->getAs<FunctionType>();

    // Compute the flags to be used for the subtyping checks of the input
    // and result types.
    unsigned SubFlags = Flags;
    if (Flags & ST_AllowNonTrivialFunction)
      SubFlags = SubFlags & ~ST_AllowNonTrivialFunction;
    else if (Flags & ST_AllowNonTrivial)
      SubFlags = SubFlags & ~ST_AllowNonTrivial;
    
    // [auto_closure] types are subtypes of the corresponding
    // non-[auto_closure] types.
    if (Func2->isAutoClosure() && !Func1->isAutoClosure())
      return false;
    
    // Result type can be covariant, ignoring labels.
    if (!isSubtypeOf(TC,
                     Func1->getResult()->getUnlabeledType(TC.Context),
                     Func2->getResult()->getUnlabeledType(TC.Context),
                     SubFlags, Trivial))
      return false;
    
    // Input types can be contravariant, ignoring labels.
    return isSubtypeOf(TC,
                       Func2->getInput()->getUnlabeledType(TC.Context),
                       Func1->getInput()->getUnlabeledType(TC.Context),
                       SubFlags, Trivial);
  }
  
  // Tuple types. The subtyping relationship for tuples is based on subtyping
  // of the elements, ignoring field names and default arguments.
  if (auto Tuple1 = T1->getAs<TupleType>()) {
    auto Tuple2 = T2->getAs<TupleType>();
    
    if (Tuple1->getFields().size() != Tuple2->getFields().size())
      return false;
    
    for (unsigned I = 0, N = Tuple1->getFields().size(); I != N; ++I)
      if (!isSubtypeOf(TC,
                       Tuple1->getElementType(I),
                       Tuple2->getElementType(I),
                       Flags & ~ST_AllowNonTrivial,
                       Trivial))
        return false;
    
    return true;
  }
  
  return false;  
}

/// \brief Determine whether T1 is a subtype of (or equivalent to) T2.
///
/// \param Trivial Will be set to 'false' if there are any subtyping
/// relationships that aren't 'trivial', in the sense that they require some
/// representation change (such as introducing a protocol-conformance mapping).
///
/// This checks for a non-strict subtyping relationship T1 <= T2.
bool TypeChecker::isSubtypeOf(Type T1, Type T2, bool &Trivial) {
  Trivial = true;
  unsigned Flags = ST_AllowNonTrivial | ST_AllowNonTrivialFunction;
  return ::isSubtypeOf(*this, T1, T2, Flags, Trivial);
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
  if (ArchetypeType *Archetype = dyn_cast<ArchetypeType>(T)) {
    TypeSubstitutionMap::const_iterator Known = Substitutions.find(Archetype);
    assert((Known != Substitutions.end()) && "Missing archetype substitution");
    return SubstArchetypeType::get(Archetype, Known->second, Context);
  }
  
  switch (T->getKind()) {
#define ALWAYS_CANONICAL_TYPE(Id, Parent) \
  case TypeKind::Id:                    \
    return T;
#define UNCHECKED_TYPE(Id, Parent) ALWAYS_CANONICAL_TYPE(Id, Parent)
#define TYPE(Id, Parent)
#include "swift/AST/TypeNodes.def"

  case TypeKind::NameAlias: {
    auto Alias = cast<NameAliasType>(T);
    auto UnderlyingTy = substType(Alias->getDecl()->getUnderlyingType(),
                                  Substitutions);
    if (!UnderlyingTy)
      return Type();
    
    if (UnderlyingTy.getPointer()
          == Alias->getDecl()->getUnderlyingType().getPointer())
      return T;
    
    // FIXME: Loses the structure of the NameAliasType!
    return UnderlyingTy;
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
    
    // FIXME: Loses the structure of the IdentifierType!
    return MappedTy;
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
      
  case TypeKind::SubstArchetype: {
    auto SubstAT = cast<SubstArchetypeType>(T);
    auto Subst = substType(SubstAT->getSubstType(), Substitutions);
    if (!Subst)
      return Type();
    
    if (Subst.getPointer() == SubstAT->getSubstType().getPointer())
      return T;
    
    return SubstArchetypeType::get(SubstAT->getArchetype(), Subst, Context);
  }
      
  case TypeKind::Function: {
    auto Function = cast<FunctionType>(T);
    auto InputTy = substType(Function->getInput(), Substitutions);
    if (!InputTy)
      return Type();
    auto ResultTy = substType(Function->getResult(), Substitutions);
    if (!ResultTy)
      return Type();
    
    if (InputTy.getPointer() == Function->getInput().getPointer() &&
        ResultTy.getPointer() == Function->getResult().getPointer())
      return T;
    
    return FunctionType::get(InputTy, ResultTy, Function->isAutoClosure(),
                             Context);
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
    
    return ProtocolCompositionType::get(Context, PC->getFirstLoc(),
                                        Context.AllocateCopy(Protocols));
  }
  }
  
  llvm_unreachable("Unhandled type in substitution");
}

Type TypeChecker::substMemberTypeWithBase(Type T, Type BaseTy) {
  if (!BaseTy)
    return T;

  auto BaseArchetype = BaseTy->getRValueType()->getAs<ArchetypeType>();
  if (!BaseArchetype)
    return T;

  // FIXME: Lame that we need to copy the substitutions here, but the
  // underlying DenseMap might get reallocated.
  TypeSubstitutionMap Substitutions = Context.AssociatedTypeMap[BaseArchetype];
  return substType(T, Substitutions);
}
