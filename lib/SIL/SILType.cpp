//===--- SILType.cpp - Defines SILType ------------------------------------===//
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

#include "swift/SIL/SILType.h"
#include "swift/AST/Type.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/AbstractionPattern.h"

using namespace swift;
using namespace swift::Lowering;

SILType SILType::getNativeObjectType(const ASTContext &C) {
  return SILType(CanType(C.TheNativeObjectType), SILValueCategory::Object);
}

SILType SILType::getUnknownObjectType(const ASTContext &C) {
  return getPrimitiveObjectType(CanType(C.TheUnknownObjectType));
}

SILType SILType::getRawPointerType(const ASTContext &C) {
  return getPrimitiveObjectType(CanType(C.TheRawPointerType));
}

SILType SILType::getBuiltinIntegerType(unsigned bitWidth,
                                       const ASTContext &C) {
  return getPrimitiveObjectType(CanType(BuiltinIntegerType::get(bitWidth, C)));
}

SILType SILType::getBuiltinFloatType(BuiltinFloatType::FPKind Kind,
                                     const ASTContext &C) {
  Type ty;
  switch (Kind) {
  case BuiltinFloatType::IEEE16:  ty = C.TheIEEE16Type; break;
  case BuiltinFloatType::IEEE32:  ty = C.TheIEEE32Type; break;
  case BuiltinFloatType::IEEE64:  ty = C.TheIEEE64Type; break;
  case BuiltinFloatType::IEEE80:  ty = C.TheIEEE80Type; break;
  case BuiltinFloatType::IEEE128: ty = C.TheIEEE128Type; break;
  case BuiltinFloatType::PPC128:  ty = C.ThePPC128Type; break;
  }
  return getPrimitiveObjectType(CanType(ty));
}

SILType SILType::getBuiltinWordType(const ASTContext &C) {
  return getPrimitiveObjectType(CanType(BuiltinIntegerType::getWordType(C)));
}

bool SILType::isTrivial(SILModule &M) const {
  return M.getTypeLowering(*this).isTrivial();
}

std::string SILType::getAsString() const {
  std::string Result;
  llvm::raw_string_ostream OS(Result);
  print(OS);
  return OS.str();
}

SILType SILType::getFieldType(VarDecl *field, SILModule &M) const {
  assert(field->getDeclContext() == getNominalOrBoundGenericNominal());
  auto origFieldTy = AbstractionPattern(field->getType());
  auto substFieldTy =
    getSwiftRValueType()->getTypeOfMember(M.getSwiftModule(),
                                          field, nullptr);
  auto loweredTy = M.Types.getLoweredType(origFieldTy, substFieldTy);
  if (isAddress() || getClassOrBoundGenericClass() != nullptr) {
    return loweredTy.getAddressType();
  } else {
    return loweredTy.getObjectType();
  }
}

SILType SILType::getEnumElementType(EnumElementDecl *elt, SILModule &M) const {
  assert(elt->getDeclContext() == getEnumOrBoundGenericEnum());
  assert(elt->hasArgumentType());
  auto origEltTy = elt->getArgumentType();
  auto substEltTy =
    getSwiftRValueType()->getTypeOfMember(M.getSwiftModule(),
                                          elt, nullptr, origEltTy);
  auto loweredTy =
    M.Types.getLoweredType(AbstractionPattern(origEltTy), substEltTy);
  return SILType(loweredTy.getSwiftRValueType(), getCategory());
}

/// True if the type, or the referenced type of an address type, is
/// address-only. For example, it could be a resilient struct or something of
/// unknown size.
bool SILType::isAddressOnly(SILModule &M) const {
  return M.getTypeLowering(*this).isAddressOnly();
}

SILType SILType::substInterfaceGenericArgs(SILModule &M,
                                           ArrayRef<Substitution> Subs) const {
  SILFunctionType *fnTy = getSwiftRValueType()->castTo<SILFunctionType>();
  if (Subs.empty()) {
    assert(!fnTy->isPolymorphic() && "function type without subs must not "
           "be polymorphic.");
    return *this;
  }
  assert(fnTy->isPolymorphic() && "Can only subst interface generic args on "
         "polymorphic function types.");
  CanSILFunctionType canFnTy =
    fnTy->substInterfaceGenericArgs(M, M.getSwiftModule(), Subs);
  return SILType::getPrimitiveObjectType(canFnTy);
}

ArrayRef<Substitution> SILType::gatherAllSubstitutions(SILModule &M) {
  return getSwiftRValueType()->gatherAllSubstitutions(M.getSwiftModule(),
                                                      nullptr);
}

bool SILType::isHeapObjectReferenceType() const {
  auto &C = getASTContext();
  if (getSwiftRValueType()->mayHaveSuperclass())
    return true;
  if (getSwiftRValueType()->isEqual(C.TheNativeObjectType))
    return true;
  if (getSwiftRValueType()->isEqual(C.TheUnknownObjectType))
    return true;
  // TODO: AnyObject type, @objc-only existentials in general
  return false;
}

SILType SILType::getMetatypeInstanceType() const {
  CanType MetatypeType = getSwiftRValueType();
  assert(MetatypeType->is<AnyMetatypeType>() &&
         "This method should only be called on SILTypes with an underlying "
         "metatype type.");
  assert(isObject() && "Should only be called on object types.");
  Type instanceType =
    MetatypeType->castTo<AnyMetatypeType>()->getInstanceType();

  return SILType::getPrimitiveObjectType(instanceType->getCanonicalType());
}

bool SILType::aggregateContainsRecord(SILType Record, SILModule &Mod) const {
  assert(!hasArchetype() && "Agg should be proven to not be generic "
                             "before passed to this function.");
  assert(!Record.hasArchetype() && "Record should be proven to not be generic "
                                    "before passed to this function.");

  llvm::SmallVector<SILType, 8> Worklist;
  Worklist.push_back(*this);

  // For each "subrecord" of agg in the worklist...
  while (!Worklist.empty()) {
    SILType Ty = Worklist.pop_back_val();

    // If it is record, we succeeded. Return true.
    if (Ty == Record)
      return true;

    // Otherwise, we gather up sub-records that need to be checked for
    // checking... First handle the tuple case.
    if (CanTupleType TT = Ty.getAs<TupleType>()) {
      for (unsigned i = 0, e = TT->getNumElements(); i != e; ++i)
        Worklist.push_back(Ty.getTupleElementType(i));
      continue;
    }

    // Then if we have an enum...
    if (EnumDecl *E = Ty.getEnumOrBoundGenericEnum()) {
      for (auto Elt : E->getAllElements())
        if (Elt->hasArgumentType())
          Worklist.push_back(Ty.getEnumElementType(Elt, Mod));
      continue;
    }

    // Then if we have a struct address...
    if (StructDecl *S = Ty.getStructOrBoundGenericStruct())
      for (VarDecl *Var : S->getStoredProperties())
        Worklist.push_back(Ty.getFieldType(Var, Mod));

    // If we have a class address, it is a pointer so it can not contain other
    // types.

    // If we reached this point, then this type has no subrecords. Since it does
    // not equal our record, we can skip it.
  }

  // Could not find the record in the aggregate.
  return false;
}

/// Prove that \p Ty is layout compatible with RawPointer.
static bool isLayoutCompatibleWithRawPointer(SILType Ty, SILModule &Mod) {
  // Builtin.RawPointer is layout compatible with heap object reference types.
  if (Ty.isHeapObjectReferenceType())
    return true;

  // Builtin.RawPointer is layout compatible with optional of heap object
  // reference types.
  if (SILType OptionalT = Ty.getOptionalObjectType(Mod).getAddressType())
    if (OptionalT.isHeapObjectReferenceType())
      return true;

  // Builtin.RawPointer is layout compatible with Builtin.Word.
  if (Ty == SILType::getBuiltinWordType(Mod.getASTContext()))
    return true;

  // Otherwise, be conservative and return false.
  return false;
}

/// Returns true if scalar type \p LHS is layout compatible with scalar \p
/// RHS. These are all commutative queries.
static bool areLayoutCompatibleNonAggregates(SILType LHS, SILType RHS,
                                             SILModule &Mod) {
  // First check canonicalize Builtin.RawPointer on the LHS.
  if (RHS.is<BuiltinRawPointerType>())
    std::swap(LHS, RHS);

  // Ok, now if either LHS or RHS was a Builtin.RawPointer on input, we know the
  // type must be in LHS.
  if (LHS.is<BuiltinRawPointerType>())
    return isLayoutCompatibleWithRawPointer(RHS, Mod);

  // Now canonicalize so that if we have a heap object reference, it is on the
  // left side.
  if (RHS.isHeapObjectReferenceType())
    std::swap(LHS, RHS);

  // Ok, we now know that if either of our types are heap object reference type,
  // then we must have a heap object reference type in LHS. If we don't, there
  // is nothing further we can do.
  if (!LHS.isHeapObjectReferenceType())
    return false;

  // Ok, we know that LHS is a heap object reference type. If RHS is an
  // Builtin.NativeObject or a Builtin.UnknownObject they are layout compatible.
  if (RHS.is<BuiltinNativeObjectType>() || RHS.is<BuiltinUnknownObjectType>())
    return true;

  // If both are classes, we treat them as being layout compatible.
  if (LHS.getClassOrBoundGenericClass() && RHS.getClassOrBoundGenericClass())
    return true;

  // Canonicalize AnyObject on the left side.
  if (RHS.isAnyObject())
    std::swap(LHS, RHS);

  // If LHS is an AnyObject and RHS is a class, we are going to say that they
  // are layout compatible.
  if (LHS.isAnyObject() && RHS.getClassOrBoundGenericClass())
    return true;

  // Otherwise conservatively assume that the two types are not layout
  // compatible.
  return false;
}

static SILType getFirstPayloadTypeOfEnum(EnumDecl *E, SILType TyIter,
                                         SILModule &Mod) {
  for (EnumElementDecl *Elt : E->getAllElements())
    if (Elt->hasArgumentType())
      return TyIter.getEnumElementType(Elt, Mod);
  return SILType();
}

static SILType getFieldOfSingleFieldStruct(StructDecl *S, SILType TyIter,
                                           SILModule &Mod) {
  // Look through S's stored properties.
  auto Range = S->getStoredProperties();

  // If S has no stored properties, there is nothing we can do, bail.
  if (Range.begin() == Range.end())
    return SILType();

  // Otherwise, grab the first field of S.
  auto Iter = Range.begin();
  VarDecl *FirstVar = *Iter;
  ++Iter;

  // If we have more than one stored field, the struct is not able to have
  // layout compatible relationships with any of its fields.
  if (Iter != Range.end())
    return SILType();

  // Otherwise return the singular field of S.
  return TyIter.getFieldType(FirstVar, Mod);
}

/// Returns true if scalar \p LHS is layout compatible with the aggregate type
/// \p RHS.
static bool isLayoutCompatibleWithAggregate(SILType LHS, SILType RHS,
                                            SILModule &Mod) {
  // The plan is to drill into RHS as far as we can until we find LHS or can not
  // find any more aggregate
  SILType TyIter = RHS;

  while (true) {
    // If TyIter is equal to Ty1 or is layout compatible with Ty1 as a non
    // aggregate, return true.
    if (TyIter == LHS || areLayoutCompatibleNonAggregates(TyIter, LHS, Mod) ||
        areLayoutCompatibleNonAggregates(LHS, TyIter, Mod))
      return true;

    // Then if we have an enum...
    if (EnumDecl *E = TyIter.getEnumOrBoundGenericEnum()) {
      // Attempt to compute the first payloaded case of the enum. If we find it,
      // continue.
      if ((TyIter = getFirstPayloadTypeOfEnum(E, TyIter, Mod)))
        continue;

      // Otherwise this enum has no payloads implying that it can not be layout
      // compatible with anything.
      return false;
    }

    // Then if we have a struct address...
    if (StructDecl *S = TyIter.getStructOrBoundGenericStruct()) {
      // If it has only one field, it is layout compatible with it, so set
      // TyIter to it. If S has more than one field, it can not be layout
      // compatible with any of them, so return false.
      if ((TyIter = getFieldOfSingleFieldStruct(S, TyIter, Mod)))
        continue;

      return false;
    }

    // If we reached this point, then this type has no subrecords we are
    // interested in. Check if LHS/RHS are layout compatible as scalars.
    return areLayoutCompatibleNonAggregates(TyIter, LHS, Mod) ||
      areLayoutCompatibleNonAggregates(LHS, TyIter, Mod);
  }
}

/// Returns true if this Type (LHS) is layout compatible with RHS. *NOTE* This
/// does not imply that RHS is layout compatible with LHS (i.e. this method
/// returning true does not imply commutativity of the layout compatible
/// relation).
bool SILType::isLayoutCompatibleWith(SILType RHS, SILModule &Mod) const {
  // Create a reference to ourselves so we can canonicalize.
  SILType LHS = *this;

  // If either LHS or RHS has archetypes, bail since we don't know what the
  // archetypes are. This could probably be improved.
  if (LHS.hasArchetype() || RHS.hasArchetype())
    return false;

  // If LHS is equal to RHS, or they are layout compatible as non aggregates,
  // return true.
  if (LHS == RHS || areLayoutCompatibleNonAggregates(LHS, RHS, Mod) ||
      areLayoutCompatibleNonAggregates(RHS, LHS, Mod))
    return true;

  // If LHS is an enum type...
  if (EnumDecl *E = LHS.getEnumOrBoundGenericEnum()) {
    // LHS can only be layout compatible with RHS if RHS is the layout
    // compatible with the first payload type of LHS. Thus grab the first
    // payload type from LHS and redo the query with it instead. We are trusting
    // the stdlib not to attempt to perform this operation on invalid types.
    LHS = getFirstPayloadTypeOfEnum(E, LHS, Mod);

    if (LHS.isLayoutCompatibleWith(RHS, Mod))
      return true;
  }

  // If RHS is an enum...
  if (EnumDecl *E = RHS.getEnumOrBoundGenericEnum()) {
    // Otherwise if we have a generic enum, try to prove that the LHS/RHS are
    // layout compatible by using the fact the enum is an aggregate to see if
    // LHS is the payload of RHS.
    RHS = getFirstPayloadTypeOfEnum(E, RHS, Mod);

    if (RHS.isLayoutCompatibleWith(LHS, Mod))
      return true;
  }

  // Then check if RHS is a struct. If so, handle it especially.
  if (RHS.getStructOrBoundGenericStruct())
    if (isLayoutCompatibleWithAggregate(LHS, RHS, Mod))
      return true;
  // Then check if LHS is a struct. If so handle it especially. This works since
  // structs are commutatively layout compatible with their contents if they
  // only have one field.
  if (LHS.getStructOrBoundGenericStruct())
    if (isLayoutCompatibleWithAggregate(RHS, LHS, Mod))
      return true;

  // Otherwise be conservative and assume that they are not layout compatible.
  return false;
}

SILType SILType::getOptionalObjectType(SILModule &M) const {
  if (auto boundTy = getObjectType().getAs<BoundGenericEnumType>()) {
    if (boundTy->getDecl()->classifyAsOptionalType() == OTK_Optional) {
      CanType cTy = boundTy->getGenericArgs()[0]->getCanonicalType();
      return M.Types.getLoweredType(cTy);
    }
  }
  return SILType();
}
