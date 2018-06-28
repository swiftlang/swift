//===--- SILType.cpp - Defines SILType ------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILType.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Type.h"
#include "swift/SIL/AbstractionPattern.h"
#include "swift/SIL/SILFunctionConventions.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/TypeLowering.h"

using namespace swift;
using namespace swift::Lowering;

SILType SILType::getExceptionType(const ASTContext &C) {
  return SILType::getPrimitiveObjectType(C.getExceptionType());
}

SILType SILType::getNativeObjectType(const ASTContext &C) {
  return SILType(C.TheNativeObjectType, SILValueCategory::Object);
}

SILType SILType::getBridgeObjectType(const ASTContext &C) {
  return SILType(C.TheBridgeObjectType, SILValueCategory::Object);
}

SILType SILType::getUnknownObjectType(const ASTContext &C) {
  return getPrimitiveObjectType(C.TheUnknownObjectType);
}

SILType SILType::getRawPointerType(const ASTContext &C) {
  return getPrimitiveObjectType(C.TheRawPointerType);
}

SILType SILType::getBuiltinIntegerType(unsigned bitWidth,
                                       const ASTContext &C) {
  return getPrimitiveObjectType(CanType(BuiltinIntegerType::get(bitWidth, C)));
}

SILType SILType::getBuiltinFloatType(BuiltinFloatType::FPKind Kind,
                                     const ASTContext &C) {
  CanType ty;
  switch (Kind) {
  case BuiltinFloatType::IEEE16:  ty = C.TheIEEE16Type; break;
  case BuiltinFloatType::IEEE32:  ty = C.TheIEEE32Type; break;
  case BuiltinFloatType::IEEE64:  ty = C.TheIEEE64Type; break;
  case BuiltinFloatType::IEEE80:  ty = C.TheIEEE80Type; break;
  case BuiltinFloatType::IEEE128: ty = C.TheIEEE128Type; break;
  case BuiltinFloatType::PPC128:  ty = C.ThePPC128Type; break;
  }
  return getPrimitiveObjectType(ty);
}

SILType SILType::getBuiltinWordType(const ASTContext &C) {
  return getPrimitiveObjectType(CanType(BuiltinIntegerType::getWordType(C)));
}

SILType SILType::getOptionalType(SILType type) {
  auto &ctx = type.getASTContext();
  auto optType = BoundGenericEnumType::get(ctx.getOptionalDecl(), Type(),
                                           { type.getASTType() });
  return getPrimitiveType(CanType(optType), type.getCategory());
}

SILType SILType::getSILTokenType(const ASTContext &C) {
  return getPrimitiveObjectType(C.TheSILTokenType);
}

bool SILType::isTrivial(SILModule &M) const {
  return M.getTypeLowering(*this).isTrivial();
}

bool SILType::isReferenceCounted(SILModule &M) const {
  return M.getTypeLowering(*this).isReferenceCounted();
}

bool SILType::isNoReturnFunction() const {
  if (auto funcTy = dyn_cast<SILFunctionType>(getASTType()))
    return funcTy->isNoReturnFunction();

  return false;
}

std::string SILType::getAsString() const {
  std::string Result;
  llvm::raw_string_ostream OS(Result);
  print(OS);
  return OS.str();
}

bool SILType::isPointerSizeAndAligned() {
  auto &C = getASTContext();
  if (isHeapObjectReferenceType()
      || getASTType()->isEqual(C.TheRawPointerType)) {
    return true;
  }
  if (auto intTy = dyn_cast<BuiltinIntegerType>(getASTType()))
    return intTy->getWidth().isPointerWidth();

  return false;
}

// Reference cast from representations with single pointer low bits.
// Only reference cast to simple single pointer representations.
//
// TODO: handle casting to a loadable existential by generating
// init_existential_ref. Until then, only promote to a heap object dest.
bool SILType::canRefCast(SILType operTy, SILType resultTy, SILModule &M) {
  auto fromTy = operTy.unwrapOptionalType();
  auto toTy = resultTy.unwrapOptionalType();
  return (fromTy.isHeapObjectReferenceType() || fromTy.isClassExistentialType())
    && toTy.isHeapObjectReferenceType();
}

SILType SILType::getFieldType(VarDecl *field, SILModule &M) const {
  auto baseTy = getASTType();

  AbstractionPattern origFieldTy = M.Types.getAbstractionPattern(field);
  CanType substFieldTy;
  if (field->hasClangNode()) {
    substFieldTy = origFieldTy.getType();
  } else {
    substFieldTy =
      baseTy->getTypeOfMember(M.getSwiftModule(),
                              field, nullptr)->getCanonicalType();
  }
  auto loweredTy = M.Types.getLoweredType(origFieldTy, substFieldTy);
  if (isAddress() || getClassOrBoundGenericClass() != nullptr) {
    return loweredTy.getAddressType();
  } else {
    return loweredTy.getObjectType();
  }
}

SILType SILType::getEnumElementType(EnumElementDecl *elt, SILModule &M) const {
  assert(elt->getDeclContext() == getEnumOrBoundGenericEnum());
  assert(elt->hasAssociatedValues());

  if (auto objectType = getASTType().getOptionalObjectType()) {
    assert(elt == M.getASTContext().getOptionalSomeDecl());
    return SILType(objectType, getCategory());
  }

  // If the case is indirect, then the payload is boxed.
  if (elt->isIndirect() || elt->getParentEnum()->isIndirect()) {
    auto box = M.Types.getBoxTypeForEnumElement(*this, elt);
    return SILType(SILType::getPrimitiveObjectType(box).getASTType(),
                   getCategory());
  }

  auto substEltTy =
    getASTType()->getTypeOfMember(M.getSwiftModule(), elt,
                                          elt->getArgumentInterfaceType());
  auto loweredTy =
    M.Types.getLoweredType(M.Types.getAbstractionPattern(elt), substEltTy);

  return SILType(loweredTy.getASTType(), getCategory());
}

bool SILType::isLoadableOrOpaque(SILModule &M) const {
  return isLoadable(M) || !SILModuleConventions(M).useLoweredAddresses();
}

/// True if the type, or the referenced type of an address type, is
/// address-only. For example, it could be a resilient struct or something of
/// unknown size.
bool SILType::isAddressOnly(SILModule &M) const {
  return M.getTypeLowering(*this).isAddressOnly();
}

SILType SILType::substGenericArgs(SILModule &M,
                                  SubstitutionMap SubMap) const {
  auto fnTy = castTo<SILFunctionType>();
  auto canFnTy = CanSILFunctionType(fnTy->substGenericArgs(M, SubMap));
  return SILType::getPrimitiveObjectType(canFnTy);
}

bool SILType::isHeapObjectReferenceType() const {
  auto &C = getASTContext();
  auto Ty = getASTType();
  if (Ty->isBridgeableObjectType())
    return true;
  if (Ty->isEqual(C.TheNativeObjectType))
    return true;
  if (Ty->isEqual(C.TheBridgeObjectType))
    return true;
  if (Ty->isEqual(C.TheUnknownObjectType))
    return true;
  if (is<SILBoxType>())
    return true;
  return false;
}

SILType SILType::getMetatypeInstanceType(SILModule &M) const {
  CanType MetatypeType = getASTType();
  assert(MetatypeType->is<AnyMetatypeType>() &&
         "This method should only be called on SILTypes with an underlying "
         "metatype type.");
  Type instanceType =
    MetatypeType->castTo<AnyMetatypeType>()->getInstanceType();

  return M.Types.getLoweredType(instanceType->getCanonicalType());
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
        if (Elt->hasAssociatedValues())
          Worklist.push_back(Ty.getEnumElementType(Elt, Mod));
      continue;
    }

    // Then if we have a struct address...
    if (StructDecl *S = Ty.getStructOrBoundGenericStruct())
      for (VarDecl *Var : S->getStoredProperties())
        Worklist.push_back(Ty.getFieldType(Var, Mod));

    // If we have a class address, it is a pointer so it cannot contain other
    // types.

    // If we reached this point, then this type has no subrecords. Since it does
    // not equal our record, we can skip it.
  }

  // Could not find the record in the aggregate.
  return false;
}

bool SILType::aggregateHasUnreferenceableStorage() const {
  if (auto s = getStructOrBoundGenericStruct()) {
    return s->hasUnreferenceableStorage();
  }
  return false;
}

SILType SILType::getOptionalObjectType() const {
  if (auto objectTy = getASTType().getOptionalObjectType()) {
    return SILType(objectTy, getCategory());
  }

  return SILType();
}

SILType SILType::unwrapOptionalType() const {
  if (auto objectTy = getOptionalObjectType()) {
    return objectTy;
  }

  return *this;
}

/// True if the given type value is nonnull, and the represented type is NSError
/// or CFError, the error classes for which we support "toll-free" bridging to
/// Error existentials.
static bool isBridgedErrorClass(SILModule &M,
                                Type t) {
  // There's no bridging if ObjC interop is disabled.
  if (!M.getASTContext().LangOpts.EnableObjCInterop)
    return false;

  if (!t)
    return false;

  if (auto archetypeType = t->getAs<ArchetypeType>())
    t = archetypeType->getSuperclass();

  // NSError (TODO: and CFError) can be bridged.
  auto nsErrorType = M.Types.getNSErrorType();
  if (t && nsErrorType && nsErrorType->isExactSuperclassOf(t)) {
    return true;
  }
  
  return false;
}

ExistentialRepresentation
SILType::getPreferredExistentialRepresentation(SILModule &M,
                                               Type containedType) const {
  // Existential metatypes always use metatype representation.
  if (is<ExistentialMetatypeType>())
    return ExistentialRepresentation::Metatype;
  
  // If the type isn't existential, then there is no representation.
  if (!isExistentialType())
    return ExistentialRepresentation::None;

  auto layout = getASTType().getExistentialLayout();

  if (layout.isErrorExistential()) {
    // NSError or CFError references can be adopted directly as Error
    // existentials.
    if (isBridgedErrorClass(M, containedType)) {
      return ExistentialRepresentation::Class;
    } else {
      return ExistentialRepresentation::Boxed;
    }
  }

  // A class-constrained protocol composition can adopt the conforming
  // class reference directly.
  if (layout.requiresClass())
    return ExistentialRepresentation::Class;
  
  // Otherwise, we need to use a fixed-sized buffer.
  return ExistentialRepresentation::Opaque;
}

bool
SILType::canUseExistentialRepresentation(SILModule &M,
                                         ExistentialRepresentation repr,
                                         Type containedType) const {
  switch (repr) {
  case ExistentialRepresentation::None:
    return !isAnyExistentialType();
  case ExistentialRepresentation::Opaque:
  case ExistentialRepresentation::Class:
  case ExistentialRepresentation::Boxed: {
    // Look at the protocols to see what representation is appropriate.
    if (!isExistentialType())
      return false;

    auto layout = getASTType().getExistentialLayout();

    // The (uncomposed) Error existential uses a special boxed
    // representation. It can also adopt class references of bridged error types
    // directly.
    if (layout.isErrorExistential())
      return repr == ExistentialRepresentation::Boxed
        || (repr == ExistentialRepresentation::Class
            && isBridgedErrorClass(M, containedType));
    
    // A class-constrained composition uses ClassReference representation;
    // otherwise, we use a fixed-sized buffer.
    if (layout.requiresClass())
      return repr == ExistentialRepresentation::Class;

    return repr == ExistentialRepresentation::Opaque;
  }
  case ExistentialRepresentation::Metatype:
    return is<ExistentialMetatypeType>();
  }

  llvm_unreachable("Unhandled ExistentialRepresentation in switch.");
}

SILType SILType::getReferentType(SILModule &M) const {
  auto Ty = castTo<ReferenceStorageType>();
  return M.Types.getLoweredType(Ty->getReferentType()->getCanonicalType());
}

CanType
SILBoxType::getFieldLoweredType(SILModule &M, unsigned index) const {
  auto fieldTy = getLayout()->getFields()[index].getLoweredType();
  
  // Apply generic arguments if the layout is generic.
  if (auto subMap = getSubstitutions()) {
    auto sig = getLayout()->getGenericSignature();
    return SILType::getPrimitiveObjectType(fieldTy)
      .subst(M,
             QuerySubstitutionMap{subMap},
             LookUpConformanceInSubstitutionMap(subMap),
             sig)
      .getASTType();
  }
  return fieldTy;
}

ValueOwnershipKind
SILResultInfo::getOwnershipKind(SILModule &M,
                                CanGenericSignature signature) const {
  GenericContextScope GCS(M.Types, signature);
  bool IsTrivial = getSILStorageType().isTrivial(M);
  switch (getConvention()) {
  case ResultConvention::Indirect:
    return SILModuleConventions(M).isSILIndirect(*this)
               ? ValueOwnershipKind::Trivial
               : ValueOwnershipKind::Owned;
  case ResultConvention::Autoreleased:
  case ResultConvention::Owned:
    return ValueOwnershipKind::Owned;
  case ResultConvention::Unowned:
  case ResultConvention::UnownedInnerPointer:
    if (IsTrivial)
      return ValueOwnershipKind::Trivial;
    return ValueOwnershipKind::Unowned;
  }

  llvm_unreachable("Unhandled ResultConvention in switch.");
}

SILModuleConventions::SILModuleConventions(const SILModule &M)
    : loweredAddresses(!M.getASTContext().LangOpts.EnableSILOpaqueValues
                       || M.getStage() == SILStage::Lowered) {}

bool SILModuleConventions::isReturnedIndirectlyInSIL(SILType type,
                                                     SILModule &M) {
  if (SILModuleConventions(M).loweredAddresses)
    return type.isAddressOnly(M);

  return false;
}

bool SILModuleConventions::isPassedIndirectlyInSIL(SILType type, SILModule &M) {
  if (SILModuleConventions(M).loweredAddresses)
    return type.isAddressOnly(M);

  return false;
}


bool SILFunctionType::isNoReturnFunction() const {
  for (unsigned i = 0, e = getNumResults(); i < e; ++i) {
    if (getResults()[i].getType()->isUninhabited())
      return true;
  }

  return false;
}

#ifndef NDEBUG
static bool areOnlyAbstractionDifferent(CanType type1, CanType type2) {
  assert(type1->isLegalSILType());
  assert(type2->isLegalSILType());

  // Exact equality is fine.
  if (type1 == type2)
    return true;

  // Either both types should be optional or neither should be.
  if (auto object1 = type1.getOptionalObjectType()) {
    auto object2 = type2.getOptionalObjectType();
    if (!object2)
      return false;
    return areOnlyAbstractionDifferent(object1, object2);
  }
  if (type2.getOptionalObjectType())
    return false;

  // Either both types should be tuples or neither should be.
  if (auto tuple1 = dyn_cast<TupleType>(type1)) {
    auto tuple2 = dyn_cast<TupleType>(type2);
    if (!tuple2)
      return false;
    if (tuple1->getNumElements() != tuple2->getNumElements())
      return false;
    for (auto i : indices(tuple2->getElementTypes()))
      if (!areOnlyAbstractionDifferent(tuple1.getElementType(i),
                                       tuple2.getElementType(i)))
        return false;
    return true;
  }
  if (isa<TupleType>(type2))
    return false;

  // Either both types should be metatypes or neither should be.
  if (auto meta1 = dyn_cast<AnyMetatypeType>(type1)) {
    auto meta2 = dyn_cast<AnyMetatypeType>(type2);
    if (!meta2)
      return false;
    if (meta1.getInstanceType() != meta2.getInstanceType())
      return false;
    return true;
  }

  // Either both types should be functions or neither should be.
  if (auto fn1 = dyn_cast<SILFunctionType>(type1)) {
    auto fn2 = dyn_cast<SILFunctionType>(type2);
    if (!fn2)
      return false;
    // TODO: maybe there are checks we can do here?
    (void)fn1;
    (void)fn2;
    return true;
  }
  if (isa<SILFunctionType>(type2))
    return false;

  llvm_unreachable("no other types should differ by abstraction");
}
#endif

/// Given two SIL types which are representations of the same type,
/// check whether they have an abstraction difference.
bool SILType::hasAbstractionDifference(SILFunctionTypeRepresentation rep,
                                       SILType type2) {
  CanType ct1 = getASTType();
  CanType ct2 = type2.getASTType();
  assert(getSILFunctionLanguage(rep) == SILFunctionLanguage::C ||
         areOnlyAbstractionDifferent(ct1, ct2));
  (void)ct1;
  (void)ct2;

  // Assuming that we've applied the same substitutions to both types,
  // abstraction equality should equal type equality.
  return (*this != type2);
}

bool SILType::isLoweringOf(SILModule &Mod, CanType formalType) {
  SILType loweredType = *this;

  // Optional lowers its contained type. The difference between Optional
  // and IUO is lowered away.
  SILType loweredObjectType = loweredType.getOptionalObjectType();
  CanType formalObjectType = formalType.getOptionalObjectType();

  if (loweredObjectType) {
    return formalObjectType &&
           loweredObjectType.isLoweringOf(Mod, formalObjectType);
  }

  // Metatypes preserve their instance type through lowering.
  if (loweredType.is<MetatypeType>()) {
    if (auto formalMT = dyn_cast<MetatypeType>(formalType)) {
      return loweredType.getMetatypeInstanceType(Mod).isLoweringOf(
          Mod, formalMT.getInstanceType());
    }
  }

  if (auto loweredEMT = loweredType.getAs<ExistentialMetatypeType>()) {
    if (auto formalEMT = dyn_cast<ExistentialMetatypeType>(formalType)) {
      return loweredEMT.getInstanceType() == formalEMT.getInstanceType();
    }
  }

  // TODO: Function types go through a more elaborate lowering.
  // For now, just check that a SIL function type came from some AST function
  // type.
  if (loweredType.is<SILFunctionType>())
    return isa<AnyFunctionType>(formalType);

  // Tuples are lowered elementwise.
  // TODO: Will this always be the case?
  if (auto loweredTT = loweredType.getAs<TupleType>()) {
    if (auto formalTT = dyn_cast<TupleType>(formalType)) {
      if (loweredTT->getNumElements() != formalTT->getNumElements())
        return false;
      for (unsigned i = 0, e = loweredTT->getNumElements(); i < e; ++i) {
        auto loweredTTEltType =
            SILType::getPrimitiveAddressType(loweredTT.getElementType(i));
        if (!loweredTTEltType.isLoweringOf(Mod, formalTT.getElementType(i)))
          return false;
      }
      return true;
    }
  }

  // Dynamic self has the same lowering as its contained type.
  if (auto dynamicSelf = dyn_cast<DynamicSelfType>(formalType))
    formalType = dynamicSelf.getSelfType();

  // Other types are preserved through lowering.
  return loweredType.getASTType() == formalType;
}
