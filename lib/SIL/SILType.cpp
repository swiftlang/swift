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
#include "swift/SIL/TypeLowering.h"
#include "swift/SIL/AbstractionPattern.h"

using namespace swift;
using namespace swift::Lowering;

SILType SILType::getNativeObjectType(const ASTContext &C) {
  return SILType(CanType(C.TheNativeObjectType), SILValueCategory::Object);
}

SILType SILType::getBridgeObjectType(const ASTContext &C) {
  return SILType(CanType(C.TheBridgeObjectType), SILValueCategory::Object);
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

bool SILType::isReferenceCounted(SILModule &M) const {
  return M.getTypeLowering(*this).isReferenceCounted();
}

std::string SILType::getAsString() const {
  std::string Result;
  llvm::raw_string_ostream OS(Result);
  print(OS);
  return OS.str();
}

SILType SILType::getFieldType(VarDecl *field, SILModule &M) const {
  assert(field->getDeclContext() == getNominalOrBoundGenericNominal());
  AbstractionPattern origFieldTy = M.Types.getAbstractionPattern(field);
  CanType substFieldTy;
  if (field->hasClangNode()) {
    substFieldTy = origFieldTy.getType();
  } else {
    substFieldTy =
      getSwiftRValueType()->getTypeOfMember(M.getSwiftModule(),
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
  assert(elt->hasArgumentType());
  auto substEltTy =
    getSwiftRValueType()->getTypeOfMember(M.getSwiftModule(),
                                          elt, nullptr,
                                          elt->getArgumentInterfaceType());
  auto loweredTy =
    M.Types.getLoweredType(M.Types.getAbstractionPattern(elt), substEltTy);
  return SILType(loweredTy.getSwiftRValueType(), getCategory());
}

bool SILType::isResilient(SILModule &M) const {
  NominalTypeDecl *nomType = getNominalOrBoundGenericNominal();
  if (!nomType) {
    assert(false && "isResilient on non-nominal types not implemented");
    return true;
  }
  return M.Types.isResilient(nomType);
}

/// True if the type, or the referenced type of an address type, is
/// address-only. For example, it could be a resilient struct or something of
/// unknown size.
bool SILType::isAddressOnly(SILModule &M) const {
  return M.getTypeLowering(*this).isAddressOnly();
}

SILType SILType::substGenericArgs(SILModule &M,
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
    fnTy->substGenericArgs(M, M.getSwiftModule(), Subs);
  return SILType::getPrimitiveObjectType(canFnTy);
}

ArrayRef<Substitution> SILType::gatherAllSubstitutions(SILModule &M) {
  return getSwiftRValueType()->gatherAllSubstitutions(M.getSwiftModule(),
                                                      nullptr);
}

bool SILType::isHeapObjectReferenceType() const {
  auto &C = getASTContext();
  if (getSwiftRValueType()->isBridgeableObjectType())
    return true;
  if (getSwiftRValueType()->isEqual(C.TheNativeObjectType))
    return true;
  if (getSwiftRValueType()->isEqual(C.TheBridgeObjectType))
    return true;
  if (getSwiftRValueType()->isEqual(C.TheUnknownObjectType))
    return true;
  // TODO: AnyObject type, @objc-only existentials in general
  return false;
}

SILType SILType::getMetatypeInstanceType(SILModule &M) const {
  CanType MetatypeType = getSwiftRValueType();
  assert(MetatypeType->is<AnyMetatypeType>() &&
         "This method should only be called on SILTypes with an underlying "
         "metatype type.");
  assert(isObject() && "Should only be called on object types.");
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

bool SILType::aggregateHasUnreferenceableStorage() const {
  if (auto s = getStructOrBoundGenericStruct()) {
    return s->hasUnreferenceableStorage();
  }
  return false;
}

SILType SILType::getAnyOptionalObjectType(SILModule &M,
                                          OptionalTypeKind &OTK) const {
  if (auto objectTy = getSwiftRValueType()->getAnyOptionalObjectType(OTK)) {
    // Lower the payload type at the abstraction level of Optional's generic
    // parameter.
    auto archetype = getNominalOrBoundGenericNominal()->getGenericParams()
      ->getPrimaryArchetypes()[0];
    
    auto loweredTy
      = M.Types.getLoweredType(AbstractionPattern(archetype), objectTy);
    
    return SILType(loweredTy.getSwiftRValueType(), getCategory());
  }

  OTK = OTK_None;
  return SILType();
}

/// True if the given type value is nonnull, and the represented type is NSError
/// or CFError, the error classes for which we support "toll-free" bridging to
/// ErrorType existentials.
static bool isBridgedErrorClass(SILModule &M,
                                Type t) {
  if (!t)
    return false;

  // NSError (TODO: and CFError) can be bridged.
  auto errorType = M.Types.getNSErrorType();
  if (errorType && t->isEqual(errorType)) {
    return true;
  }
  
  return false;
}

static bool isErrorTypeExistential(ArrayRef<ProtocolDecl*> protocols) {
  return protocols.size() == 1
    && protocols[0]->isSpecificProtocol(KnownProtocolKind::_ErrorType);
}

ExistentialRepresentation
SILType::getPreferredExistentialRepresentation(SILModule &M,
                                               Type containedType) const {
  SmallVector<ProtocolDecl *, 4> protocols;
  
  // Existential metatypes always use metatype representation.
  if (is<ExistentialMetatypeType>())
    return ExistentialRepresentation::Metatype;
  
  // Get the list of existential constraints. If the type isn't existential,
  // then there is no representation.
  if (!getSwiftRValueType()->isAnyExistentialType(protocols))
    return ExistentialRepresentation::None;
  
  // The (uncomposed) ErrorType existential uses a special boxed representation.
  if (isErrorTypeExistential(protocols)) {
    // NSError or CFError references can be adopted directly as ErrorType
    // existentials.
    if (isBridgedErrorClass(M, containedType)) {
      return ExistentialRepresentation::Class;
    } else {
      return ExistentialRepresentation::Boxed;
    }
  }
  
  // A class-constrained protocol composition can adopt the conforming
  // class reference directly.
  for (auto proto : protocols) {
    if (proto->requiresClass())
      return ExistentialRepresentation::Class;
  }
  
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
    SmallVector<ProtocolDecl *, 4> protocols;
    if (!getSwiftRValueType()->isAnyExistentialType(protocols))
      return false;
    // The (uncomposed) ErrorType existential uses a special boxed
    // representation. It can also adopt class references of bridged error types
    // directly.
    if (isErrorTypeExistential(protocols))
      return repr == ExistentialRepresentation::Boxed
        || (repr == ExistentialRepresentation::Class
            && isBridgedErrorClass(M, containedType));
    
    // A class-constrained composition uses ClassReference representation;
    // otherwise, we use a fixed-sized buffer
    for (auto *proto : protocols) {
      if (proto->requiresClass())
        return repr == ExistentialRepresentation::Class;
    }
    return repr == ExistentialRepresentation::Opaque;
  }
  case ExistentialRepresentation::Metatype:
    return is<ExistentialMetatypeType>();
  }
}
