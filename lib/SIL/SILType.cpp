//===--- SILTypeInfo.cpp - Defines the SILTypeInfo class hierarchy --------===//
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

#include "llvm/Support/ErrorHandling.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILType.h"
#include "swift/AST/Decl.h"
using namespace swift;

/// True if the type, or the referenced type of an address type, is loadable.
/// This is the opposite of isAddressOnly.
bool SILType::isLoadable(CanType Ty, SILModule &M) {
  // Handle the obvious cases inline.

  // Reference types are always loadable.
  if (Ty->hasReferenceSemantics())
    return true;

  // Archetypes and existentials are always address-only.
  // FIXME: Class archetypes and existentials will one day be representable as
  // reference types.
  // FIXME: resilient structs
  if (Ty->is<ArchetypeType>() || Ty->isExistentialType())
    return false;

  // Structs and tuples are loadable if all of their elements are loadable.
  if (TupleType *TTy = Ty->getAs<TupleType>()) {
    for (const TupleTypeElt &elt : TTy->getFields())
      if (!isLoadable(elt.getType()->getCanonicalType(), M))
        return false;
    return true;
  }

  StructDecl *SD = nullptr;
  if (NominalType *NTy = Ty->getAs<NominalType>()) {
    SD = dyn_cast<StructDecl>(NTy->getDecl());
  } else if (BoundGenericType *BGTy = Ty->getAs<BoundGenericType>()) {
    SD = dyn_cast<StructDecl>(BGTy->getDecl());
  }

  if (SD) {
    // FIXME: if this struct has a resilient attribute, it is obviously
    // AddressOnly.
    for (Decl *D : SD->getMembers())
      if (VarDecl *VD = dyn_cast<VarDecl>(D))
        if (!VD->isProperty() &&
            !isLoadable(VD->getType()->getCanonicalType(), M))
          return false;
    return true;
  }

  // Otherwise, it must be loadable.
  return true;
}


SILFunctionTypeInfo *SILFunctionTypeInfo::create(CanType swiftType,
                                       ArrayRef<SILType> inputTypes,
                                       SILType resultType,
                                       ArrayRef<unsigned> uncurriedInputCounts,
                                       bool hasIndirectReturn,
                                       AbstractCC cc,
                                       SILModule &M) {
  // We allocate room for an extra unsigned in the uncurriedInputCounts array,
  // so that we can stuff a leading zero in there and be able to efficiently
  // return both the begins and ends of each uncurried argument group.
  void *buffer = M.allocate(sizeof(SILFunctionTypeInfo)
                                 + sizeof(SILType)*inputTypes.size()
                             + sizeof(unsigned)*(1+uncurriedInputCounts.size()),
                               alignof(SILFunctionTypeInfo));
  SILFunctionTypeInfo *fi = ::new (buffer) SILFunctionTypeInfo(
                                                     swiftType,
                                                     inputTypes.size(),
                                                     resultType,
                                                    uncurriedInputCounts.size(),
                                                    hasIndirectReturn,
                                                     cc);
  memcpy(fi->getInputTypeBuffer(), inputTypes.data(),
         sizeof(SILType) * inputTypes.size());
  fi->getUncurryBuffer()[0] = 0;
  memcpy(fi->getUncurryBuffer()+1, uncurriedInputCounts.data(),
         sizeof(unsigned) * uncurriedInputCounts.size());
  return fi;
}

unsigned SILType::getUncurryLevel() const {
  if (auto *finfo = value.getPointer().dyn_cast<SILFunctionTypeInfo*>())
    return finfo->getUncurryLevel();
  return 0;
}
