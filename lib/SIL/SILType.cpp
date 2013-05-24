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

/// True if the type, or the referenced type of an address
/// type, is address-only.  For example, it could be a resilient struct or
/// something of unknown size.
bool SILType::isAddressOnly(CanType Ty, SILModule &M) {
  // Handle the obvious cases inline.

  // Reference types are always loadable.
  if (Ty->hasReferenceSemantics())
    return false;

  // Archetypes and existentials are always address-only.
  // FIXME: Class archetypes and existentials will one day be representable as
  // reference types.
  // FIXME: if this is a struct has a resilient attribute, it is obviously
  // AddressOnly.
  if (Ty->is<ArchetypeType>() || Ty->isExistentialType())
    return true;

  // Structs and tuples are address-only if any of their elements are.
  if (TupleType *TTy = Ty->getAs<TupleType>()) {
    // Check to see if we've computed this property for this tuple yet.
    auto Entry = M.AddressOnlyTypeCache.find(TTy);
    // If we got a hit, then return the precomputed value.
    if (Entry != M.AddressOnlyTypeCache.end())
      return Entry->second;
    
    for (const TupleTypeElt &elt : TTy->getFields())
      if (isAddressOnly(elt.getType()->getCanonicalType(), M))
        return M.AddressOnlyTypeCache[TTy] = true;
    
    return M.AddressOnlyTypeCache[TTy] = false;
  }

  StructDecl *SD = nullptr;
  if (NominalType *NTy = Ty->getAs<NominalType>()) {
    SD = dyn_cast<StructDecl>(NTy->getDecl());
  } else if (BoundGenericType *BGTy = Ty->getAs<BoundGenericType>()) {
    SD = dyn_cast<StructDecl>(BGTy->getDecl());
  }

  if (SD) {
    // Check to see if we've computed this property for this tuple yet.
    auto Entry = M.AddressOnlyTypeCache.find(Ty.getPointer());
    // If we got a hit, then return the precomputed value.
    if (Entry != M.AddressOnlyTypeCache.end())
      return Entry->second;

    for (Decl *D : SD->getMembers())
      if (VarDecl *VD = dyn_cast<VarDecl>(D))
        if (!VD->isProperty() &&
            isAddressOnly(VD->getType()->getCanonicalType(), M))
          return M.AddressOnlyTypeCache[Ty.getPointer()] = true;
    return M.AddressOnlyTypeCache[Ty.getPointer()] = false;
  }

  // Otherwise, it must not be address-only.
  return false;
}

SILFunctionTypeInfo *SILType::getFunctionTypeInfo(SILModule &M) const {
  AnyFunctionType *ft = cast<AnyFunctionType>(getSwiftRValueType());
  
  auto found = M.FunctionTypeInfoCache.find(ft);
  if (found != M.FunctionTypeInfoCache.end())
    return found->second;
  
  SILFunctionTypeInfo *info = M.makeFunctionTypeInfo(ft);
  M.FunctionTypeInfoCache[ft] = info;
  return info;
}

SILFunctionTypeInfo *SILFunctionTypeInfo::create(CanType swiftType,
                                                 ArrayRef<SILType> inputTypes,
                                                 SILType resultType,
                                                 bool hasIndirectReturn,
                                                 SILModule &M) {
  // We allocate room for an extra unsigned in the uncurriedInputCounts array,
  // so that we can stuff a leading zero in there and be able to efficiently
  // return both the begins and ends of each uncurried argument group.
  void *buffer = M.allocate(sizeof(SILFunctionTypeInfo)
                              + sizeof(SILType)*inputTypes.size(),
                            alignof(SILFunctionTypeInfo));
  SILFunctionTypeInfo *fi
    = ::new (buffer) SILFunctionTypeInfo(swiftType,
                                         inputTypes.size(),
                                         resultType,
                                         hasIndirectReturn);
  memcpy(fi->getInputTypeBuffer(), inputTypes.data(),
         sizeof(SILType) * inputTypes.size());
  return fi;
}
