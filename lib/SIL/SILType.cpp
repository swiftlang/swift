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

#include "swift/SIL/SILType.h"
#include "swift/SIL/SILModule.h"
#include "swift/AST/CanTypeVisitor.h"
#include "swift/AST/Decl.h"
#include "llvm/Support/ErrorHandling.h"
using namespace swift;

/// True if the type, or the referenced type of an address
/// type, is address-only.  For example, it could be a resilient struct or
/// something of unknown size.
bool SILType::isAddressOnly(CanType type, SILModule &M) {
  // Handle the obvious cases inline.

  // Reference types are always loadable.
  // NB: class archetypes and existentials are not address-only. This
  // check must come before the check for archetype or existential types
  // below.
  if (type.hasReferenceSemantics())
    return false;

  // Non-class archetypes and existentials are always address-only.
  if (isa<ArchetypeType>(type) || type.isExistentialType())
    return true;

  // FIXME: if this is a struct has a resilient attribute, it is obviously
  // AddressOnly.

  // Structs and tuples are address-only if any of their elements are.
  if (auto tuple = dyn_cast<TupleType>(type)) {
    // Check to see if we've computed this property for this tuple yet.
    auto entry = M.AddressOnlyTypeCache.find(tuple);
    // If we got a hit, then return the precomputed value.
    if (entry != M.AddressOnlyTypeCache.end())
      return entry->second;
    
    for (auto eltType : tuple.getElementTypes())
      if (isAddressOnly(eltType, M))
        return M.AddressOnlyTypeCache[tuple] = true;
    
    return M.AddressOnlyTypeCache[tuple] = false;
  }

  StructDecl *SD = nullptr;
  if (auto structTy = dyn_cast<StructType>(type)) {
    SD = structTy->getDecl();
  } else if (auto structTy = dyn_cast<BoundGenericStructType>(type)) {
    SD = structTy->getDecl();
  }

  if (SD) {
    // Check to see if we've computed this property for this tuple yet.
    auto Entry = M.AddressOnlyTypeCache.find(type.getPointer());
    // If we got a hit, then return the precomputed value.
    if (Entry != M.AddressOnlyTypeCache.end())
      return Entry->second;

    for (Decl *D : SD->getMembers())
      if (VarDecl *VD = dyn_cast<VarDecl>(D))
        if (!VD->isProperty() &&
            isAddressOnly(VD->getType()->getCanonicalType(), M))
          return M.AddressOnlyTypeCache[type.getPointer()] = true;
    return M.AddressOnlyTypeCache[type.getPointer()] = false;
  }

  // [weak] types are address-only, but [unowned] can just be passed around.
  if (auto refTy = dyn_cast<ReferenceStorageType>(type)) {
    switch (refTy->getOwnership()) {
    case Ownership::Strong: llvm_unreachable("explicit strong ownership");
    case Ownership::Weak: return true;
    case Ownership::Unowned: return false;
    }
    llvm_unreachable("bad ownership kind");
  }

  // Otherwise, it must not be address-only.
  return false;
}

namespace {
  /// Recursively destructure tuple-type arguments into SIL argument types.
  class LoweredFunctionInputTypeVisitor
  : public CanTypeVisitor<LoweredFunctionInputTypeVisitor>
  {
    SILModule &M;
    SmallVectorImpl<SILType> &inputTypes;
  public:
    LoweredFunctionInputTypeVisitor(SILModule &M,
                                    SmallVectorImpl<SILType> &inputTypes)
    : M(M), inputTypes(inputTypes) {}
    
    void visitType(CanType t) {
      inputTypes.push_back(M.Types.getLoweredType(t));
    }
    
    void visitTupleType(CanTupleType tt) {
      for (auto eltType : tt.getElementTypes()) {
        visit(eltType);
      }
    }
  };
} // end anonymous namespace


SILFunctionTypeInfo *SILType::getFunctionTypeInfo(SILModule &M) const {
  AnyFunctionType *ft = cast<AnyFunctionType>(getSwiftRValueType());
  
  auto found = M.FunctionTypeInfoCache.find(ft);
  if (found != M.FunctionTypeInfoCache.end())
    return found->second;

  
  SmallVector<SILType, 8> inputTypes;
  
  // If the result type lowers to an address-only type, add it as an indirect
  // return argument.
  SILType resultType = M.Types.getLoweredType(ft->getResult());
  bool hasIndirectReturn = resultType.isAddressOnly(M);
  if (hasIndirectReturn) {
    inputTypes.push_back(resultType);
    resultType = M.Types.getEmptyTupleType();
  }
  
  // Destructure the input tuple type.
  LoweredFunctionInputTypeVisitor(M, inputTypes)
  .visit(ft->getInput()->getCanonicalType());
  
  
  // We allocate room for an extra unsigned in the uncurriedInputCounts array,
  // so that we can stuff a leading zero in there and be able to efficiently
  // return both the begins and ends of each uncurried argument group.
  void *buffer = M.allocate(sizeof(SILFunctionTypeInfo)
                            + sizeof(SILType)*inputTypes.size(),
                            alignof(SILFunctionTypeInfo));
  SILFunctionTypeInfo *info
  = ::new (buffer) SILFunctionTypeInfo(CanType(ft), inputTypes.size(),
                                       resultType, hasIndirectReturn);
  memcpy(info->getInputTypeBuffer(), inputTypes.data(),
         sizeof(SILType) * inputTypes.size());

  
  M.FunctionTypeInfoCache[ft] = info;
  return info;
}

static CanType getDestructuredArgument(CanType T, unsigned &ArgNo) {
  if (TupleType *TT = T->getAs<TupleType>()) {
    // Destructure tuples.
    for (auto eltType : TT->getElementTypes())
      if (CanType T = getDestructuredArgument(eltType->getCanonicalType(),
                                              ArgNo))
        return T;
    
    return CanType();
  }
  
  if (ArgNo == 0)
    return T;
  --ArgNo;
  return CanType();
}


/// getSwiftArgumentType - Return the swift type of the argument, numbered by
/// the arguments to an apply or partial_apply instruction.
CanType SILFunctionTypeInfo::getSwiftArgumentType(unsigned ArgNo) const {
  // If the call has an indirect return, the first argument is the return slot,
  // not an argument.
  if (hasIndirectReturn()) {
    assert(ArgNo != 0 && "This method cannot handle indirect return reference");
    --ArgNo;
  }
  
  AnyFunctionType *FT = cast<AnyFunctionType>(getSwiftType());
  return getDestructuredArgument(FT->getInput()->getCanonicalType(), ArgNo);
}



