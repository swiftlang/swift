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
#include "swift/AST/Decl.h"
#include "llvm/Support/ErrorHandling.h"
using namespace swift;

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
  auto visitFn = [&](CanType type) {
    inputTypes.push_back(M.Types.getLoweredType(type));
  };
  SILFunctionTypeInfo::DestructuredArgumentTypeVisitor<decltype(visitFn)>(visitFn)
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

/// getSwiftResultType - Return the swift type of the result
CanType SILFunctionTypeInfo::getSwiftResultType() const {
  AnyFunctionType *FT = cast<AnyFunctionType>(getSwiftType());
  return FT->getResult()->getCanonicalType();
}
