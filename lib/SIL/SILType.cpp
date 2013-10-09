//===--- SILType.cpp - Defines the SILTypeInfo class hierarchy ------------===//
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
  CanAnyFunctionType fnType = cast<AnyFunctionType>(getSwiftRValueType());
  
  auto found = M.FunctionTypeInfoCache.find(fnType);
  if (found != M.FunctionTypeInfoCache.end())
    return found->second;

  typedef SILFunctionType::ParameterType ParameterType;
  typedef SILFunctionType::ResultType ResultType;

  SmallVector<ParameterType, 8> inputs;
  
  // If the result type lowers to an address-only type, add it as an indirect
  // return argument.
  auto &resultTL = M.Types.getTypeLowering(fnType.getResult());
  bool hasIndirectReturn = resultTL.isAddressOnly();
  CanType resultType = resultTL.getLoweredType().getSwiftRValueType();
  ResultType result;
  if (hasIndirectReturn) {
    inputs.push_back(ParameterType(resultType,
                                   ParameterConvention::Indirect_Out));
    resultType = TupleType::getEmpty(M.getASTContext());    
    result = ResultType(resultType, ResultConvention::Unowned);
  } else {
    auto convention = (resultTL.isTrivial() ? ResultConvention::Unowned
                                            : ResultConvention::Owned);
    result = ResultType(resultType, convention);
  }
  
  // Destructure the input tuple type.
  auto visitFn = [&](CanType type) {
    auto &paramTL = M.Types.getTypeLowering(type);
    ParameterConvention convention;
    if (isa<LValueType>(type)) {
      convention = ParameterConvention::Indirect_Inout;
    } else if (paramTL.isAddressOnly()) {
      convention = ParameterConvention::Indirect_In;
    } else if (paramTL.isTrivial()) {
      convention = ParameterConvention::Direct_Unowned;
    } else {
      convention = ParameterConvention::Direct_Owned;
    }
    assert(isIndirectParameter(convention)
             == paramTL.getLoweredType().isAddress());
    auto loweredType = paramTL.getLoweredType().getSwiftRValueType();
    inputs.push_back(ParameterType(loweredType, convention));
  };
  SILFunctionTypeInfo::visitSwiftArgumentTypes(visitFn, fnType.getInput());

  // Find the generic parameters.
  GenericParamList *genericParams = nullptr;
  if (auto polyFnType = dyn_cast<PolymorphicFunctionType>(fnType)) {
    genericParams = &polyFnType->getGenericParams();
  }

  auto silFnType = SILFunctionType::get(genericParams, fnType->getExtInfo(),
                                        inputs, result, M.getASTContext());

  // We allocate room for an extra unsigned in the uncurriedInputCounts array,
  // so that we can stuff a leading zero in there and be able to efficiently
  // return both the begins and ends of each uncurried argument group.
  void *buffer = M.allocate(sizeof(SILFunctionTypeInfo),
                            alignof(SILFunctionTypeInfo));
  auto info = ::new (buffer)
                  SILFunctionTypeInfo(fnType, CanSILFunctionType(silFnType));
  
  M.FunctionTypeInfoCache[fnType] = info;
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
