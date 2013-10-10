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

namespace {
  template<typename F>
  class DestructuredArgumentTypeVisitor
    : public CanTypeVisitor<DestructuredArgumentTypeVisitor<F>> {
    const F &fn;
  public:
    DestructuredArgumentTypeVisitor(const F &fn) : fn(fn) { }

    void visitType(CanType t) {
      fn(t);
    }

    void visitTupleType(CanTupleType tt) {
      for (auto eltType : tt.getElementTypes()) {
        this->visit(eltType);
      }
    }
  };

  template <typename F>
  void visitDestructuredArgumentTypes(const F &fn, CanType type) {
    DestructuredArgumentTypeVisitor<F>(fn).visit(type);
  }
}

SILFunctionType *SILType::getFunctionTypeInfo(SILModule &M) const {
  CanAnyFunctionType fnType = cast<AnyFunctionType>(getSwiftRValueType());
  
  auto found = M.FunctionTypeInfoCache.find(fnType);
  if (found != M.FunctionTypeInfoCache.end())
    return found->second;

  SmallVector<SILParameterInfo, 8> inputs;
  
  // If the result type lowers to an address-only type, add it as an indirect
  // return argument.
  auto &resultTL = M.Types.getTypeLowering(fnType.getResult());
  bool hasIndirectReturn = resultTL.isAddressOnly();
  CanType resultType = resultTL.getLoweredType().getSwiftRValueType();
  SILResultInfo result;
  if (hasIndirectReturn) {
    inputs.push_back(SILParameterInfo(resultType,
                                   ParameterConvention::Indirect_Out));
    resultType = TupleType::getEmpty(M.getASTContext());    
    result = SILResultInfo(resultType, ResultConvention::Unowned);
  } else {
    auto convention = (resultTL.isTrivial() ? ResultConvention::Unowned
                                            : ResultConvention::Owned);
    result = SILResultInfo(resultType, convention);
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
    inputs.push_back(SILParameterInfo(loweredType, convention));
  };
  visitDestructuredArgumentTypes(visitFn, fnType.getInput());

  // Find the generic parameters.
  GenericParamList *genericParams = nullptr;
  if (auto polyFnType = dyn_cast<PolymorphicFunctionType>(fnType)) {
    genericParams = &polyFnType->getGenericParams();
  }

  auto silFnType = SILFunctionType::get(genericParams, fnType->getExtInfo(),
                                        inputs, result, M.getASTContext());  
  M.FunctionTypeInfoCache[fnType] = silFnType;
  return silFnType;
}
