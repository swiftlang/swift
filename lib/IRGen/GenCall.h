//===--- GenCall.h - IR generation for calls and prologues ------*- C++ -*-===//
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
//
//  This file provides the private interface to the function call
//  and prologue emission support code.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENCALL_H
#define SWIFT_IRGEN_GENCALL_H

#include <stdint.h>

#include "swift/Basic/LLVM.h"
#include "swift/AST/Types.h"
#include "llvm/IR/CallingConv.h"

namespace llvm {
  class AttributeList;
  class Constant;
  class Twine;
  class Type;
  class Value;
}

namespace clang {
  template <class> class CanQual;
  class Type;
}

namespace swift {
namespace irgen {
  class Address;
  class Alignment;
  class Callee;
  class CalleeInfo;
  class Explosion;
  class ExplosionSchema;
  class ForeignFunctionInfo;
  class IRGenFunction;
  class IRGenModule;
  class LoadableTypeInfo;
  class Size;
  class TypeInfo;

  enum class TranslationDirection : bool {
    ToForeign,
    ToNative
  };
  inline TranslationDirection reverse(TranslationDirection direction) {
    return TranslationDirection(!bool(direction));
  }

  llvm::CallingConv::ID expandCallingConv(IRGenModule &IGM,
                                     SILFunctionTypeRepresentation convention);

  /// Does the given function have a self parameter that should be given
  /// the special treatment for self parameters?
  bool hasSelfContextParameter(CanSILFunctionType fnType);

  /// Add function attributes to an attribute set for a byval argument.
  void addByvalArgumentAttributes(IRGenModule &IGM,
                                  llvm::AttributeList &attrs,
                                  unsigned argIndex,
                                  Alignment align);

  /// Add signext or zeroext attribute set for an argument that needs
  /// extending.
  void addExtendAttribute(IRGenModule &IGM, llvm::AttributeList &attrs,
                          unsigned index, bool signExtend);

  /// Can a series of values be simply pairwise coerced to (or from) an
  /// explosion schema, or do they need to traffic through memory?
  bool canCoerceToSchema(IRGenModule &IGM,
                         ArrayRef<llvm::Type*> types,
                         const ExplosionSchema &schema);

  void emitForeignParameter(IRGenFunction &IGF, Explosion &params,
                            ForeignFunctionInfo foreignInfo,
                            unsigned foreignParamIndex, SILType paramTy,
                            const LoadableTypeInfo &paramTI,
                            Explosion &paramExplosion, bool isOutlined);

  void emitClangExpandedParameter(IRGenFunction &IGF,
                                  Explosion &in, Explosion &out,
                                  clang::CanQual<clang::Type> clangType,
                                  SILType swiftType,
                                  const LoadableTypeInfo &swiftTI);

  bool addNativeArgument(IRGenFunction &IGF,
                         Explosion &in,
                         CanSILFunctionType fnTy,
                         SILParameterInfo origParamInfo, Explosion &args,
                         bool isOutlined);

  /// Allocate a stack buffer of the appropriate size to bitwise-coerce a value
  /// between two LLVM types.
  std::pair<Address, Size>
  allocateForCoercion(IRGenFunction &IGF,
                      llvm::Type *fromTy,
                      llvm::Type *toTy,
                      const llvm::Twine &basename);

  void extractScalarResults(IRGenFunction &IGF, llvm::Type *bodyType,
                            llvm::Value *call, Explosion &out);

  Callee getBlockPointerCallee(IRGenFunction &IGF, llvm::Value *blockPtr,
                               CalleeInfo &&info);

  Callee getCFunctionPointerCallee(IRGenFunction &IGF, llvm::Value *fnPtr,
                                   CalleeInfo &&info);

  Callee getSwiftFunctionPointerCallee(IRGenFunction &IGF,
                                       llvm::Value *fnPtr,
                                       llvm::Value *contextPtr,
                                       CalleeInfo &&info,
                                       bool castOpaqueToRefcountedContext);

  Address emitAllocYieldOnceCoroutineBuffer(IRGenFunction &IGF);
  void emitDeallocYieldOnceCoroutineBuffer(IRGenFunction &IGF, Address buffer);
  void emitYieldOnceCoroutineEntry(IRGenFunction &IGF,
                                   CanSILFunctionType coroutineType,
                                   Explosion &allParams);

  Address emitAllocYieldManyCoroutineBuffer(IRGenFunction &IGF);
  void emitDeallocYieldManyCoroutineBuffer(IRGenFunction &IGF, Address buffer);
  void emitYieldManyCoroutineEntry(IRGenFunction &IGF,
                                   CanSILFunctionType coroutineType,
                                   Explosion &allParams);

  /// Yield the given values from the current continuation.
  ///
  /// \return an i1 indicating whether the caller wants to unwind this
  ///   coroutine instead of resuming it normally
  llvm::Value *emitYield(IRGenFunction &IGF,
                         CanSILFunctionType coroutineType,
                         Explosion &yieldedValues);

} // end namespace irgen
} // end namespace swift

#endif
