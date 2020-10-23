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

#include "swift/AST/Types.h"
#include "swift/Basic/LLVM.h"
#include "swift/SIL/ApplySite.h"
#include "llvm/IR/CallingConv.h"

#include "GenHeap.h"
#include "IRGenModule.h"

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
  class NativeCCEntryPointArgumentEmission;
  class Size;
  class TypeInfo;

  enum class TranslationDirection : bool {
    ToForeign,
    ToNative
  };
  inline TranslationDirection reverse(TranslationDirection direction) {
    return TranslationDirection(!bool(direction));
  }

  // struct SwiftContext {
  //   SwiftContext * __ptrauth(...) callerContext;
  //   SwiftPartialFunction * __ptrauth(...) returnToCaller;
  //   SwiftActor * __ptrauth(...) callerActor;
  //   SwiftPartialFunction * __ptrauth(...) yieldToCaller?;
  //   SwiftError *errorResult;
  //   IndirectResultTypes *indirectResults...;
  //   union {
  //     struct {
  //       SwiftPartialFunction * __ptrauth(...) resumeFromYield?;
  //       SwiftPartialFunction * __ptrauth(...) abortFromYield?;
  //       SwiftActor * __ptrauth(...) calleeActorDuringYield?;
  //       YieldTypes yieldValues...;
  //     };
  //     ResultTypes directResults...;
  //   };
  //   ArgTypes formalArguments...;
  //   SelfType self?;
  // };
  struct AsyncContextLayout : StructLayout {
    struct ArgumentInfo {
      SILType type;
      ParameterConvention convention;
    };
    struct TrailingWitnessInfo {};

  private:
    enum class FixedIndex : unsigned {
      Error = 0,
    };
    enum class FixedCount : unsigned {
      Error = 1,
    };
    IRGenFunction &IGF;
    CanSILFunctionType originalType;
    CanSILFunctionType substitutedType;
    SubstitutionMap substitutionMap;
    SILType errorType;
    bool canHaveValidError;
    SmallVector<SILResultInfo, 4> directReturnInfos;
    SmallVector<SILResultInfo, 4> indirectReturnInfos;
    Optional<ArgumentInfo> localContextInfo;
    NecessaryBindings bindings;
    Optional<TrailingWitnessInfo> trailingWitnessInfo;
    SmallVector<ArgumentInfo, 4> argumentInfos;

    unsigned getErrorIndex() { return (unsigned)FixedIndex::Error; }
    unsigned getFirstIndirectReturnIndex() {
      return getErrorIndex() + getErrorCount();
    }
    unsigned getIndexAfterIndirectReturns() {
      return getFirstIndirectReturnIndex() + getIndirectReturnCount();
    }
    unsigned getFirstDirectReturnIndex() {
      return getIndexAfterIndirectReturns();
    }
    unsigned getIndexAfterDirectReturns() {
      return getFirstDirectReturnIndex() + getDirectReturnCount();
    }
    unsigned getFirstArgumentIndex() { return getIndexAfterDirectReturns(); }
    unsigned getIndexAfterArguments() {
      return getFirstArgumentIndex() + getArgumentCount();
    }
    unsigned getBindingsIndex() {
      assert(hasBindings());
      return getIndexAfterArguments();
    }
    unsigned getIndexAfterBindings() {
      return getIndexAfterArguments() + (hasBindings() ? 1 : 0);
    }
    unsigned getLocalContextIndex() {
      assert(hasLocalContext());
      return getIndexAfterBindings();
    }
    unsigned getIndexAfterLocalContext() {
      return getIndexAfterBindings() +
             (hasLocalContext() ? 1 : 0);
    }
    unsigned getSelfMetadataIndex() {
      assert(hasTrailingWitnesses());
      return getIndexAfterLocalContext();
    }
    unsigned getSelfWitnessTableIndex() {
      assert(hasTrailingWitnesses());
      return getIndexAfterLocalContext() + 1;
    }
    unsigned getIndexAfterTrailingWitnesses() {
      return getIndexAfterLocalContext() + (hasTrailingWitnesses() ? 2 : 0);
    }

  public:
    bool canHaveError() { return canHaveValidError; }
    ElementLayout getErrorLayout() { return getElement(getErrorIndex()); }
    unsigned getErrorCount() { return (unsigned)FixedCount::Error; }
    SILType getErrorType() { return errorType; }

    ElementLayout getIndirectReturnLayout(unsigned index) {
      return getElement(getFirstIndirectReturnIndex() + index);
    }
    unsigned getIndirectReturnCount() { return indirectReturnInfos.size(); }

    bool hasLocalContext() { return (bool)localContextInfo; }
    ElementLayout getLocalContextLayout() {
      assert(hasLocalContext());
      return getElement(getLocalContextIndex());
    }
    SILType getLocalContextType() {
      assert(hasLocalContext());
      return localContextInfo->type;
    }

    bool hasBindings() const { return !bindings.empty(); }
    ElementLayout getBindingsLayout() {
      assert(hasBindings());
      return getElement(getBindingsIndex());
    }
    const NecessaryBindings &getBindings() const { return bindings; }

    ElementLayout getArgumentLayout(unsigned index) {
      return getElement(getFirstArgumentIndex() + index);
    }
    SILType getArgumentType(unsigned index) {
      return argumentInfos[index].type;
    }
    // Returns the type of a parameter of the substituted function using the
    // indexing of the function parameters, *not* the indexing of
    // AsyncContextLayout.
    SILType getParameterType(unsigned index) {
      SILFunctionConventions origConv(substitutedType, IGF.getSILModule());
      return origConv.getSILArgumentType(
          index, IGF.IGM.getMaximalTypeExpansionContext());
    }
    unsigned getArgumentCount() { return argumentInfos.size(); }
    bool hasTrailingWitnesses() { return (bool)trailingWitnessInfo; }
    ElementLayout getSelfMetadataLayout() {
      assert(hasTrailingWitnesses());
      return getElement(getSelfMetadataIndex());
    }
    ElementLayout getSelfWitnessTableLayout() {
      return getElement(getSelfWitnessTableIndex());
    }

    unsigned getDirectReturnCount() { return directReturnInfos.size(); }
    ElementLayout getDirectReturnLayout(unsigned index) {
      return getElement(getFirstDirectReturnIndex() + index);
    }

    AsyncContextLayout(
        IRGenModule &IGM, LayoutStrategy strategy, ArrayRef<SILType> fieldTypes,
        ArrayRef<const TypeInfo *> fieldTypeInfos, IRGenFunction &IGF,
        CanSILFunctionType originalType, CanSILFunctionType substitutedType,
        SubstitutionMap substitutionMap, NecessaryBindings &&bindings,
        Optional<TrailingWitnessInfo> trailingWitnessInfo, SILType errorType,
        bool canHaveValidError, ArrayRef<ArgumentInfo> argumentInfos,
        ArrayRef<SILResultInfo> directReturnInfos,
        ArrayRef<SILResultInfo> indirectReturnInfos,
        Optional<ArgumentInfo> localContextInfo);
  };

  llvm::Value *getDynamicAsyncContextSize(IRGenFunction &IGF,
                                          AsyncContextLayout layout,
                                          CanSILFunctionType functionType,
                                          llvm::Value *thickContext);
  AsyncContextLayout getAsyncContextLayout(IRGenFunction &IGF,
                                           SILFunction *function);

  AsyncContextLayout getAsyncContextLayout(IRGenFunction &IGF,
                                           CanSILFunctionType originalType,
                                           CanSILFunctionType substitutedType,
                                           SubstitutionMap substitutionMap);

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
  void
  emitYieldOnceCoroutineEntry(IRGenFunction &IGF,
                              CanSILFunctionType coroutineType,
                              NativeCCEntryPointArgumentEmission &emission);

  Address emitAllocYieldManyCoroutineBuffer(IRGenFunction &IGF);
  void emitDeallocYieldManyCoroutineBuffer(IRGenFunction &IGF, Address buffer);
  void
  emitYieldManyCoroutineEntry(IRGenFunction &IGF,
                              CanSILFunctionType coroutineType,
                              NativeCCEntryPointArgumentEmission &emission);

  Address emitTaskAlloc(IRGenFunction &IGF, llvm::Value *size,
                        Alignment alignment);
  void emitTaskDealloc(IRGenFunction &IGF, Address address, llvm::Value *size);
  /// Allocate task local storage for the specified layout but using the
  /// provided dynamic size.  Allowing the size to be specified dynamically is
  /// necessary for applies of thick functions the sizes of whose async contexts
  /// are dependent on the underlying, already partially applied, called
  /// function.  The provided sizeLowerBound will be used to track the lifetime
  /// of the allocation that is known statically.
  std::pair<Address, Size> emitAllocAsyncContext(IRGenFunction &IGF,
                                                 AsyncContextLayout layout,
                                                 llvm::Value *sizeValue,
                                                 Size sizeLowerBound);
  std::pair<Address, Size> emitAllocAsyncContext(IRGenFunction &IGF,
                                                 AsyncContextLayout layout);
  void emitDeallocAsyncContext(IRGenFunction &IGF, Address context, Size size);

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
