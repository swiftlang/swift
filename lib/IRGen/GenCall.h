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
  //   SelfType self;
  //   ArgTypes formalArguments...;
  // };
  struct AsyncContextLayout : StructLayout {
    struct ArgumentInfo {
      SILType type;
      ParameterConvention convention;
    };
    struct TrailingWitnessInfo {};

  private:
    enum class FixedIndex : unsigned {
      Parent = 0,
      ResumeParent = 1,
      ResumeParentExecutor = 2,
      Flags = 3,
      YieldToParent = 4,
    };
    enum class FixedCount : unsigned {
      Parent = 1,
      ResumeParent = 1,
      ResumeParentExecutor = 1,
      Error = 1,
    };
    IRGenModule &IGM;
    CanSILFunctionType originalType;
    CanSILFunctionType substitutedType;
    SubstitutionMap substitutionMap;
    SILType errorType;
    bool canHaveValidError;
    bool isCoroutine;
    SmallVector<SILYieldInfo, 4> yieldInfos;
    SmallVector<SILResultInfo, 4> directReturnInfos;
    SmallVector<SILResultInfo, 4> indirectReturnInfos;
    Optional<ArgumentInfo> localContextInfo;
    NecessaryBindings bindings;
    Optional<TrailingWitnessInfo> trailingWitnessInfo;
    SmallVector<ArgumentInfo, 4> argumentInfos;

    unsigned getParentIndex() { return (unsigned)FixedIndex::Parent; }
    unsigned getResumeParentIndex() {
      return (unsigned)FixedIndex::ResumeParent;
    }
    unsigned getResumeParentExecutorIndex() {
      return (unsigned)FixedIndex::ResumeParentExecutor;
    }
    unsigned getFlagsIndex() { return (unsigned)FixedIndex::Flags; }
    unsigned getYieldToParentIndex() {
      assert(isCoroutine);
      return (unsigned)FixedIndex::YieldToParent;
    }
    unsigned getErrorIndex() {
      return (isCoroutine ? getYieldToParentIndex() : getFlagsIndex()) + 1;
    }
    unsigned getFirstIndirectReturnIndex() {
      return getErrorIndex() + getErrorCount();
    }
    unsigned getIndexAfterIndirectReturns() {
      return getFirstIndirectReturnIndex() + getIndirectReturnCount();
    }
    unsigned getFirstDirectReturnIndex() {
      assert(!isCoroutine);
      return getIndexAfterIndirectReturns();
    }
    unsigned getIndexAfterDirectReturns() {
      assert(!isCoroutine);
      return getFirstDirectReturnIndex() + getDirectReturnCount();
    }
    unsigned getResumeFromYieldIndex() {
      assert(isCoroutine);
      return getIndexAfterIndirectReturns();
    }
    unsigned getAbortFromYieldIndex() {
      assert(isCoroutine);
      return getResumeFromYieldIndex() + 1;
      ;
    }
    unsigned getCalleeExecutorDuringYieldIndex() {
      assert(isCoroutine);
      return getAbortFromYieldIndex() + 1;
    }
    unsigned getFirstYieldIndex() {
      assert(isCoroutine);
      return getCalleeExecutorDuringYieldIndex() + 1;
    }
    unsigned getIndexAfterYields() {
      assert(isCoroutine);
      return getFirstYieldIndex() + getYieldCount();
    }
    unsigned getIndexAfterUnion() {
      if (isCoroutine) {
        return getIndexAfterYields();
      } else {
        return getIndexAfterDirectReturns();
      }
    }
    unsigned getLocalContextIndex() {
      assert(hasLocalContext());
      return getIndexAfterUnion();
    }
    unsigned getIndexAfterLocalContext() {
      return getIndexAfterUnion() + (hasLocalContext() ? 1 : 0);
    }
    unsigned getFirstArgumentIndex() { return getIndexAfterLocalContext(); }
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
    unsigned getSelfMetadataIndex() {
      assert(hasTrailingWitnesses());
      return getIndexAfterBindings();
    }
    unsigned getSelfWitnessTableIndex() {
      assert(hasTrailingWitnesses());
      return getIndexAfterBindings() + 1;
    }
    unsigned getIndexAfterTrailingWitnesses() {
      return getIndexAfterBindings() + (hasTrailingWitnesses() ? 2 : 0);
    }

  public:
    ElementLayout getParentLayout() { return getElement(getParentIndex()); }
    ElementLayout getResumeParentLayout() {
      return getElement(getResumeParentIndex());
    }
    ElementLayout getResumeParentExecutorLayout() {
      return getElement(getResumeParentExecutorIndex());
    }
    ElementLayout getFlagsLayout() { return getElement(getFlagsIndex()); }
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
      SILFunctionConventions origConv(substitutedType, IGM.getSILModule());
      return origConv.getSILArgumentType(index,
                                         IGM.getMaximalTypeExpansionContext());
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

    unsigned getDirectReturnCount() {
      assert(!isCoroutine);
      return directReturnInfos.size();
    }
    ElementLayout getDirectReturnLayout(unsigned index) {
      assert(!isCoroutine);
      return getElement(getFirstDirectReturnIndex() + index);
    }
    unsigned getYieldCount() {
      assert(isCoroutine);
      return yieldInfos.size();
    }

    AsyncContextLayout(
        IRGenModule &IGM, LayoutStrategy strategy, ArrayRef<SILType> fieldTypes,
        ArrayRef<const TypeInfo *> fieldTypeInfos,
        CanSILFunctionType originalType, CanSILFunctionType substitutedType,
        SubstitutionMap substitutionMap, NecessaryBindings &&bindings,
        Optional<TrailingWitnessInfo> trailingWitnessInfo, SILType errorType,
        bool canHaveValidError, ArrayRef<ArgumentInfo> argumentInfos,
        bool isCoroutine, ArrayRef<SILYieldInfo> yieldInfos,
        ArrayRef<SILResultInfo> indirectReturnInfos,
        ArrayRef<SILResultInfo> directReturnInfos,
        Optional<ArgumentInfo> localContextInfo);
  };

  AsyncContextLayout getAsyncContextLayout(IRGenModule &IGM,
                                           SILFunction *function);

  AsyncContextLayout getAsyncContextLayout(IRGenModule &IGM,
                                           CanSILFunctionType originalType,
                                           CanSILFunctionType substitutedType,
                                           SubstitutionMap substitutionMap);

  /// Given an async function, get the pointer to the function to be called and
  /// the size of the context to be allocated.
  ///
  /// \param values Whether any code should be emitted to retrieve the function
  ///               pointer and the size, respectively.  If false is passed, no
  ///               code will be emitted to generate that value and null will
  ///               be returned for it.
  ///
  /// \return {function, size}
  std::pair<llvm::Value *, llvm::Value *> getAsyncFunctionAndSize(
      IRGenFunction &IGF, SILFunctionTypeRepresentation representation,
      FunctionPointer functionPointer, llvm::Value *thickContext,
      std::pair<bool, bool> values = {true, true},
      Size initialContextSize = Size(0));
  llvm::CallingConv::ID expandCallingConv(IRGenModule &IGM,
                                     SILFunctionTypeRepresentation convention);

  Signature emitCastOfFunctionPointer(IRGenFunction &IGF, llvm::Value *&fnPtr,
                                      CanSILFunctionType fnType);

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

  void emitTaskCancel(IRGenFunction &IGF, llvm::Value *task);

  /// Emit a class to swift_task_create[_f] or swift_task_create_future[__f]
  /// with the given flags, parent task, and task function.
  ///
  /// When \c futureResultType is non-null, calls the future variant to create
  /// a future.
  llvm::Value *emitTaskCreate(
    IRGenFunction &IGF, llvm::Value *flags, llvm::Value *parentTask,
    llvm::Value *futureResultType,
    llvm::Value *taskFunction, llvm::Value *localContextInfo,
    SubstitutionMap subs);

  /// Allocate task local storage for the provided dynamic size.
  Address emitAllocAsyncContext(IRGenFunction &IGF, llvm::Value *sizeValue);
  void emitDeallocAsyncContext(IRGenFunction &IGF, Address context);

  void emitAsyncFunctionEntry(IRGenFunction &IGF, SILFunction *asyncFunc);

  /// Yield the given values from the current continuation.
  ///
  /// \return an i1 indicating whether the caller wants to unwind this
  ///   coroutine instead of resuming it normally
  llvm::Value *emitYield(IRGenFunction &IGF,
                         CanSILFunctionType coroutineType,
                         Explosion &yieldedValues);

  enum class AsyncFunctionArgumentIndex : unsigned {
    Task = 0,
    Executor = 1,
    Context = 2,
  };

  void emitAsyncReturn(IRGenFunction &IGF, AsyncContextLayout &layout,
                       CanSILFunctionType fnType);
} // end namespace irgen
} // end namespace swift

#endif
