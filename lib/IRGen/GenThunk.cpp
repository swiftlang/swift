//===--- GenThunk.cpp - IR Generation for Method Dispatch Thunks ----------===//
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
//  This file implements IR generation for class and protocol method dispatch
//  thunks, which are used in resilient builds to hide vtable and witness table
//  offsets from clients.
//
//===----------------------------------------------------------------------===//

#include "Callee.h"
#include "CallEmission.h"
#include "ClassMetadataVisitor.h"
#include "ConstantBuilder.h"
#include "Explosion.h"
#include "GenCall.h"
#include "GenClass.h"
#include "GenDecl.h"
#include "GenHeap.h"
#include "GenOpaque.h"
#include "GenPointerAuth.h"
#include "GenProto.h"
#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "LoadableTypeInfo.h"
#include "MetadataLayout.h"
#include "NativeConventionSchema.h"
#include "ProtocolInfo.h"
#include "Signature.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/Basic/Assertions.h"
#include "swift/IRGen/Linking.h"
#include "swift/SIL/SILDeclRef.h"
#include "llvm/IR/Function.h"

using namespace swift;
using namespace irgen;

/// Find the entry point for a method dispatch thunk.
llvm::Function *
IRGenModule::getAddrOfDispatchThunk(SILDeclRef declRef,
                                    ForDefinition_t forDefinition) {
  LinkEntity entity = LinkEntity::forDispatchThunk(declRef);

  llvm::Function *&entry = GlobalFuncs[entity];
  if (entry) {
    if (forDefinition) updateLinkageForDefinition(*this, entry, entity);
    return entry;
  }

  auto fnType = getSILModule().Types.getConstantFunctionType(
      getMaximalTypeExpansionContext(), declRef);
  Signature signature = getSignature(fnType);
  LinkInfo link = LinkInfo::get(*this, entity, forDefinition);

  entry = createFunction(*this, link, signature);
  return entry;
}

namespace {

class IRGenThunk {
  IRGenFunction &IGF;
  SILDeclRef declRef;
  TypeExpansionContext expansionContext;
  CanSILFunctionType origTy;
  CanSILFunctionType substTy;
  SubstitutionMap subMap;
  bool isAsync;
  bool isCoroutine;
  bool isCalleeAllocatedCoroutine;
  bool isWitnessMethod;
  llvm::Value *allocator;
  llvm::Value *buffer;

  std::optional<AsyncContextLayout> asyncLayout;

  // Initialized by prepareArguments()
  llvm::Value *indirectReturnSlot = nullptr;
  llvm::Value *selfValue = nullptr;
  llvm::Value *errorResult = nullptr;
  llvm::Value *typedErrorIndirectErrorSlot = nullptr;
  WitnessMetadata witnessMetadata;
  Explosion params;

  void prepareArguments();
  Callee lookupMethod();

public:
  IRGenThunk(IRGenFunction &IGF, SILDeclRef declRef);
  void emit();
};

} // end namespace

IRGenThunk::IRGenThunk(IRGenFunction &IGF, SILDeclRef declRef)
  : IGF(IGF), declRef(declRef),
    expansionContext(IGF.IGM.getMaximalTypeExpansionContext()) {
  auto &Types = IGF.IGM.getSILModule().Types;
  origTy = Types.getConstantFunctionType(expansionContext, declRef);

  if (auto *genericEnv = Types.getConstantGenericEnvironment(declRef))
    subMap = genericEnv->getForwardingSubstitutionMap();

  substTy = origTy->substGenericArgs(
      IGF.IGM.getSILModule(), subMap, expansionContext);

  isAsync = origTy->isAsync();
  isCoroutine = origTy->isCoroutine();
  isCalleeAllocatedCoroutine = origTy->isCalleeAllocatedCoroutine();

  auto *decl = cast<AbstractFunctionDecl>(declRef.getDecl());
  isWitnessMethod = isa<ProtocolDecl>(decl->getDeclContext());

  if (isAsync) {
    asyncLayout.emplace(irgen::getAsyncContextLayout(
        IGF.IGM, origTy, substTy, subMap));
  }
}

// FIXME: This duplicates the structure of CallEmission. It should be
// possible to refactor some code and simplify this drastically, since
// conceptually all we're doing is forwarding the arguments verbatim
// using the sync or async calling convention.
void IRGenThunk::prepareArguments() {
  Explosion original = IGF.collectParameters();

  if (isWitnessMethod) {
    witnessMetadata.SelfWitnessTable = original.takeLast();
    witnessMetadata.SelfMetadata = original.takeLast();
  }

  SILFunctionConventions conv(origTy, IGF.getSILModule());

  if (origTy->hasErrorResult()) {
    typedErrorIndirectErrorSlot = nullptr;

    // Set the typed error value result slot.
    if (conv.isTypedError() && !conv.hasIndirectSILErrorResults()) {
      auto errorType =
        conv.getSILErrorType(IGF.IGM.getMaximalTypeExpansionContext());
      auto &errorTI = cast<FixedTypeInfo>(IGF.getTypeInfo(errorType));
      auto &errorSchema = errorTI.nativeReturnValueSchema(IGF.IGM);
      auto resultType =
          conv.getSILResultType(IGF.IGM.getMaximalTypeExpansionContext());
      auto &resultTI = cast<FixedTypeInfo>(IGF.getTypeInfo(resultType));
      auto &resultSchema = resultTI.nativeReturnValueSchema(IGF.IGM);

      if (resultSchema.requiresIndirect() ||
          errorSchema.shouldReturnTypedErrorIndirectly() ||
          conv.hasIndirectSILResults()) {
        auto directTypedErrorAddr = original.takeLast();
        IGF.setCalleeTypedErrorResultSlot(Address(directTypedErrorAddr,
                                                  errorTI.getStorageType(),
                                                  errorTI.getFixedAlignment()));
      }
    } else if (conv.isTypedError()) {
      auto directTypedErrorAddr = original.takeLast();
      // Store for later processing when we know the argument index.
      // (i.e. emission->setIndirectTypedErrorResultSlotArgsIndex was called)
      typedErrorIndirectErrorSlot = directTypedErrorAddr;
    }

    if (isAsync) {
      // nothing to do.
    } else {
      errorResult = original.takeLast();
      IGF.setCallerErrorResultSlot(Address(errorResult,
                                           IGF.IGM.Int8PtrTy,
                                           IGF.IGM.getPointerAlignment()));
    }
  }

  if (isCalleeAllocatedCoroutine) {
    buffer = original.claimNext();
    allocator = original.claimNext();
  } else if (isCoroutine) {
    original.transferInto(params, 1);
  }

  selfValue = original.takeLast();

  // Prepare indirect results, if any.
  SILType directResultType = conv.getSILResultType(expansionContext);
  auto &directResultTL = IGF.IGM.getTypeInfo(directResultType);
  auto &schema = directResultTL.nativeReturnValueSchema(IGF.IGM);
  if (schema.requiresIndirect()) {
    indirectReturnSlot = original.claimNext();
  }

  original.transferInto(params, conv.getNumIndirectSILResults());

  // Chop off the async context parameters.
  if (isAsync) {
    unsigned numAsyncContextParams =
        (unsigned)AsyncFunctionArgumentIndex::Context + 1;
    (void)original.claim(numAsyncContextParams);
  }

  // Prepare each parameter.
  for (auto param : origTy->getParameters().drop_back()) {
    auto paramType = conv.getSILType(param, expansionContext);

    // If the SIL parameter isn't passed indirectly, we need to map it
    // to an explosion.
    if (paramType.isObject()) {
      auto &paramTI = IGF.getTypeInfo(paramType);
      auto &loadableParamTI = cast<LoadableTypeInfo>(paramTI);
      auto &nativeSchema = loadableParamTI.nativeParameterValueSchema(IGF.IGM);
      unsigned size = nativeSchema.size();

      Explosion nativeParam;
      if (nativeSchema.requiresIndirect()) {
        // If the explosion must be passed indirectly, load the value from the
        // indirect address.
        Address paramAddr =
            loadableParamTI.getAddressForPointer(original.claimNext());
        loadableParamTI.loadAsTake(IGF, paramAddr, nativeParam);
      } else {
        if (!nativeSchema.empty()) {
          // Otherwise, we map from the native convention to the type's
          // explosion schema.
          Explosion paramExplosion;
          original.transferInto(paramExplosion, size);
          nativeParam = nativeSchema.mapFromNative(IGF.IGM, IGF, paramExplosion,
                                                   paramType);
        }
      }

      nativeParam.transferInto(params, nativeParam.size());
    } else {
      params.add(original.claimNext());
    }
  }

  // Anything else, just pass along.  This will include things like
  // generic arguments.
  params.add(original.claimAll());
}

Callee IRGenThunk::lookupMethod() {
  CalleeInfo info(origTy, substTy, subMap);

  // Protocol case.
  if (isWitnessMethod) {
    // Find the witness we're interested in.
    auto *wtable = witnessMetadata.SelfWitnessTable;
    auto witness = emitWitnessMethodValue(IGF, wtable, declRef);

    return Callee(std::move(info), witness, selfValue);
  }

  // Class case.

  // Load the metadata, or use the 'self' value if we have a static method.
  auto selfTy = origTy->getSelfParameter().getSILStorageType(
      IGF.IGM.getSILModule(), origTy, expansionContext);

  // If 'self' is an instance, load the class metadata.
  llvm::Value *metadata;
  if (selfTy.is<MetatypeType>()) {
    metadata = selfValue;
  } else {
    metadata = emitHeapMetadataRefForHeapObject(IGF, selfValue, selfTy,
                                                /*suppress cast*/ true);
  }

  // Find the method we're interested in.
  auto method = emitVirtualMethodValue(IGF, metadata, declRef, origTy);

  return Callee(std::move(info), method, selfValue);
}

void IRGenThunk::emit() {
  PrettyStackTraceDecl stackTraceRAII("emitting dispatch thunk for",
                                      declRef.getDecl());
  TemporarySet Temporaries;

  GenericContextScope scope(IGF.IGM, origTy->getInvocationGenericSignature());

  if (isAsync) {
    auto asyncContextIdx = Signature::forAsyncEntry(
                               IGF.IGM, origTy,
                               FunctionPointerKind::defaultAsync())
                               .getAsyncContextIndex();

    auto entity = LinkEntity::forDispatchThunk(declRef);
    emitAsyncFunctionEntry(IGF, *asyncLayout, entity, asyncContextIdx);
    emitAsyncFunctionPointer(IGF.IGM, IGF.CurFn, entity,
                             asyncLayout->getSize());
  }

  prepareArguments();

  if (isCalleeAllocatedCoroutine) {
    auto entity = LinkEntity::forDispatchThunk(declRef);
    auto *cfp = emitCoroFunctionPointer(IGF.IGM, IGF.CurFn, entity);
    emitYieldOnce2CoroutineEntry(IGF, origTy, buffer, allocator,
                                 cast<llvm::GlobalVariable>(cfp));
  }

  auto callee = lookupMethod();

  std::unique_ptr<CallEmission> emission =
      getCallEmission(IGF, callee.getSwiftContext(), std::move(callee));

  if (typedErrorIndirectErrorSlot) {
    emission->setIndirectTypedErrorResultSlot(typedErrorIndirectErrorSlot);
  }

  emission->begin();

  if (isCalleeAllocatedCoroutine) {
    params.insert(0, emission->getCoroAllocator());
    params.insert(0, emission->getCoroStaticFrame().getAddressPointer());
  }

  emission->setArgs(params, /*isOutlined=*/false, &witnessMetadata);

  if (isCoroutine && !isCalleeAllocatedCoroutine) {
    assert(!isAsync);

    auto *result = emission->emitCoroutineAsOrdinaryFunction();
    emission->end();

    IGF.Builder.CreateRet(result);
    return;
  }

  Explosion result;

  // Determine if the result is returned indirectly.
  SILFunctionConventions conv(origTy, IGF.getSILModule());
  SILType directResultType = conv.getSILResultType(expansionContext);
  auto &directResultTL = IGF.IGM.getTypeInfo(directResultType);
  auto &schema = directResultTL.nativeReturnValueSchema(IGF.IGM);
  if (schema.requiresIndirect()) {
    Address indirectReturnAddr(indirectReturnSlot,
                               IGF.IGM.getStorageType(directResultType),
                               directResultTL.getBestKnownAlignment());
    emission->emitToMemory(indirectReturnAddr,
                           cast<LoadableTypeInfo>(directResultTL), false);
  } else {
    emission->emitToExplosion(result, /*isOutlined=*/false);
  }

  llvm::Value *errorValue = nullptr;

  if (emission->getTypedErrorExplosion() ||
      (isAsync && origTy->hasErrorResult())) {
    SILType errorType = conv.getSILErrorType(expansionContext);
    Address calleeErrorSlot = emission->getCalleeErrorSlot(
        errorType, /*isCalleeAsync=*/origTy->isAsync());
    errorValue = IGF.Builder.CreateLoad(calleeErrorSlot);
  }

  if (isCalleeAllocatedCoroutine) {
    emission->claimTemporaries();
  }

  emission->end();

  if (isCalleeAllocatedCoroutine) {
    // Thunks for callee-allocated coroutines thunk both the ramp function and
    // continuations in order to deallocate the fixed-per-callee-size frame
    // allocated in the thunk in the case of the malloc allocator.
    //
    // EARLIER:
    // %allocation = call token @llvm.coro.alloca.alloc.i64(i64 %size, i32 16)
    // %allocation_handle = call ptr @llvm.coro.alloca.get(token %allocation)
    // WE ARE HERE:
    // call ptr (...) @llvm.coro.suspend.retcon(...callee's yields...)
    // call swiftcc void %continuation(ptr noalias %callee_frame, ptr %allocator)
    // call void @llvm.lifetime.end.p0(i64 -1, ptr %allocation_handle)
    // call void @llvm.coro.alloca.free(token %allocation)
    // call i1 @llvm.coro.end(ptr %3, i1 false, token none)
    // unreachable
    auto *continuation = result.claimNext();
    auto sig = Signature::forCoroutineContinuation(IGF.IGM, origTy);
    continuation = IGF.Builder.CreateBitCast(continuation, IGF.IGM.PtrTy);
    auto schemaAndEntity =
        getCoroutineResumeFunctionPointerAuth(IGF.IGM, origTy);
    auto pointerAuth = PointerAuthInfo::emit(
        IGF, schemaAndEntity.first,
        emission->getCoroStaticFrame().getAddress().getAddress(),
        schemaAndEntity.second);
    auto callee =
        FunctionPointer::createSigned(FunctionPointerKind::BasicKind::Function,
                                      continuation, pointerAuth, sig);
    SmallVector<llvm::Value *, 8> yieldArgs;
    while (!result.empty()) {
      yieldArgs.push_back(result.claimNext());
    }
    IGF.Builder.CreateIntrinsicCall(llvm::Intrinsic::coro_suspend_retcon,
                                    {IGF.IGM.CoroAllocatorPtrTy}, yieldArgs);
    IGF.Builder.CreateCall(
        callee,
        {emission->getCoroStaticFrame().getAddress().getAddress(), allocator});
    Temporaries.destroyAll(IGF);
    emitDeallocYieldOnce2CoroutineFrame(IGF, emission->getCoroStaticFrame());
    IGF.Builder.CreateIntrinsicCall(
        llvm::Intrinsic::coro_end,
        {IGF.getCoroutineHandle(), IGF.Builder.getFalse(),
         llvm::ConstantTokenNone::get(IGF.Builder.getContext())});
    IGF.Builder.CreateUnreachable();
    return;
  }

  // FIXME: we shouldn't have to generate all of this. We should just forward
  // the value as is
  if (auto &error = emission->getTypedErrorExplosion()) {
    llvm::BasicBlock *successBB = IGF.createBasicBlock("success");
    llvm::BasicBlock *errorBB = IGF.createBasicBlock("failure");

    llvm::Value *nil = llvm::ConstantPointerNull::get(
        cast<llvm::PointerType>(errorValue->getType()));
    auto *hasError = IGF.Builder.CreateICmpNE(errorValue, nil);

    // Predict no error is thrown.
    hasError = IGF.IGM.getSILModule().getOptions().EnableThrowsPrediction
                   ? IGF.Builder.CreateExpectCond(IGF.IGM, hasError, false)
                   : hasError;

    IGF.Builder.CreateCondBr(hasError, errorBB, successBB);

    IGF.Builder.emitBlock(errorBB);
    if (isAsync) {
      auto &IGM = IGF.IGM;
      SILType silErrorTy = conv.getSILErrorType(expansionContext);
      auto &errorTI = IGF.IGM.getTypeInfo(silErrorTy);
      auto &errorSchema = errorTI.nativeReturnValueSchema(IGF.IGM);
      auto combined = combineResultAndTypedErrorType(IGM, schema, errorSchema);

      Explosion errorArgValues;

      if (!combined.combinedTy->isVoidTy()) {
        llvm::Value *expandedResult =
            llvm::UndefValue::get(combined.combinedTy);
        if (!errorSchema.getExpandedType(IGM)->isVoidTy()) {
          auto nativeError =
              errorSchema.mapIntoNative(IGM, IGF, *error, silErrorTy, false);

          if (auto *structTy =
                  dyn_cast<llvm::StructType>(combined.combinedTy)) {
            for (unsigned i : combined.errorValueMapping) {
              llvm::Value *elt = nativeError.claimNext();
              auto *nativeTy = structTy->getElementType(i);
              elt = convertForDirectError(IGF, elt, nativeTy,
                                          /*forExtraction*/ false);
              expandedResult =
                  IGF.Builder.CreateInsertValue(expandedResult, elt, i);
            }
            IGF.emitAllExtractValues(expandedResult, structTy, errorArgValues);
          } else if (!errorSchema.getExpandedType(IGM)->isVoidTy()) {
            errorArgValues = convertForDirectError(IGF, nativeError.claimNext(),
                                                   combined.combinedTy,
                                                   /*forExtraction*/ false);
          }
        } else if (auto *structTy =
                       dyn_cast<llvm::StructType>(combined.combinedTy)) {
          IGF.emitAllExtractValues(expandedResult, structTy, errorArgValues);
        } else {
          errorArgValues = expandedResult;
        }
      }
      errorArgValues.add(errorValue);
      emitAsyncReturn(IGF, *asyncLayout, origTy, errorArgValues.claimAll());

      IGF.Builder.emitBlock(successBB);

      Explosion resultArgValues;
      if (result.empty()) {
        if (!combined.combinedTy->isVoidTy()) {
          if (auto *structTy =
                  dyn_cast<llvm::StructType>(combined.combinedTy)) {
            IGF.emitAllExtractValues(llvm::UndefValue::get(structTy), structTy,
                                     resultArgValues);
          } else {
            resultArgValues = llvm::UndefValue::get(combined.combinedTy);
          }
        }
      } else {
        if (auto *structTy = dyn_cast<llvm::StructType>(combined.combinedTy)) {
          llvm::Value *expandedResult =
              llvm::UndefValue::get(combined.combinedTy);
          for (size_t i = 0, count = result.size(); i < count; i++) {
            llvm::Value *elt = result.claimNext();
            auto *nativeTy = structTy->getElementType(i);
            elt = convertForDirectError(IGF, elt, nativeTy,
                                        /*forExtraction*/ false);
            expandedResult =
                IGF.Builder.CreateInsertValue(expandedResult, elt, i);
          }
          IGF.emitAllExtractValues(expandedResult, structTy, resultArgValues);
        } else {
          resultArgValues = convertForDirectError(IGF, result.claimNext(),
                                                  combined.combinedTy,
                                                  /*forExtraction*/ false);
        }
      }

      resultArgValues.add(errorValue);
      emitAsyncReturn(IGF, *asyncLayout, origTy, resultArgValues.claimAll());

      return;
    } else {
      if (!error->empty()) {
        // Map the direct error explosion from the call back to the native
        // explosion for the return.
        SILType silErrorTy = conv.getSILErrorType(expansionContext);
        auto &errorTI = IGF.IGM.getTypeInfo(silErrorTy);
        auto &errorSchema = errorTI.nativeReturnValueSchema(IGF.IGM);
        auto combined =
            combineResultAndTypedErrorType(IGF.IGM, schema, errorSchema);
        Explosion nativeAgg;
        buildDirectError(IGF, combined, errorSchema, silErrorTy, *error,
                         /*forAsync*/ false, nativeAgg);
        IGF.emitScalarReturn(IGF.CurFn->getReturnType(), nativeAgg);
      } else {
        if (IGF.CurFn->getReturnType()->isVoidTy()) {
          IGF.Builder.CreateRetVoid();
        } else {
          IGF.Builder.CreateRet(
              llvm::UndefValue::get(IGF.CurFn->getReturnType()));
        }
      }
      IGF.Builder.emitBlock(successBB);
    }
  } else {
    if (isAsync) {
      Explosion error;
      if (errorValue)
        error.add(errorValue);
      emitAsyncReturn(IGF, *asyncLayout, directResultType, origTy, result,
                      error);
      return;
    }
  }

  // Return the result.
  if (result.empty()) {
    if (emission->getTypedErrorExplosion() &&
        !IGF.CurFn->getReturnType()->isVoidTy()) {
      IGF.Builder.CreateRet(llvm::UndefValue::get(IGF.CurFn->getReturnType()));
    } else {
      IGF.Builder.CreateRetVoid();
    }
    return;
  }

  auto resultTy = conv.getSILResultType(expansionContext);
  resultTy = resultTy.subst(IGF.getSILModule(), subMap);
  IGF.emitScalarReturn(resultTy, resultTy, result,
                       /*swiftCCReturn=*/false,
                       /*isOutlined=*/false);
}

void IRGenModule::emitDispatchThunk(SILDeclRef declRef) {
  auto *f = getAddrOfDispatchThunk(declRef, ForDefinition);
  if (!f->isDeclaration()) {
    return;
  }

  IRGenFunction IGF(*this, f);
  IRGenThunk(IGF, declRef).emit();
}

llvm::Constant *
IRGenModule::getAddrOfAsyncFunctionPointer(LinkEntity entity) {
  llvm::Constant *Pointer =
      getAddrOfLLVMVariable(LinkEntity::forAsyncFunctionPointer(entity),
                            NotForDefinition, DebugTypeInfo());
  if (!getOptions().IndirectAsyncFunctionPointer)
    return Pointer;

  // When the symbol does not have DLL Import storage, we must directly address
  // it. Otherwise, we will form an invalid reference.
  if (!Pointer->isDLLImportDependent())
    return Pointer;

  llvm::Constant *PointerPointer =
      getOrCreateGOTEquivalent(Pointer,
                               LinkEntity::forAsyncFunctionPointer(entity));
  llvm::Constant *PointerPointerConstant =
      llvm::ConstantExpr::getPtrToInt(PointerPointer, IntPtrTy);
  llvm::Constant *Marker =
      llvm::Constant::getIntegerValue(IntPtrTy, APInt(IntPtrTy->getBitWidth(),
                                                      1));
  // TODO(compnerd) ensure that the pointer alignment guarantees that bit-0 is
  // cleared. We cannot use an `getOr` here as it does not form a relocatable
  // expression.
  llvm::Constant *Address =
      llvm::ConstantExpr::getAdd(PointerPointerConstant, Marker);

  IndirectAsyncFunctionPointers[entity] = Address;
  return llvm::ConstantExpr::getIntToPtr(Address, PtrTy);
}

llvm::Constant *
IRGenModule::getAddrOfAsyncFunctionPointer(SILFunction *function) {
  (void)getAddrOfSILFunction(function, NotForDefinition);
  return getAddrOfAsyncFunctionPointer(
      LinkEntity::forSILFunction(function));
}

llvm::Constant *IRGenModule::defineAsyncFunctionPointer(LinkEntity entity,
                                                        ConstantInit init) {
  auto asyncEntity = LinkEntity::forAsyncFunctionPointer(entity);
  auto *var = cast<llvm::GlobalVariable>(
      getAddrOfLLVMVariable(asyncEntity, init, DebugTypeInfo()));
  return var;
}

SILFunction *
IRGenModule::getSILFunctionForAsyncFunctionPointer(llvm::Constant *afp) {
  for (auto &entry : GlobalVars) {
    if (entry.getSecond() == afp) {
      auto entity = entry.getFirst();
      return entity.getSILFunction();
    }
  }
  for (auto &entry : IndirectAsyncFunctionPointers) {
    if (entry.getSecond() == afp) {
      auto entity = entry.getFirst();
      assert(getOptions().IndirectAsyncFunctionPointer &&
             "indirect async function found for non-indirect async function"
             " target?");
      return entity.getSILFunction();
    }
  }
  return nullptr;
}

llvm::Constant *IRGenModule::getAddrOfCoroFunctionPointer(LinkEntity entity) {
  llvm::Constant *Pointer =
      getAddrOfLLVMVariable(LinkEntity::forCoroFunctionPointer(entity),
                            NotForDefinition, DebugTypeInfo());
  if (!getOptions().IndirectCoroFunctionPointer)
    return Pointer;

  // When the symbol does not have DLL Import storage, we must directly address
  // it. Otherwise, we will form an invalid reference.
  if (!Pointer->isDLLImportDependent())
    return Pointer;

  llvm::Constant *PointerPointer = getOrCreateGOTEquivalent(
      Pointer, LinkEntity::forCoroFunctionPointer(entity));
  llvm::Constant *PointerPointerConstant =
      llvm::ConstantExpr::getPtrToInt(PointerPointer, IntPtrTy);
  llvm::Constant *Marker = llvm::Constant::getIntegerValue(
      IntPtrTy, APInt(IntPtrTy->getBitWidth(), 1));
  // TODO(compnerd) ensure that the pointer alignment guarantees that bit-0 is
  // cleared. We cannot use an `getOr` here as it does not form a relocatable
  // expression.
  llvm::Constant *Address =
      llvm::ConstantExpr::getAdd(PointerPointerConstant, Marker);

  IndirectCoroFunctionPointers[entity] = Address;
  return llvm::ConstantExpr::getIntToPtr(Address, PtrTy);
}

llvm::Constant *
IRGenModule::getAddrOfCoroFunctionPointer(SILFunction *function) {
  (void)getAddrOfSILFunction(function, NotForDefinition);
  return getAddrOfCoroFunctionPointer(LinkEntity::forSILFunction(function));
}

llvm::Constant *IRGenModule::defineCoroFunctionPointer(LinkEntity entity,
                                                       ConstantInit init) {
  auto coroEntity = LinkEntity::forCoroFunctionPointer(entity);
  auto *var = cast<llvm::GlobalVariable>(
      getAddrOfLLVMVariable(coroEntity, init, DebugTypeInfo()));
  return var;
}

SILFunction *
IRGenModule::getSILFunctionForCoroFunctionPointer(llvm::Constant *cfp) {
  for (auto &entry : GlobalVars) {
    if (entry.getSecond() == cfp) {
      auto entity = entry.getFirst();
      return entity.getSILFunction();
    }
  }
  for (auto &entry : IndirectCoroFunctionPointers) {
    if (entry.getSecond() == cfp) {
      auto entity = entry.getFirst();
      assert(getOptions().IndirectCoroFunctionPointer &&
             "indirect coro function found for non-indirect coro function"
             " target?");
      return entity.getSILFunction();
    }
  }
  return nullptr;
}

llvm::GlobalValue *IRGenModule::defineMethodDescriptor(
    SILDeclRef declRef, NominalTypeDecl *nominalDecl,
    llvm::Constant *definition, llvm::Type *typeOfDefinitionValue) {
  auto entity = LinkEntity::forMethodDescriptor(declRef);
  return defineAlias(entity, definition, typeOfDefinitionValue);
}

/// Get or create a method descriptor variable.
llvm::Constant *
IRGenModule::getAddrOfMethodDescriptor(SILDeclRef declRef,
                                       ForDefinition_t forDefinition) {
  assert(forDefinition == NotForDefinition);
  assert(declRef.getOverriddenWitnessTableEntry() == declRef &&
         "Overriding protocol requirements do not have method descriptors");
  LinkEntity entity = LinkEntity::forMethodDescriptor(declRef);
  return getAddrOfLLVMVariable(entity, forDefinition, DebugTypeInfo());
}

/// Fetch the method lookup function for a resilient class.
llvm::Function *
IRGenModule::getAddrOfMethodLookupFunction(ClassDecl *classDecl,
                                           ForDefinition_t forDefinition) {
  IRGen.noteUseOfTypeMetadata(classDecl);

  LinkEntity entity = LinkEntity::forMethodLookupFunction(classDecl);
  llvm::Function *&entry = GlobalFuncs[entity];
  if (entry) {
    if (forDefinition) updateLinkageForDefinition(*this, entry, entity);
    return entry;
  }

  llvm::Type *params[] = {TypeMetadataPtrTy, PtrTy};
  auto fnType = llvm::FunctionType::get(Int8PtrTy, params, false);
  Signature signature(fnType, llvm::AttributeList(), SwiftCC);
  LinkInfo link = LinkInfo::get(*this, entity, forDefinition);
  entry = createFunction(*this, link, signature);
  return entry;
}

void IRGenModule::emitMethodLookupFunction(ClassDecl *classDecl) {
  auto *f = getAddrOfMethodLookupFunction(classDecl, ForDefinition);
  if (!f->isDeclaration()) {
    assert(IRGen.isLazilyReemittingNominalTypeDescriptor(classDecl));
    return;
  }

  IRGenFunction IGF(*this, f);

  auto params = IGF.collectParameters();
  auto *metadata = params.claimNext();
  auto *method = params.claimNext();

  auto *description = getAddrOfTypeContextDescriptor(classDecl,
                                                     RequireMetadata);

  // Check for lookups of nonoverridden methods first.
  class LookUpNonoverriddenMethods
    : public ClassMetadataScanner<LookUpNonoverriddenMethods> {
  
    IRGenFunction &IGF;
    llvm::Value *methodArg;
      
  public:
    LookUpNonoverriddenMethods(IRGenFunction &IGF,
                               ClassDecl *classDecl,
                               llvm::Value *methodArg)
      : ClassMetadataScanner(IGF.IGM, classDecl), IGF(IGF),
        methodArg(methodArg) {}
      
    void noteNonoverriddenMethod(SILDeclRef method) {
      // The method lookup function would be used only for `super.` calls
      // from other modules, so we only need to look at public-visibility
      // methods here.
      if (!hasPublicVisibility(method.getLinkage(NotForDefinition))) {
        return;
      }
      
      auto methodDesc = IGM.getAddrOfMethodDescriptor(method, NotForDefinition);
      
      auto isMethod = IGF.Builder.CreateICmpEQ(methodArg, methodDesc);
      
      auto falseBB = IGF.createBasicBlock("");
      auto trueBB = IGF.createBasicBlock("");

      IGF.Builder.CreateCondBr(isMethod, trueBB, falseBB);
      
      IGF.Builder.emitBlock(trueBB);
      // Since this method is nonoverridden, we can produce a static result.
      auto entry = VTable->getEntry(IGM.getSILModule(), method);
      llvm::Value *impl = IGM.getAddrOfSILFunction(entry->getImplementation(),
                                                   NotForDefinition);
      // Sign using the discriminator we would include in the method
      // descriptor.
      if (auto &schema =
              entry->getImplementation()->getLoweredFunctionType()->isAsync()
                  ? IGM.getOptions().PointerAuth.AsyncSwiftClassMethods
              : entry->getImplementation()
                      ->getLoweredFunctionType()
                      ->isCalleeAllocatedCoroutine()
                  ? IGM.getOptions().PointerAuth.CoroSwiftClassMethods
                  : IGM.getOptions().PointerAuth.SwiftClassMethods) {
        auto discriminator =
          PointerAuthInfo::getOtherDiscriminator(IGM, schema, method);
        
        impl = emitPointerAuthSign(IGF, impl,
                            PointerAuthInfo(schema.getKey(), discriminator));
      }
      impl = IGF.Builder.CreateBitCast(impl, IGM.Int8PtrTy);
      IGF.Builder.CreateRet(impl);
      
      IGF.Builder.emitBlock(falseBB);
      // Continue emission on the false branch.
    }
      
    void noteResilientSuperclass() {}
    void noteStartOfImmediateMembers(ClassDecl *clazz) {}
  };
  
  LookUpNonoverriddenMethods(IGF, classDecl, method).layout();
  
  // Use the runtime to look up vtable entries.
  auto *result = IGF.Builder.CreateCall(getLookUpClassMethodFunctionPointer(),
                                        {metadata, method, description});
  IGF.Builder.CreateRet(result);
}
