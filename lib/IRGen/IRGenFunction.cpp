//===--- IRGenFunction.cpp - Swift Per-Function IR Generation -------------===//
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
//  This file implements basic setup and teardown for the class which
//  performs IR generation for function bodies.
//
//===----------------------------------------------------------------------===//
#include "swift/ABI/MetadataValues.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/IRGen/Linking.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"

#include "Callee.h"
#include "Explosion.h"
#include "IRGenDebugInfo.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "LoadableTypeInfo.h"

using namespace swift;
using namespace irgen;

static llvm::cl::opt<bool> EnableTrapDebugInfo(
    "enable-trap-debug-info", llvm::cl::init(true), llvm::cl::Hidden,
    llvm::cl::desc("Generate failure-message functions in the debug info"));

IRGenFunction::IRGenFunction(IRGenModule &IGM, llvm::Function *Fn,
                             OptimizationMode OptMode,
                             const SILDebugScope *DbgScope,
                             Optional<SILLocation> DbgLoc)
    : IGM(IGM), Builder(IGM.getLLVMContext(),
                        IGM.DebugInfo && !IGM.Context.LangOpts.DebuggerSupport),
      OptMode(OptMode), CurFn(Fn), DbgScope(DbgScope) {

  // Make sure the instructions in this function are attached its debug scope.
  if (IGM.DebugInfo) {
    // Functions, especially artificial thunks and closures, are often
    // generated on-the-fly while we are in the middle of another
    // function. Be nice and preserve the current debug location until
    // after we're done with this function.
    IGM.DebugInfo->pushLoc();
  }

  emitPrologue();
}

IRGenFunction::~IRGenFunction() {
  emitEpilogue();

  // Restore the debug location.
  if (IGM.DebugInfo) IGM.DebugInfo->popLoc();

  // Tear down any side-table data structures.
  if (LocalTypeData) destroyLocalTypeData();
}

OptimizationMode IRGenFunction::getEffectiveOptimizationMode() const {
  if (OptMode != OptimizationMode::NotSet)
    return OptMode;

  return IGM.getOptions().OptMode;
}

ModuleDecl *IRGenFunction::getSwiftModule() const {
  return IGM.getSwiftModule();
}

SILModule &IRGenFunction::getSILModule() const {
  return IGM.getSILModule();
}

Lowering::TypeConverter &IRGenFunction::getSILTypes() const {
  return IGM.getSILTypes();
}

const IRGenOptions &IRGenFunction::getOptions() const {
  return IGM.getOptions();
}

// Returns the default atomicity of the module.
Atomicity IRGenFunction::getDefaultAtomicity() {
  return getSILModule().isDefaultAtomic() ? Atomicity::Atomic : Atomicity::NonAtomic;
}

/// Call the llvm.memcpy intrinsic.  The arguments need not already
/// be of i8* type.
void IRGenFunction::emitMemCpy(llvm::Value *dest, llvm::Value *src,
                               Size size, Alignment align) {
  emitMemCpy(dest, src, IGM.getSize(size), align);
}

void IRGenFunction::emitMemCpy(llvm::Value *dest, llvm::Value *src,
                               llvm::Value *size, Alignment align) {
  Builder.CreateMemCpy(dest, llvm::MaybeAlign(align.getValue()), src,
                       llvm::MaybeAlign(align.getValue()), size);
}

void IRGenFunction::emitMemCpy(Address dest, Address src, Size size) {
  emitMemCpy(dest, src, IGM.getSize(size));
}

void IRGenFunction::emitMemCpy(Address dest, Address src, llvm::Value *size) {
  // Map over to the inferior design of the LLVM intrinsic.
  emitMemCpy(dest.getAddress(), src.getAddress(), size,
             std::min(dest.getAlignment(), src.getAlignment()));
}

static llvm::Value *emitAllocatingCall(IRGenFunction &IGF,
                                       llvm::Constant *fn,
                                       ArrayRef<llvm::Value*> args,
                                       const llvm::Twine &name) {
  auto allocAttrs = IGF.IGM.getAllocAttrs();
  llvm::CallInst *call =
    IGF.Builder.CreateCall(fn, makeArrayRef(args.begin(), args.size()));
  call->setAttributes(allocAttrs);
  return call;
}

/// Emit a 'raw' allocation, which has no heap pointer and is
/// not guaranteed to be zero-initialized.
llvm::Value *IRGenFunction::emitAllocRawCall(llvm::Value *size,
                                             llvm::Value *alignMask,
                                             const llvm::Twine &name) {
  // For now, all we have is swift_slowAlloc.
  return emitAllocatingCall(*this, IGM.getSlowAllocFn(),
                            {size, alignMask},
                            name);
}

/// Emit a heap allocation.
llvm::Value *IRGenFunction::emitAllocObjectCall(llvm::Value *metadata,
                                                llvm::Value *size,
                                                llvm::Value *alignMask,
                                                const llvm::Twine &name) {
  // For now, all we have is swift_allocObject.
  return emitAllocatingCall(*this, IGM.getAllocObjectFn(),
                            { metadata, size, alignMask }, name);
}

llvm::Value *IRGenFunction::emitInitStackObjectCall(llvm::Value *metadata,
                                                    llvm::Value *object,
                                                    const llvm::Twine &name) {
  llvm::CallInst *call =
    Builder.CreateCall(IGM.getInitStackObjectFn(), { metadata, object }, name);
  call->setDoesNotThrow();
  return call;
}

llvm::Value *IRGenFunction::emitInitStaticObjectCall(llvm::Value *metadata,
                                                     llvm::Value *object,
                                                     const llvm::Twine &name) {
  llvm::CallInst *call =
    Builder.CreateCall(IGM.getInitStaticObjectFn(), { metadata, object }, name);
  call->setDoesNotThrow();
  return call;
}

llvm::Value *IRGenFunction::emitVerifyEndOfLifetimeCall(llvm::Value *object,
                                                      const llvm::Twine &name) {
  llvm::CallInst *call =
    Builder.CreateCall(IGM.getVerifyEndOfLifetimeFn(), { object }, name);
  call->setDoesNotThrow();
  return call;
}

void IRGenFunction::emitAllocBoxCall(llvm::Value *typeMetadata,
                                      llvm::Value *&box,
                                      llvm::Value *&valueAddress) {
  auto attrs = llvm::AttributeList::get(IGM.getLLVMContext(),
                                        llvm::AttributeList::FunctionIndex,
                                        llvm::Attribute::NoUnwind);

  llvm::CallInst *call =
    Builder.CreateCall(IGM.getAllocBoxFn(), typeMetadata);
  call->setAttributes(attrs);

  box = Builder.CreateExtractValue(call, 0);
  valueAddress = Builder.CreateExtractValue(call, 1);
}

void IRGenFunction::emitMakeBoxUniqueCall(llvm::Value *box,
                                          llvm::Value *typeMetadata,
                                          llvm::Value *alignMask,
                                          llvm::Value *&outBox,
                                          llvm::Value *&outValueAddress) {
  auto attrs = llvm::AttributeList::get(IGM.getLLVMContext(),
                                        llvm::AttributeList::FunctionIndex,
                                        llvm::Attribute::NoUnwind);

  llvm::CallInst *call = Builder.CreateCall(IGM.getMakeBoxUniqueFn(),
                                            {box, typeMetadata, alignMask});
  call->setAttributes(attrs);

  outBox = Builder.CreateExtractValue(call, 0);
  outValueAddress = Builder.CreateExtractValue(call, 1);
}


void IRGenFunction::emitDeallocBoxCall(llvm::Value *box,
                                        llvm::Value *typeMetadata) {
  auto attrs = llvm::AttributeList::get(IGM.getLLVMContext(),
                                        llvm::AttributeList::FunctionIndex,
                                        llvm::Attribute::NoUnwind);

  llvm::CallInst *call =
    Builder.CreateCall(IGM.getDeallocBoxFn(), box);
  call->setCallingConv(IGM.DefaultCC);
  call->setAttributes(attrs);
}

llvm::Value *IRGenFunction::emitProjectBoxCall(llvm::Value *box,
                                                llvm::Value *typeMetadata) {
  llvm::Attribute::AttrKind attrKinds[] = {
    llvm::Attribute::NoUnwind,
    llvm::Attribute::ReadNone,
  };
  auto attrs = llvm::AttributeList::get(
      IGM.getLLVMContext(), llvm::AttributeList::FunctionIndex, attrKinds);
  llvm::CallInst *call =
    Builder.CreateCall(IGM.getProjectBoxFn(), box);
  call->setCallingConv(IGM.DefaultCC);
  call->setAttributes(attrs);
  return call;
}

llvm::Value *IRGenFunction::emitAllocEmptyBoxCall() {
  auto attrs = llvm::AttributeList::get(IGM.getLLVMContext(),
                                        llvm::AttributeList::FunctionIndex,
                                        llvm::Attribute::NoUnwind);
  llvm::CallInst *call =
    Builder.CreateCall(IGM.getAllocEmptyBoxFn(), {});
  call->setCallingConv(IGM.DefaultCC);
  call->setAttributes(attrs);
  return call;
}

static void emitDeallocatingCall(IRGenFunction &IGF, llvm::Constant *fn,
                                 std::initializer_list<llvm::Value *> args) {
  auto cc = IGF.IGM.DefaultCC;
  if (auto fun = dyn_cast<llvm::Function>(fn))
    cc = fun->getCallingConv();


  llvm::CallInst *call =
    IGF.Builder.CreateCall(fn, makeArrayRef(args.begin(), args.size()));
  call->setCallingConv(cc);
  call->setDoesNotThrow();
}

/// Emit a 'raw' deallocation, which has no heap pointer and is not
/// guaranteed to be zero-initialized.
void IRGenFunction::emitDeallocRawCall(llvm::Value *pointer,
                                       llvm::Value *size,
                                       llvm::Value *alignMask) {
  // For now, all we have is swift_slowDealloc.
  return emitDeallocatingCall(*this, IGM.getSlowDeallocFn(),
                              {pointer, size, alignMask});
}

void IRGenFunction::emitTSanInoutAccessCall(llvm::Value *address) {
  llvm::Function *fn = cast<llvm::Function>(IGM.getTSanInoutAccessFn());

  llvm::Value *castAddress = Builder.CreateBitCast(address, IGM.Int8PtrTy);

  // Passing 0 as the caller PC causes compiler-rt to get our PC.
  llvm::Value *callerPC = llvm::ConstantPointerNull::get(IGM.Int8PtrTy);

  // A magic number agreed upon with compiler-rt to indicate a modifying
  // access.
  const unsigned kExternalTagSwiftModifyingAccess = 0x1;
  llvm::Value *tagValue =
      llvm::ConstantInt::get(IGM.SizeTy, kExternalTagSwiftModifyingAccess);
  llvm::Value *castTag = Builder.CreateIntToPtr(tagValue, IGM.Int8PtrTy);

  Builder.CreateCall(fn, {castAddress, callerPC, castTag});
}


/// Initialize a relative indirectable pointer to the given value.
/// This always leaves the value in the direct state; if it's not a
/// far reference, it's the caller's responsibility to ensure that the
/// pointer ranges are sufficient.
void IRGenFunction::emitStoreOfRelativeIndirectablePointer(llvm::Value *value,
                                                           Address addr,
                                                           bool isFar) {
  value = Builder.CreatePtrToInt(value, IGM.IntPtrTy);
  auto addrAsInt =
    Builder.CreatePtrToInt(addr.getAddress(), IGM.IntPtrTy);

  auto difference = Builder.CreateSub(value, addrAsInt);
  if (!isFar) {
    difference = Builder.CreateTrunc(difference, IGM.RelativeAddressTy);
  }

  Builder.CreateStore(difference, addr);
}

llvm::Value *
IRGenFunction::emitLoadOfRelativePointer(Address addr, bool isFar,
                                         llvm::PointerType *expectedType,
                                         const llvm::Twine &name) {
  llvm::Value *value = Builder.CreateLoad(addr);
  assert(value->getType() ==
         (isFar ? IGM.FarRelativeAddressTy : IGM.RelativeAddressTy));
  if (!isFar) {
    value = Builder.CreateSExt(value, IGM.IntPtrTy);
  }
  auto *addrInt = Builder.CreatePtrToInt(addr.getAddress(), IGM.IntPtrTy);
  auto *uncastPointerInt = Builder.CreateAdd(addrInt, value);
  auto *uncastPointer = Builder.CreateIntToPtr(uncastPointerInt, IGM.Int8PtrTy);
  auto uncastPointerAddress = Address(uncastPointer, IGM.getPointerAlignment());
  auto pointer = Builder.CreateBitCast(uncastPointerAddress, expectedType);
  return pointer.getAddress();
}

llvm::Value *
IRGenFunction::emitLoadOfRelativeIndirectablePointer(Address addr,
                                                bool isFar,
                                                llvm::PointerType *expectedType,
                                                const llvm::Twine &name) {
  // Load the pointer and turn it back into a pointer.
  llvm::Value *value = Builder.CreateLoad(addr);
  assert(value->getType() == (isFar ? IGM.FarRelativeAddressTy
                                    : IGM.RelativeAddressTy));
  if (!isFar) {
    value = Builder.CreateSExt(value, IGM.IntPtrTy);
  }
  assert(value->getType() == IGM.IntPtrTy);

  llvm::BasicBlock *origBB = Builder.GetInsertBlock();
  llvm::Value *directResult = Builder.CreateIntToPtr(value, expectedType);

  // Check whether the low bit is set.
  llvm::Constant *one = llvm::ConstantInt::get(IGM.IntPtrTy, 1);
  llvm::BasicBlock *indirectBB = createBasicBlock("relptr.indirect");
  llvm::BasicBlock *contBB = createBasicBlock("relptr.cont");
  llvm::Value *isIndirect = Builder.CreateAnd(value, one);
  isIndirect = Builder.CreateIsNotNull(isIndirect);
  Builder.CreateCondBr(isIndirect, indirectBB, contBB);

  // In the indirect block, clear the low bit and perform an additional load.
  llvm::Value *indirectResult; {
    Builder.emitBlock(indirectBB);

    // Clear the low bit.
    llvm::Value *ptr = Builder.CreateSub(value, one);
    ptr = Builder.CreateIntToPtr(ptr, expectedType->getPointerTo());

    // Load.
    Address indirectAddr(ptr, IGM.getPointerAlignment());
    indirectResult = Builder.CreateLoad(indirectAddr);

    Builder.CreateBr(contBB);
  }

  Builder.emitBlock(contBB);
  auto phi = Builder.CreatePHI(expectedType, 2, name);
  phi->addIncoming(directResult, origBB);
  phi->addIncoming(indirectResult, indirectBB);

  return phi;
}

void IRGenFunction::emitFakeExplosion(const TypeInfo &type,
                                      Explosion &explosion) {
  if (!isa<LoadableTypeInfo>(type)) {
    explosion.add(llvm::UndefValue::get(type.getStorageType()->getPointerTo()));
    return;
  }

  ExplosionSchema schema = cast<LoadableTypeInfo>(type).getSchema();
  for (auto &element : schema) {
    llvm::Type *elementType;
    if (element.isAggregate()) {
      elementType = element.getAggregateType()->getPointerTo();
    } else {
      elementType = element.getScalarType();
    }
    
    explosion.add(llvm::UndefValue::get(elementType));
  }
}

void IRGenFunction::unimplemented(SourceLoc Loc, StringRef Message) {
  return IGM.unimplemented(Loc, Message);
}

// Debug output for Explosions.

void Explosion::print(llvm::raw_ostream &OS) {
  for (auto value : makeArrayRef(Values).slice(NextValue)) {
    value->print(OS);
    OS << '\n';
  }
}

void Explosion::dump() {
  print(llvm::errs());
}

llvm::Value *Offset::getAsValue(IRGenFunction &IGF) const {
  if (isStatic()) {
    return IGF.IGM.getSize(getStatic());
  } else {
    return getDynamic();
  }
}

Offset Offset::offsetBy(IRGenFunction &IGF, Size other) const {
  if (isStatic()) {
    return Offset(getStatic() + other);
  }
  auto otherVal = llvm::ConstantInt::get(IGF.IGM.SizeTy, other.getValue());
  return Offset(IGF.Builder.CreateAdd(getDynamic(), otherVal));
}

Address IRGenFunction::emitAddressAtOffset(llvm::Value *base, Offset offset,
                                           llvm::Type *objectTy,
                                           Alignment objectAlignment,
                                           const llvm::Twine &name) {
  // Use a slightly more obvious IR pattern if it's a multiple of the type
  // size.  I'll confess that this is partly just to avoid updating tests.
  if (offset.isStatic()) {
    auto byteOffset = offset.getStatic();
    Size objectSize(IGM.DataLayout.getTypeAllocSize(objectTy));
    if (byteOffset.isMultipleOf(objectSize)) {
      // Cast to T*.
      auto objectPtrTy = objectTy->getPointerTo();
      base = Builder.CreateBitCast(base, objectPtrTy);

      // GEP to the slot, computing the index as a signed number.
      auto scaledIndex =
        int64_t(byteOffset.getValue()) / int64_t(objectSize.getValue());
      auto indexValue = IGM.getSize(Size(scaledIndex));
      auto slotPtr = Builder.CreateInBoundsGEP(base, indexValue);

      return Address(slotPtr, objectAlignment);
    }
  }

  // GEP to the slot.
  auto offsetValue = offset.getAsValue(*this);
  auto slotPtr = emitByteOffsetGEP(base, offsetValue, objectTy);
  return Address(slotPtr, objectAlignment);
}

llvm::CallInst *IRBuilder::CreateNonMergeableTrap(IRGenModule &IGM,
                                                  StringRef failureMsg) {
  if (IGM.IRGen.Opts.shouldOptimize()) {
    // Emit unique side-effecting inline asm calls in order to eliminate
    // the possibility that an LLVM optimization or code generation pass
    // will merge these blocks back together again. We emit an empty asm
    // string with the side-effect flag set, and with a unique integer
    // argument for each cond_fail we see in the function.
    llvm::IntegerType *asmArgTy = IGM.Int32Ty;
    llvm::Type *argTys = {asmArgTy};
    llvm::FunctionType *asmFnTy =
        llvm::FunctionType::get(IGM.VoidTy, argTys, false /* = isVarArg */);
    llvm::InlineAsm *inlineAsm =
        llvm::InlineAsm::get(asmFnTy, "", "n", true /* = SideEffects */);
    CreateAsmCall(inlineAsm,
                  llvm::ConstantInt::get(asmArgTy, NumTrapBarriers++));
  }

  // Emit the trap instruction.
  llvm::Function *trapIntrinsic =
      llvm::Intrinsic::getDeclaration(&IGM.Module, llvm::Intrinsic::trap);
  if (EnableTrapDebugInfo && IGM.DebugInfo && !failureMsg.empty()) {
    IGM.DebugInfo->addFailureMessageToCurrentLoc(*this, failureMsg);
  }
  auto Call = IRBuilderBase::CreateCall(trapIntrinsic, {});
  setCallingConvUsingCallee(Call);
  return Call;
}

void IRGenFunction::emitTrap(StringRef failureMessage, bool EmitUnreachable) {
  Builder.CreateNonMergeableTrap(IGM, failureMessage);
  if (EmitUnreachable)
    Builder.CreateUnreachable();
}

Address IRGenFunction::emitTaskAlloc(llvm::Value *size, Alignment alignment) {
  auto *call = Builder.CreateCall(IGM.getTaskAllocFn(), {getAsyncTask(), size});
  call->setDoesNotThrow();
  call->setCallingConv(IGM.SwiftCC);
  auto address = Address(call, alignment);
  return address;
}

void IRGenFunction::emitTaskDealloc(Address address) {
  auto *call = Builder.CreateCall(IGM.getTaskDeallocFn(),
                                  {getAsyncTask(), address.getAddress()});
  call->setDoesNotThrow();
  call->setCallingConv(IGM.SwiftCC);
}

llvm::Value *IRGenFunction::alignUpToMaximumAlignment(llvm::Type *sizeTy, llvm::Value *val) {
  auto *alignMask = llvm::ConstantInt::get(sizeTy, MaximumAlignment - 1);
  auto *invertedMask = Builder.CreateNot(alignMask);
  return Builder.CreateAnd(Builder.CreateAdd(val, alignMask), invertedMask);
}

/// Returns the current task \p currTask as a Builtin.RawUnsafeContinuation at +1.
static llvm::Value *unsafeContinuationFromTask(IRGenFunction &IGF,
                                               llvm::Value *currTask) {
  auto &IGM = IGF.IGM;
  auto &Builder = IGF.Builder;

  auto &rawPointerTI = IGM.getRawUnsafeContinuationTypeInfo();
  return Builder.CreateBitOrPointerCast(currTask, rawPointerTI.getStorageType());
}

void IRGenFunction::emitGetAsyncContinuation(SILType resumeTy,
                                             StackAddress resultAddr,
                                             Explosion &out) {
  // Create the continuation.
  // void current_sil_function(AsyncTask *currTask, Executor *currExecutor,
  //                           AsyncContext *currCtxt) {
  //
  // A continuation is the current AsyncTask 'currTask' with:
  //   currTask->ResumeTask = @llvm.coro.async.resume();
  //   currTask->ResumeContext = &continuation_context;
  //
  // Where:
  //
  // struct {
  //   AsyncContext *resumeCtxt;
  //   void *awaitSynchronization;
  //   SwiftError *errResult;
  //   Result *result;
  //   ExecutorRef *resumeExecutor;
  // } continuation_context; // local variable of current_sil_function
  //
  // continuation_context.resumeCtxt = currCtxt;
  // continuation_context.errResult = nulllptr;
  // continuation_context.result = ... // local alloca.
  // continuation_context.resumeExecutor = .. // current executor

  auto currTask = getAsyncTask();
  auto unsafeContinuation = unsafeContinuationFromTask(*this, currTask);

  // Create and setup the continuation context.
  // continuation_context.resumeCtxt = currCtxt;
  // continuation_context.errResult = nulllptr;
  // continuation_context.result = ... // local alloca T
  auto pointerAlignment = IGM.getPointerAlignment();
  auto continuationContext =
      createAlloca(IGM.AsyncContinuationContextTy, pointerAlignment);
  AsyncCoroutineCurrentContinuationContext = continuationContext.getAddress();
  // TODO: add lifetime with matching lifetime in await_async_continuation
  auto contResumeAddr =
      Builder.CreateStructGEP(continuationContext.getAddress(), 0);
  Builder.CreateStore(getAsyncContext(),
                      Address(contResumeAddr, pointerAlignment));
  auto contErrResultAddr =
      Builder.CreateStructGEP(continuationContext.getAddress(), 2);
  Builder.CreateStore(
      llvm::Constant::getNullValue(
          contErrResultAddr->getType()->getPointerElementType()),
      Address(contErrResultAddr, pointerAlignment));
  auto contResultAddr =
      Builder.CreateStructGEP(continuationContext.getAddress(), 3);
  if (!resultAddr.getAddress().isValid()) {
    auto &resumeTI = getTypeInfo(resumeTy);
    auto resultAddr =
        resumeTI.allocateStack(*this, resumeTy, "async.continuation.result");
    Builder.CreateStore(Builder.CreateBitOrPointerCast(
                            resultAddr.getAddress().getAddress(),
                            contResultAddr->getType()->getPointerElementType()),
                        Address(contResultAddr, pointerAlignment));
  } else {
    Builder.CreateStore(Builder.CreateBitOrPointerCast(
                            resultAddr.getAddress().getAddress(),
                            contResultAddr->getType()->getPointerElementType()),
                        Address(contResultAddr, pointerAlignment));
  }
  // continuation_context.resumeExecutor = // current executor
  auto contExecutorRefAddr =
      Builder.CreateStructGEP(continuationContext.getAddress(), 4);
  Builder.CreateStore(
      Builder.CreateBitOrPointerCast(
          getAsyncExecutor(),
          contExecutorRefAddr->getType()->getPointerElementType()),
      Address(contExecutorRefAddr, pointerAlignment));

  // Fill the current task (i.e the continuation) with the continuation
  // information.
  // currTask->ResumeTask = @llvm.coro.async.resume();
  assert(currTask->getType() == IGM.SwiftTaskPtrTy);
  auto currTaskResumeTaskAddr = Builder.CreateStructGEP(currTask, 4);
  auto coroResume =
      Builder.CreateIntrinsicCall(llvm::Intrinsic::coro_async_resume, {});

  assert(AsyncCoroutineCurrentResume == nullptr &&
         "Don't support nested get_async_continuation");
  AsyncCoroutineCurrentResume = coroResume;
  Builder.CreateStore(
      Builder.CreateBitOrPointerCast(coroResume, IGM.FunctionPtrTy),
      Address(currTaskResumeTaskAddr, pointerAlignment));
  // currTask->ResumeContext = &continuation_context;
  auto currTaskResumeCtxtAddr = Builder.CreateStructGEP(currTask, 5);
  Builder.CreateStore(
      Builder.CreateBitOrPointerCast(continuationContext.getAddress(),
                                     IGM.SwiftContextPtrTy),
      Address(currTaskResumeCtxtAddr, pointerAlignment));

  // Publish all the writes.
  // continuation_context.awaitSynchronization =(atomic release) nullptr;
  auto contAwaitSyncAddr =
      Builder.CreateStructGEP(continuationContext.getAddress(), 1);
  auto null = llvm::ConstantInt::get(
      contAwaitSyncAddr->getType()->getPointerElementType(), 0);
  auto atomicStore =
      Builder.CreateStore(null, Address(contAwaitSyncAddr, pointerAlignment));
  atomicStore->setAtomic(llvm::AtomicOrdering::Release,
                         llvm::SyncScope::System);
  out.add(unsafeContinuation);
}

void IRGenFunction::emitAwaitAsyncContinuation(
    SILType resumeTy, bool isIndirectResult,
    Explosion &outDirectResult, llvm::BasicBlock *&normalBB,
    llvm::PHINode *&optionalErrorResult, llvm::BasicBlock *&optionalErrorBB) {
  assert(AsyncCoroutineCurrentContinuationContext && "no active continuation");
  auto pointerAlignment = IGM.getPointerAlignment();

  // First check whether the await reached this point first. Meaning we still
  // have to wait for the continuation result. If the await reaches first we
  // abort the control flow here (resuming the continuation will execute the
  // remaining control flow).
  auto contAwaitSyncAddr =
      Builder.CreateStructGEP(AsyncCoroutineCurrentContinuationContext, 1);
  auto null = llvm::ConstantInt::get(
      contAwaitSyncAddr->getType()->getPointerElementType(), 0);
  auto one = llvm::ConstantInt::get(
      contAwaitSyncAddr->getType()->getPointerElementType(), 1);
  auto results = Builder.CreateAtomicCmpXchg(
      contAwaitSyncAddr, null, one,
      llvm::AtomicOrdering::Release /*success ordering*/,
      llvm::AtomicOrdering::Acquire /* failure ordering */,
      llvm::SyncScope::System);
  auto firstAtAwait = Builder.CreateExtractValue(results, 1);
  auto contBB = createBasicBlock("await.async.maybe.resume");
  auto abortBB = createBasicBlock("await.async.abort");
  Builder.CreateCondBr(firstAtAwait, abortBB, contBB);
  Builder.emitBlock(abortBB);
  {
    // We are first to the sync point. Abort. The continuation's result is not
    // available yet.
    emitCoroutineOrAsyncExit();
  }

  auto contBB2 = createBasicBlock("await.async.resume");
  Builder.emitBlock(contBB);
  {
    // Setup the suspend point.
    SmallVector<llvm::Value *, 8> arguments;
    arguments.push_back(AsyncCoroutineCurrentResume);
    auto resumeProjFn = getOrCreateResumePrjFn();
    arguments.push_back(
        Builder.CreateBitOrPointerCast(resumeProjFn, IGM.Int8PtrTy));
    // The dispatch function just calls the resume point.
    auto resumeFnPtr =
        getFunctionPointerForResumeIntrinsic(AsyncCoroutineCurrentResume);
    arguments.push_back(Builder.CreateBitOrPointerCast(
        createAsyncDispatchFn(resumeFnPtr,
                              {IGM.Int8PtrTy, IGM.Int8PtrTy, IGM.Int8PtrTy}),
        IGM.Int8PtrTy));
    arguments.push_back(AsyncCoroutineCurrentResume);
    arguments.push_back(
        Builder.CreateBitOrPointerCast(getAsyncTask(), IGM.Int8PtrTy));
    arguments.push_back(
        Builder.CreateBitOrPointerCast(getAsyncExecutor(), IGM.Int8PtrTy));
    arguments.push_back(Builder.CreateBitOrPointerCast(
        AsyncCoroutineCurrentContinuationContext, IGM.Int8PtrTy));
    emitSuspendAsyncCall(arguments);

    auto results = Builder.CreateAtomicCmpXchg(
        contAwaitSyncAddr, null, one,
        llvm::AtomicOrdering::Release /*success ordering*/,
        llvm::AtomicOrdering::Acquire /* failure ordering */,
        llvm::SyncScope::System);
    // Again, are we first at the wait (can only reach that state after
    // continuation.resume/abort is called)? If so abort to wait for the end of
    // the await point to be reached.
    auto firstAtAwait = Builder.CreateExtractValue(results, 1);
    Builder.CreateCondBr(firstAtAwait, abortBB, contBB2);
  }

  Builder.emitBlock(contBB2);
  auto contBB3 = createBasicBlock("await.async.normal");
  if (optionalErrorBB) {
    auto contErrResultAddr = Address(
        Builder.CreateStructGEP(AsyncCoroutineCurrentContinuationContext, 2),
        pointerAlignment);
    auto errorRes = Builder.CreateLoad(contErrResultAddr);
    auto nullError = llvm::Constant::getNullValue(errorRes->getType());
    auto hasError = Builder.CreateICmpNE(errorRes, nullError);
    optionalErrorResult->addIncoming(errorRes, Builder.GetInsertBlock());
    Builder.CreateCondBr(hasError, optionalErrorBB, contBB3);
  } else {
    Builder.CreateBr(contBB3);
  }

  Builder.emitBlock(contBB3);
  if (!isIndirectResult) {
    auto contResultAddrAddr =
        Builder.CreateStructGEP(AsyncCoroutineCurrentContinuationContext, 3);
    auto resultAddrVal =
        Builder.CreateLoad(Address(contResultAddrAddr, pointerAlignment));
    // Take the result.
    auto &resumeTI = cast<LoadableTypeInfo>(getTypeInfo(resumeTy));
    auto resultStorageTy = resumeTI.getStorageType();
    auto resultAddr =
        Address(Builder.CreateBitOrPointerCast(resultAddrVal,
                                               resultStorageTy->getPointerTo()),
                resumeTI.getFixedAlignment());
    resumeTI.loadAsTake(*this, resultAddr, outDirectResult);
  }
  Builder.CreateBr(normalBB);
  AsyncCoroutineCurrentResume = nullptr;
  AsyncCoroutineCurrentContinuationContext = nullptr;
}
