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

#include "swift/AST/IRGenOptions.h"
#include "swift/Basic/SourceLoc.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/raw_ostream.h"

#include "Explosion.h"
#include "IRGenDebugInfo.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Linking.h"
#include "LoadableTypeInfo.h"

using namespace swift;
using namespace irgen;

IRGenFunction::IRGenFunction(IRGenModule &IGM, llvm::Function *Fn,
                             const SILDebugScope *DbgScope,
                             Optional<SILLocation> DbgLoc)
    : IGM(IGM), Builder(IGM.getLLVMContext(),
                        IGM.DebugInfo && !IGM.Context.LangOpts.DebuggerSupport),
      CurFn(Fn), DbgScope(DbgScope) {

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

ModuleDecl *IRGenFunction::getSwiftModule() const {
  return IGM.getSwiftModule();
}

SILModule &IRGenFunction::getSILModule() const {
  return IGM.getSILModule();
}

Lowering::TypeConverter &IRGenFunction::getSILTypes() const {
  return IGM.getSILTypes();
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
  Builder.CreateMemCpy(dest, src, size, align.getValue(), false);
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
                                       llvm::Value *fn,
                                       std::initializer_list<llvm::Value*> args,
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
  auto attrs = llvm::AttributeList::get(IGM.LLVMContext,
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
  auto attrs = llvm::AttributeSet::get(IGM.LLVMContext,
                                       llvm::AttributeSet::FunctionIndex,
                                       llvm::Attribute::NoUnwind);

  llvm::CallInst *call = Builder.CreateCall(IGM.getMakeBoxUniqueFn(),
                                            {box, typeMetadata, alignMask});
  call->setAttributes(attrs);

  outBox = Builder.CreateExtractValue(call, 0);
  outValueAddress = Builder.CreateExtractValue(call, 1);
}


void IRGenFunction::emitDeallocBoxCall(llvm::Value *box,
                                        llvm::Value *typeMetadata) {
  auto attrs = llvm::AttributeList::get(IGM.LLVMContext,
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
      IGM.LLVMContext, llvm::AttributeList::FunctionIndex, attrKinds);
  llvm::CallInst *call =
    Builder.CreateCall(IGM.getProjectBoxFn(), box);
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
  Builder.CreateCall(fn, {castAddress});
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
