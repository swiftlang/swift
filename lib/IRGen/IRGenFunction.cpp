//===--- IRGenFunction.cpp - Swift Per-Function IR Generation -------------===//
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
//
//  This file implements basic setup and teardown for the class which
//  performs IR generation for function bodies.
//
//===----------------------------------------------------------------------===//

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

IRGenFunction::IRGenFunction(IRGenModule &IGM,
                             llvm::Function *Fn,
                             SILDebugScope *DbgScope,
                             Optional<SILLocation> DbgLoc)
  : IGM(IGM), Builder(IGM.getLLVMContext()),
    CurFn(Fn),  DbgScope(DbgScope)
  {

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
  call->setCallingConv(IGF.IGM.RuntimeCC);
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

void IRGenFunction::emitAllocBoxCall(llvm::Value *typeMetadata,
                                     llvm::Value *&box,
                                     llvm::Value *&valueAddress) {
  auto attrs = llvm::AttributeSet::get(IGM.LLVMContext,
                                       llvm::AttributeSet::FunctionIndex,
                                       llvm::Attribute::NoUnwind);
  
  llvm::CallInst *call =
    Builder.CreateCall(IGM.getAllocBoxFn(), typeMetadata);
  call->setCallingConv(IGM.RuntimeCC);
  call->setAttributes(attrs);

  box = Builder.CreateExtractValue(call, 0);
  valueAddress = Builder.CreateExtractValue(call, 1);
}

void IRGenFunction::emitAllocBox2Call(llvm::Value *typeMetadata,
                                      llvm::Value *&box,
                                      llvm::Value *&valueAddress) {
  auto attrs = llvm::AttributeSet::get(IGM.LLVMContext,
                                       llvm::AttributeSet::FunctionIndex,
                                       llvm::Attribute::NoUnwind);
  
  llvm::CallInst *call =
    Builder.CreateCall(IGM.getAllocBox2Fn(), typeMetadata);
  call->setCallingConv(IGM.RuntimeCC);
  call->setAttributes(attrs);

  box = Builder.CreateExtractValue(call, 0);
  valueAddress = Builder.CreateExtractValue(call, 1);
}

void IRGenFunction::emitDeallocBoxCall(llvm::Value *box,
                                       llvm::Value *typeMetadata) {
  auto attrs = llvm::AttributeSet::get(IGM.LLVMContext,
                                       llvm::AttributeSet::FunctionIndex,
                                       llvm::Attribute::NoUnwind);

  llvm::CallInst *call =
    Builder.CreateCall(IGM.getDeallocBoxFn(), {box, typeMetadata});
  call->setCallingConv(IGM.RuntimeCC);
  call->setAttributes(attrs);
}

void IRGenFunction::emitDeallocBox2Call(llvm::Value *box,
                                        llvm::Value *typeMetadata) {
  auto attrs = llvm::AttributeSet::get(IGM.LLVMContext,
                                       llvm::AttributeSet::FunctionIndex,
                                       llvm::Attribute::NoUnwind);

  llvm::CallInst *call =
    Builder.CreateCall(IGM.getDeallocBox2Fn(), {box, typeMetadata});
  call->setCallingConv(IGM.RuntimeCC);
  call->setAttributes(attrs);
}

llvm::Value *IRGenFunction::emitProjectBox2Call(llvm::Value *box,
                                                llvm::Value *typeMetadata) {
  llvm::Attribute::AttrKind attrKinds[] = {
    llvm::Attribute::NoUnwind,
    llvm::Attribute::ReadNone,
  };
  auto attrs = llvm::AttributeSet::get(IGM.LLVMContext,
                                       llvm::AttributeSet::FunctionIndex,
                                       attrKinds);
  llvm::CallInst *call =
    Builder.CreateCall(IGM.getProjectBox2Fn(), {box, typeMetadata});
  call->setCallingConv(IGM.RuntimeCC);
  call->setAttributes(attrs);
  return call;
}

static void emitDeallocatingCall(IRGenFunction &IGF, llvm::Constant *fn,
                                 std::initializer_list<llvm::Value *> args) {
  llvm::CallInst *call =
    IGF.Builder.CreateCall(fn, makeArrayRef(args.begin(), args.size()));
  call->setCallingConv(IGF.IGM.RuntimeCC);
  call->setDoesNotThrow();
}

/// Emit a 'raw' deallocation, which has no heap pointer and is not
/// guaranteed to be zero-initialized.
void IRGenFunction::emitDeallocRawCall(llvm::Value *pointer,
                                       llvm::Value *size,
                                       llvm::Value *alignMask) {
  // For now, all we have is swift_slowDelloc.
  return emitDeallocatingCall(*this, IGM.getSlowDeallocFn(),
                              {pointer, size, alignMask});
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

llvm::Value *IRGenFunction::lookupTypeDataMap(CanType type, LocalTypeData index,
                                              const TypeDataMap &scopedMap) {
  
  // First try to lookup in the unscoped cache (= definitions in the entry block
  // of the function).
  auto key = getLocalTypeDataKey(type, index);
  auto it = LocalTypeDataMap.find(key);
  if (it != LocalTypeDataMap.end())
    return it->second;
  
  // Now try to lookup in the scoped cache.
  auto it2 = scopedMap.find(key);
  if (it2 == scopedMap.end())
    return nullptr;
  
  if (auto *I = dyn_cast<llvm::Instruction>(it2->second)) {
    // This is a very very simple dominance check: either the definition is in the
    // entry block or in the current block.
    // TODO: do a better dominance check.
    if (I->getParent() == &CurFn->getEntryBlock() ||
        I->getParent() == Builder.GetInsertBlock()) {
      return I;
    }
    return nullptr;
  }
  
  if (isa<llvm::Constant>(it2->second)) {
    return it2->second;
  }
  
  // TODO: other kinds of value?
  return nullptr;
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
