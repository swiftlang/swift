//===--- IRGenModule.cpp - Swift Global LLVM IR Generation ----------------===//
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
//  This file implements IR generation for global declarations in Swift.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/Module.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Diagnostics.h"
#include "llvm/Constants.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Intrinsics.h"
#include "llvm/Module.h"
#include "llvm/Type.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Target/TargetData.h"

#include "GenType.h"
#include "IRGenModule.h"
#include "Linking.h"

using namespace swift;
using namespace irgen;

IRGenModule::IRGenModule(ASTContext &Context,
			 Options &Opts, llvm::Module &Module,
                         const llvm::TargetData &TargetData)
  : Context(Context), Opts(Opts),
    Module(Module), LLVMContext(Module.getContext()),
    TargetData(TargetData), Types(*new TypeConverter()) {
  VoidTy = llvm::Type::getVoidTy(getLLVMContext());
  Int1Ty = llvm::Type::getInt1Ty(getLLVMContext());
  Int8Ty = llvm::Type::getInt8Ty(getLLVMContext());
  Int16Ty = llvm::Type::getInt16Ty(getLLVMContext());
  Int32Ty = llvm::Type::getInt32Ty(getLLVMContext());
  Int64Ty = llvm::Type::getInt64Ty(getLLVMContext());
  Int8PtrTy = llvm::Type::getInt8PtrTy(getLLVMContext());
  SizeTy = TargetData.getIntPtrType(getLLVMContext());
  MemCpyFn = nullptr;
  AllocFn = nullptr;
  AllocRawFn = nullptr;
  RetainFn = nullptr;
  ReleaseFn = nullptr;
  DeallocFn = nullptr;
  DeallocRawFn = nullptr;

  RefCountedStructTy =
    llvm::StructType::create(getLLVMContext(), "swift.refcounted");
  RefCountedPtrTy = RefCountedStructTy->getPointerTo(/*addrspace*/ 0);
  RefCountedNull = llvm::ConstantPointerNull::get(RefCountedPtrTy);

  DtorTy = llvm::FunctionType::get(SizeTy, RefCountedPtrTy, false);
  llvm::Type *dtorPtrTy = DtorTy->getPointerTo();
  llvm::Type *heapMetadataElts[] = { dtorPtrTy, dtorPtrTy };
  HeapMetadataStructTy =
    llvm::StructType::create(getLLVMContext(), heapMetadataElts,
                             "swift.heapmetadata");
  HeapMetadataPtrTy = HeapMetadataStructTy->getPointerTo(/*addrspace*/ 0);

  llvm::Type *refCountedElts[] = { HeapMetadataPtrTy, SizeTy };
  RefCountedStructTy->setBody(refCountedElts);

  PtrSize = Size(TargetData.getPointerSize());

  llvm::Type *funcElts[] = { Int8PtrTy, RefCountedPtrTy };
  FunctionPairTy = llvm::StructType::get(LLVMContext, funcElts,
                                         /*packed*/ false);

  OpaqueStructTy = nullptr;
  FixedBufferTy = nullptr;
  for (unsigned i = 0; i != NumValueWitnesses; ++i)
    ValueWitnessTys[i] = nullptr;
}

IRGenModule::~IRGenModule() {
  delete &Types;
}

llvm::StructType *IRGenModule::getOpaqueStructTy() {
  if (OpaqueStructTy) return OpaqueStructTy;
  OpaqueStructTy = llvm::StructType::create(LLVMContext);
  return OpaqueStructTy;
}

llvm::Constant *IRGenModule::getAllocFn() {
  if (AllocFn) return AllocFn;

  llvm::Type *types[] = { HeapMetadataPtrTy, SizeTy, SizeTy };
  llvm::FunctionType *fnType =
    llvm::FunctionType::get(RefCountedPtrTy, types, false);
  AllocFn = Module.getOrInsertFunction("swift_alloc", fnType);
  return AllocFn;
}

llvm::Constant *IRGenModule::getAllocRawFn() {
  if (AllocRawFn) return AllocRawFn;

  llvm::Type *types[] = { SizeTy, SizeTy };
  llvm::FunctionType *fnType =
    llvm::FunctionType::get(Int8PtrTy, types, false);
  AllocRawFn = Module.getOrInsertFunction("swift_allocRaw", fnType);
  return AllocRawFn;
}

llvm::Constant *IRGenModule::getDeallocRawFn() {
  if (DeallocRawFn) return DeallocRawFn;

  llvm::Type *types[] = { Int8PtrTy, SizeTy };
  llvm::FunctionType *fnType =
    llvm::FunctionType::get(VoidTy, types, false);
  DeallocRawFn = Module.getOrInsertFunction("swift_deallocRaw", fnType);
  return DeallocRawFn;
}

llvm::Constant *IRGenModule::getRetainFn() {
  if (RetainFn) return RetainFn;

  llvm::FunctionType *fnType =
    llvm::FunctionType::get(RefCountedPtrTy, RefCountedPtrTy, false);
  RetainFn = Module.getOrInsertFunction("swift_retain", fnType);
  return RetainFn;
}

llvm::Constant *IRGenModule::getReleaseFn() {
  if (ReleaseFn) return ReleaseFn;

  llvm::FunctionType *fnType =
    llvm::FunctionType::get(VoidTy, RefCountedPtrTy, false);
  ReleaseFn = Module.getOrInsertFunction("swift_release", fnType);
  return ReleaseFn;
}

llvm::Constant *IRGenModule::getDeallocFn() {
  if (DeallocFn) return DeallocFn;

  llvm::FunctionType *fnType =
    llvm::FunctionType::get(VoidTy, RefCountedPtrTy, false);
  DeallocFn = Module.getOrInsertFunction("swift_dealloc", fnType);
  return DeallocFn;
}

void IRGenModule::unimplemented(SourceLoc Loc, StringRef Message) {
  Context.Diags.diagnose(Loc, diag::irgen_unimplemented, Message);
}
