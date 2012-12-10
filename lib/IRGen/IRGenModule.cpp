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
#include "llvm/DataLayout.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Intrinsics.h"
#include "llvm/Module.h"
#include "llvm/Type.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/SourceMgr.h"

#include "GenType.h"
#include "IRGenModule.h"
#include "Linking.h"

using namespace swift;
using namespace irgen;

IRGenModule::IRGenModule(ASTContext &Context,
			 Options &Opts, llvm::Module &Module,
                         const llvm::DataLayout &DataLayout)
  : Context(Context), Opts(Opts),
    Module(Module), LLVMContext(Module.getContext()),
    DataLayout(DataLayout), Types(*new TypeConverter(*this)) {
  VoidTy = llvm::Type::getVoidTy(getLLVMContext());
  Int1Ty = llvm::Type::getInt1Ty(getLLVMContext());
  Int8Ty = llvm::Type::getInt8Ty(getLLVMContext());
  Int16Ty = llvm::Type::getInt16Ty(getLLVMContext());
  Int32Ty = llvm::Type::getInt32Ty(getLLVMContext());
  Int64Ty = llvm::Type::getInt64Ty(getLLVMContext());
  Int8PtrTy = llvm::Type::getInt8PtrTy(getLLVMContext());
  Int8PtrPtrTy = Int8PtrTy->getPointerTo(0);
  SizeTy = DataLayout.getIntPtrType(getLLVMContext(), /*addrspace*/ 0);
  MemCpyFn = nullptr;
  AllocObjectFn = nullptr;
  RetainNoResultFn = nullptr;
  ReleaseFn = nullptr;
  DeallocObjectFn = nullptr;
  ObjCRetainFn = nullptr;
  ObjCReleaseFn = nullptr;
  RawAllocFn = nullptr;
  RawDeallocFn = nullptr;
  SlowAllocFn = nullptr;
  SlowRawDeallocFn = nullptr;

  const unsigned defaultAS = 0;

  RefCountedStructTy =
    llvm::StructType::create(getLLVMContext(), "swift.refcounted");
  RefCountedPtrTy = RefCountedStructTy->getPointerTo(/*addrspace*/ 0);
  RefCountedNull = llvm::ConstantPointerNull::get(RefCountedPtrTy);

  // A type metadata is the structure pointed to by the canonical
  // address point of a type metadata.  This is at least one word, and
  // potentially more than that, past the start of the actual global
  // structure.
  llvm::Type *typeMetadataElts[] = { MetadataKindTy };
  TypeMetadataStructTy =
    llvm::StructType::create(getLLVMContext(), typeMetadataElts,
                             "swift.type");
  TypeMetadataPtrTy = TypeMetadataStructTy->getPointerTo(defaultAS);

  // A full type metadata is basically just an adjustment to the
  // address point of a type metadata.  Resilience may cause
  // additional data to be laid out prior to this address point.
  llvm::Type *fullTypeMetadataElts[] = {
    WitnessTablePtrTy,
    TypeMetadataStructTy
  };
  FullTypeMetadataStructTy =
    llvm::StructType::create(getLLVMContext(), fullTypeMetadataElts,
                             "swift.full_type");
  FullTypeMetadataPtrTy = FullTypeMetadataStructTy->getPointerTo(defaultAS);

  // A metadata pattern is a structure from which generic type
  // metadata are allocated.  We leave this struct type intentionally
  // opaque, because the compiler basically never needs to access
  // anything from one.
  TypeMetadataPatternStructTy =
    llvm::StructType::create(getLLVMContext(), "swift.type_pattern");
  TypeMetadataPatternPtrTy =
    TypeMetadataPatternStructTy->getPointerTo(defaultAS);

  DtorTy = llvm::FunctionType::get(SizeTy, RefCountedPtrTy, false);
  llvm::Type *dtorPtrTy = DtorTy->getPointerTo();

  // A full heap metadata is basically just an additional small prefix
  // on a full metadata, used for metadata corresponding to heap
  // allocations.
  llvm::Type *fullHeapMetadataElts[] = {
    dtorPtrTy,
    WitnessTablePtrTy,
    TypeMetadataStructTy
  };
  FullHeapMetadataStructTy =
    llvm::StructType::create(getLLVMContext(), fullHeapMetadataElts,
                             "swift.full_heapmetadata");
  FullHeapMetadataPtrTy = FullHeapMetadataStructTy->getPointerTo(defaultAS);

  llvm::Type *refCountedElts[] = { TypeMetadataPtrTy, SizeTy };
  RefCountedStructTy->setBody(refCountedElts);

  PtrSize = Size(DataLayout.getPointerSize(defaultAS));

  llvm::Type *funcElts[] = { Int8PtrTy, RefCountedPtrTy };
  FunctionPairTy = llvm::StructType::get(LLVMContext, funcElts,
                                         /*packed*/ false);

  OpaquePtrTy = llvm::StructType::create(LLVMContext, "swift.opaque")
                  ->getPointerTo(defaultAS);

  FixedBufferTy = nullptr;
  for (unsigned i = 0; i != NumValueWitnessFunctions; ++i)
    ValueWitnessTys[i] = nullptr;

  ObjCPtrTy = llvm::StructType::create(getLLVMContext(), "objc_object")
                ->getPointerTo(defaultAS);

  // TODO: use "tinycc" on platforms that support it
  RuntimeCC = llvm::CallingConv::C;
}

IRGenModule::~IRGenModule() {
  delete &Types;
}

/// Create a function using swift's runtime calling convention.
static llvm::Constant *createRuntimeFunction(IRGenModule &IGM, StringRef name,
                                             llvm::FunctionType *fnType) {
  llvm::Constant *addr = IGM.Module.getOrInsertFunction(name, fnType);
  if (auto fn = dyn_cast<llvm::Function>(addr))
    fn->setCallingConv(IGM.RuntimeCC);
  return addr;
}

llvm::Constant *IRGenModule::getAllocObjectFn() {
  if (AllocObjectFn) return AllocObjectFn;

  llvm::Type *types[] = { TypeMetadataPtrTy, SizeTy, SizeTy };
  llvm::FunctionType *fnType =
    llvm::FunctionType::get(RefCountedPtrTy, types, false);
  AllocObjectFn = createRuntimeFunction(*this, "swift_allocObject", fnType);
  return AllocObjectFn;
}

llvm::Constant *IRGenModule::getRawAllocFn() {
  if (RawAllocFn) return RawAllocFn;

  /// void *swift_rawAlloc(SwiftAllocIndex index);
  llvm::FunctionType *fnType =
    llvm::FunctionType::get(Int8PtrTy, SizeTy, false);
  RawAllocFn = createRuntimeFunction(*this, "swift_rawAlloc", fnType);
  return RawAllocFn;
}

llvm::Constant *IRGenModule::getRawDeallocFn() {
  if (RawDeallocFn) return RawDeallocFn;

  /// void swift_rawDealloc(void *ptr, SwiftAllocIndex index);
  llvm::Type *types[] = { Int8PtrTy, SizeTy };
  llvm::FunctionType *fnType =
    llvm::FunctionType::get(VoidTy, types, false);
  RawDeallocFn = createRuntimeFunction(*this, "swift_rawDealloc", fnType);
  return RawDeallocFn;
}

llvm::Constant *IRGenModule::getSlowAllocFn() {
  if (SlowAllocFn) return SlowAllocFn;

  /// void *swift_slowAlloc(size_t size, size_t flags);
  llvm::Type *argTypes[] = { SizeTy, SizeTy };
  llvm::FunctionType *fnType =
    llvm::FunctionType::get(Int8PtrTy, argTypes, false);
  SlowAllocFn = createRuntimeFunction(*this, "swift_slowAlloc", fnType);
  return SlowAllocFn;
}

llvm::Constant *IRGenModule::getSlowRawDeallocFn() {
  if (SlowRawDeallocFn) return SlowRawDeallocFn;

  /// void swift_slowRawDealloc(void *ptr, size_t size);
  llvm::Type *types[] = { Int8PtrTy, SizeTy };
  llvm::FunctionType *fnType =
    llvm::FunctionType::get(VoidTy, types, false);
  SlowRawDeallocFn = createRuntimeFunction(*this, "swift_slowRawDealloc", fnType);
  return SlowRawDeallocFn;
}

llvm::Constant *IRGenModule::getRetainNoResultFn() {
  if (RetainNoResultFn) return RetainNoResultFn;

  // void swift_retainNoResult(void *ptr);
  auto fnType = llvm::FunctionType::get(VoidTy, RefCountedPtrTy, false);
  RetainNoResultFn = createRuntimeFunction(*this, "swift_retain_noresult", fnType);
  if (auto fn = dyn_cast<llvm::Function>(RetainNoResultFn))
    fn->setDoesNotCapture(1);
  return RetainNoResultFn;
}

llvm::Constant *IRGenModule::getReleaseFn() {
  if (ReleaseFn) return ReleaseFn;

  // void swift_release(void *ptr);
  auto fnType = llvm::FunctionType::get(VoidTy, RefCountedPtrTy, false);
  ReleaseFn = createRuntimeFunction(*this, "swift_release", fnType);
  if (auto fn = dyn_cast<llvm::Function>(ReleaseFn))
    fn->setDoesNotCapture(1);
  return ReleaseFn;
}

llvm::Constant *IRGenModule::getDeallocObjectFn() {
  if (DeallocObjectFn) return DeallocObjectFn;

  // void swift_deallocObject(void *ptr, size_t size);
  llvm::Type *argTypes[] = { RefCountedPtrTy, SizeTy };
  llvm::FunctionType *fnType =
    llvm::FunctionType::get(VoidTy, argTypes, false);
  DeallocObjectFn = createRuntimeFunction(*this, "swift_deallocObject", fnType);
  return DeallocObjectFn;
}

llvm::Constant *IRGenModule::getGetFunctionMetadataFn() {
  if (GetFunctionMetadataFn) return GetFunctionMetadataFn;

  // type_metadata_t *swift_getFunctionMetadata(type_metadata_t *arg,
  //                                            type_metadata_t *result);
  llvm::Type *argTypes[] = { TypeMetadataPtrTy, TypeMetadataPtrTy };
  llvm::FunctionType *fnType =
    llvm::FunctionType::get(TypeMetadataPtrTy, argTypes, false);
  GetFunctionMetadataFn =
    createRuntimeFunction(*this, "swift_getFunctionMetadata", fnType);
  return GetFunctionMetadataFn;
}

llvm::Constant *IRGenModule::getGetGenericMetadataFn() {
  if (GetGenericMetadataFn) return GetGenericMetadataFn;

  // type_metadata_t *swift_getGenericMetadata(type_metadata_pattern_t *pattern,
  //                                           const void *arguments);
  llvm::Type *argTypes[] = { TypeMetadataPatternPtrTy, Int8PtrTy };
  llvm::FunctionType *fnType =
    llvm::FunctionType::get(TypeMetadataPtrTy, argTypes, false);
  GetGenericMetadataFn =
    createRuntimeFunction(*this, "swift_getGenericMetadata", fnType);
  return GetGenericMetadataFn;
}

llvm::Constant *IRGenModule::getGetMetatypeMetadataFn() {
  if (GetMetatypeMetadataFn) return GetMetatypeMetadataFn;

  // type_metadata_t *swift_getMetatypeMetadata(type_metadata_t *instanceTy);
  llvm::Type *argTypes[] = { TypeMetadataPtrTy };
  llvm::FunctionType *fnType =
    llvm::FunctionType::get(TypeMetadataPtrTy, argTypes, false);
  GetMetatypeMetadataFn =
    createRuntimeFunction(*this, "swift_getMetatypeMetadata", fnType);
  return GetMetatypeMetadataFn;
}

llvm::Constant *IRGenModule::getGetObjCClassMetadataFn() {
  if (GetObjCClassMetadataFn) return GetObjCClassMetadataFn;

  // type_metadata_t *swift_getObjCClassMetadata(type_metadata_t *theClass);
  llvm::Type *argTypes[] = { TypeMetadataPtrTy };
  llvm::FunctionType *fnType =
    llvm::FunctionType::get(TypeMetadataPtrTy, argTypes, false);
  GetObjCClassMetadataFn =
    createRuntimeFunction(*this, "swift_getObjCClassMetadata", fnType);
  return GetObjCClassMetadataFn;
}

llvm::Constant *IRGenModule::getGetTupleMetadataFn() {
  if (GetTupleMetadataFn) return GetTupleMetadataFn;

  // type_metadata_t *swift_getTupleMetadata(size_t numElements,
  //                                         type_metadata_t * const *pattern,
  //                                         const char *labels,
  //                                         value_witness_table_t *proposed);
  llvm::Type *argTypes[] = {
    SizeTy,
    TypeMetadataPtrTy->getPointerTo(0),
    Int8PtrTy,
    WitnessTablePtrTy
  };
  llvm::FunctionType *fnType =
    llvm::FunctionType::get(TypeMetadataPtrTy, argTypes, false);
  GetTupleMetadataFn =
    createRuntimeFunction(*this, "swift_getTupleMetadata", fnType);
  return GetTupleMetadataFn;
}

void IRGenModule::unimplemented(SourceLoc loc, StringRef message) {
  Context.Diags.diagnose(loc, diag::irgen_unimplemented, message);
}

void IRGenModule::error(SourceLoc loc, const Twine &message) {
  SmallVector<char, 128> buffer;
  Context.Diags.diagnose(loc, diag::irgen_failure,
                         message.toStringRef(buffer));
}
