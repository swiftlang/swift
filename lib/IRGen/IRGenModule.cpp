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
#include "swift/AST/Diagnostics.h"
#include "swift/IRGen/Options.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/SourceMgr.h"

#include "GenType.h"
#include "IRGenModule.h"
#include "Linking.h"

#include <initializer_list>

using namespace swift;
using namespace irgen;

const unsigned DefaultAS = 0;

/// A helper for creating LLVM struct types.
static llvm::StructType *createStructType(IRGenModule &IGM,
                                          StringRef name,
                                  std::initializer_list<llvm::Type*> types) {
  return llvm::StructType::create(IGM.getLLVMContext(),
                                  ArrayRef<llvm::Type*>(types.begin(),
                                                        types.size()),
                                  name);
};

/// A helper for creating pointer-to-struct types.
static llvm::PointerType *createStructPointerType(IRGenModule &IGM,
                                                  StringRef name,
                                  std::initializer_list<llvm::Type*> types) {
  return createStructType(IGM, name, types)->getPointerTo(DefaultAS);
};

IRGenModule::IRGenModule(ASTContext &Context,
                         Options &Opts, llvm::Module &Module,
                         const llvm::DataLayout &DataLayout,
                         SILModule *SILMod)
  : Context(Context), Opts(Opts),
    Module(Module), LLVMContext(Module.getContext()),
    DataLayout(DataLayout), SILMod(SILMod),
    Types(*new TypeConverter(*this)) {
  VoidTy = llvm::Type::getVoidTy(getLLVMContext());
  Int1Ty = llvm::Type::getInt1Ty(getLLVMContext());
  Int8Ty = llvm::Type::getInt8Ty(getLLVMContext());
  Int16Ty = llvm::Type::getInt16Ty(getLLVMContext());
  Int32Ty = llvm::Type::getInt32Ty(getLLVMContext());
  Int64Ty = llvm::Type::getInt64Ty(getLLVMContext());
  Int8PtrTy = llvm::Type::getInt8PtrTy(getLLVMContext());
  Int8PtrPtrTy = Int8PtrTy->getPointerTo(0);
  SizeTy = DataLayout.getIntPtrType(getLLVMContext(), /*addrspace*/ 0);

  RefCountedStructTy =
    llvm::StructType::create(getLLVMContext(), "swift.refcounted");
  RefCountedPtrTy = RefCountedStructTy->getPointerTo(/*addrspace*/ 0);
  RefCountedNull = llvm::ConstantPointerNull::get(RefCountedPtrTy);

  // A type metadata is the structure pointed to by the canonical
  // address point of a type metadata.  This is at least one word, and
  // potentially more than that, past the start of the actual global
  // structure.
  TypeMetadataStructTy = createStructType(*this, "swift.type", {
    MetadataKindTy          // MetadataKind Kind;
  });
  TypeMetadataPtrTy = TypeMetadataStructTy->getPointerTo(DefaultAS);

  // A tuple type metadata has a couple extra fields.
  auto tupleElementTy = createStructType(*this, "swift.tuple_element_type", {
    TypeMetadataPtrTy,      // Metadata *Type;
    SizeTy                  // size_t Offset;
  });
  TupleTypeMetadataPtrTy = createStructPointerType(*this, "swift.tuple_type", {
    TypeMetadataStructTy,   // (base)
    SizeTy,                 // size_t NumElements;
    Int8PtrTy,              // const char *Labels;
    llvm::ArrayType::get(tupleElementTy, 0) // Element Elements[];
  });

  // A full type metadata is basically just an adjustment to the
  // address point of a type metadata.  Resilience may cause
  // additional data to be laid out prior to this address point.
  FullTypeMetadataStructTy = createStructType(*this, "swift.full_type", {
    WitnessTablePtrTy,
    TypeMetadataStructTy
  });
  FullTypeMetadataPtrTy = FullTypeMetadataStructTy->getPointerTo(DefaultAS);

  // A metadata pattern is a structure from which generic type
  // metadata are allocated.  We leave this struct type intentionally
  // opaque, because the compiler basically never needs to access
  // anything from one.
  TypeMetadataPatternStructTy =
    llvm::StructType::create(getLLVMContext(), "swift.type_pattern");
  TypeMetadataPatternPtrTy =
    TypeMetadataPatternStructTy->getPointerTo(DefaultAS);

  DeallocatingDtorTy = llvm::FunctionType::get(VoidTy, RefCountedPtrTy, false);
  llvm::Type *dtorPtrTy = DeallocatingDtorTy->getPointerTo();

  // A full heap metadata is basically just an additional small prefix
  // on a full metadata, used for metadata corresponding to heap
  // allocations.
  FullHeapMetadataStructTy =
                  createStructType(*this, "swift.full_heapmetadata", {
    dtorPtrTy,
    WitnessTablePtrTy,
    TypeMetadataStructTy
  });
  FullHeapMetadataPtrTy = FullHeapMetadataStructTy->getPointerTo(DefaultAS);

  llvm::Type *refCountedElts[] = { TypeMetadataPtrTy, SizeTy };
  RefCountedStructTy->setBody(refCountedElts);

  PtrSize = Size(DataLayout.getPointerSize(DefaultAS));

  FunctionPairTy = createStructType(*this, "swift.function", {
    Int8PtrTy,
    RefCountedPtrTy
  });
  OpaquePtrTy = llvm::StructType::create(LLVMContext, "swift.opaque")
                  ->getPointerTo(DefaultAS);

  FixedBufferTy = nullptr;
  for (unsigned i = 0; i != NumValueWitnessFunctions; ++i)
    ValueWitnessTys[i] = nullptr;

  ObjCPtrTy = llvm::StructType::create(getLLVMContext(), "objc_object")
                ->getPointerTo(DefaultAS);

  ObjCClassStructTy = llvm::StructType::create(LLVMContext, "objc_class");
  ObjCClassPtrTy = ObjCClassStructTy->getPointerTo(DefaultAS);
  llvm::Type *objcClassElts[] = {
    ObjCClassPtrTy,
    ObjCClassPtrTy,
    OpaquePtrTy,
    OpaquePtrTy,
    IntPtrTy
  };
  ObjCClassStructTy->setBody(objcClassElts);

  ObjCSuperStructTy = llvm::StructType::create(LLVMContext, "objc_super");
  ObjCSuperPtrTy = ObjCSuperStructTy->getPointerTo(DefaultAS);
  llvm::Type *objcSuperElts[] = {
    ObjCPtrTy,
    ObjCClassPtrTy
  };
  ObjCSuperStructTy->setBody(objcSuperElts);
      
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

/// Create a readonly runtime function.
///
/// 'readonly' permits calls to this function to be removed, GVN'ed,
/// and re-ordered, but not necessarily around writes to memory.  It
/// is a promise that the function has no side effects.  We actually
/// apply this attribute to functions that do have side effects, but
/// those side effects are things like allocating a cache entry: that
/// is, they are not visible outside of the abstraction of the
/// function (except by e.g. monitoring memory usage).  This is
/// permitted, as it does not affect the validity of transformations.
static llvm::Constant *createReadonlyRuntimeFunction(IRGenModule &IGM,
                                                     StringRef name,
                                               llvm::FunctionType *fnType) {
  llvm::Constant *addr = createRuntimeFunction(IGM, name, fnType);
  if (auto fn = dyn_cast<llvm::Function>(addr)) {
    fn->setOnlyReadsMemory();
  }
  return addr;
}

/// Create a readnone runtime function.
///
/// 'readnone' is a stronger version of 'readonly'; it permits calls
/// to this function to be removed, GVN'ed, and re-ordered regardless
/// of any intervening writes to memory.  It is an additional promise
/// that the function does not depend on the current state of memory.
/// We actually apply this attribute to functions that do depend on
/// the current state of memory, but only when that memory is known to
/// be immutable.  This is permitted, as it does not affect the
/// validity of transformations.
///
/// Note that functions like swift_getTupleMetadata which read values
/// out of a local array cannot be marked 'readnone'.
static llvm::Constant *createReadnoneRuntimeFunction(IRGenModule &IGM,
                                                     StringRef name,
                                               llvm::FunctionType *fnType) {
  llvm::Constant *addr = createRuntimeFunction(IGM, name, fnType);
  if (auto fn = dyn_cast<llvm::Function>(addr)) {
    fn->setDoesNotAccessMemory();
  }
  return addr;
}

static llvm::Constant *getRuntimeFn(IRGenModule &IGM,
                      llvm::Constant *&cache,
                      char const *name,
                      llvm::Constant *(*createFn)(IRGenModule &IGM,
                                                  StringRef name,
                                                  llvm::FunctionType *fnType),
                      std::initializer_list<llvm::Type*> retTypes,
                      std::initializer_list<llvm::Type*> argTypes)
{
  if (cache)
    return cache;
  
  llvm::Type *retTy;
  if (retTypes.size() == 1)
    retTy = *retTypes.begin();
  else
    retTy = llvm::StructType::get(IGM.LLVMContext,
                                  {retTypes.begin(), retTypes.end()},
                                  /*packed*/ false);
  auto fnTy = llvm::FunctionType::get(retTy,
                                      {argTypes.begin(), argTypes.end()},
                                      /*isVararg*/ false);
  
  cache = createFn(IGM, name, fnTy);
  return cache;
}

llvm::Constant *IRGenModule::getAllocBoxFn() {
  // struct { RefCounted *box; void *value; } swift_allocBox(Metadata *type);
  return getRuntimeFn(*this, AllocBoxFn,
                      "swift_allocBox", createRuntimeFunction,
                      { RefCountedPtrTy, OpaquePtrTy },
                      { TypeMetadataPtrTy });
}

llvm::Constant *IRGenModule::getAllocObjectFn() {
  // RefCounted *swift_allocObject(Metadata *type, size_t size, size_t align);
  return getRuntimeFn(*this, AllocObjectFn,
                      "swift_allocObject", createRuntimeFunction,
                      { RefCountedPtrTy },
                      { TypeMetadataPtrTy, SizeTy, SizeTy });
}

llvm::Constant *IRGenModule::getDeallocObjectFn() {
  // void swift_deallocObject(RefCounted *obj, size_t size);
  return getRuntimeFn(*this, DeallocObjectFn,
                      "swift_deallocObject", createRuntimeFunction,
                      { VoidTy },
                      { RefCountedPtrTy, SizeTy });
}

llvm::Constant *IRGenModule::getRawAllocFn() {
  /// void *swift_rawAlloc(SwiftAllocIndex index);
  return getRuntimeFn(*this, RawAllocFn,
                      "swift_rawAlloc", createRuntimeFunction,
                      { Int8PtrTy },
                      { SizeTy });
}

llvm::Constant *IRGenModule::getRawDeallocFn() {
  /// void swift_rawDealloc(void *ptr, SwiftAllocIndex index);
  return getRuntimeFn(*this, RawDeallocFn,
                      "swift_rawDealloc", createRuntimeFunction,
                      { VoidTy },
                      { Int8PtrTy, SizeTy });
}

llvm::Constant *IRGenModule::getSlowAllocFn() {
  /// void *swift_slowAlloc(size_t size, size_t flags);
  return getRuntimeFn(*this, SlowAllocFn,
                      "swift_slowAlloc", createRuntimeFunction,
                      { Int8PtrTy },
                      { SizeTy, SizeTy });
}

llvm::Constant *IRGenModule::getSlowRawDeallocFn() {
  /// void swift_slowRawDealloc(void *ptr, size_t size);
  return getRuntimeFn(*this, SlowRawDeallocFn, "swift_slowRawDealloc",
                      createRuntimeFunction,
                      { VoidTy },
                      { Int8PtrTy, SizeTy });
}

llvm::Constant *IRGenModule::getCopyPODFn() {
  /// void *swift_copyPOD(void *dest, void *src, Metadata *self);
  return getRuntimeFn(*this, CopyPODFn, "swift_copyPOD", createRuntimeFunction,
                      { OpaquePtrTy },
                      { OpaquePtrTy, OpaquePtrTy, TypeMetadataPtrTy });
}

llvm::Constant *IRGenModule::getDynamicCastClassFn() {
  // void *swift_dynamicCastClass(void*, void*);
  return getRuntimeFn(*this, DynamicCastClassFn, "swift_dynamicCastClass",
                      createReadonlyRuntimeFunction,
                      { Int8PtrTy },
                      { Int8PtrTy, Int8PtrTy });
}

llvm::Constant *IRGenModule::getDynamicCastClassUnconditionalFn() {
  // void *swift_dynamicCastClassUnconditional(void*, void*);
  return getRuntimeFn(*this, DynamicCastClassUnconditionalFn, "swift_dynamicCastClassUnconditional",
                      createReadonlyRuntimeFunction,
                      { Int8PtrTy },
                      { Int8PtrTy, Int8PtrTy });
}

llvm::Constant *IRGenModule::getDynamicCastFn() {
  // void *swift_dynamicCast(void*, void*);
  return getRuntimeFn(*this, DynamicCastFn, "swift_dynamicCast",
                      createReadonlyRuntimeFunction,
                      { Int8PtrTy },
                      { Int8PtrTy, Int8PtrTy });
}

llvm::Constant *IRGenModule::getDynamicCastUnconditionalFn() {
  // void *swift_dynamicCastUnconditional(void*, void*);
  return getRuntimeFn(*this, DynamicCastUnconditionalFn, "swift_dynamicCastUnconditional",
                      createReadonlyRuntimeFunction,
                      { Int8PtrTy },
                      { Int8PtrTy, Int8PtrTy });
}

llvm::Constant *IRGenModule::getRetainNoResultFn() {
  // void swift_retainNoResult(void *ptr);
  getRuntimeFn(*this, RetainNoResultFn, "swift_retain_noresult",
               createRuntimeFunction,
               { VoidTy }, { RefCountedPtrTy });
  if (auto fn = dyn_cast<llvm::Function>(RetainNoResultFn))
    fn->setDoesNotCapture(1);
  return RetainNoResultFn;
}

llvm::Constant *IRGenModule::getReleaseFn() {
  // void swift_release(void *ptr);
  getRuntimeFn(*this, ReleaseFn, "swift_release",
               createRuntimeFunction,
               { VoidTy }, { RefCountedPtrTy });
  if (auto fn = dyn_cast<llvm::Function>(ReleaseFn))
    fn->setDoesNotCapture(1);
  return ReleaseFn;
}

llvm::Constant *IRGenModule::getGetFunctionMetadataFn() {
  // type_metadata_t *swift_getFunctionMetadata(type_metadata_t *arg,
  //                                            type_metadata_t *result);
  return getRuntimeFn(*this, GetFunctionMetadataFn, "swift_getFunctionMetadata",
                      createReadnoneRuntimeFunction,
                      { TypeMetadataPtrTy },
                      { TypeMetadataPtrTy, TypeMetadataPtrTy });
}

llvm::Constant *IRGenModule::getGetGenericMetadataFn() {
  // type_metadata_t *swift_getGenericMetadata(type_metadata_pattern_t *pattern,
  //                                           const void *arguments);
  return getRuntimeFn(*this, GetGenericMetadataFn, "swift_getGenericMetadata",
                      createRuntimeFunction,
                      { TypeMetadataPtrTy },
                      { TypeMetadataPatternPtrTy, Int8PtrTy });
}

llvm::Constant *IRGenModule::getGetMetatypeMetadataFn() {
  // type_metadata_t *swift_getMetatypeMetadata(type_metadata_t *instanceTy);
  return getRuntimeFn(*this, GetMetatypeMetadataFn, "swift_getMetatypeMetadata",
                      createReadnoneRuntimeFunction,
                      { TypeMetadataPtrTy },
                      { TypeMetadataPtrTy });
}

llvm::Constant *IRGenModule::getGetObjCClassMetadataFn() {
  // type_metadata_t *swift_getObjCClassMetadata(struct objc_class *theClass);
  return getRuntimeFn(*this, GetObjCClassMetadataFn, "swift_getObjCClassMetadata",
                      createReadnoneRuntimeFunction,
                      { TypeMetadataPtrTy },
                      { TypeMetadataPtrTy });
}

llvm::Constant *IRGenModule::getStaticTypeofFn() {
  // type_metadata_t *swift_staticTypeof(opaque_t *obj, type_metadata_t *self);
  return getRuntimeFn(*this, StaticTypeofFn, "swift_staticTypeof",
                      createReadnoneRuntimeFunction,
                      { TypeMetadataPtrTy },
                      { OpaquePtrTy, TypeMetadataPtrTy });
}

llvm::Constant *IRGenModule::getObjectTypeofFn() {
  // type_metadata_t *swift_objectTypeof(opaque_t *obj, type_metadata_t *self);
  return getRuntimeFn(*this, ObjectTypeofFn, "swift_objectTypeof",
                      createRuntimeFunction,
                      { TypeMetadataPtrTy },
                      { OpaquePtrTy, TypeMetadataPtrTy });
}

llvm::Constant *IRGenModule::getObjCTypeofFn() {
  // type_metadata_t *swift_objcTypeof(opaque_t *obj, type_metadata_t *self);
  return getRuntimeFn(*this, ObjCTypeofFn, "swift_objcTypeof",
                      createRuntimeFunction,
                      { TypeMetadataPtrTy },
                      { OpaquePtrTy, TypeMetadataPtrTy });
}

llvm::Constant *IRGenModule::getEmptyTupleMetadata() {
  if (EmptyTupleMetadata)
    return EmptyTupleMetadata;

  return EmptyTupleMetadata =
    Module.getOrInsertGlobal("_TMdT_",
                             TypeMetadataPtrTy->getPointerElementType());
}

llvm::Constant *IRGenModule::getGetTupleMetadataFn() {
  // type_metadata_t *swift_getTupleMetadata(size_t numElements,
  //                                         type_metadata_t * const *pattern,
  //                                         const char *labels,
  //                                         value_witness_table_t *proposed);
  return getRuntimeFn(*this, GetTupleMetadataFn, "swift_getTupleTypeMetadata",
                      createReadonlyRuntimeFunction,
                      { TypeMetadataPtrTy },
                      {
                        SizeTy,
                        TypeMetadataPtrTy->getPointerTo(0),
                        Int8PtrTy,
                        WitnessTablePtrTy
                      });
}

llvm::Constant *IRGenModule::getGetTupleMetadata2Fn() {
  // type_metadata_t *swift_getTupleMetadata2(type_metadata_t *elt0,
  //                                         type_metadata_t *elt1,
  //                                         const char *labels,
  //                                         value_witness_table_t *proposed);
  return getRuntimeFn(*this, GetTupleMetadata2Fn, "swift_getTupleTypeMetadata2",
                      createReadnoneRuntimeFunction,
                      { TypeMetadataPtrTy },
                      {
                        TypeMetadataPtrTy, 
                        TypeMetadataPtrTy,
                        Int8PtrTy,
                        WitnessTablePtrTy
                      });
}

llvm::Constant *IRGenModule::getGetTupleMetadata3Fn() {
  // type_metadata_t *swift_getTupleMetadata3(type_metadata_t *elt0,
  //                                         type_metadata_t *elt1,
  //                                         type_metadata_t *elt2,
  //                                         type_metadata_t * const *pattern,
  //                                         const char *labels,
  //                                         value_witness_table_t *proposed);
  return getRuntimeFn(*this, GetTupleMetadata3Fn, "swift_getTupleTypeMetadata3",
                      createReadnoneRuntimeFunction,
                      { TypeMetadataPtrTy },
                      {
                        TypeMetadataPtrTy,
                        TypeMetadataPtrTy,
                        TypeMetadataPtrTy,
                        Int8PtrTy,
                        WitnessTablePtrTy
                      });
}


llvm::Constant *IRGenModule::getGetObjectClassFn() {
  if (GetObjectClassFn) return GetObjectClassFn;

  // Class object_getClass(id object);
  // This is an Objective-C runtime function.
  // We have to mark it readonly instead of readnone because isa-rewriting
  // can have a noticeable effect here.
  llvm::FunctionType *fnType =
    llvm::FunctionType::get(TypeMetadataPtrTy, ObjCPtrTy, false);
  GetObjectClassFn = Module.getOrInsertFunction("object_getClass", fnType);
  if (auto fn = dyn_cast<llvm::Function>(GetObjectClassFn))
    fn->setOnlyReadsMemory();
  return GetObjectClassFn;
}

llvm::Constant *IRGenModule::getGetObjectTypeFn() {
  if (GetObjectTypeFn) return GetObjectTypeFn;

  // type_metadata_t *swift_getObjectType(id object);
  // Since this supposedly looks through dynamic subclasses, it's
  // invariant across reasonable isa-rewriting schemes and therefore
  // can be readnone.
  llvm::FunctionType *fnType =
    llvm::FunctionType::get(TypeMetadataPtrTy, ObjCPtrTy, false);
  GetObjectTypeFn =
    createReadnoneRuntimeFunction(*this, "swift_getObjectType", fnType);
  return GetObjectTypeFn;
}

llvm::Constant *IRGenModule::getObjCEmptyCachePtr() {
  if (ObjCEmptyCachePtr) return ObjCEmptyCachePtr;

  // struct objc_cache _objc_empty_cache;
  ObjCEmptyCachePtr = Module.getOrInsertGlobal("_objc_empty_cache",
                                               OpaquePtrTy->getElementType());
  return ObjCEmptyCachePtr;
}

llvm::Constant *IRGenModule::getObjCEmptyVTablePtr() {
  if (ObjCEmptyVTablePtr) return ObjCEmptyVTablePtr;

  // IMP _objc_empty_vtable;
  if (Opts.UseJIT) {
    ObjCEmptyVTablePtr = llvm::ConstantPointerNull::get(OpaquePtrTy);
  } else {
    ObjCEmptyVTablePtr = Module.getOrInsertGlobal("_objc_empty_vtable",
                                                  OpaquePtrTy->getElementType());
  }
  return ObjCEmptyVTablePtr;
}

llvm::Constant *IRGenModule::getSize(Size size) {
  return llvm::ConstantInt::get(SizeTy, size.getValue());
}

void IRGenModule::unimplemented(SourceLoc loc, StringRef message) {
  Context.Diags.diagnose(loc, diag::irgen_unimplemented, message);
}

void IRGenModule::error(SourceLoc loc, const Twine &message) {
  SmallVector<char, 128> buffer;
  Context.Diags.diagnose(loc, diag::irgen_failure,
                         message.toStringRef(buffer));
}
