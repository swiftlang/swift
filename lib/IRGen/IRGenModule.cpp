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
using llvm::Attribute;

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

static llvm::Constant *getRuntimeFn(IRGenModule &IGM,
                      llvm::Constant *&cache,
                      char const *name,
                      std::initializer_list<llvm::Type*> retTypes,
                      std::initializer_list<llvm::Type*> argTypes,
                      std::initializer_list<Attribute::AttrKind> attrs
                         = std::initializer_list<Attribute::AttrKind>()) {
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

  cache = IGM.Module.getOrInsertFunction(name, fnTy);

  // Add any function attributes and set the calling convention.
  if (auto fn = dyn_cast<llvm::Function>(cache)) {
    fn->setCallingConv(IGM.RuntimeCC);

    for (auto Attr : attrs)
      fn->addFnAttr(Attr);
  }

  return cache;
}

llvm::Constant *IRGenModule::getAllocBoxFn() {
  // struct { RefCounted *box; void *value; } swift_allocBox(Metadata *type);
  return getRuntimeFn(*this, AllocBoxFn, "swift_allocBox",
                      { RefCountedPtrTy, OpaquePtrTy },
                      { TypeMetadataPtrTy },
                      { Attribute::NoUnwind });
}

llvm::Constant *IRGenModule::getAllocObjectFn() {
  // RefCounted *swift_allocObject(Metadata *type, size_t size, size_t align);
  return getRuntimeFn(*this, AllocObjectFn, "swift_allocObject",
                      { RefCountedPtrTy },
                      { TypeMetadataPtrTy, SizeTy, SizeTy },
                      { Attribute::NoUnwind });
}

llvm::Constant *IRGenModule::getDeallocObjectFn() {
  // void swift_deallocObject(RefCounted *obj, size_t size);
  return getRuntimeFn(*this, DeallocObjectFn, "swift_deallocObject",
                      { VoidTy },
                      { RefCountedPtrTy, SizeTy },
                      { Attribute::NoUnwind });
}

llvm::Constant *IRGenModule::getRawAllocFn() {
  /// void *swift_rawAlloc(SwiftAllocIndex index);
  return getRuntimeFn(*this, RawAllocFn, "swift_rawAlloc",
                      { Int8PtrTy },
                      { SizeTy },
                      { Attribute::NoUnwind });
}

llvm::Constant *IRGenModule::getRawDeallocFn() {
  /// void swift_rawDealloc(void *ptr, SwiftAllocIndex index);
  return getRuntimeFn(*this, RawDeallocFn, "swift_rawDealloc",
                      { VoidTy },
                      { Int8PtrTy, SizeTy },
                      { Attribute::NoUnwind });
}

llvm::Constant *IRGenModule::getSlowAllocFn() {
  /// void *swift_slowAlloc(size_t size, size_t flags);
  return getRuntimeFn(*this, SlowAllocFn, "swift_slowAlloc",
                      { Int8PtrTy },
                      { SizeTy, SizeTy },
                      { Attribute::NoUnwind });
}

llvm::Constant *IRGenModule::getSlowRawDeallocFn() {
  /// void swift_slowRawDealloc(void *ptr, size_t size);
  return getRuntimeFn(*this, SlowRawDeallocFn, "swift_slowRawDealloc",
                      { VoidTy },
                      { Int8PtrTy, SizeTy },
                      { Attribute::NoUnwind });
}

llvm::Constant *IRGenModule::getCopyPODFn() {
  /// void *swift_copyPOD(void *dest, void *src, Metadata *self);
  return getRuntimeFn(*this, CopyPODFn, "swift_copyPOD",
                      { OpaquePtrTy },
                      { OpaquePtrTy, OpaquePtrTy, TypeMetadataPtrTy },
                      { Attribute::NoUnwind });
}

llvm::Constant *IRGenModule::getDynamicCastClassFn() {
  // void *swift_dynamicCastClass(void*, void*);
  return getRuntimeFn(*this, DynamicCastClassFn, "swift_dynamicCastClass",
                      { Int8PtrTy },
                      { Int8PtrTy, Int8PtrTy },
                      { Attribute::NoUnwind, Attribute::ReadOnly });
}

llvm::Constant *IRGenModule::getDynamicCastClassUnconditionalFn() {
  // void *swift_dynamicCastClassUnconditional(void*, void*);
  return getRuntimeFn(*this, DynamicCastClassUnconditionalFn,
                      "swift_dynamicCastClassUnconditional",
                      { Int8PtrTy },
                      { Int8PtrTy, Int8PtrTy },
                      { Attribute::NoUnwind, Attribute::ReadOnly });
}

llvm::Constant *IRGenModule::getDynamicCastFn() {
  // void *swift_dynamicCast(void*, void*);
  return getRuntimeFn(*this, DynamicCastFn, "swift_dynamicCast",
                      { Int8PtrTy },
                      { Int8PtrTy, Int8PtrTy },
                      { Attribute::NoUnwind, Attribute::ReadOnly });
}

llvm::Constant *IRGenModule::getDynamicCastUnconditionalFn() {
  // void *swift_dynamicCastUnconditional(void*, void*);
  return getRuntimeFn(*this, DynamicCastUnconditionalFn,
                      "swift_dynamicCastUnconditional",
                      { Int8PtrTy },
                      { Int8PtrTy, Int8PtrTy },
                      { Attribute::NoUnwind, Attribute::ReadOnly });
}

llvm::Constant *IRGenModule::getRetainNoResultFn() {
  // void swift_retainNoResult(void *ptr);
  getRuntimeFn(*this, RetainNoResultFn, "swift_retain_noresult",
               { VoidTy }, { RefCountedPtrTy },
               { Attribute::NoUnwind });
  if (auto fn = dyn_cast<llvm::Function>(RetainNoResultFn))
    fn->setDoesNotCapture(1);
  return RetainNoResultFn;
}

llvm::Constant *IRGenModule::getReleaseFn() {
  // void swift_release(void *ptr);
  getRuntimeFn(*this, ReleaseFn, "swift_release",
               { VoidTy }, { RefCountedPtrTy }, { Attribute::NoUnwind });
  if (auto fn = dyn_cast<llvm::Function>(ReleaseFn))
    fn->setDoesNotCapture(1);
  return ReleaseFn;
}

llvm::Constant *IRGenModule::getGetFunctionMetadataFn() {
  // type_metadata_t *swift_getFunctionMetadata(type_metadata_t *arg,
  //                                            type_metadata_t *result);
  return getRuntimeFn(*this, GetFunctionMetadataFn,
                      "swift_getFunctionTypeMetadata",
                      { TypeMetadataPtrTy },
                      { TypeMetadataPtrTy, TypeMetadataPtrTy },
                      { Attribute::NoUnwind, Attribute::ReadNone });
}

llvm::Constant *IRGenModule::getGetGenericMetadataFn() {
  // type_metadata_t *swift_getGenericMetadata(type_metadata_pattern_t *pattern,
  //                                           const void *arguments);
  return getRuntimeFn(*this, GetGenericMetadataFn, "swift_getGenericMetadata",
                      { TypeMetadataPtrTy },
                      { TypeMetadataPatternPtrTy, Int8PtrTy });
}

llvm::Constant *IRGenModule::getGetMetatypeMetadataFn() {
  // type_metadata_t *swift_getMetatypeMetadata(type_metadata_t *instanceTy);
  return getRuntimeFn(*this, GetMetatypeMetadataFn, "swift_getMetatypeMetadata",
                      { TypeMetadataPtrTy },
                      { TypeMetadataPtrTy },
                      { Attribute::NoUnwind, Attribute::ReadNone });
}

llvm::Constant *IRGenModule::getGetObjCClassMetadataFn() {
  // type_metadata_t *swift_getObjCClassMetadata(struct objc_class *theClass);
  return getRuntimeFn(*this, GetObjCClassMetadataFn, "swift_getObjCClassMetadata",
                      { TypeMetadataPtrTy },
                      { TypeMetadataPtrTy },
                      { Attribute::NoUnwind, Attribute::ReadNone });
}

llvm::Constant *IRGenModule::getStaticTypeofFn() {
  // type_metadata_t *swift_staticTypeof(opaque_t *obj, type_metadata_t *self);
  return getRuntimeFn(*this, StaticTypeofFn, "swift_staticTypeof",
                      { TypeMetadataPtrTy },
                      { OpaquePtrTy, TypeMetadataPtrTy },
                      { Attribute::NoUnwind, Attribute::ReadNone });
}

llvm::Constant *IRGenModule::getObjectTypeofFn() {
  // type_metadata_t *swift_objectTypeof(opaque_t *obj, type_metadata_t *self);
  return getRuntimeFn(*this, ObjectTypeofFn, "swift_objectTypeof",
                      { TypeMetadataPtrTy },
                      { OpaquePtrTy, TypeMetadataPtrTy },
                      { Attribute::NoUnwind });
}

llvm::Constant *IRGenModule::getObjCTypeofFn() {
  // type_metadata_t *swift_objcTypeof(opaque_t *obj, type_metadata_t *self);
  return getRuntimeFn(*this, ObjCTypeofFn, "swift_objcTypeof",
                      { TypeMetadataPtrTy },
                      { OpaquePtrTy, TypeMetadataPtrTy },
                      { Attribute::NoUnwind });
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
  //                                         type_metadata_t * const *elements,
  //                                         const char *labels,
  //                                         value_witness_table_t *proposed);
  return getRuntimeFn(*this, GetTupleMetadataFn, "swift_getTupleTypeMetadata",
                      { TypeMetadataPtrTy },
                      {
                        SizeTy,
                        TypeMetadataPtrTy->getPointerTo(0),
                        Int8PtrTy,
                        WitnessTablePtrTy
                      },
                      { Attribute::NoUnwind, Attribute::ReadOnly });
}

llvm::Constant *IRGenModule::getGetTupleMetadata2Fn() {
  // type_metadata_t *swift_getTupleMetadata2(type_metadata_t *elt0,
  //                                          type_metadata_t *elt1,
  //                                          const char *labels,
  //                                          value_witness_table_t *proposed);
  return getRuntimeFn(*this, GetTupleMetadata2Fn, "swift_getTupleTypeMetadata2",
                      { TypeMetadataPtrTy },
                      {
                        TypeMetadataPtrTy, 
                        TypeMetadataPtrTy,
                        Int8PtrTy,
                        WitnessTablePtrTy
                      },
                      { Attribute::NoUnwind, Attribute::ReadNone });
}

llvm::Constant *IRGenModule::getGetTupleMetadata3Fn() {
  // type_metadata_t *swift_getTupleMetadata3(type_metadata_t *elt0,
  //                                          type_metadata_t *elt1,
  //                                          type_metadata_t *elt2,
  //                                          const char *labels,
  //                                          value_witness_table_t *proposed);
  return getRuntimeFn(*this, GetTupleMetadata3Fn, "swift_getTupleTypeMetadata3",
                      { TypeMetadataPtrTy },
                      {
                        TypeMetadataPtrTy,
                        TypeMetadataPtrTy,
                        TypeMetadataPtrTy,
                        Int8PtrTy,
                        WitnessTablePtrTy
                      },
                      { Attribute::NoUnwind, Attribute::ReadNone });
}


llvm::Constant *IRGenModule::getGetObjectClassFn() {
  if (GetObjectClassFn) return GetObjectClassFn;

  // Class object_getClass(id object);
  // This is an Objective-C runtime function.
  // We have to mark it readonly instead of readnone because isa-rewriting
  // can have a noticeable effect here.
  return getRuntimeFn(*this, GetObjectClassFn, "object_getClass",
                      { TypeMetadataPtrTy },
                      { ObjCPtrTy },
                      { Attribute::NoUnwind, Attribute::ReadOnly });
}

llvm::Constant *IRGenModule::getGetObjectTypeFn() {
  // type_metadata_t *swift_getObjectType(id object);

  // Since this supposedly looks through dynamic subclasses, it's
  // invariant across reasonable isa-rewriting schemes and therefore
  // can be readnone.
  return getRuntimeFn(*this, GetObjectTypeFn, "swift_getObjectType",
                      { TypeMetadataPtrTy },
                      { ObjCPtrTy },
                      { Attribute::NoUnwind, Attribute::ReadNone });
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
