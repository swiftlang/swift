//===--- IRGenModule.h - Swift Global IR Generation Module ------*- C++ -*-===//
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
// This file defines the interface used 
// the AST into LLVM IR.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_IRGENMODULE_H
#define SWIFT_IRGEN_IRGENMODULE_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/ValueHandle.h"
#include "llvm/IR/CallingConv.h"
#include "IRGen.h"

namespace llvm {
  class Constant;
  class DataLayout;
  class Function;
  class FunctionType;
  class GlobalVariable;
  class IntegerType;
  class LLVMContext;
  class Module;
  class PointerType;
  class StructType;
  class StringRef;
  class Type;
}

namespace swift {
  class ASTContext;
  class BraceStmt;
  class CanType;
  class ClassDecl;
  class ConstructorDecl;
  class Decl;
  class DestructorDecl;
  class ExtensionDecl;
  class FuncDecl;
  class OneOfElementDecl;
  class OneOfDecl;
  class ProtocolCompositionType;
  class ProtocolDecl;
  class SILModule;
  class SourceLoc;
  class StructDecl;
  class TranslationUnit;
  class Type;
  class TypeAliasDecl;
  class TypeDecl;
  class ValueDecl;
  class VarDecl;

namespace irgen {
  class Address;
  class CodeRef;
  enum class ExplosionKind : unsigned;
  class ExplosionSchema;
  class FormalType;
  class FunctionRef;
  class LinkEntity;
  class Options;
  class ProtocolInfo;
  class TypeConverter;
  class TypeInfo;
  enum class ValueWitness : unsigned;

/// IRGenModule - Primary class for emitting IR for global declarations.
/// 
class IRGenModule {
public:
  ASTContext &Context;
  Options &Opts;
  llvm::Module &Module;
  llvm::LLVMContext &LLVMContext;
  const llvm::DataLayout &DataLayout;
  SILModule *SILMod;

  /// Does the current target require Objective-C interoperation?
  static const bool ObjCInterop = true;

  llvm::Type *VoidTy;                  /// void (usually {})
  llvm::IntegerType *Int1Ty;           /// i1
  llvm::IntegerType *Int8Ty;           /// i8
  llvm::IntegerType *Int16Ty;          /// i16
  llvm::IntegerType *Int32Ty;          /// i32
  llvm::IntegerType *Int64Ty;          /// i64
  union {
    llvm::IntegerType *SizeTy;         /// usually i32 or i64
    llvm::IntegerType *IntPtrTy;
    llvm::IntegerType *MetadataKindTy;
  };
  union {
    llvm::PointerType *Int8PtrTy;      /// i8*
    llvm::PointerType *WitnessTableTy;
    llvm::PointerType *ObjCSELTy;
  };
  union {
    llvm::PointerType *Int8PtrPtrTy;   /// i8**
    llvm::PointerType *WitnessTablePtrTy;
  };
  llvm::StructType *RefCountedStructTy;/// %swift.refcounted = type { ... }
  llvm::PointerType *RefCountedPtrTy;  /// %swift.refcounted*
  llvm::Constant *RefCountedNull;      /// %swift.refcounted* null
  llvm::StructType *FunctionPairTy;    /// { i8*, %swift.refcounted* }
  llvm::FunctionType *DtorTy;          /// size_t (%swift.refcounted*)
  llvm::StructType *TypeMetadataStructTy; /// %swift.type = type { ... }
  llvm::PointerType *TypeMetadataPtrTy;/// %swift.type*
  llvm::StructType *FullHeapMetadataStructTy; /// %swift.full_heapmetadata = type { ... }
  llvm::PointerType *FullHeapMetadataPtrTy;/// %swift.full_heapmetadata*
  llvm::StructType *TypeMetadataPatternStructTy;/// %swift.type_pattern = type { ... }
  llvm::PointerType *TypeMetadataPatternPtrTy;/// %swift.type_pattern*
  llvm::StructType *FullTypeMetadataStructTy; /// %swift.full_type = type { ... }
  llvm::PointerType *FullTypeMetadataPtrTy;/// %swift.full_type*
  llvm::PointerType *ObjCPtrTy;        /// %objc_object*
  llvm::PointerType *OpaquePtrTy;      /// %swift.opaque*
  llvm::StructType *ObjCClassStructTy; /// %objc_class
  llvm::PointerType *ObjCClassPtrTy;   /// %objc_class*
  llvm::CallingConv::ID RuntimeCC;     /// lightweight calling convention

  Size getPointerSize() const { return PtrSize; }
  Alignment getPointerAlignment() const {
    // We always use the pointer's width as its swift ABI alignment.
    return Alignment(PtrSize.getValue());
  }

  llvm::Type *getFixedBufferTy();
  llvm::Type *getValueWitnessTy(ValueWitness index);

  void unimplemented(SourceLoc, StringRef Message);
  void error(SourceLoc loc, const Twine &message);

private:
  Size PtrSize;
  llvm::Type *FixedBufferTy;           /// [N x i8], where N == 3 * sizeof(void*)

  enum { NumValueWitnessFunctions = 12 };
  llvm::PointerType *ValueWitnessTys[NumValueWitnessFunctions]; /// pointer-to-functions

//--- Types -----------------------------------------------------------------
public:
  const ProtocolInfo &getProtocolInfo(ProtocolDecl *D);
  const TypeInfo &getFragileTypeInfo(CanType T);
  const TypeInfo &getFragileTypeInfo(Type T);
  const TypeInfo &getWitnessTablePtrTypeInfo();
  const TypeInfo &getTypeMetadataPtrTypeInfo();
  llvm::Type *getFragileType(CanType T);
  llvm::StructType *createNominalType(TypeDecl *D);
  llvm::StructType *createNominalType(ProtocolCompositionType *T);
  void getSchema(CanType T, ExplosionSchema &schema);
  ExplosionSchema getSchema(CanType T, ExplosionKind kind);
  unsigned getExplosionSize(CanType T, ExplosionKind kind);
  llvm::PointerType *isSingleIndirectValue(CanType T, ExplosionKind kind);
  llvm::PointerType *requiresIndirectResult(CanType T, ExplosionKind kind);
  bool hasTrivialMetatype(CanType type);
  bool isPOD(CanType type, ResilienceScope scope);
  ObjectSize classifyTypeSize(CanType type, ResilienceScope scope);

  bool isResilient(Decl *decl, ResilienceScope scope);

private:
  TypeConverter &Types;
  friend class TypeConverter;

//--- Globals ---------------------------------------------------------------
public:
  llvm::Constant *getAddrOfGlobalString(llvm::StringRef string);
  llvm::Constant *getAddrOfObjCSelectorRef(llvm::StringRef selector);
  llvm::Constant *getAddrOfObjCMethodName(llvm::StringRef methodName);
  void addUsedGlobal(llvm::GlobalValue *global);
  void addObjCClass(llvm::Constant *addr);

private:
  llvm::DenseMap<LinkEntity, llvm::GlobalVariable*> GlobalVars;
  llvm::DenseMap<LinkEntity, llvm::Function*> GlobalFuncs;
  llvm::StringMap<llvm::Constant*> GlobalStrings;
  llvm::StringMap<llvm::Constant*> ObjCSelectorRefs;
  llvm::StringMap<llvm::Constant*> ObjCMethodNames;

  /// LLVMUsed - List of global values which are required to be
  /// present in the object file; bitcast to i8*. This is used for
  /// forcing visibility of symbols which may otherwise be optimized
  /// out.
  llvm::SmallVector<llvm::WeakVH, 4> LLVMUsed;

  /// ObjCClasses - List of Objective-C classes, bitcast to i8*.
  llvm::SmallVector<llvm::WeakVH, 4> ObjCClasses;

  void mangleGlobalInitializer(raw_ostream &buffer, TranslationUnit *D);
  void emitGlobalLists();

//--- Runtime ---------------------------------------------------------------
public:
  llvm::Constant *getAllocObjectFn();
  llvm::Constant *getRetainNoResultFn();
  llvm::Constant *getReleaseFn();
  llvm::Constant *getDeallocObjectFn();

  llvm::Constant *getRawAllocFn();
  llvm::Constant *getRawDeallocFn();
  llvm::Constant *getSlowAllocFn();
  llvm::Constant *getSlowRawDeallocFn();

  llvm::Constant *getDynamicCastClassFn();
  llvm::Constant *getDynamicCastClassUnconditionalFn();
  llvm::Constant *getDynamicCastFn();
  llvm::Constant *getDynamicCastUnconditionalFn();

  llvm::Constant *getObjCRetainFn();
  llvm::Constant *getObjCRetainAutoreleasedReturnValueFn();
  llvm::Constant *getObjCReleaseFn();
  llvm::Constant *getObjCAutoreleaseReturnValueFn();

  llvm::Constant *getObjCMsgSendFn();
  llvm::Constant *getObjCMsgSendStretFn();

  llvm::Constant *getObjCSelRegisterNameFn();
  llvm::Constant *getGetObjectClassFn();
  llvm::Constant *getGetObjectTypeFn();

  llvm::Constant *getGetFunctionMetadataFn();
  llvm::Constant *getGetGenericMetadataFn();
  llvm::Constant *getGetMetatypeMetadataFn();
  llvm::Constant *getGetTupleMetadataFn();
  llvm::Constant *getGetObjCClassMetadataFn();
  ClassDecl *getSwiftRootClass();

private:
  llvm::Function *MemCpyFn;
  llvm::Constant *AllocObjectFn;
  llvm::Constant *RetainNoResultFn;
  llvm::Constant *ReleaseFn;
  llvm::Constant *DeallocObjectFn;
  llvm::Constant *RawAllocFn;
  llvm::Constant *RawDeallocFn;
  llvm::Constant *SlowAllocFn;
  llvm::Constant *SlowRawDeallocFn;
  llvm::Constant *DynamicCastClassFn = nullptr;
  llvm::Constant *DynamicCastClassUnconditionalFn = nullptr;
  llvm::Constant *DynamicCastFn = nullptr;
  llvm::Constant *DynamicCastUnconditionalFn = nullptr;
  llvm::Constant *GetFunctionMetadataFn = nullptr;
  llvm::Constant *GetGenericMetadataFn = nullptr;
  llvm::Constant *GetMetatypeMetadataFn = nullptr;
  llvm::Constant *GetObjCClassMetadataFn = nullptr;
  llvm::Constant *GetTupleMetadataFn = nullptr;
  llvm::Constant *GetObjectClassFn = nullptr;
  llvm::Constant *GetObjectTypeFn = nullptr;
  llvm::Constant *ObjCAutoreleaseReturnValueFn = nullptr;
  llvm::Constant *ObjCRetainFn = nullptr;
  llvm::Constant *ObjCRetainAutoreleasedReturnValueFn = nullptr;
  llvm::Constant *ObjCReleaseFn = nullptr;
  llvm::Constant *ObjCMsgSendFn = nullptr;
  llvm::Constant *ObjCMsgSendStretFn = nullptr;
  llvm::Constant *ObjCSelRegisterNameFn = nullptr;
  ClassDecl *SwiftRootClass = nullptr;

//--- Generic ---------------------------------------------------------------
public:
  IRGenModule(ASTContext &Context, Options &Opts, llvm::Module &Module,
              const llvm::DataLayout &DataLayout,
              SILModule *SILMod);
  ~IRGenModule();

  llvm::LLVMContext &getLLVMContext() const { return LLVMContext; }

  void emitTranslationUnit(TranslationUnit *TU, unsigned StartElem);

  void emitProtocolDecl(ProtocolDecl *D);
  void emitOneOfDecl(OneOfDecl *D);
  void emitStructDecl(StructDecl *D);
  void emitClassDecl(ClassDecl *D);
  void emitExtension(ExtensionDecl *D);
  void emitGlobalFunction(FuncDecl *D);  
  void emitStaticMethod(FuncDecl *D);
  void emitInstanceMethod(FuncDecl *D);
  void emitConstructor(ConstructorDecl *D);
  void emitClassConstructor(ConstructorDecl *D);

  llvm::FunctionType *getFunctionType(CanType fnType, ExplosionKind kind,
                                      unsigned uncurryLevel,
                                      ExtraData data);

  FormalType getTypeOfGetter(ValueDecl *D);
  FormalType getTypeOfSetter(ValueDecl *D);

  Address getAddrOfGlobalVariable(VarDecl *D);
  Address getAddrOfFieldOffset(VarDecl *D, bool isIndirect);
  llvm::Function *getAddrOfFunction(FunctionRef ref, ExtraData data);
  llvm::Function *getAddrOfInjectionFunction(OneOfElementDecl *D);
  llvm::Function *getAddrOfGetter(ValueDecl *D, FormalType type,
                                  ExplosionKind kind);
  llvm::Function *getAddrOfGetter(ValueDecl *D, ExplosionKind kind);
  llvm::Function *getAddrOfSetter(ValueDecl *D, FormalType type,
                                  ExplosionKind kind);
  llvm::Function *getAddrOfSetter(ValueDecl *D, ExplosionKind kind);
  Address getAddrOfWitnessTableOffset(CodeRef code);
  Address getAddrOfWitnessTableOffset(VarDecl *field);
  llvm::Function *getAddrOfValueWitness(CanType concreteType,
                                        ValueWitness index);
  llvm::Constant *getAddrOfValueWitnessTable(CanType concreteType,
                                             llvm::Type *definitionType = nullptr);
  llvm::Function *getAddrOfConstructor(ConstructorDecl *D, ExplosionKind kind);
  llvm::Function *getAddrOfDestructor(ClassDecl *D);
  llvm::Constant *getAddrOfTypeMetadata(CanType concreteType,
                                        bool isIndirect, bool isPattern,
                                        llvm::Type *definitionType = nullptr);
  llvm::Constant *getAddrOfObjCClass(ClassDecl *D);
  llvm::Constant *getAddrOfObjCMetaclass(ClassDecl *D);
  llvm::Constant *getAddrOfSwiftMetaclassStub(ClassDecl *D);
  llvm::Constant *getAddrOfMetaclassObject(ClassDecl *D);

  llvm::StringRef mangleType(CanType type,
                             llvm::SmallVectorImpl<char> &buffer);
};

} // end namespace irgen
} // end namespace swift

#endif
