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
#include "llvm/CallingConv.h"
#include "IRGen.h"

namespace llvm {
  class Constant;
  class Function;
  class FunctionType;
  class GlobalVariable;
  class IntegerType;
  class LLVMContext;
  class Module;
  class PointerType;
  class StructType;
  class TargetData;
  class Type;
}

namespace swift {
  class ASTContext;
  class BraceStmt;
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
  enum class ExplosionKind : unsigned;
  class ExplosionSchema;
  class FormalType;
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
  const llvm::TargetData &TargetData;

  llvm::Type *VoidTy;                  /// void (usually {})
  llvm::IntegerType *Int1Ty;           /// i1
  llvm::IntegerType *Int8Ty;           /// i8
  llvm::IntegerType *Int16Ty;          /// i16
  llvm::IntegerType *Int32Ty;          /// i32
  llvm::IntegerType *Int64Ty;          /// i64
  llvm::IntegerType *SizeTy;           /// usually i32 or i64
  llvm::PointerType *Int8PtrTy;        /// i8*
  union {
    llvm::PointerType *Int8PtrPtrTy;   /// i8**
    llvm::PointerType *WitnessTablePtrTy;
  };
  llvm::StructType *RefCountedStructTy;/// %swift.refcounted = type { ... }
  llvm::PointerType *RefCountedPtrTy;  /// %swift.refcounted*
  llvm::Constant *RefCountedNull;      /// %swift.refcounted* null
  llvm::StructType *FunctionPairTy;    /// { i8*, %swift.refcounted* }
  llvm::FunctionType *DtorTy;          /// size_t (%swift.refcounted*)
  llvm::StructType *HeapMetadataStructTy; /// %swift.heapmetadata = type { ... }
  llvm::PointerType *HeapMetadataPtrTy;/// %swift.heapmetadata*
  llvm::PointerType *ObjCPtrTy;        /// %objc_object*
  llvm::PointerType *OpaquePtrTy;      /// %swift.opaque*
  llvm::CallingConv::ID RuntimeCC;     /// lightweight calling convention

  Size getPointerSize() const { return PtrSize; }
  Alignment getPointerAlignment() const {
    // We always use the pointer's width as its swift ABI alignment.
    return Alignment(PtrSize.getValue());
  }

  llvm::Type *getFixedBufferTy();
  llvm::PointerType *getValueWitnessTy(ValueWitness index);

  void unimplemented(SourceLoc, StringRef Message);
  void error(SourceLoc loc, const Twine &message);

private:
  Size PtrSize;
  llvm::Type *FixedBufferTy;           /// [16 x i8]

  enum { NumValueWitnesses = 13 };
  llvm::PointerType *ValueWitnessTys[NumValueWitnesses]; /// pointer-to-functions

//--- Types -----------------------------------------------------------------
public:
  const ProtocolInfo &getProtocolInfo(ProtocolDecl *D);
  const TypeInfo &getFragileTypeInfo(Type T);
  const TypeInfo &getWitnessTablePtrTypeInfo();
  llvm::Type *getFragileType(Type T);
  llvm::StructType *createNominalType(TypeDecl *D);
  llvm::StructType *createNominalType(ProtocolCompositionType *T);
  void getSchema(Type T, ExplosionSchema &schema);
  ExplosionSchema getSchema(Type T, ExplosionKind kind);
  unsigned getExplosionSize(Type T, ExplosionKind kind);
  llvm::PointerType *isSingleIndirectValue(Type T, ExplosionKind kind);
  llvm::PointerType *requiresIndirectResult(Type T, ExplosionKind kind);

  bool isResilient(Decl *decl) { return false; }

private:
  TypeConverter &Types;
  friend class TypeConverter;
  friend class GenProto;

//--- Globals ---------------------------------------------------------------
private:
  llvm::DenseMap<Decl*, llvm::GlobalVariable*> GlobalVars;
  llvm::DenseMap<LinkEntity, llvm::Function*> GlobalFuncs;

  void mangleGlobalInitializer(raw_ostream &buffer, TranslationUnit *D);

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

  llvm::Constant *getObjCRetainFn();
  llvm::Constant *getObjCReleaseFn();

private:
  llvm::Function *MemCpyFn;
  llvm::Constant *AllocObjectFn;
  llvm::Constant *RetainNoResultFn;
  llvm::Constant *ReleaseFn;
  llvm::Constant *ObjCRetainFn;
  llvm::Constant *ObjCReleaseFn;
  llvm::Constant *DeallocObjectFn;
  llvm::Constant *RawAllocFn;
  llvm::Constant *RawDeallocFn;
  llvm::Constant *SlowAllocFn;
  llvm::Constant *SlowRawDeallocFn;

//--- Generic ---------------------------------------------------------------
public:
  IRGenModule(ASTContext &Context, Options &Opts, llvm::Module &Module,
              const llvm::TargetData &TargetData);
  ~IRGenModule();

  llvm::LLVMContext &getLLVMContext() const { return LLVMContext; }

  void emitTranslationUnit(TranslationUnit *TU, unsigned StartElem);

  void emitOneOfDecl(OneOfDecl *D);
  void emitStructDecl(StructDecl *D);
  void emitClassDecl(ClassDecl *D);
  void emitExtension(ExtensionDecl *D);
  void emitGlobalFunction(FuncDecl *D);  
  void emitStaticMethod(FuncDecl *D);
  void emitInstanceMethod(FuncDecl *D);
  void emitConstructor(ConstructorDecl *D);

  llvm::FunctionType *getFunctionType(Type fnType, ExplosionKind kind,
                                      unsigned uncurryLevel, bool withData);

  FormalType getTypeOfGetter(ValueDecl *D);
  FormalType getTypeOfSetter(ValueDecl *D);

  Address getAddrOfGlobalVariable(VarDecl *D);
  llvm::Function *getAddrOfFunction(FuncDecl *D, ExplosionKind kind,
                                    unsigned uncurryLevel, bool needsData);
  llvm::Function *getAddrOfInjectionFunction(OneOfElementDecl *D);
  llvm::Function *getAddrOfGetter(ValueDecl *D, FormalType type,
                                  ExplosionKind kind);
  llvm::Function *getAddrOfGetter(ValueDecl *D, ExplosionKind kind);
  llvm::Function *getAddrOfSetter(ValueDecl *D, FormalType type,
                                  ExplosionKind kind);
  llvm::Function *getAddrOfSetter(ValueDecl *D, ExplosionKind kind);
  llvm::Function *getAddrOfValueWitness(Type concreteType, ValueWitness index);
  llvm::Function *getAddrOfConstructor(ConstructorDecl *D, ExplosionKind kind);
  llvm::Function *getAddrOfDestructor(ClassDecl *D);
};

} // end namespace irgen
} // end namespace swift

#endif
