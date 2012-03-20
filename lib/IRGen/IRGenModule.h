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

#include "swift/AST/LLVM.h"
#include "llvm/ADT/DenseMap.h"
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
  class Decl;
  class ExtensionDecl;
  class FuncDecl;
  class NamedDecl;
  class OneOfElementDecl;
  class OneOfType;
  class SourceLoc;
  class TranslationUnit;
  class Type;
  class TypeAliasDecl;
  class VarDecl;

namespace irgen {
  class Address;
  enum class ExplosionKind : unsigned;
  class ExplosionSchema;
  class LinkEntity;
  class Options;
  class TypeConverter;
  class TypeInfo;

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
  llvm::StructType *RefCountedTy;      /// %swift.refcounted = type { i8* }
  llvm::PointerType *RefCountedPtrTy;  /// %swift.refcounted*
  llvm::StructType *FunctionPairTy;    /// { i8*, %swift.refcounted* }

  Size getPointerSize() const { return PtrSize; }
  Alignment getPointerAlignment() const {
    // We always use the pointer's width as its swift ABI alignment.
    return Alignment(PtrSize.getValue());
  }

  void unimplemented(SourceLoc, StringRef Message);

private:
  Size PtrSize;

//--- Types -----------------------------------------------------------------
public:
  const TypeInfo &getFragileTypeInfo(Type T);
  llvm::Type *getFragileType(Type T);
  llvm::StructType *createNominalType(TypeAliasDecl *D);
  void emitTypeAlias(Type T);
  void getSchema(Type T, ExplosionSchema &schema);

private:
  TypeConverter &Types;
  friend class TypeConverter;

//--- Globals ---------------------------------------------------------------
private:
  llvm::DenseMap<Decl*, llvm::GlobalVariable*> GlobalVars;
  llvm::DenseMap<LinkEntity, llvm::Function*> GlobalFuncs;

  void mangleGlobalInitializer(raw_ostream &buffer, TranslationUnit *D);

//--- Runtime ---------------------------------------------------------------
public:
  llvm::Constant *getMemCpyFn();
  llvm::Constant *getAllocFn();
  llvm::Constant *getRetainFn();
  llvm::Constant *getReleaseFn();

private:
  llvm::Function *MemCpyFn;
  llvm::Constant *AllocFn;
  llvm::Constant *RetainFn;
  llvm::Constant *ReleaseFn;

//--- Generic ---------------------------------------------------------------
public:
  IRGenModule(ASTContext &Context, Options &Opts, llvm::Module &Module,
              const llvm::TargetData &TargetData);
  ~IRGenModule();

  llvm::LLVMContext &getLLVMContext() const { return LLVMContext; }

  void emitTranslationUnit(TranslationUnit *TU);

  void emitOneOfType(OneOfType *type);
  void emitExtension(ExtensionDecl *D);
  void emitGlobalFunction(FuncDecl *D);  
  void emitPlusMethod(FuncDecl *D);
  void emitInstanceMethod(FuncDecl *D);

  llvm::FunctionType *getFunctionType(Type fnType, ExplosionKind kind,
                                      unsigned uncurryLevel, bool withData);

  Address getAddrOfGlobalVariable(VarDecl *D);
  llvm::Function *getAddrOfGlobalFunction(FuncDecl *D, ExplosionKind kind,
                                          unsigned curryingLevel);
  llvm::Function *getAddrOfInjectionFunction(OneOfElementDecl *D);
};

} // end namespace irgen
} // end namespace swift

#endif
