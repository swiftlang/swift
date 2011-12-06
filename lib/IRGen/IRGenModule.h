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

namespace llvm {
  class Constant;
  class Function;
  class FunctionType;
  class GlobalVariable;
  class IntegerType;
  class LLVMContext;
  class Module;
  class PointerType;
  class TargetData;
  class Type;
}

namespace swift {
  class ASTContext;
  class BraceStmt;
  class Component;
  class Decl;
  class FuncDecl;
  class NamedDecl;
  class SourceLoc;
  class TranslationUnit;
  class Type;
  class VarDecl;

namespace irgen {
  class Options;
  class TypeConverter;
  class TypeInfo;

/// IRGenModule - Primary class for emitting IR for global declarations.
/// 
class IRGenModule {
public:
  ASTContext &Context;
  Options &Opts;
  swift::Component *Component;
  llvm::Module &Module;
  llvm::LLVMContext &LLVMContext;
  const llvm::TargetData &TargetData;

  llvm::IntegerType *Int1Ty;
  llvm::IntegerType *Int8Ty;
  llvm::IntegerType *Int16Ty;
  llvm::IntegerType *Int32Ty;
  llvm::IntegerType *Int64Ty;
  llvm::IntegerType *SizeTy;
  llvm::PointerType *Int8PtrTy;
  llvm::Constant *getMemCpyFn();

  void unimplemented(SourceLoc, StringRef Message);

private:
  llvm::Function *MemCpyFn;


//--- Types -----------------------------------------------------------------
public:
  const TypeInfo &getFragileTypeInfo(Type T);
  llvm::Type *getFragileType(Type T);

private:
  TypeConverter &Types;
  friend class TypeConverter;

//--- Globals ---------------------------------------------------------------
private:
  llvm::DenseMap<Decl*, llvm::Constant*> Globals;

  void emitTopLevel(BraceStmt *S);
  void emitGlobalDecl(Decl *D);
  void emitGlobalVariable(VarDecl *D);
  void emitGlobalFunction(FuncDecl *D);
  void mangle(raw_ostream &Mangled, NamedDecl *D);

  class LinkInfo;
  LinkInfo getLinkInfo(NamedDecl *D);

  llvm::FunctionType *getFunctionType(FuncDecl *D);

public:
  IRGenModule(ASTContext &Context, swift::Component *Component,
	      Options &Opts, llvm::Module &Module,
              const llvm::TargetData &TargetData);
  ~IRGenModule();

  llvm::LLVMContext &getLLVMContext() const { return LLVMContext; }

  void emitTranslationUnit(TranslationUnit *TU);
  llvm::GlobalVariable *getAddrOfGlobalVariable(VarDecl *D);
  llvm::Function *getAddrOfGlobalFunction(FuncDecl *D);
};

} // end namespace irgen
} // end namespace swift

#endif
