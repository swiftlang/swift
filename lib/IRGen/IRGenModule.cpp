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

using namespace swift;
using namespace irgen;

IRGenModule::IRGenModule(ASTContext &Context, Options &Opts,
                         llvm::Module &Module,
                         const llvm::TargetData &TargetData)
  : Context(Context), Opts(Opts),
    Module(Module), LLVMContext(Module.getContext()),
    TargetData(TargetData), Types(*new TypeConverter()) {
  Int1Ty = llvm::Type::getInt1Ty(getLLVMContext());
  Int8Ty = llvm::Type::getInt8Ty(getLLVMContext());
  Int16Ty = llvm::Type::getInt16Ty(getLLVMContext());
  Int32Ty = llvm::Type::getInt32Ty(getLLVMContext());
  Int64Ty = llvm::Type::getInt64Ty(getLLVMContext());
  Int8PtrTy = llvm::Type::getInt8PtrTy(getLLVMContext());
  SizeTy = TargetData.getIntPtrType(getLLVMContext());
  MemCpyFn = nullptr;
}

IRGenModule::~IRGenModule() {
  delete &Types;
}

llvm::Constant *IRGenModule::getMemCpyFn() {
  if (MemCpyFn) return MemCpyFn;

  llvm::Type *types[] = { SizeTy };
  MemCpyFn = llvm::Intrinsic::getDeclaration(&Module, llvm::Intrinsic::memcpy,
                                             types);
  return MemCpyFn;
}

/// Emit a top-level context.
void IRGenModule::emitTopLevel(BraceStmt *Body) {
  for (auto Elt : Body->getElements()) {
    if (Decl *D = Elt.dyn_cast<Decl*>())
      emitGlobalDecl(D);
    // TODO: handle top-level statements and expressions.
  }
}

/// Emit all the top-level code in the translation unit.
void IRGenModule::emitTranslationUnit(TranslationUnit *TU) {
  // The semantics of the top-level BraceStmt are a bit different from
  // the normal semantics.
  emitTopLevel(TU->Body);
}

void IRGenModule::unimplemented(SourceLoc Loc, StringRef Message) {
  Context.Diags.diagnose(Loc, diag::irgen_unimplemented, Message);
}
