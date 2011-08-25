//===--- IRGenFunction.h - IR Generation for Swift Functions ---*- C++ -*-===//
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
// This file defines the structure used to generate the IR body of a
// function.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_IRGENFUNCTION_H
#define SWIFT_IRGEN_IRGENFUNCTION_H

#include "llvm/ADT/DenseMap.h"
#include "IRBuilder.h"
#include "LValue.h"

namespace llvm {
  class Constant;
  class Function;
  class Twine;
}

namespace swift {
  class ApplyExpr;
  class BraceStmt;
  class Expr;
  class FuncDecl;
  class ValueDecl;
  class VarDecl;

namespace irgen {
  class IRGenModule;
  class RValue;
  class TypeInfo;

/// IRGenFunction - Primary class for emitting LLVM instructions for a
/// specific function.
class IRGenFunction {
public:
  IRGenModule &IGM;
  IRBuilder Builder;

  FuncDecl *CurDecl;
  llvm::Function *CurFn;

  IRGenFunction(IRGenModule &IGM, FuncDecl *D, llvm::Function *Fn);

//--- Function prologue and epilogue -------------------------------------------
public:
  void emitPrologue();
  void emitEpilogue();
private:
  LValue ReturnSlot;
  llvm::BasicBlock *ReturnBB;

//--- Helper methods -----------------------------------------------------------
public:
  llvm::AllocaInst *createFullExprAlloca(llvm::Type *Ty, Alignment Align,
                                         const llvm::Twine &Name);
  llvm::AllocaInst *createScopeAlloca(llvm::Type *Ty, Alignment Align,
                                      const llvm::Twine &Name);
  llvm::BasicBlock *createBasicBlock(const llvm::Twine &Name);
private:
  llvm::Instruction *AllocaIP;

//--- Expression emission ------------------------------------------------------
public:
  void emitIgnored(Expr *E);

  LValue emitLValue(Expr *E);
  LValue emitLValue(Expr *E, const TypeInfo &TInfo);

  RValue emitRValue(Expr *E);
  RValue emitRValue(Expr *E, const TypeInfo &TInfo);

private:
  RValue getRValueForGlobalFunction(FuncDecl *Fn);
  RValue emitApplyExpr(ApplyExpr *Apply, const TypeInfo &TInfo);

//--- Declaration emission -----------------------------------------------------
public:
  LValue getLocal(VarDecl *D);

private:
  llvm::DenseMap<ValueDecl*, LValue> Locals;
};

} // end namespace irgen
} // end namespace swift

#endif
