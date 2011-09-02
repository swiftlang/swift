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
  class SMLoc;
  class Twine;
}

namespace swift {
  class ApplyExpr;
  class AssignStmt;
  class BraceStmt;
  class Decl;
  class DeclRefExpr;
  class Expr;
  class FuncDecl;
  class FuncExpr;
  class IfStmt;
  class ReturnStmt;
  class Stmt;
  class TupleExpr;
  class TupleElementExpr;
  class TupleShuffleExpr;
  class Type;
  class ValueDecl;
  class VarDecl;
  class WhileStmt;

namespace irgen {
  class IRGenModule;
  class JumpDest;
  class RValue;
  class TypeInfo;

/// IRGenFunction - Primary class for emitting LLVM instructions for a
/// specific function.
class IRGenFunction {
public:
  IRGenModule &IGM;
  IRBuilder Builder;

  FuncExpr *CurFuncExpr;
  llvm::Function *CurFn;

  IRGenFunction(IRGenModule &IGM, FuncExpr *FE, llvm::Function *Fn);

  void unimplemented(llvm::SMLoc Loc, const llvm::Twine &Message);

//--- Control flow -------------------------------------------------------------
public:
  void emitBranch(JumpDest D);

//--- Function prologue and epilogue -------------------------------------------
public:
  void emitFunction();
private:
  void emitPrologue();
  void emitEpilogue();

  LValue ReturnSlot;
  llvm::BasicBlock *ReturnBB;
  JumpDest getReturnDest();

//--- Helper methods -----------------------------------------------------------
public:
  LValue createFullExprAlloca(llvm::Type *Ty, Alignment Align,
                              const llvm::Twine &Name);
  LValue createScopeAlloca(llvm::Type *Ty, Alignment Align,
                           const llvm::Twine &Name);
  llvm::BasicBlock *createBasicBlock(const llvm::Twine &Name);
  const TypeInfo &getFragileTypeInfo(Type T);
private:
  llvm::Instruction *AllocaIP;

//--- Statement emission -------------------------------------------------------
public:
  void emitBraceStmt(BraceStmt *S);
  void emitStmt(Stmt *S);

private:
  void emitAssignStmt(AssignStmt *S);
  void emitIfStmt(IfStmt *S);
  void emitReturnStmt(ReturnStmt *S);
  void emitWhileStmt(WhileStmt *S);

//--- Expression emission ------------------------------------------------------
public:
  RValue emitFakeRValue(const TypeInfo &TInfo);
  LValue emitFakeLValue(const TypeInfo &TInfo);

  void emitIgnored(Expr *E);

  LValue emitLValue(Expr *E);
  LValue emitLValue(Expr *E, const TypeInfo &TInfo);

  RValue emitRValue(Expr *E);
  RValue emitRValue(Expr *E, const TypeInfo &TInfo);

private:
  RValue emitRValueForFunction(FuncDecl *Fn);
  RValue emitApplyExpr(ApplyExpr *Apply, const TypeInfo &TInfo);
  RValue emitDeclRefRValue(DeclRefExpr *DeclRef, const TypeInfo &TInfo);
  RValue emitTupleExpr(TupleExpr *E, const TypeInfo &TInfo);
  RValue emitTupleElementRValue(TupleElementExpr *E, const TypeInfo &TInfo);
  LValue emitTupleElementLValue(TupleElementExpr *E, const TypeInfo &TInfo);
  RValue emitTupleShuffleExpr(TupleShuffleExpr *E, const TypeInfo &TInfo);

//--- Declaration emission -----------------------------------------------------
public:
  void emitLocal(Decl *D);
  LValue getLocal(ValueDecl *D);
  LValue getGlobal(VarDecl *D, const TypeInfo &TInfo);

private:
  llvm::DenseMap<ValueDecl*, LValue> Locals;
};

} // end namespace irgen
} // end namespace swift

#endif
