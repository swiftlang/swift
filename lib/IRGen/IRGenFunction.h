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

#include "swift/AST/LLVM.h"
#include "swift/AST/Type.h"
#include "llvm/ADT/DenseMap.h"
#include "IRBuilder.h"

namespace llvm {
  class Constant;
  class Function;
}

namespace swift {
  class ApplyExpr;
  class AssignStmt;
  class BraceStmt;
  class ClosureExpr;
  class Decl;
  class DeclRefExpr;
  class Expr;
  class FuncDecl;
  class FuncExpr;
  class IfStmt;
  class LookThroughOneofExpr;
  class OneOfElementDecl;
  template<typename T> class Optional;
  class Pattern;
  class RequalifyExpr;
  class ReturnStmt;
  class SourceLoc;
  class Stmt;
  class TupleExpr;
  class TupleElementExpr;
  class TupleShuffleExpr;
  class ValueDecl;
  class VarDecl;
  class WhileStmt;

namespace irgen {
  class Condition;
  class Explosion;
  enum class ExplosionKind : unsigned;
  class IRGenModule;
  class JumpDest;
  class LValue;
  class RValue;
  class TypeInfo;

/// Prologue - A value indicating controlling the kind of prologue/epilogue
/// code to emit.
enum class Prologue : unsigned char {
  /// The standard prologue/epilogue is useful for emitting normal
  /// function bodies consisting of statements and expressions.  It implies:
  ///   - everything from Prologue::Bare, plus
  ///   - materializing the argument variables and mapping them in Locals,
  ///   - creating and managing ReturnBB, and
  ///   - initializing ReturnSlot and extracting the return value from it.
  Standard,

  /// The bare prologue is useful for emitting small functions that
  /// don't need proper statement/expression emission.  It implies:
  ///   - creating the entry block
  ///   - setting up and tearing down the alloca point
  /// Functions using this prologue style are responsible for emitting
  /// the 'ret' instruction before epilogue emission.
  Bare
};

/// IRGenFunction - Primary class for emitting LLVM instructions for a
/// specific function.
class IRGenFunction {
public:
  IRGenModule &IGM;
  IRBuilder Builder;

  Type CurFuncType;
  ArrayRef<Pattern*> CurFuncParamPatterns;
  llvm::Function *CurFn;
  ExplosionKind CurExplosionLevel;
  unsigned CurUncurryLevel;
  Prologue CurPrologue;

  IRGenFunction(IRGenModule &IGM, Type t, ArrayRef<Pattern*> p,
                ExplosionKind explosion,
                unsigned uncurryLevel, llvm::Function *fn,
                Prologue prologue = Prologue::Standard);
  ~IRGenFunction();

  void unimplemented(SourceLoc Loc, StringRef Message);

//--- Control flow -------------------------------------------------------------
public:
  void emitBranch(JumpDest D);

//--- Function prologue and epilogue -------------------------------------------
public:
  Explosion collectParameters();
  void emitFunctionTopLevel(BraceStmt *S);
  void emitScalarReturn(Explosion &scalars);
private:
  void emitPrologue();
  void emitEpilogue();

  Address ReturnSlot;
  llvm::BasicBlock *ReturnBB;
  JumpDest getReturnDest();
  const TypeInfo &getResultTypeInfo() const;

//--- Helper methods -----------------------------------------------------------
public:
  OwnedAddress createFullExprAlloca(const TypeInfo &type, OnHeap_t onHeap,
                                    const llvm::Twine &name);
  OwnedAddress createScopeAlloca(const TypeInfo &type, OnHeap_t onHeap,
                                 const llvm::Twine &name);

  llvm::BasicBlock *createBasicBlock(const llvm::Twine &Name);
  const TypeInfo &getFragileTypeInfo(Type T);
  void emitMemCpy(llvm::Value *dest, llvm::Value *src,
                  Size size, Alignment align);
  void popCleanup();
private:
  llvm::Instruction *AllocaIP;

//--- Reference-counting methods -----------------------------------------------
public:
  void emitLoadAndRetain(Address addr, Explosion &explosion);
  void emitAssignRetained(llvm::Value *value, Address addr);
  void emitInitializeRetained(llvm::Value *value, Address addr);
  void emitRetain(llvm::Value *value, Explosion &explosion);
  void emitRelease(llvm::Value *value);
  void enterReleaseCleanup(llvm::Value *value);

//--- Statement emission -------------------------------------------------------
public:
  void emitStmt(Stmt *S);

private:
  void emitBraceStmt(BraceStmt *S);
  void emitAssignStmt(AssignStmt *S);
  void emitIfStmt(IfStmt *S);
  void emitReturnStmt(ReturnStmt *S);
  void emitWhileStmt(WhileStmt *S);

//--- Expression emission ------------------------------------------------------
public:
  void emitFakeExplosion(const TypeInfo &type, Explosion &explosion);
  LValue emitFakeLValue(const TypeInfo &type);

  void emitIgnored(Expr *E);

  LValue emitLValue(Expr *E);
  Optional<Address> tryEmitAsAddress(Expr *E, const TypeInfo &type);
  LValue emitAddressLValue(OwnedAddress addr);
  OwnedAddress emitAddressForPhysicalLValue(const LValue &lvalue);
  void emitLValueAsScalar(const LValue &lvalue, OnHeap_t onHeap,
                          Explosion &explosion);

  void emitRValueToMemory(Expr *E, Address addr, const TypeInfo &type);
  void emitRValue(Expr *E, Explosion &explosion);

  llvm::Value *emitAsPrimitiveScalar(Expr *E);

  void emitLoad(const LValue &lvalue, const TypeInfo &type,
                Explosion &explosion);
  void emitAssign(Explosion &rvalue, const LValue &lvalue,
                  const TypeInfo &type);
  void emitAssign(Expr *E, const LValue &lvalue, const TypeInfo &type);

  void emitInit(Address addr, Expr *E, const TypeInfo &type);
  void emitZeroInit(Address addr, const TypeInfo &type);

  enum ByrefKind { BK_byval, BK_byref, BK_byrefHeap };
  OwnedAddress getAddrForParameter(VarDecl *param, Explosion &paramValues);

private:
  void emitRValueForFunction(FuncDecl *Fn, Explosion &explosion);

  void emitApplyExpr(ApplyExpr *apply, Explosion &explosion);
  void emitNullaryCall(llvm::Value *fn, Type resultType,
                               Explosion &result);
  Optional<Address> tryEmitApplyAsAddress(ApplyExpr *apply, const TypeInfo &);
  void emitApplyExprToMemory(ApplyExpr *apply, Address addr,
                             const TypeInfo &type);

  void emitDeclRef(DeclRefExpr *DeclRef, Explosion &explosion);
  void emitOneOfElementRef(OneOfElementDecl *elt, Explosion &explosion);

  void emitLookThroughOneof(LookThroughOneofExpr *E, Explosion &expl);
  Optional<Address> tryEmitLookThroughOneofAsAddress(LookThroughOneofExpr *E);
  LValue emitLookThroughOneofLValue(LookThroughOneofExpr *E);

  void emitRequalify(RequalifyExpr *E, Explosion &expl);

  void emitTupleElement(TupleElementExpr *E, Explosion &explosion);
  Optional<Address> tryEmitTupleElementAsAddress(TupleElementExpr *E);
  LValue emitTupleElementLValue(TupleElementExpr *E);

  void emitTupleLiteral(TupleExpr *E, Explosion &explosion);
  void emitTupleShuffle(TupleShuffleExpr *E, Explosion &explosion);
  Condition emitCondition(Expr *E, bool hasFalseCode);

  void emitClosure(ClosureExpr *E, Explosion &explosion);
  void emitClosureBody(ClosureExpr *E);

//--- Declaration emission -----------------------------------------------------
public:
  void emitLocal(Decl *D);
  OwnedAddress getLocal(ValueDecl *D);
  LValue getGlobal(VarDecl *D);
  void setLocal(ValueDecl *D, OwnedAddress addr);

private:
  void emitLocalVar(VarDecl *D);

  struct LocalVarRecord {
    OwnedAddress Addr;
  };
  union LocalRecord {
    LocalVarRecord Var;

    LocalRecord() {}
  };
  llvm::DenseMap<ValueDecl*, LocalRecord> Locals;

//--- Global context emission --------------------------------------------------
public:
  void emitGlobalTopLevel(BraceStmt *S);
private:
  void emitGlobalDecl(Decl *D);
  void emitGlobalVariable(VarDecl *D);
};

} // end namespace irgen
} // end namespace swift

#endif
