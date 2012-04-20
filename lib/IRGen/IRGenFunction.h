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
#include "DiverseStack.h"
#include "IRBuilder.h"

namespace llvm {
  class AllocaInst;
  class Constant;
  class Function;
}

namespace swift {
  class AssignStmt;
  class BraceStmt;
  class Decl;
  class Expr;
  class ForStmt;
  class IfStmt;
  template<typename T> class Optional;
  class Pattern;
  class PatternBindingDecl;
  class ReturnStmt;
  class SourceLoc;
  class Stmt;
  class TranslationUnit;
  class ValueDecl;
  class VarDecl;
  class WhileStmt;

namespace irgen {
  class Cleanup;
  class Condition;
  class Explosion;
  enum class ExplosionKind : unsigned;
  class HeapLayout;
  class IRGenModule;
  class JumpDest;
  class LValue;
  class ManagedValue;
  class Scope;
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

  // Standard prologue/epilogue, but the function uses the context pointer.
  StandardWithContext,

  /// The bare prologue is useful for emitting small functions that
  /// don't need proper statement/expression emission.  It implies:
  ///   - creating the entry block
  ///   - setting up and tearing down the alloca point
  /// Functions using this prologue style are responsible for emitting
  /// the 'ret' instruction before epilogue emission.
  Bare
};

/// The valid states that a cleanup can be in.
enum class CleanupState {
  /// The cleanup is inactive but may be activated later.
  Dormant,

  /// The cleanup is currently active.
  Active,

  /// The cleanup is inactive and will not be activated later.
  Dead
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
  llvm::Value *ContextPtr;

  IRGenFunction(IRGenModule &IGM, Type t, ArrayRef<Pattern*> p,
                ExplosionKind explosion,
                unsigned uncurryLevel, llvm::Function *fn,
                Prologue prologue = Prologue::Standard);
  ~IRGenFunction();

  void unimplemented(SourceLoc Loc, StringRef Message);

//--- Control flow -------------------------------------------------------------
public:
  void emitBranch(JumpDest D);

  /// Push a new cleanup in the current scope.
  template <class T, class... A>
  T &pushCleanup(A &&... args) {
    return pushCleanupInState<T, A...>(CleanupState::Active,
                                       ::std::forward<A>(args)...);
  }

  /// Push a new cleanup in the current scope.
  template <class T, class... A>
  T &pushCleanupInState(CleanupState state, A &&... args) {
    assert(state != CleanupState::Dead);

#ifndef NDEBUG
    CleanupsDepth oldTop = Cleanups.stable_begin();
#endif

    T &cleanup = Cleanups.push<T, A...>(::std::forward<A>(args)...);
    T &result = static_cast<T&>(initCleanup(cleanup, sizeof(T), state));

#ifndef NDEBUG
    auto newTop = Cleanups.begin(); ++newTop;
    assert(newTop == Cleanups.find(oldTop));
#endif
    return result;
  }

  /// Push a new cleanup which is expected to be destroyed at the end
  /// of the current full-expression.
  ///
  /// The relevant property here is that full-expression cleanups may
  /// not be dominated by the locations in which they're active in a
  /// full-expression expression.
  template <class T, class... A>
  T &pushFullExprCleanup(A &&... args) {
    assert(!isConditionallyEvaluated());
    return pushCleanup<T, A...>(::std::forward<A>(args)...);
  }

  template <class T, class... A>
  T &pushFullExprCleanupInState(CleanupState state, A &&... args) {
    assert(!isConditionallyEvaluated());
    return pushCleanupInState<T, A...>(state, ::std::forward<A>(args)...);
  }

  typedef DiverseStackImpl<Cleanup>::stable_iterator CleanupsDepth;

  /// Retun a stable reference to the current cleanup.
  CleanupsDepth getCleanupsDepth() const {
    return Cleanups.stable_begin();
  }

  /// Set the state of the cleanup at the given depth.
  /// The transition must be non-trivial and legal.
  void setCleanupState(CleanupsDepth depth, CleanupState state);

  Cleanup &findCleanup(CleanupsDepth depth) {
    assert(depth != Cleanups.stable_end());
    return *Cleanups.find(depth);
  }

  void endScope(CleanupsDepth depth);
  void endSingleCleanupScope();

  /// Is the current emission point conditionally evaluated?  Right
  /// now we don't have any expressions which introduce conditional
  /// evaluation, but it's not at all unlikely that this will change.
  bool isConditionallyEvaluated() const { return false; }

  llvm::Value *getJumpDestSlot();
  static Alignment getJumpDestAlignment() { return Alignment(4); }
  llvm::BasicBlock *getUnreachableBlock();

private:
  DiverseStack<Cleanup, 128> Cleanups;
  llvm::BasicBlock *UnreachableBB;
  llvm::Instruction *JumpDestSlot;
  CleanupsDepth InnermostScope;

  friend class Scope;
  Cleanup &initCleanup(Cleanup &cleanup, size_t allocSize, CleanupState state);
  void setCleanupState(Cleanup &cleanup, CleanupState state);

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
  const TypeInfo &getResultTypeInfo() const;

//--- Helper methods -----------------------------------------------------------
public:
  Address createAlloca(llvm::Type *ty, Alignment align,
                       const llvm::Twine &name);
  llvm::AllocaInst *createSupportAlloca(llvm::Type *type, Alignment align,
                                        const llvm::Twine &name);

  llvm::BasicBlock *createBasicBlock(const llvm::Twine &Name);
  const TypeInfo &getFragileTypeInfo(Type T);
  void emitMemCpy(llvm::Value *dest, llvm::Value *src,
                  Size size, Alignment align);
private:
  llvm::Instruction *AllocaIP;

//--- Reference-counting methods -----------------------------------------------
public:
  ManagedValue emitAlloc(const HeapLayout &layout, const llvm::Twine &name);
  llvm::Value *emitUnmanagedAlloc(const HeapLayout &layout,
                                  const llvm::Twine &name);
  CleanupsDepth pushDeallocCleanup(llvm::Value *allocation);
  void emitLoadAndRetain(Address addr, Explosion &explosion);
  void emitAssignRetained(llvm::Value *value, Address addr);
  void emitInitializeRetained(llvm::Value *value, Address addr);
  void emitRetain(llvm::Value *value, Explosion &explosion);
  void emitRelease(llvm::Value *value);
  ManagedValue enterReleaseCleanup(llvm::Value *value);

//--- Statement emission -------------------------------------------------------
public:
  void emitStmt(Stmt *S);

private:
  void emitBraceStmt(BraceStmt *S);
  void emitAssignStmt(AssignStmt *S);
  void emitIfStmt(IfStmt *S);
  void emitReturnStmt(ReturnStmt *S);
  void emitWhileStmt(WhileStmt *S);
  void emitForStmt(ForStmt *S);

//--- Expression emission ------------------------------------------------------
public:
  void emitFakeExplosion(const TypeInfo &type, Explosion &explosion);
  LValue emitFakeLValue(const TypeInfo &type);

  void emitIgnored(Expr *E);

  void emitInit(Expr *E, Address address, const TypeInfo &type);

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

  void emitPatternBindingInit(Pattern *P, Expr *E, bool isGlobal);
  void emitPatternBindingInit(Pattern *P, Explosion &E, bool isGlobal);

  OwnedAddress getAddrForParameter(VarDecl *param, Explosion &paramValues);

  void emitNullaryCall(llvm::Value *fn, Type resultType,
                               Explosion &result);

private:
  Condition emitCondition(Expr *E, bool hasFalseCode);

//--- Declaration emission -----------------------------------------------------
public:
  void emitLocal(Decl *D);
  OwnedAddress getLocal(ValueDecl *D);
  LValue getGlobal(VarDecl *D);
  void setLocal(ValueDecl *D, OwnedAddress addr);
  void emitPatternBindingDecl(PatternBindingDecl *D);

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
  void emitGlobalTopLevel(TranslationUnit *TU, unsigned StartElem);
private:
  void emitGlobalDecl(Decl *D);
};

} // end namespace irgen
} // end namespace swift

#endif
