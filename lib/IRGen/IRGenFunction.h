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
#include "llvm/CallingConv.h"
#include "DiverseStack.h"
#include "IRBuilder.h"
#include "JumpDest.h"

namespace llvm {
  class AllocaInst;
  class CallSite;
  class Constant;
  class Function;
}

namespace swift {
  class ArchetypeType;
  class AssignStmt;
  class BraceStmt;
  class BreakStmt;
  class ClassType;
  class ConstructorDecl;
  class ContinueStmt;
  class Decl;
  class Expr;
  class ExtensionDecl;
  class ForEachStmt;
  class ForStmt;
  class FuncDecl;
  class IfStmt;
  class OneOfElementDecl;
  class OneOfType;
  template<typename T> class Optional;
  class Pattern;
  class PatternBindingDecl;
  class ReturnStmt;
  class SourceLoc;
  class Stmt;
  class StructType;
  class TranslationUnit;
  class ValueDecl;
  class VarDecl;
  class WhileStmt;
  class DoWhileStmt;

namespace irgen {
  class Condition;
  class Explosion;
  enum class ExplosionKind : unsigned;
  class HeapLayout;
  class IRGenModule;
  class LinkEntity;
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

  void enterDestroyCleanup(Address addr, const TypeInfo &addrTI,
                           Explosion &out);
  void enterDestroyCleanup(Address addr, const TypeInfo &addrTI);

  /// Is the current emission point conditionally evaluated?  Right
  /// now we don't have any expressions which introduce conditional
  /// evaluation, but it's not at all unlikely that this will change.
  bool isConditionallyEvaluated() const { return false; }

  llvm::Value *getJumpDestSlot();
  static Alignment getJumpDestAlignment() { return Alignment(4); }
  llvm::BasicBlock *getUnreachableBlock();

  llvm::CallSite emitInvoke(llvm::CallingConv::ID cc, llvm::Value *fn,
                            ArrayRef<llvm::Value*> args,
                            const llvm::AttrListPtr &attrs);

private:
  DiverseStack<Cleanup, 128> Cleanups;
  llvm::BasicBlock *UnreachableBB;
  llvm::Instruction *JumpDestSlot;
  CleanupsDepth InnermostScope;
  std::vector<JumpDest> BreakDestStack;
  std::vector<JumpDest> ContinueDestStack;

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
  void emitMemCpy(Address dest, Address src, Size size);

  llvm::Value *emitAllocObjectCall(llvm::Value *metadata, llvm::Value *size,
                                   llvm::Value *align,
                                   const llvm::Twine &name = "");
  void emitDeallocObjectCall(llvm::Value *pointer, llvm::Value *size);
  llvm::Value *emitAllocRawCall(llvm::Value *size, llvm::Value *align,
                                const llvm::Twine &name ="");
  void emitDeallocRawCall(llvm::Value *pointer, llvm::Value *size);
private:
  llvm::Instruction *AllocaIP;

//--- Reference-counting methods -----------------------------------------------
public:
  ManagedValue emitAlloc(const HeapLayout &layout, const llvm::Twine &name);
  llvm::Value *emitUnmanagedAlloc(const HeapLayout &layout,
                                  const llvm::Twine &name);
  CleanupsDepth pushDeallocCleanup(llvm::Value *allocation,
                                   llvm::Value *size);
  void emitLoadAndRetain(Address addr, Explosion &explosion);
  void emitAssignRetained(llvm::Value *value, Address addr);
  void emitInitializeRetained(llvm::Value *value, Address addr);
  void emitRetain(llvm::Value *value, Explosion &explosion);
  void emitRetainCall(llvm::Value *value);
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
  void emitDoWhileStmt(DoWhileStmt *S);
  void emitForStmt(ForStmt *S);
  void emitForEachStmt(ForEachStmt *S);
  void emitBreakStmt(BreakStmt *S);
  void emitContinueStmt(ContinueStmt *S);

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
  void emitLValueAsScalar(LValue &&lvalue, OnHeap_t onHeap,
                          Explosion &explosion);
  Address emitMaterializeWithWriteback(LValue &&lvalue, OnHeap_t onHeap);

  void emitRValueAsInit(Expr *E, Address addr, const TypeInfo &type);
  void emitRValue(Expr *E, Explosion &explosion);

  llvm::Value *emitAsPrimitiveScalar(Expr *E);

  void emitLoad(const LValue &lvalue, const TypeInfo &type,
                Explosion &explosion);
  void emitAssign(Expr *E, const LValue &lvalue, const TypeInfo &type);
  void emitAssign(Explosion &explosion, const LValue &lvalue,
                  const TypeInfo &type);

  OwnedAddress getAddrForParameter(VarDecl *param, Explosion &paramValues);

  void emitNullaryCall(llvm::Value *fn, Type resultType, Explosion &result);

  Condition emitCondition(Expr *E, bool hasFalseCode, bool invertValue = false);

  void constructObject(Address addr, ConstructorDecl *CD, Expr *Input);

//--- Declaration emission -----------------------------------------------------
public:
  void emitLocal(Decl *D);
  LValue getGlobal(VarDecl *D);
  OwnedAddress getLocalVar(VarDecl *D);
  void setLocalVar(VarDecl *D, OwnedAddress addr);
  void emitPatternBindingDecl(PatternBindingDecl *D);

  llvm::Value *getLocalFuncData(FuncDecl *fn);
  IRGenFunction *getLocalFuncDefiner(FuncDecl *func);
  void setLocalFuncData(FuncDecl *fn, llvm::Value *data,
                        IRGenFunction *definingIGF);
  void emitLocalFunction(FuncDecl *func);

  llvm::Function *getAddrOfLocalFunction(FuncDecl *func,
                                         ExplosionKind explosionLevel,
                                         unsigned uncurryLevel);

  void bindArchetype(ArchetypeType *type, llvm::Value *wtable);

private:
  union LocalEntry {
    struct {
      OwnedAddress Addr;
    } Var;
    struct {
      llvm::Value *Data;
      IRGenFunction *Definer;
    } Func;

    LocalEntry() {}
  };
  llvm::DenseMap<Decl*, LocalEntry> Locals;

//--- Global context emission --------------------------------------------------
public:
  void emitGlobalTopLevel(TranslationUnit *TU, unsigned StartElem);
private:
  void emitGlobalDecl(Decl *D);
};

} // end namespace irgen
} // end namespace swift

#endif
