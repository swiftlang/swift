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

#include "swift/Basic/LLVM.h"
#include "swift/AST/Type.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/IR/CallingConv.h"
#include "IRBuilder.h"
#include "swift/Basic/DiverseStack.h"


namespace llvm {
  class AllocaInst;
  class CallSite;
  class Constant;
  class Function;
}

namespace swift {
  class ArchetypeType;
  class ClassDecl;
  class ConstructorDecl;
  class Decl;
  class ExtensionDecl;
  class FuncDecl;
  class OneOfElementDecl;
  class OneOfType;
  template<typename T> class Optional;
  class Pattern;
  class PatternBindingDecl;
  class SourceLoc;
  class StructType;
  class Substitution;
  class TranslationUnit;
  class ValueDecl;
  class VarDecl;

namespace irgen {
  class Explosion;
  enum class ExplosionKind : unsigned;
  class FunctionRef;
  class HeapLayout;
  class IRGenModule;
  class LinkEntity;
   class ManagedValue;
  class Scope;
  class TypeInfo;
  class Cleanup;
  typedef DiverseStackImpl<Cleanup>::stable_iterator CleanupsDepth;


/// LocalTypeData - A nonce value for storing some sort of
/// locally-known information about a type.
/// 
/// The enumerated values are all in the "negative" range and so do
/// not collide with reasonable index values.
enum class LocalTypeData : unsigned {
  /// A reference to a metatype.
  Metatype = ~0U
};

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

  CanType CurFuncType;
  ArrayRef<Pattern*> CurFuncParamPatterns;
  llvm::Function *CurFn;
  ExplosionKind CurExplosionLevel;
  unsigned CurUncurryLevel;
  Prologue CurPrologue;
  llvm::Value *ContextPtr;

  IRGenFunction(IRGenModule &IGM, CanType t, ArrayRef<Pattern*> p,
                ExplosionKind explosion,
                unsigned uncurryLevel, llvm::Function *fn,
                Prologue prologue = Prologue::Standard);
  ~IRGenFunction();

  void unimplemented(SourceLoc Loc, StringRef Message);

//--- Control flow -------------------------------------------------------------
public:
  void emitBranch(llvm::BasicBlock *block, CleanupsDepth depth);

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

  /// Return a stable reference to the current cleanup.
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
                            const llvm::AttributeSet &attrs);

private:
  DiverseStack<Cleanup, 128> Cleanups;
  llvm::BasicBlock *UnreachableBB;
  llvm::Instruction *JumpDestSlot;
  CleanupsDepth InnermostScope;
  
  friend class Cleanup; // just so that it can befriend initCleanup
  friend class Scope;
  Cleanup &initCleanup(Cleanup &cleanup, size_t allocSize, CleanupState state);
  void setCleanupState(Cleanup &cleanup, CleanupState state);

//--- Function prologue and epilogue -------------------------------------------
public:
  Explosion collectParameters();
  void emitScalarReturn(Explosion &scalars);
  
  void emitBBForReturn();
  bool emitBranchToReturnBB();
  
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
  const TypeInfo &getFragileTypeInfo(CanType T);
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
  
  void emitAllocBoxCall(llvm::Value *typeMetadata,
                        llvm::Value *&box,
                        llvm::Value *&valueAddress);
  void emitDeallocBoxCall(llvm::Value *box, llvm::Value *type);
  
private:
  llvm::Instruction *AllocaIP;

//--- Reference-counting methods -----------------------------------------------
public:
  llvm::Value *emitUnmanagedAlloc(const HeapLayout &layout,
                                  const llvm::Twine &name);
  CleanupsDepth pushDeallocCleanup(llvm::Value *allocation,
                                   llvm::Value *size);
  void emitLoadAndRetain(Address addr, Explosion &explosion);
  void emitAssignRetained(llvm::Value *value, Address addr);
  void emitInitializeRetained(llvm::Value *value, Address addr);
  void emitRetain(llvm::Value *value, Explosion &explosion);
  void emitRetainCall(llvm::Value *value);
  llvm::Value *emitBestRetainCall(llvm::Value *value, ClassDecl *theClass);
  void emitRelease(llvm::Value *value);
  ManagedValue enterReleaseCleanup(llvm::Value *value);
  void emitObjCRetain(llvm::Value *value, Explosion &explosion);
  llvm::Value *emitObjCRetainCall(llvm::Value *value);
  void emitObjCRelease(llvm::Value *value);
  ManagedValue enterObjCReleaseCleanup(llvm::Value *value);

//--- Expression emission ------------------------------------------------------
public:
  void emitFakeExplosion(const TypeInfo &type, Explosion &explosion);

  /// \brief Convert the given explosion to the given destination archetype,
  /// using a runtime-checked cast.
  void emitSupertoArchetypeConversion(Explosion &input,
                                    CanType destType, Address outputArchetype);

  /// \brief Convert the given value to the given destination type, using a
  /// runtime-checked cast.
  llvm::Value *emitUnconditionalDowncast(llvm::Value *from,
                                         CanType toType);
  

  OwnedAddress getAddrForParameter(VarDecl *param, Explosion &paramValues);

  void emitNullaryCall(llvm::Value *fn, CanType resultType, Explosion &result);

//--- Declaration emission -----------------------------------------------------
public:
  OwnedAddress getLocalVar(VarDecl *D);
  void setLocalVar(VarDecl *D, OwnedAddress addr);

  llvm::Value *getLocalFuncData(FuncDecl *fn);
  IRGenFunction *getLocalFuncDefiner(FuncDecl *func);
  void setLocalFuncData(FuncDecl *fn, llvm::Value *data,
                        IRGenFunction *definingIGF);

  void bindArchetype(ArchetypeType *type,
                     llvm::Value *metadata,
                     ArrayRef<llvm::Value*> wtables);

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
  void emitExternalDefinition(Decl *D);

//--- Type emission ------------------------------------------------------------
public:
  /// Look for a mapping for a local type-metadata reference.
  llvm::Value *tryGetLocalTypeData(CanType type, LocalTypeData index) {
    auto key = getLocalTypeDataKey(type, index);
    auto it = LocalTypeDataMap.find(key);
    if (it == LocalTypeDataMap.end())
      return nullptr;
    return it->second;
  }

  /// Retrieve a local type-metadata reference which is known to exist.
  llvm::Value *getLocalTypeData(CanType type, LocalTypeData index) {
    auto key = getLocalTypeDataKey(type, index);
    assert(LocalTypeDataMap.count(key) && "no mapping for local type data");
    return LocalTypeDataMap.find(key)->second;
  }

  /// Add a local type-metadata reference at a point which dominates
  /// the entire function.
  void setUnscopedLocalTypeData(CanType type, LocalTypeData index,
                                llvm::Value *data) {
    assert(data && "setting a null value for type data!");

    auto key = getLocalTypeDataKey(type, index);
    assert(!LocalTypeDataMap.count(key) &&
           "existing mapping for local type data");
    LocalTypeDataMap.insert(std::make_pair(key, data));
  }

  /// Add a local type-metadata reference at a point which does not
  /// necessarily dominate the entire function.
  void setScopedLocalTypeData(CanType type, LocalTypeData index,
                              llvm::Value *data) {
    ScopedLocalTypeData.push_back(getLocalTypeDataKey(type, index));
    setUnscopedLocalTypeData(type, index, data);
  }

private:
  typedef unsigned LocalTypeDataDepth;
  typedef std::pair<TypeBase*,unsigned> LocalTypeDataPair;
  LocalTypeDataPair getLocalTypeDataKey(CanType type, LocalTypeData index) {
    return LocalTypeDataPair(type.getPointer(), unsigned(index));
  }

  llvm::DenseMap<LocalTypeDataPair, llvm::Value*> LocalTypeDataMap;
  llvm::SmallVector<LocalTypeDataPair, 4> ScopedLocalTypeData;

  void endLocalTypeDataScope(LocalTypeDataDepth depth) {
    assert(ScopedLocalTypeData.size() >= depth);
    while (ScopedLocalTypeData.size() != depth) {
      LocalTypeDataMap.erase(ScopedLocalTypeData.back());
      ScopedLocalTypeData.pop_back();
    }
  }
};

} // end namespace irgen
} // end namespace swift

#endif
