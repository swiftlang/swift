//===--- IRGenSIL.h - IR Generation from SIL --------------------*- C++ -*-===//
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
// function from its SIL representation.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_IRGENSIL_H
#define SWIFT_IRGEN_IRGENSIL_H

#include "swift/Basic/LLVM.h"
#include "swift/AST/Type.h"
#include "swift/SIL/SILVisitor.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/IR/CallingConv.h"
#include "CallEmission.h"
#include "IRBuilder.h"
#include "IRGenFunction.h"
#include "JumpDest.h"
#include <map>

namespace swift {
namespace irgen {

/// Represents a SIL value lowered to IR, in one of these forms:
/// - an Address, corresponding to a SIL address value;
/// - an Explosion of (unmanaged) Values, corresponding to a SIL "register"; or
/// - a CallEmission for a partially-applied curried function or method.
class LoweredValue {
public:
  enum class Kind {
    Address,
    Explosion,
  };
  
  Kind kind;
  
private:
  using ExplosionVector = llvm::SmallVector<llvm::Value*, 4>;
  
  union {
    Address address;
    struct {
      ExplosionKind kind;
      ExplosionVector values;
    } explosion;
  };

public:
  LoweredValue(Address const &address)
    : kind(Kind::Address), address(address)
  {}
  
  LoweredValue(Explosion &e)
    : kind(Kind::Explosion),
      explosion{e.getKind(), {}}
  {
    e.claimUnmanaged(e.size(), explosion.values);
  }
  
  /// This is a hack to kill off cleanups emitted by some IRGen infrastructure.
  /// SIL code should always have memory management within it explicitly lowered.
  LoweredValue(Explosion &e, IRGenFunction &IGF)
    : kind(Kind::Explosion),
      explosion{e.getKind(), {}}
  {
    e.forward(IGF, e.size(), explosion.values);
  }
  
  LoweredValue(LoweredValue &&lv)
    : kind(lv.kind)
  {    
    switch (kind) {
    case Kind::Address:
      ::new (&address) Address(std::move(lv.address));
      break;
    case Kind::Explosion:
      explosion.kind = lv.explosion.kind;
      ::new (&explosion.values) ExplosionVector(std::move(lv.explosion.values));
      break;
    }
  }
           
  Address getAddress() {
    assert(kind == Kind::Address && "not an address");
    return address;
  }
  
  void getExplosion(Explosion &ex) {
    assert(kind == Kind::Explosion && "not an explosion");
    assert(ex.getKind() == explosion.kind &&
           "destination explosion kind mismatch");
    for (auto *value : explosion.values)
      ex.addUnmanaged(value);
  }
  
  Explosion getExplosion() {
    assert(kind == Kind::Explosion && "not an explosion");
    Explosion e(explosion.kind);
    getExplosion(e);
    return e;
  }
  
  ExplosionKind getExplosionKind() {
    assert(kind == Kind::Explosion && "not an explosion");
    return explosion.kind;
  }
  
  ~LoweredValue() {
    switch (kind) {
    case Kind::Address:
      address.~Address();
      break;
    case Kind::Explosion:
      explosion.values.~ExplosionVector();
      break;
    }
  }
};
  
/// Represents a lowered SIL basic block. This keeps track^W^Wwill keep track
/// of SIL branch arguments so that they can be lowered to LLVM phi nodes.
struct LoweredBB {
  llvm::BasicBlock *bb;
  
  LoweredBB() = default;
  explicit LoweredBB(llvm::BasicBlock *bb) : bb(bb) {}
};

/// Visits a SIL Function and generates LLVM IR.
class IRGenSILFunction :
  public IRGenFunction, public SILVisitor<IRGenSILFunction>
{
public:
  llvm::DenseMap<swift::Value, LoweredValue> loweredValues;
  llvm::MapVector<swift::BasicBlock *, LoweredBB> loweredBBs;
  
  SILConstant CurConstant;
  swift::Function *CurSILFn;
  
  IRGenSILFunction(IRGenModule &IGM,
                   CanType t,
                   ExplosionKind explosionLevel,
                   llvm::Function *fn);
  ~IRGenSILFunction();
  
  /// Generate IR for the given SIL Function.
  void emitSILFunction(SILConstant c, swift::Function *f);

  /// Generate code from the global toplevel. This will emit all the
  /// declarations in the given translation unit along with the toplevel
  /// of the given SILModule.
  void emitGlobalTopLevel(TranslationUnit *TU,
                          SILModule *SILMod);
  
  /// Generate local decls in the given function body. This skips VarDecls and
  /// other locals that are consumed by SIL.
  void emitLocalDecls(BraceStmt *body);
  
  /// Create a new Address corresponding to the given SIL address value.
  void newLoweredAddress(swift::Value v, Address const &address) {
    assert(v.getType().isAddress() && "address for non-address value?!");
    auto inserted = loweredValues.insert({v, address});
    assert(inserted.second && "already had lowered value for sil value?!");
    (void)inserted;
  }
  
  /// Create a new Explosion corresponding to the given SIL value.
  void newLoweredExplosion(swift::Value v, Explosion &e) {
    assert(!v.getType().isAddress() && "explosion for address value?!");
    auto inserted = loweredValues.insert({v, LoweredValue(e)});
    assert(inserted.second && "already had lowered value for sil value?!");
  }
  
  /// Create a new Explosion corresponding to the given SIL value, disabling
  /// cleanups on the input Explosion if necessary.
  void newLoweredExplosion(swift::Value v, Explosion &e, IRGenFunction &IGF) {
    assert(!v.getType().isAddress() && "explosion for address value?!");
    auto inserted = loweredValues.insert({v, LoweredValue(e, IGF)});
    assert(inserted.second && "already had lowered value for sil value?!");
  }
  
  /// Get the Explosion corresponding to the given SIL value, which must
  /// previously exist.
  LoweredValue &getLoweredValue(swift::Value v) {
    auto foundValue = loweredValues.find(v);
    assert(foundValue != loweredValues.end() &&
           "no lowered explosion for sil value!");
    return foundValue->second;
  }
  
  Address getLoweredAddress(swift::Value v) {
    return getLoweredValue(v).getAddress();
  }
  void getLoweredExplosion(swift::Value v, Explosion &e) {
    getLoweredValue(v).getExplosion(e);
  }
  Explosion getLoweredExplosion(swift::Value v) {
    return getLoweredValue(v).getExplosion();
  }
  ExplosionKind getExplosionKind(swift::Value v) {
    return getLoweredValue(v).getExplosionKind();
  }
  
  LoweredBB &getLoweredBB(swift::BasicBlock *bb) {
    auto foundBB = loweredBBs.find(bb);
    assert(foundBB != loweredBBs.end() && "no llvm bb for sil bb?!");
    return foundBB->second;
  }
  
  //===--------------------------------------------------------------------===//
  // SIL instruction lowering
  //===--------------------------------------------------------------------===//

  void visitBasicBlock(BasicBlock *BB);
  
  void visitInstruction(Instruction *i) {
    i->dump();
    llvm_unreachable("irgen for SIL instruction not yet implemented");
  }
  
  void visitAllocVarInst(AllocVarInst *i);
  void visitAllocRefInst(AllocRefInst *i);
  void visitAllocBoxInst(AllocBoxInst *i);
  void visitAllocArrayInst(AllocArrayInst *i);

  void visitApplyInst(ApplyInst *i);
  void visitPartialApplyInst(PartialApplyInst *i);
  //void visitSpecializeInst(SpecializeInst *i);

  void visitConstantRefInst(ConstantRefInst *i);

  void visitIntegerLiteralInst(IntegerLiteralInst *i);
  void visitFloatLiteralInst(FloatLiteralInst *i);
  void visitStringLiteralInst(StringLiteralInst *i);

  void visitLoadInst(LoadInst *i);
  void visitStoreInst(StoreInst *i);
  void visitTupleInst(TupleInst *i);
  void visitMetatypeInst(MetatypeInst *i);
  void visitClassMetatypeInst(ClassMetatypeInst *i);
  //void visitAssociatedMetatypeInst(AssociatedMetatypeInst *i);
  void visitExtractInst(ExtractInst *i);
  void visitElementAddrInst(ElementAddrInst *i);
  void visitRefElementAddrInst(RefElementAddrInst *i);

  void visitClassMethodInst(ClassMethodInst *i);
  void visitSuperMethodInst(SuperMethodInst *i);
  //void visitArchetypeMethodInst(ArchetypeMethodInst *i);
  void visitProtocolMethodInst(ProtocolMethodInst *i);
  
  void visitProjectExistentialInst(ProjectExistentialInst *i);
  void visitInitExistentialInst(InitExistentialInst *i);
  void visitUpcastExistentialInst(UpcastExistentialInst *i);
  //void visitDeinitExistentialInst(DeinitExistentialInst *i);

  void visitRetainInst(RetainInst *i);
  void visitReleaseInst(ReleaseInst *i);
  void visitDeallocVarInst(DeallocVarInst *i);
  //void visitDeallocRefInst(DeallocRefInst *i);

  void visitInitializeVarInst(InitializeVarInst *i);
  void visitCopyAddrInst(CopyAddrInst *i);
  void visitDestroyAddrInst(DestroyAddrInst *i);

  void visitImplicitConvertInst(ImplicitConvertInst *i);
  void visitCoerceInst(CoerceInst *i);
  void visitUpcastInst(UpcastInst *i);
  void visitDowncastInst(DowncastInst *i);
  void visitAddressToPointerInst(AddressToPointerInst *i);
  //void visitArchetypeToSuperInst(ArchetypeToSuperInst *i);
  //void visitSuperToArchetypeInst(SuperToArchetypeInst *i);

  void visitIndexAddrInst(IndexAddrInst *i);
  void visitIntegerValueInst(IntegerValueInst *i);
  
  void visitUnreachableInst(UnreachableInst *i);
  void visitReturnInst(ReturnInst *i);
  void visitBranchInst(BranchInst *i);
  void visitCondBranchInst(CondBranchInst *i);
};

} // end namespace irgen
} // end namespace swift

#endif
