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

struct PartialCall {
  CallEmission emission;
  unsigned remainingCurryLevels : 31;
  bool isDestructor : 1;
  
  PartialCall() = default;
  PartialCall(PartialCall const &) = default;
  PartialCall(PartialCall &&) = default;
  PartialCall &operator=(PartialCall const &) = default;
  PartialCall &operator=(PartialCall &&) = default;
  
  ~PartialCall() {
    // It's ok in SIL to reference a function without applying it, so suppress
    // ~CallEmission's "RemainingArgsForCallee == 0" sanity check.
    emission.invalidate();
  }
};
  
/// Represents a SIL value lowered to IR, in one of these forms:
/// - an Address, corresponding to a SIL address value;
/// - an Explosion of (unmanaged) Values, corresponding to a SIL "register"; or
/// - a CallEmission for a partially-applied curried function or method.
struct LoweredValue {
  enum class Kind {
    Address,
    Explosion,
    PartialCall
  };
  
  Kind kind;
  
private:
  union {
    Address address;
    Explosion explosion;
    PartialCall partialCall;
  };

public:
  LoweredValue(Address const &address)
    : kind(Kind::Address), address(address)
  {}
  
  LoweredValue(Explosion &&explosion)
    : kind(Kind::Explosion), explosion(std::move(explosion))
  {}
  
  LoweredValue(PartialCall &&call)
    : kind(Kind::PartialCall), partialCall(std::move(call))
  {}
  
  LoweredValue(LoweredValue &&lv)
    : kind(lv.kind)
  {
    switch (kind) {
    case Kind::Address:
      ::new (&address) Address(std::move(lv.address));
      break;
    case Kind::Explosion:
      ::new (&explosion) Explosion(std::move(lv.explosion));
      break;
    case Kind::PartialCall:
      ::new (&partialCall) PartialCall(std::move(lv.partialCall));
      break;
    }
  }
           
  Address getAddress() {
    assert(kind == Kind::Address && "not an address");
    return address;
  }
  
  Explosion &getExplosion() {
    assert(kind == Kind::Explosion && "not an explosion");
    return explosion;
  }
  
  PartialCall &getPartialCall() {
    assert(kind == Kind::PartialCall && "not a partial call");
    return partialCall;
  }
  
  ~LoweredValue() {
    switch (kind) {
    case Kind::Address:
      address.~Address();
      break;
    case Kind::Explosion:
      // Work around Explosion's "empty" sanity check by discarding all the
      // managed values (which should never actually be managed when generated
      // from SIL).
      explosion.claimAll();
      explosion.~Explosion();
      break;
    case Kind::PartialCall:
      partialCall.~PartialCall();
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
  Explosion &newLoweredExplosion(swift::Value v) {
    assert(!v.getType().isAddress() && "explosion for address value?!");
    auto inserted = loweredValues.insert({v, LoweredValue{
      Explosion(ExplosionKind::Minimal)
    }});
    assert(inserted.second && "already had lowered value for sil value?!");
    return inserted.first->second.getExplosion();
  }
  
  /// Create a new PartialCall corresponding to the given SIL value.
  PartialCall &newLoweredPartialCall(swift::Value v,
                                     SILConstant c,
                                     unsigned naturalCurryLevel,
                                     CallEmission &&emission) {
    auto inserted = loweredValues.insert({v, LoweredValue{
      PartialCall{
        std::move(emission),
        naturalCurryLevel+1,
        /*isDestructor=*/ c.isDestructor()
      }
    }});
    assert(inserted.second && "already had lowered value for sil value?!");
    return inserted.first->second.getPartialCall();
  }
  
  void moveLoweredPartialCall(swift::Value v, PartialCall parent) {
    auto inserted = loweredValues.insert({v, LoweredValue{std::move(parent)}});
    (void)inserted;
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
  Explosion &getLoweredExplosion(swift::Value v) {
    return getLoweredValue(v).getExplosion();
  }
  PartialCall getLoweredPartialCall(swift::Value v);
  
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
  void visitClosureInst(ClosureInst *i);
  //void visitSpecializeInst(SpecializeInst *i);

  void visitConstantRefInst(ConstantRefInst *i);

  void visitZeroValueInst(ZeroValueInst *i);
  void visitIntegerLiteralInst(IntegerLiteralInst *i);
  void visitFloatLiteralInst(FloatLiteralInst *i);
  void visitStringLiteralInst(StringLiteralInst *i);

  void visitLoadInst(LoadInst *i);
  void visitStoreInst(StoreInst *i);
  void visitTupleInst(TupleInst *i);
  void visitMetatypeInst(MetatypeInst *i);
  //void visitAssociatedMetatypeInst(AssociatedMetatypeInst *i);
  void visitExtractInst(ExtractInst *i);
  void visitElementAddrInst(ElementAddrInst *i);
  void visitRefElementAddrInst(RefElementAddrInst *i);

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

  void visitZeroAddrInst(ZeroAddrInst *i);
  void visitCopyAddrInst(CopyAddrInst *i);
  void visitDestroyAddrInst(DestroyAddrInst *i);

  void visitImplicitConvertInst(ImplicitConvertInst *i);
  void visitCoerceInst(CoerceInst *i);
  void visitUpcastInst(UpcastInst *i);
  void visitDowncastInst(DowncastInst *i);
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
