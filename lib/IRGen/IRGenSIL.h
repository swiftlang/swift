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
#include "GenObjC.h"
#include "IRBuilder.h"
#include "IRGenFunction.h"
#include "JumpDest.h"
#include <map>

namespace swift {
namespace irgen {

/// Represents a statically-known function as a SIL thin function value.
class StaticFunction {
  /// The function reference.
  llvm::Function *function;
  /// The function's native calling convention.
  AbstractCC cc;
  
public:
  StaticFunction(llvm::Function *function, AbstractCC cc)
    : function(function), cc(cc)
  {}
  
  llvm::Function *getFunction() const { return function; }
  AbstractCC getCC() const { return cc; }
  
  llvm::Value *getExplosionValue(IRGenFunction &IGF) const;
};
  
/// Represents an ObjC method reference that will be invoked by a form of
/// objc_msgSend.
class ObjCMethod {
  /// The ValueDecl declaring the method.
  ValueDecl *method;
  /// For a super call, the type to pass to msgSendSuper2 dispatch.
  /// Null for non-super calls.
  CanType superSearchType;

public:
  ObjCMethod(ValueDecl *method, CanType superSearchType)
    : method(method), superSearchType(superSearchType)
  {}
  
  ValueDecl *getMethodDecl() const { return method; }
  CanType getSuperSearchType() const { return superSearchType; }
  
  /// FIXME: Thunk down to a Swift function value?
  llvm::Value *getExplosionValue(IRGenFunction &IGF) const {
    llvm_unreachable("thunking unapplied objc method to swift function "
                     "not yet implemented");
  }
};

/// Represents a SIL value lowered to IR, in one of these forms:
/// - an Address, corresponding to a SIL address value;
/// - an Explosion of (unmanaged) Values, corresponding to a SIL "register"; or
/// - a CallEmission for a partially-applied curried function or method.
class LoweredValue {
public:
  enum class Kind {
    /// This LoweredValue corresponds to a SIL address value.
    Address,
    
    /// The following kinds correspond to SIL non-address values.
    Value_First,
      /// A normal value, represented as an exploded array of llvm Values.
      Explosion = Value_First,
    
      /// A value that represents a statically-known function symbol that
      /// can be called directly, represented as a StaticFunction.
      StaticFunction,
    
      /// A value that represents an Objective-C method that must be called with
      /// a form of objc_msgSend.
      ObjCMethod,
    Value_Last = ObjCMethod
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
    StaticFunction staticFunction;
    ObjCMethod objcMethod;
  };

public:
  LoweredValue(Address const &address)
    : kind(Kind::Address), address(address)
  {}
  
  LoweredValue(StaticFunction &&staticFunction)
    : kind(Kind::StaticFunction), staticFunction(std::move(staticFunction))
  {}

  LoweredValue(ObjCMethod &&objcMethod)
    : kind(Kind::ObjCMethod), objcMethod(std::move(objcMethod))
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
    case Kind::StaticFunction:
      ::new (&staticFunction) StaticFunction(std::move(lv.staticFunction));
      break;
    case Kind::ObjCMethod:
      ::new (&objcMethod) ObjCMethod(std::move(lv.objcMethod));
    }
  }
  
  bool isAddress() const { return kind == Kind::Address; }
  bool isValue() const {
    return kind >= Kind::Value_First && kind <= Kind::Value_Last;
  }
  
  Address getAddress() const {
    assert(kind == Kind::Address && "not an address");
    return address;
  }
  
  void getExplosion(IRGenFunction &IGF, Explosion &ex) const;
  
  ExplosionKind getExplosionKind() const;
  
  Explosion getExplosion(IRGenFunction &IGF) const {
    Explosion e(getExplosionKind());
    getExplosion(IGF, e);
    return e;
  }
  
  StaticFunction const &getStaticFunction() const {
    assert(kind == Kind::StaticFunction && "not a static function");
    return staticFunction;
  }
  
  ObjCMethod const &getObjCMethod() const {
    assert(kind == Kind::ObjCMethod && "not an objc method");
    return objcMethod;
  }
  
  ~LoweredValue() {
    switch (kind) {
    case Kind::Address:
      address.~Address();
      break;
    case Kind::Explosion:
      explosion.values.~ExplosionVector();
      break;
    case Kind::StaticFunction:
      staticFunction.~StaticFunction();
      break;
    case Kind::ObjCMethod:
      objcMethod.~ObjCMethod();
    }
  }
};
  
/// Represents a lowered SIL basic block. This keeps track^W^Wwill keep track
/// of SIL branch arguments so that they can be lowered to LLVM phi nodes.
struct LoweredBB {
  llvm::BasicBlock *bb;
  std::vector<llvm::PHINode*> phis;
  
  LoweredBB() = default;
  explicit LoweredBB(llvm::BasicBlock *bb,
                     std::vector<llvm::PHINode*> &&phis)
    : bb(bb), phis(std::move(phis))
  {}
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
  Address IndirectReturn;
  
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
  
  /// Create a new StaticFunction corresponding to the given SIL value.
  void newLoweredStaticFunction(swift::Value v,
                                llvm::Function *f,
                                AbstractCC cc) {
    assert(!v.getType().isAddress() && "function for address value?!");
    assert(v.getType().is<AnyFunctionType>() &&
           "function for non-function value?!");
    auto inserted = loweredValues.insert({v, StaticFunction{f, cc}});
    assert(inserted.second && "already had lowered value for sil value?!");
  }
  
  void newLoweredObjCMethod(swift::Value v, ValueDecl *method,
                            CanType superSearchType = CanType()) {
    assert(!v.getType().isAddress() && "function for address value?!");
    assert(v.getType().is<AnyFunctionType>() &&
           "function for non-function value?!");
    auto inserted = loweredValues.insert({v,
                                          ObjCMethod{method, superSearchType}});
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
    getLoweredValue(v).getExplosion(*this, e);
  }
  Explosion getLoweredExplosion(swift::Value v) {
    return getLoweredValue(v).getExplosion(*this);
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

  void visitConvertFunctionInst(ConvertFunctionInst *i);
  void visitCoerceInst(CoerceInst *i);
  void visitUpcastInst(UpcastInst *i);
  void visitDowncastInst(DowncastInst *i);
  void visitAddressToPointerInst(AddressToPointerInst *i);
  void visitThinToThickFunctionInst(ThinToThickFunctionInst *i);
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
