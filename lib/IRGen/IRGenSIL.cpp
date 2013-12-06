//===--- IRGenSIL.cpp - Swift Per-Function IR Generation ------------------===//
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
//  This file implements basic setup and teardown for the class which
//  performs IR generation for function bodies.
//
//===----------------------------------------------------------------------===//

#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/Support/Debug.h"
#include "swift/Basic/Range.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/STLExtras.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/Stmt.h"
#include "swift/IRGen/Options.h"
#include "swift/SIL/PrettyStackTrace.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILType.h"
#include "swift/SIL/SILVisitor.h"

#include "CallEmission.h"
#include "Explosion.h"
#include "GenClass.h"
#include "GenFunc.h"
#include "GenHeap.h"
#include "GenMeta.h"
#include "GenObjC.h"
#include "GenPoly.h"
#include "GenProto.h"
#include "GenStruct.h"
#include "GenTuple.h"
#include "GenEnum.h"
#include "IRGenDebugInfo.h"
#include "IRGenModule.h"
#include "Linking.h"
#include "ReferenceTypeInfo.h"
#include "WeakTypeInfo.h"

using namespace swift;
using namespace irgen;

namespace {

class LoweredValue;
  
/// Represents a statically-known function as a SIL thin function value.
class StaticFunction {
  /// The function reference.
  llvm::Function *function;
  /// The function's native calling convention.
  AbstractCC cc;
  /// The function's explosion level.
  ExplosionKind explosionLevel;
  
public:
  StaticFunction(llvm::Function *function, AbstractCC cc, ExplosionKind level)
    : function(function), cc(cc), explosionLevel(level)
  {}
  
  llvm::Function *getFunction() const { return function; }
  AbstractCC getAbstractCC() const { return cc; }
  ExplosionKind getExplosionLevel() const { return explosionLevel; }
  
  llvm::Value *getExplosionValue(IRGenFunction &IGF) const;
};
  
/// Represents an ObjC method reference that will be invoked by a form of
/// objc_msgSend.
class ObjCMethod {
  /// The SILDeclRef declaring the method.
  SILDeclRef method;
  /// For a super call, the type to pass to msgSendSuper2 dispatch.
  /// Null for non-super calls.
  SILType superSearchType;

public:
  ObjCMethod(SILDeclRef method, SILType superSearchType)
    : method(method), superSearchType(superSearchType)
  {}
  
  SILDeclRef getMethod() const { return method; }
  SILType getSuperSearchType() const { return superSearchType; }
  
  /// FIXME: Thunk down to a Swift function value?
  llvm::Value *getExplosionValue(IRGenFunction &IGF) const {
    llvm_unreachable("thunking unapplied objc method to swift function "
                     "not yet implemented");
  }
};
  
/// Represents a metatype value. May be produced as a Swift metatype,
/// Objective-C Class, or both, based on use.
class MetatypeValue {
  /// The Swift metatype, or null if the value is not used as a swift metatype.
  llvm::Value *swiftMetatype;
  /// The Objective-C Class, or null if the value is not used as an ObjC class.
  llvm::Value *objcClass;

public:
  MetatypeValue(llvm::Value *swiftMetatype, llvm::Value *objcClass)
    : swiftMetatype(swiftMetatype), objcClass(objcClass)
  {}
  
  llvm::Value *getSwiftMetatype() const {
    assert(swiftMetatype && "not used as swift metatype?!");
    return swiftMetatype;
  }
  
  llvm::Value *getObjCClass() const {
    assert(objcClass && "not used as objc class?!");
    return objcClass;
  }
};

/// Represents a builtin function.
class BuiltinValue {
  Identifier Id;
public:
  BuiltinValue(Identifier Id) : Id(Id) {}
  
  Identifier getId() const { return Id; }
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
    
      /// A value that represents a metatype.
      MetatypeValue,
    
      /// A builtin function.
      BuiltinValue,
    Value_Last = BuiltinValue
  };
  
  Kind kind;
  
private:
  using ExplosionVector = SmallVector<llvm::Value *, 4>;
  
  union {
    Address address;
    struct {
      ExplosionKind kind;
      ExplosionVector values;
    } explosion;
    StaticFunction staticFunction;
    ObjCMethod objcMethod;
    MetatypeValue metatypeValue;
    BuiltinValue builtinValue;
  };

public:
  LoweredValue(const Address &address)
    : kind(Kind::Address), address(address)
  {}
  
  LoweredValue(StaticFunction &&staticFunction)
    : kind(Kind::StaticFunction), staticFunction(std::move(staticFunction))
  {}

  LoweredValue(ObjCMethod &&objcMethod)
    : kind(Kind::ObjCMethod), objcMethod(std::move(objcMethod))
  {}
  
  LoweredValue(MetatypeValue &&metatypeValue)
    : kind(Kind::MetatypeValue), metatypeValue(std::move(metatypeValue))
  {}
  
  LoweredValue(BuiltinValue &&builtinValue)
    : kind(Kind::BuiltinValue), builtinValue(std::move(builtinValue))
  {}
  
  LoweredValue(Explosion &e)
    : kind(Kind::Explosion),
      explosion{e.getKind(), {}} {
    auto Elts = e.claimAll();
    explosion.values.append(Elts.begin(), Elts.end());
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
      break;
    case Kind::MetatypeValue:
      ::new (&metatypeValue) MetatypeValue(std::move(lv.metatypeValue));
      break;
    case Kind::BuiltinValue:
      ::new (&builtinValue) BuiltinValue(std::move(lv.builtinValue));
      break;
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
  
  const StaticFunction &getStaticFunction() const {
    assert(kind == Kind::StaticFunction && "not a static function");
    return staticFunction;
  }
  
  const ObjCMethod &getObjCMethod() const {
    assert(kind == Kind::ObjCMethod && "not an objc method");
    return objcMethod;
  }
  
  const MetatypeValue &getMetatypeValue() const {
    assert(kind == Kind::MetatypeValue && "not a metatype value");
    return metatypeValue;
  }
  
  const BuiltinValue &getBuiltinValue() const {
    assert(kind == Kind::BuiltinValue && "not a builtin");
    return builtinValue;
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
      break;
    case Kind::MetatypeValue:
      metatypeValue.~MetatypeValue();
      break;
    case Kind::BuiltinValue:
      builtinValue.~BuiltinValue();
      break;
    }
  }
};
  
/// Represents a lowered SIL basic block. This keeps track
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
  public IRGenFunction, public SILInstructionVisitor<IRGenSILFunction>
{
public:
  llvm::DenseMap<SILValue, LoweredValue> LoweredValues;
  llvm::DenseMap<SILType, LoweredValue> LoweredUndefs;
  llvm::MapVector<SILBasicBlock *, LoweredBB> LoweredBBs;
  llvm::DenseMap<const VarDecl *, unsigned> ArgNo;
  
  SILFunction *CurSILFn;
  ExplosionKind CurSILFnExplosionLevel;
  Address IndirectReturn;
  
  IRGenSILFunction(IRGenModule &IGM,
                   SILFunction *f,
                   ExplosionKind explosionLevel);
  ~IRGenSILFunction();
  
  /// Generate IR for the SIL Function.
  void emitSILFunction();

  void setLoweredValue(SILValue v, LoweredValue &&lv) {
    auto inserted = LoweredValues.insert({v, std::move(lv)});
    assert(inserted.second && "already had lowered value for sil value?!");
    (void)inserted;
  }
  
  /// Create a new Address corresponding to the given SIL address value.
  void setLoweredAddress(SILValue v, const Address &address) {
    assert((v.getType().isAddress() || v.getType().isLocalStorage()) &&
           "address for non-address value?!");
    setLoweredValue(v, address);
  }

  /// Create a new Explosion corresponding to the given SIL value.
  void setLoweredExplosion(SILValue v, Explosion &e) {
    assert(v.getType().isObject() && "explosion for address value?!");
    setLoweredValue(v, LoweredValue(e));
  }

  void setLoweredSingleValue(SILValue v, llvm::Value *scalar) {
    Explosion e(ExplosionKind::Maximal);
    e.add(scalar);
    setLoweredExplosion(v, e);
  }
  
  /// Create a new StaticFunction corresponding to the given SIL value.
  void setLoweredStaticFunction(SILValue v,
                                llvm::Function *f,
                                AbstractCC cc,
                                ExplosionKind explosionLevel) {
    assert(v.getType().isObject() && "function for address value?!");
    assert(v.getType().is<SILFunctionType>() &&
           "function for non-function value?!");
    setLoweredValue(v, StaticFunction{f, cc, explosionLevel});
  }
  
  void setLoweredObjCMethod(SILValue v, SILDeclRef method,
                            SILType superSearchType = SILType()) {
    assert(v.getType().isObject() && "function for address value?!");
    assert(v.getType().is<SILFunctionType>() &&
           "function for non-function value?!");
    setLoweredValue(v, ObjCMethod{method, superSearchType});
  }
  
  void setLoweredMetatypeValue(SILValue v,
                               llvm::Value /*nullable*/ *swiftMetatype,
                               llvm::Value /*nullable*/ *objcMetatype) {
    setLoweredValue(v, MetatypeValue{swiftMetatype, objcMetatype});
  }
  
  void setLoweredBuiltinValue(SILValue v, Identifier builtin) {
    setLoweredValue(v, BuiltinValue{builtin});
  }
  
  LoweredValue &getUndefLoweredValue(SILType t) {
    auto found = LoweredUndefs.find(t);
    if (found != LoweredUndefs.end())
      return found->second;
    
    auto &ti = getTypeInfo(t);
    switch (t.getCategory()) {
    case SILValueCategory::Address:
    case SILValueCategory::LocalStorage: {
      Address undefAddr = ti.getAddressForPointer(
                  llvm::UndefValue::get(ti.getStorageType()->getPointerTo()));
      LoweredUndefs.insert({t, LoweredValue(undefAddr)});
      break;
    }

    case SILValueCategory::Object: {
      auto schema = ti.getSchema(ExplosionKind::Maximal);
      Explosion e(ExplosionKind::Maximal);
      for (auto &elt : schema) {
        assert(!elt.isAggregate()
               && "non-scalar element in loadable type schema?!");
        e.add(llvm::UndefValue::get(elt.getScalarType()));
      }
      LoweredUndefs.insert({t, LoweredValue(e)});
      break;
    }
    }
    
    found = LoweredUndefs.find(t);
    assert(found != LoweredUndefs.end());
    return found->second;
  }
  
  /// Get the LoweredValue corresponding to the given SIL value, which must
  /// have been lowered.
  LoweredValue &getLoweredValue(SILValue v) {
    if (isa<SILUndef>(v))
      return getUndefLoweredValue(v.getType());
    
    auto foundValue = LoweredValues.find(v);
    assert(foundValue != LoweredValues.end() &&
           "no lowered explosion for sil value!");
    return foundValue->second;
  }
  
  /// Get the Address of a SIL value of address type, which must have been
  /// lowered.
  Address getLoweredAddress(SILValue v) {
    return getLoweredValue(v).getAddress();
  }
  /// Add the unmanaged LLVM values lowered from a SIL value to an explosion.
  void getLoweredExplosion(SILValue v, Explosion &e) {
    getLoweredValue(v).getExplosion(*this, e);
  }
  /// Create an Explosion containing the unmanaged LLVM values lowered from a
  /// SIL value.
  Explosion getLoweredExplosion(SILValue v) {
    return getLoweredValue(v).getExplosion(*this);
  }
  /// Get the explosion level the value was lowered to.
  ExplosionKind getExplosionKind(SILValue v) {
    return getLoweredValue(v).getExplosionKind();
  }
  
  LoweredBB &getLoweredBB(SILBasicBlock *bb) {
    auto foundBB = LoweredBBs.find(bb);
    assert(foundBB != LoweredBBs.end() && "no llvm bb for sil bb?!");
    return foundBB->second;
  }

  /// Emit a shadow copy of an Address in an alloca, so the register
  /// allocator doesn't elide the dbg.value intrinsic when register
  /// pressure is high.
  llvm::Value *emitShadowCopy(const Address &Source,
                              StringRef Name) {
    if (IGM.Opts.OptLevel == 0) {
      auto Alloca = createAlloca(Source->getType(),
                                 Source.getAlignment(),
                                 Name+".addr");
      Builder.CreateAlignedStore(Source.getAddress(),
                                 Alloca.getAddress(),
                                 Source.getAlignment().getValue());
      return Alloca.getAddress();
    } else
      return Source.getAddress();
  }

  
  //===--------------------------------------------------------------------===//
  // SIL instruction lowering
  //===--------------------------------------------------------------------===//

  void visitSILBasicBlock(SILBasicBlock *BB);
  void emitFunctionArgDebugInfo(SILBasicBlock *BB);

  void visitAllocStackInst(AllocStackInst *i);
  void visitAllocRefInst(AllocRefInst *i);
  void visitAllocBoxInst(AllocBoxInst *i);
  void visitAllocArrayInst(AllocArrayInst *i);

  void visitApplyInst(ApplyInst *i);
  void visitPartialApplyInst(PartialApplyInst *i);

  void visitBuiltinFunctionRefInst(BuiltinFunctionRefInst *i);
  void visitFunctionRefInst(FunctionRefInst *i);
  void visitGlobalAddrInst(GlobalAddrInst *i);
  void visitSILGlobalAddrInst(SILGlobalAddrInst *i);

  void visitIntegerLiteralInst(IntegerLiteralInst *i);
  void visitFloatLiteralInst(FloatLiteralInst *i);
  void visitStringLiteralInst(StringLiteralInst *i);

  void visitLoadInst(LoadInst *i);
  void visitStoreInst(StoreInst *i);
  void visitAssignInst(AssignInst *i) {
    llvm_unreachable("assign is not valid in canonical SIL");
  }
  void visitMarkUninitializedInst(MarkUninitializedInst *i) {
    llvm_unreachable("mark_uninitialized is not valid in canonical SIL");
  }
  void visitMarkFunctionEscapeInst(MarkFunctionEscapeInst *i) {
    llvm_unreachable("mark_function_escape is not valid in canonical SIL");
  }
  void visitLoadWeakInst(LoadWeakInst *i);
  void visitStoreWeakInst(StoreWeakInst *i);
  void visitCopyValueInst(CopyValueInst *i);
  void visitDestroyValueInst(DestroyValueInst *i);
  void visitStructInst(StructInst *i);
  void visitTupleInst(TupleInst *i);
  void visitEnumInst(EnumInst *i);
  void visitEnumDataAddrInst(EnumDataAddrInst *i);
  void visitInjectEnumAddrInst(InjectEnumAddrInst *i);
  void visitBuiltinZeroInst(BuiltinZeroInst *i);
  void visitMetatypeInst(MetatypeInst *i);
  void visitClassMetatypeInst(ClassMetatypeInst *i);
  void visitArchetypeMetatypeInst(ArchetypeMetatypeInst *i);
  void visitProtocolMetatypeInst(ProtocolMetatypeInst *i);
  void visitTupleExtractInst(TupleExtractInst *i);
  void visitTupleElementAddrInst(TupleElementAddrInst *i);
  void visitStructExtractInst(StructExtractInst *i);
  void visitStructElementAddrInst(StructElementAddrInst *i);
  void visitRefElementAddrInst(RefElementAddrInst *i);

  void visitClassMethodInst(ClassMethodInst *i);
  void visitSuperMethodInst(SuperMethodInst *i);
  void visitArchetypeMethodInst(ArchetypeMethodInst *i);
  void visitProtocolMethodInst(ProtocolMethodInst *i);
  void visitDynamicMethodInst(DynamicMethodInst *i);

  void visitProjectExistentialInst(ProjectExistentialInst *i);
  void visitProjectExistentialRefInst(ProjectExistentialRefInst *i);
  void visitInitExistentialInst(InitExistentialInst *i);
  void visitInitExistentialRefInst(InitExistentialRefInst *i);
  void visitUpcastExistentialInst(UpcastExistentialInst *i);
  void visitUpcastExistentialRefInst(UpcastExistentialRefInst *i);
  void visitDeinitExistentialInst(DeinitExistentialInst *i);

  void visitStrongRetainInst(StrongRetainInst *i);
  void visitStrongReleaseInst(StrongReleaseInst *i);
  void visitStrongRetainAutoreleasedInst(StrongRetainAutoreleasedInst *i);
  void visitStrongRetainUnownedInst(StrongRetainUnownedInst *i);
  void visitUnownedRetainInst(UnownedRetainInst *i);
  void visitUnownedReleaseInst(UnownedReleaseInst *i);
  void visitDeallocStackInst(DeallocStackInst *i);
  void visitDeallocBoxInst(DeallocBoxInst *i);
  void visitDeallocRefInst(DeallocRefInst *i);

  void visitInitializeVarInst(InitializeVarInst *i);
  void visitCopyAddrInst(CopyAddrInst *i);
  void visitDestroyAddrInst(DestroyAddrInst *i);

  void visitCondFailInst(CondFailInst *i);
  
  void visitConvertFunctionInst(ConvertFunctionInst *i);
  void visitCoerceInst(CoerceInst *i);
  void visitUpcastInst(UpcastInst *i);
  void visitAddressToPointerInst(AddressToPointerInst *i);
  void visitPointerToAddressInst(PointerToAddressInst *i);
  void visitRefToObjectPointerInst(RefToObjectPointerInst *i);
  void visitObjectPointerToRefInst(ObjectPointerToRefInst *i);
  void visitRefToRawPointerInst(RefToRawPointerInst *i);
  void visitRawPointerToRefInst(RawPointerToRefInst *i);
  void visitRefToUnownedInst(RefToUnownedInst *i);
  void visitUnownedToRefInst(UnownedToRefInst *i);
  void visitThinToThickFunctionInst(ThinToThickFunctionInst *i);
  void visitBridgeToBlockInst(BridgeToBlockInst *i);
  void visitArchetypeRefToSuperInst(ArchetypeRefToSuperInst *i);
  void visitUnconditionalCheckedCastInst(UnconditionalCheckedCastInst *i);

  void visitIsNonnullInst(IsNonnullInst *i);

  void visitIndexAddrInst(IndexAddrInst *i);
  void visitIndexRawPointerInst(IndexRawPointerInst *i);
  
  void visitUnreachableInst(UnreachableInst *i);
  void visitBranchInst(BranchInst *i);
  void visitCondBranchInst(CondBranchInst *i);
  void visitReturnInst(ReturnInst *i);
  void visitAutoreleaseReturnInst(AutoreleaseReturnInst *i);
  void visitSwitchIntInst(SwitchIntInst *i);
  void visitSwitchEnumInst(SwitchEnumInst *i);
  void visitDestructiveSwitchEnumAddrInst(DestructiveSwitchEnumAddrInst *i);
  void visitDynamicMethodBranchInst(DynamicMethodBranchInst *i);
  void visitCheckedCastBranchInst(CheckedCastBranchInst *i);
};

}

llvm::Value *StaticFunction::getExplosionValue(IRGenFunction &IGF) const {
  return IGF.Builder.CreateBitCast(function, IGF.IGM.Int8PtrTy);
}

void LoweredValue::getExplosion(IRGenFunction &IGF, Explosion &ex) const {
  switch (kind) {
  case Kind::Address:
    llvm_unreachable("not a value");
      
  case Kind::Explosion:
    assert(ex.getKind() == explosion.kind &&
           "destination explosion kind mismatch");
    for (auto *value : explosion.values)
      ex.add(value);
    break;

  case Kind::StaticFunction:
    ex.add(staticFunction.getExplosionValue(IGF));
    break;
      
  case Kind::ObjCMethod:
    ex.add(objcMethod.getExplosionValue(IGF));
    break;
  
  case Kind::MetatypeValue:
    ex.add(metatypeValue.getSwiftMetatype());
    break;
  
  case Kind::BuiltinValue:
    llvm_unreachable("reifying builtin function not yet supported");
  }
}

ExplosionKind LoweredValue::getExplosionKind() const {
  switch (kind) {
  case Kind::Address:
    llvm_unreachable("not a value");
  case Kind::Explosion:
    return explosion.kind;
  case Kind::StaticFunction:
  case Kind::ObjCMethod:
  case Kind::MetatypeValue:
  case Kind::BuiltinValue:
    return ExplosionKind::Minimal;
  }
}

IRGenSILFunction::IRGenSILFunction(IRGenModule &IGM,
                                   SILFunction *f,
                                   ExplosionKind explosionLevel)
  : IRGenFunction(IGM, IGM.getAddrOfSILFunction(f, explosionLevel),
                  f->getDebugScope(), f->getLocation()),
    CurSILFn(f), CurSILFnExplosionLevel(explosionLevel)
{}

IRGenSILFunction::~IRGenSILFunction() {
  DEBUG(CurFn->print(llvm::dbgs()));
}

static std::vector<llvm::PHINode*>
emitPHINodesForBBArgs(IRGenSILFunction &IGF,
                      SILBasicBlock *silBB,
                      llvm::BasicBlock *llBB) {
  std::vector<llvm::PHINode*> phis;
  unsigned predecessors = std::distance(silBB->pred_begin(), silBB->pred_end());
  
  IGF.Builder.SetInsertPoint(llBB);
  if (IGF.IGM.DebugInfo) {
    // Use the location of the first instruction in the basic block
    // for the φ-nodes.
    if (!silBB->empty()) {
      SILInstruction &I = *silBB->begin();
      auto DS = I.getDebugScope();
      if (!DS) DS = IGF.CurSILFn->getDebugScope();
      IGF.IGM.DebugInfo->setCurrentLoc(IGF.Builder, DS, I.getLoc());
    }
  }

  for (SILArgument *arg : make_range(silBB->bbarg_begin(), silBB->bbarg_end())) {
    size_t first = phis.size();
    
    const TypeInfo &ti = IGF.getTypeInfo(arg->getType());
    
    if (arg->getType().isAddress()) {
      phis.push_back(IGF.Builder.CreatePHI(ti.getStorageType()->getPointerTo(),
                                           predecessors));
      IGF.setLoweredAddress(SILValue(arg,0),
                            ti.getAddressForPointer(phis.back()));
    } else {
      // PHIs are always emitted with maximal explosion.
      ExplosionSchema schema = ti.getSchema(ExplosionKind::Maximal);
      for (auto &elt : schema) {
        if (elt.isScalar())
          phis.push_back(
                     IGF.Builder.CreatePHI(elt.getScalarType(), predecessors));
        else
          phis.push_back(
                     IGF.Builder.CreatePHI(elt.getAggregateType()->getPointerTo(),
                     predecessors));
      }
      
      Explosion argValue(ExplosionKind::Maximal);
      for (llvm::PHINode *phi : make_range(phis.begin()+first, phis.end()))
        argValue.add(phi);
      IGF.setLoweredExplosion(SILValue(arg,0), argValue);
    }
  }

  // Since we return to the entry of the function, reset the location.
  if (IGF.IGM.DebugInfo)
    IGF.IGM.DebugInfo->clearLoc(IGF.Builder);

  return phis;
}

static ArrayRef<SILArgument*> emitEntryPointIndirectReturn(
                                 IRGenSILFunction &IGF,
                                 SILBasicBlock *entry,
                                 Explosion &params,
                                 CanSILFunctionType funcTy,
                                 std::function<bool()> requiresIndirectResult) {
  // Map the indirect return if present.
  if (funcTy->hasIndirectResult()) {
    SILArgument *ret = entry->bbarg_begin()[0];
    SILValue retv(ret, 0);
    auto &retTI = IGF.IGM.getTypeInfo(ret->getType());
    
    IGF.setLoweredAddress(retv, retTI.getAddressForPointer(params.claimNext()));
    return entry->getBBArgs().slice(1);
  } else {
    // Map an indirect return for a type SIL considers loadable but still
    // requires an indirect return at the IR level.
    if (requiresIndirectResult()) {
      auto &retTI = IGF.IGM.getTypeInfo(funcTy->getResult().getSILType());
      IGF.IndirectReturn = retTI.getAddressForPointer(params.claimNext());
    }
    return entry->getBBArgs();
  }  
}

/// Emit entry point arguments for a SILFunction with the Swift calling
/// convention.
static void emitEntryPointArgumentsNativeCC(IRGenSILFunction &IGF,
                                            SILBasicBlock *entry,
                                            Explosion &allParamValues,
                                            CanSILFunctionType funcTy) {
  // Map the indirect return if present.
  ArrayRef<SILArgument*> params
    = emitEntryPointIndirectReturn(IGF, entry, allParamValues, funcTy,
      [&]() -> bool {
        auto retType = funcTy->getResult().getSILType();
        return IGF.IGM.requiresIndirectResult(retType, IGF.CurSILFnExplosionLevel);
      });

  // Map the remaining SIL parameters to LLVM parameters.
  for (SILArgument *param : params) {
    // Pull out the parameter value and its formal type.
    auto &paramTI = IGF.getTypeInfo(param->getType());

    // If the SIL parameter isn't passed indirectly, we need to map it
    // to an explosion.  Fortunately, in this case we have a guarantee
    // that it's passed directly in IR.
    if (param->getType().isObject()) {
      Explosion paramValues(IGF.CurSILFnExplosionLevel);
      cast<LoadableTypeInfo>(paramTI).reexplode(IGF, allParamValues, paramValues);
      IGF.setLoweredExplosion(SILValue(param, 0), paramValues);
      continue;
    }

    // Okay, the type is passed indirectly in SIL, so we need to map
    // it to an address.
    // FIXME: that doesn't mean we should physically pass it
    // indirectly at this explosion level, but SIL currently gives us
    // no ability to distinguish between an l-value and a byval argument.
    Address paramAddr
      = paramTI.getAddressForPointer(allParamValues.claimNext());
    IGF.setLoweredAddress(SILValue(param, 0), paramAddr);
  }
  
  // Bind polymorphic arguments.
  if (funcTy->isPolymorphic())
    emitPolymorphicParameters(IGF, funcTy, allParamValues);
}

/// Emit entry point arguments for the parameters of a C function, or the
/// method parameters of an ObjC method.
static void emitEntryPointArgumentsCOrObjC(IRGenSILFunction &IGF,
                                           SILBasicBlock *entry,
                                           Explosion &params,
                                           ArrayRef<SILArgument*> args) {
  for (SILArgument *arg : args) {
    auto &argTI = IGF.getTypeInfo(arg->getType());
    if (arg->getType().isAddress()) {
      IGF.setLoweredAddress(arg,
                            argTI.getAddressForPointer(params.claimNext()));
      continue;
    }

    auto &loadableArgTI = cast<LoadableTypeInfo>(argTI);
    Explosion argExplosion(IGF.CurSILFnExplosionLevel);

    // Load and explode an argument that is 'byval' in the C calling convention.
    if (requiresExternalByvalArgument(IGF.IGM, arg->getType())) {
      Address byval = loadableArgTI.getAddressForPointer(params.claimNext());
      loadableArgTI.loadAsTake(IGF, byval, argExplosion);
    } else {
      loadableArgTI.reexplode(IGF, params, argExplosion);
    }
    
    IGF.setLoweredExplosion(arg, argExplosion);
  }
}


/// Emit entry point arguments for a SILFunction with the ObjC method calling
/// convention. This convention inserts the '_cmd' objc_msgSend argument after
/// the first non-sret argument.
static void emitEntryPointArgumentsObjCMethodCC(IRGenSILFunction &IGF,
                                                SILBasicBlock *entry,
                                                Explosion &params,
                                                CanSILFunctionType funcTy) {
  // Map the indirect return if present.
  ArrayRef<SILArgument*> args
    = emitEntryPointIndirectReturn(IGF, entry, params, funcTy, [&] {
      return requiresExternalIndirectResult(IGF.IGM, funcTy,
                                            IGF.CurSILFnExplosionLevel);
    });
  
  // Map the self argument. This should always be an ObjC pointer type so
  // should never need to be loaded from a byval.
  SILArgument *selfArg = args.back();
  auto &selfType = IGF.getTypeInfo(selfArg->getType());
  Explosion self(IGF.CurSILFnExplosionLevel);
  cast<LoadableTypeInfo>(selfType).reexplode(IGF, params, self);
  IGF.setLoweredExplosion(selfArg, self);
  
  // Discard the implicit _cmd argument.
  params.claimNext();
  
  // Map the rest of the arguments as in the C calling convention.
  emitEntryPointArgumentsCOrObjC(IGF, entry, params,
                                 args.slice(0, args.size() - 1));
}

/// Emit entry point arguments for a SILFunction with the C calling
/// convention.
static void emitEntryPointArgumentsCCC(IRGenSILFunction &IGF,
                                       SILBasicBlock *entry,
                                       Explosion &params,
                                       CanSILFunctionType funcTy) {
  // Map the indirect return if present.
  ArrayRef<SILArgument*> args
    = emitEntryPointIndirectReturn(IGF, entry, params, funcTy, [&] {
      return requiresExternalIndirectResult(IGF.IGM, funcTy,
                                            IGF.CurSILFnExplosionLevel);
    });
  emitEntryPointArgumentsCOrObjC(IGF, entry, params, args);
}

/// Emit the definition for the given SIL constant.
void IRGenModule::emitSILFunction(SILFunction *f) {
  if (f->isExternalDeclaration())
    return;

  PrettyStackTraceSILFunction stackTrace("emitting IR", f);
    
  // FIXME: Emit all needed explosion levels.
  ExplosionKind explosionLevel = ExplosionKind::Minimal;
  IRGenSILFunction(*this, f, explosionLevel).emitSILFunction();
}

void IRGenSILFunction::emitSILFunction() {
  DEBUG(llvm::dbgs() << "emitting SIL function: ";
        CurSILFn->printName(llvm::dbgs());
        llvm::dbgs() << '\n';
        CurSILFn->print(llvm::dbgs()));
  
  assert(!CurSILFn->empty() && "function has no basic blocks?!");
  
  // Map the entry bb.
  LoweredBBs[CurSILFn->begin()] = LoweredBB(CurFn->begin(), {});
  // Create LLVM basic blocks for the other bbs.
  for (SILBasicBlock *bb = CurSILFn->begin()->getNextNode();
       bb != CurSILFn->end(); bb = bb->getNextNode()) {
    // FIXME: Use the SIL basic block's name.
    llvm::BasicBlock *llBB = llvm::BasicBlock::Create(IGM.getLLVMContext());
    std::vector<llvm::PHINode*> phis = emitPHINodesForBBArgs(*this, bb, llBB);
    CurFn->getBasicBlockList().push_back(llBB);
    LoweredBBs[bb] = LoweredBB(llBB, std::move(phis));
  }

  auto entry = LoweredBBs.begin();
  Builder.SetInsertPoint(entry->second.bb);

  // Map the LLVM arguments to arguments on the entry point BB.
  Explosion params = collectParameters(CurSILFnExplosionLevel);
  auto funcTy = CurSILFn->getLoweredFunctionType();

  switch (CurSILFn->getAbstractCC()) {
  case AbstractCC::Freestanding:
  case AbstractCC::Method:
    emitEntryPointArgumentsNativeCC(*this, entry->first, params, funcTy);
    break;
  case AbstractCC::WitnessMethod:
    llvm_unreachable("@cc(witness_method) not implemented");
  case AbstractCC::ObjCMethod:
    emitEntryPointArgumentsObjCMethodCC(*this, entry->first, params, funcTy);
    break;
  case AbstractCC::C:
    emitEntryPointArgumentsCCC(*this, entry->first, params, funcTy);
    break;
  }
  
  assert(params.empty() && "did not map all llvm params to SIL params?!");

  // It's really nice to be able to assume that we've already emitted
  // all the values from dominating blocks --- it makes simple
  // peepholing more powerful and allows us to avoid the need for
  // nasty "forward-declared" values.  We can do this by emitting
  // blocks using a simple walk through the successor graph.
  //
  // We do want to preserve the original source order, but that's done
  // by having previously added all the primary blocks to the LLVM
  // function in their original order.  As long as any secondary
  // blocks are inserted after the current IP instead of at the end
  // of the function, we're fine.

  // Invariant: for every block in the work queue, we have visited all
  // of its dominators.
  llvm::SmallPtrSet<SILBasicBlock*, 8> visitedBlocks;
  SmallVector<SILBasicBlock*, 8> workQueue; // really a stack

  // Queue up the entry block, for which the invariant trivially holds.
  visitedBlocks.insert(CurSILFn->begin());
  workQueue.push_back(CurSILFn->begin());

  while (!workQueue.empty()) {
    auto bb = workQueue.pop_back_val();

    // Emit the block.
    visitSILBasicBlock(bb);

#ifndef NDEBUG
    // Assert that the current IR IP (if valid) is immediately prior
    // to the initial IR block for the next primary SIL block.
    // It's not semantically necessary to preserve SIL block order,
    // but we really should.
    if (auto curBB = Builder.GetInsertBlock()) {
      auto next = llvm::next(SILFunction::iterator(bb));
      if (next != CurSILFn->end()) {
        auto nextBB = LoweredBBs[&*next].bb;
        assert(curBB->getNextNode() == nextBB &&
               "lost source SIL order?");
      }
    }
#endif

    // The immediate dominator of a successor of this block needn't be
    // this block, but it has to be something which dominates this
    // block.  In either case, we've visited it.
    //
    // Therefore the invariant holds of all the successors, and we can
    // queue them up if we haven't already visited them.
    for (auto &succ : bb->getSuccs()) {
      auto succBB = succ.getBB();
      if (visitedBlocks.insert(succBB))
        workQueue.push_back(succBB);
    }
  }

  // If there are dead blocks in the SIL function, we might have left
  // invalid blocks in the IR.  Do another pass and kill them off.
  for (SILBasicBlock &bb : *CurSILFn)
    if (!visitedBlocks.count(&bb))
      LoweredBBs[&bb].bb->eraseFromParent();
}

void IRGenSILFunction::emitFunctionArgDebugInfo(SILBasicBlock *BB) {
  assert(BB->pred_empty());
  if (!IGM.DebugInfo)
    return;

  // This is the prologue of a function. Emit debug info for all
  // trivial arguments and any captured and promoted [inout]
  // variables.
  int N = 0;
  for (auto Arg : BB->getBBArgs()) {
    ++N;
    const LoweredValue &LoweredArg = getLoweredValue(Arg);
    if (!Arg->getDecl())
      continue;

    // Generic types were already handled in visitAllocStackInst.
    if (Arg->getType().isExistentialType())
      continue;

    if (LoweredArg.isAddress()) {
      auto Name = Arg->getDecl()->getName().str();
      auto AddrIR = emitShadowCopy(LoweredArg.getAddress(), Name);
      DebugTypeInfo DTI(const_cast<ValueDecl*>(Arg->getDecl()),
                        getTypeInfo(Arg->getType()),
                        getDebugScope());
      IGM.DebugInfo->emitArgVariableDeclaration(Builder, AddrIR, DTI, Name,
                                                N, DirectValue);
    }
  }
}

void IRGenSILFunction::visitSILBasicBlock(SILBasicBlock *BB) {
  // Insert into the lowered basic block.
  llvm::BasicBlock *llBB = getLoweredBB(BB).bb;
  Builder.SetInsertPoint(llBB);

  // FIXME: emit a phi node to bind the bb arguments from all the predecessor
  // branches.

  bool InEntryBlock = BB->pred_empty();
  bool ArgsEmitted = false;

  if (InEntryBlock) {
    // Establish a mapping from VarDecl -> ArgNo to be used by
    // visitAllocStackInst().
    unsigned N = 1;
    for (auto Arg : BB->getBBArgs()) {
      if (auto VD = dyn_cast_or_null<VarDecl>(Arg->getDecl()))
        ArgNo.insert( {VD, N} );
      ++N;
    }
  }

  // Generate the body.
  for (auto &I : *BB) {
    if (IGM.DebugInfo) {
      // Set the debug info location for I, if applicable.
      auto ILoc = I.getLoc();
      if (ILoc.getKind() == SILLocation::CleanupKind) {
        // Cleanup locations point to the decl of the the value that
        // is being destroyed (for diagnostic generation). For the
        // linetable they should point to the cleanup location, which
        // is the location of the last instruction in the basic block.
        assert(BB->getTerminator());
        ILoc = BB->getTerminator()->getLoc();
      }
      if (auto DS = I.getDebugScope())
        IGM.DebugInfo->setCurrentLoc(Builder, DS, ILoc);
      else {
        if (auto FnDS = CurSILFn->getDebugScope())
          IGM.DebugInfo->setCurrentLoc(Builder, FnDS, ILoc);
        else
          // We don't expect a scope from transparent functions. They
          // should be elided during IR generation anyway.
          assert(CurSILFn->isTransparent() && "function without a debug scope");
      }

      if (InEntryBlock && !ArgsEmitted) {
        if (!I.getLoc().isInPrologue()) {
          if (I.getLoc().getSourceLoc().isValid()) {
            // This is the first non-prologue instruction in the entry
            // block.  The function prologue is where the stack frame is
            // set up and storage for local variables and function
            // arguments is initialized.  We need to emit the debug info
            // for the function arguments after the function prologue,
            // after the initialization.
            emitFunctionArgDebugInfo(BB);
            ArgsEmitted = true;
          } else {
            // There may be instructions without a valid location
            // following the prologue. We need to associate them at
            // least with the function scope or LLVM won't know were
            // the prologue ends.
            IGM.DebugInfo->setCurrentLoc(Builder, CurSILFn->getDebugScope());
          }
        }
      }

    }
    visit(&I);
  }
  
  assert(Builder.hasPostTerminatorIP() && "SIL bb did not terminate block?!");
}

/// Find the entry point for a SIL function.
llvm::Function *IRGenModule::getAddrOfSILFunction(SILFunction *f,
                                                  ExplosionKind level) {
  // Check whether we've created the function already.
  // FIXME: We should integrate this into the LinkEntity cache more cleanly.
  llvm::Function *fn = Module.getFunction(f->getName());
  if (fn) return fn;
    
  LinkEntity entity = LinkEntity::forSILFunction(f, level);
  
  llvm::AttributeSet attrs;
  llvm::FunctionType *fnType = getFunctionType(f->getLoweredFunctionType(),
                                               level,
                                               ExtraData::None,
                                               attrs);
  
  auto cc = expandAbstractCC(*this, f->getAbstractCC());
  LinkInfo link = LinkInfo::get(*this, entity);

  fn = link.createFunction(*this, fnType, cc, attrs);

  // Unless this is an external reference, emit debug info for it.
  if (DebugInfo && !f->isExternalDeclaration())
    DebugInfo->emitFunction(f, fn);

  return fn;
}

Address IRGenModule::getAddrOfSILGlobalVariable(SILGlobalVariable *var) {
  // Check whether we've created the global variable already.
  // FIXME: We should integrate this into the LinkEntity cache more cleanly.
  auto gvar = Module.getGlobalVariable(var->getName(), /*allowInternal*/ true);
  if (gvar) return Address(gvar, Alignment(gvar->getAlignment()));

  LinkEntity entity = LinkEntity::forSILGlobalVariable(var);
  LinkInfo link = LinkInfo::get(*this, entity);
  auto &ti = getTypeInfo(var->getLoweredType());
  // TODO: Debug info needs to be able to use a SILGlobalVariable.
  // We also ought to have a better debug name.
  DebugTypeInfo DbgTy(var->getLoweredType().getSwiftRValueType(),
                      ti);
  Optional<SILLocation> loc;
  if (var->hasLocation())
    loc = var->getLocation();
  gvar = link.createVariable(*this, ti.StorageType, DbgTy,
                             loc, var->getName());
  
  // Set the alignment from the TypeInfo.
  Address gvarAddr = ti.getAddressForPointer(gvar);
  gvar->setAlignment(gvarAddr.getAlignment().getValue());
  
  return gvarAddr;
}

void IRGenSILFunction::visitBuiltinFunctionRefInst(BuiltinFunctionRefInst *i) {
  setLoweredBuiltinValue(SILValue(i, 0), i->getName());
}

void IRGenSILFunction::visitFunctionRefInst(FunctionRefInst *i) {
  // FIXME: pick the best available explosion level
  ExplosionKind explosionLevel = ExplosionKind::Minimal;
  llvm::Function *fnptr =
    IGM.getAddrOfSILFunction(i->getReferencedFunction(), explosionLevel);
  
  // Store the function constant and calling
  // convention as a StaticFunction so we can avoid bitcasting or thunking if
  // we don't need to.
  setLoweredStaticFunction(SILValue(i, 0), fnptr,
                           i->getReferencedFunction()->getAbstractCC(),
                           explosionLevel);
}

void IRGenSILFunction::visitGlobalAddrInst(GlobalAddrInst *i) {
  VarDecl *global = i->getGlobal();
  auto &type = getTypeInfoForUnlowered(global->getType());
  
  Address addr;
  
  // If the variable is empty, don't actually emit it; just return undef.
  // FIXME: global destructors?
  if (type.isKnownEmpty()) {
    addr = type.getUndefAddress();
  } else {
    addr = IGM.getAddrOfGlobalVariable(global);
  }
  
  setLoweredAddress(SILValue(i, 0), addr);
}

void IRGenSILFunction::visitSILGlobalAddrInst(SILGlobalAddrInst *i) {
  auto &ti = getTypeInfo(i->getType());
  
  Address addr;
  // If the variable is empty, don't actually emit it; just return undef.
  if (ti.isKnownEmpty()) {
    addr = ti.getUndefAddress();
  } else {
    addr = IGM.getAddrOfSILGlobalVariable(i->getReferencedGlobal());
  }
  
  setLoweredAddress(SILValue(i, 0), addr);
}

/// Determine whether a metatype value is used as a Swift metatype, ObjC class,
/// or both.
static void getMetatypeUses(ValueBase *i,
                            bool &isUsedAsSwiftMetatype,
                            bool &isUsedAsObjCClass) {
  isUsedAsSwiftMetatype = isUsedAsObjCClass = false;
  for (auto *use : i->getUses()) {
    // Ignore retains or releases of metatypes.
    if (isa<RefCountingInst>(use->getUser()))
      continue;
    
    // If a class_method lookup of an ObjC method is done on us, we'll need the
    // objc class.
    if (auto *cm = dyn_cast<ClassMethodInst>(use->getUser())) {
      if (cm->getMember().getDecl()->isObjC()) {
        isUsedAsObjCClass = true;
        continue;
      }
    }
    
    // If we're applied as the 'self' argument to a class_method of an objc
    // method, we'll need the objc class.
    // FIXME: Metatypes as other arguments should probably also pass the
    // Class too.
    if (auto *apply = dyn_cast<ApplyInst>(use->getUser())) {
      if (auto *method = dyn_cast<ClassMethodInst>(apply->getCallee())) {
        if (method->getMember().getDecl()->isObjC()
            && apply->getArguments().size() >= 1
            && apply->getArguments()[0].getDef() == i) {
          isUsedAsObjCClass = true;
          continue;
        }
      }
    }
    
    // All other uses are as Swift metatypes.
    isUsedAsSwiftMetatype = true;
  }
  
  // If there were no uses, assume it's used as a Swift metatype.
  isUsedAsSwiftMetatype = true;
}

static void emitMetatypeInst(IRGenSILFunction &IGF,
                             SILInstruction *i, CanType instanceType) {
  llvm::Value *swiftMetatype = nullptr, *objcClass = nullptr;
  
  bool isUsedAsSwiftMetatype, isUsedAsObjCClass;
  getMetatypeUses(i, isUsedAsSwiftMetatype, isUsedAsObjCClass);
  
  if (isUsedAsSwiftMetatype) {
    Explosion e(ExplosionKind::Maximal);
    emitMetaTypeRef(IGF, instanceType, e);
    if (!isUsedAsObjCClass) {
      IGF.setLoweredExplosion(SILValue(i, 0), e);
      return;
    }
    swiftMetatype = e.claimNext();
  }
  if (isUsedAsObjCClass) {
    objcClass = emitClassHeapMetadataRef(IGF, instanceType);
  }
  IGF.setLoweredMetatypeValue(SILValue(i,0), swiftMetatype, objcClass);
}

void IRGenSILFunction::visitMetatypeInst(swift::MetatypeInst *i) {
  CanType instanceType(i->getType().castTo<MetaTypeType>()->getInstanceType());
  emitMetatypeInst(*this, i, instanceType);
}

void IRGenSILFunction::visitClassMetatypeInst(swift::ClassMetatypeInst *i) {
  Explosion base = getLoweredExplosion(i->getOperand());
  auto baseValue = base.claimNext();
  
  bool isUsedAsSwiftMetatype, isUsedAsObjCClass;
  getMetatypeUses(i, isUsedAsSwiftMetatype, isUsedAsObjCClass);
  
  SILType instanceType = i->getOperand().getType();
  
  llvm::Value *swiftMetatype = nullptr, *objcClass = nullptr;
  if (isUsedAsSwiftMetatype)
    swiftMetatype = emitTypeMetadataRefForHeapObject(*this, baseValue,
                                                     instanceType);
  
  if (isUsedAsObjCClass)
    objcClass = emitHeapMetadataRefForHeapObject(*this, baseValue, instanceType);
  
  setLoweredMetatypeValue(SILValue(i,0), swiftMetatype, objcClass);
}

void IRGenSILFunction::visitArchetypeMetatypeInst(
                                              swift::ArchetypeMetatypeInst *i) {
  Address base = getLoweredAddress(i->getOperand());
  
  llvm::Value *metatype = emitTypeMetadataRefForArchetype(*this, base,
                                                    i->getOperand().getType());
  Explosion result(ExplosionKind::Maximal);
  result.add(metatype);
  setLoweredExplosion(SILValue(i, 0), result);
}

void IRGenSILFunction::visitProtocolMetatypeInst(
                                               swift::ProtocolMetatypeInst *i) {
  llvm::Value *metatype;
  if (i->getOperand().getType().isClassExistentialType()) {
    Explosion existential = getLoweredExplosion(i->getOperand());
    metatype = emitTypeMetadataRefForClassExistential(*this, existential,
                                                     i->getOperand().getType());
  } else {
    Address existential = getLoweredAddress(i->getOperand());
    metatype = emitTypeMetadataRefForOpaqueExistential(*this, existential,
                                                 i->getOperand().getType());
  }
  Explosion result(ExplosionKind::Maximal);
  result.add(metatype);
  setLoweredExplosion(SILValue(i, 0), result);
}

static void emitApplyArgument(IRGenSILFunction &IGF,
                              SILValue arg,
                              SILParameterInfo param,
                              ArrayRef<Substitution> subs,
                              Explosion &out) {
  bool isSubstituted = (arg.getType() != param.getSILType());

  // For indirect arguments, we just need to pass a pointer.
  if (param.isIndirect()) {
    // This address is of the substituted type.
    auto addr = IGF.getLoweredAddress(arg);

    // If a substitution is in play, just bitcast the address.
    if (isSubstituted) {
      auto origType = IGF.IGM.getStoragePointerType(param.getSILType());
      addr = IGF.Builder.CreateBitCast(addr, origType);
    }
      
    out.add(addr.getAddress());
    return;
  }

  // Otherwise, it's an explosion, which we may need to translate,
  // both in terms of explosion level and substitution levels.
  assert(arg.getType().isObject());

  // Fast path: avoid an unnecessary temporary explosion.
  if (!isSubstituted && out.getKind() == IGF.getExplosionKind(arg)) {
    IGF.getLoweredExplosion(arg, out);
    return;
  }

  Explosion temp = IGF.getLoweredExplosion(arg);

  // Handle the last unsubstituted case.
  if (!isSubstituted) {
    auto &substArgTI = cast<LoadableTypeInfo>(IGF.getTypeInfo(arg.getType()));
    substArgTI.reexplode(IGF, temp, out);
    return;
  }

  reemitAsUnsubstituted(IGF, param.getType(), arg.getType().getSwiftRValueType(),
                        subs, temp, out);
}

static CallEmission getCallEmissionForLoweredValue(IRGenSILFunction &IGF,
                                         CanSILFunctionType origCalleeType,
                                         CanSILFunctionType substCalleeType,
                                         const LoweredValue &lv,
                                         ArrayRef<Substitution> substitutions) {
  llvm::Value *calleeFn, *calleeData;
  ExtraData extraData;
  ExplosionKind explosionLevel;
  
  switch (lv.kind) {
  case LoweredValue::Kind::StaticFunction:
    calleeFn = lv.getStaticFunction().getFunction();
    explosionLevel = lv.getStaticFunction().getExplosionLevel();
    calleeData = nullptr;
    extraData = ExtraData::None;
    break;
      
  case LoweredValue::Kind::ObjCMethod: {
    auto &objcMethod = lv.getObjCMethod();
    return prepareObjCMethodRootCall(IGF, objcMethod.getMethod(),
                                     origCalleeType,
                                     substCalleeType,
                                     substitutions,
                                     ExplosionKind::Minimal,
                                     bool(objcMethod.getSuperSearchType()));
  }
      
  case LoweredValue::Kind::Explosion: {
    Explosion calleeValues = lv.getExplosion(IGF);
    
    calleeFn = calleeValues.claimNext();
    if (!origCalleeType->isThin())
      calleeData = calleeValues.claimNext();
    else
      calleeData = nullptr;

    // Guess the "ExtraData" kind from the type of CalleeData.
    // FIXME: Should these be typed differently by SIL?
    if (!calleeData)
      extraData = ExtraData::None;
    else if (calleeData->getType() == IGF.IGM.RefCountedPtrTy)
      extraData = ExtraData::Retainable;
    else if (calleeData->getType() == IGF.IGM.TypeMetadataPtrTy)
      extraData = ExtraData::Metatype;
    else
      llvm_unreachable("unexpected extra data for function value");

    // Indirect functions are always minimal.
    explosionLevel = ExplosionKind::Minimal;

    // Cast the callee pointer to the right function type.
    llvm::AttributeSet attrs;
    auto fnPtrTy = IGF.IGM.getFunctionType(origCalleeType, explosionLevel,
                                           extraData, attrs)->getPointerTo();
    calleeFn = IGF.Builder.CreateBitCast(calleeFn, fnPtrTy);
    break;
  }
      
  case LoweredValue::Kind::MetatypeValue:
    llvm_unreachable("metatype isn't a valid callee");
    
  case LoweredValue::Kind::Address:
    llvm_unreachable("sil address isn't a valid callee");
  
  case LoweredValue::Kind::BuiltinValue:
    llvm_unreachable("builtins should be handled before reaching here");
  }
  
  Callee callee = Callee::forKnownFunction(origCalleeType, substCalleeType,
                                           substitutions, calleeFn, calleeData,
                                           explosionLevel);
  return CallEmission(IGF, callee);
}

static llvm::Value *getObjCClassForValue(IRGenSILFunction &IGF,
                                         SILValue v) {
  const LoweredValue &lv = IGF.getLoweredValue(v);
  switch (lv.kind) {
  case LoweredValue::Kind::Address:
    llvm_unreachable("address isn't a valid metatype");
  
  case LoweredValue::Kind::ObjCMethod:
  case LoweredValue::Kind::StaticFunction:
  case LoweredValue::Kind::BuiltinValue:
    llvm_unreachable("function isn't a valid metatype");
  
  case LoweredValue::Kind::MetatypeValue:
    return lv.getMetatypeValue().getObjCClass();

  // Map a Swift metatype value back to the heap metadata, which will be the
  // Class for an ObjC type.
  case LoweredValue::Kind::Explosion: {
    Explosion e = lv.getExplosion(IGF);
    llvm::Value *swiftMeta = e.claimNext();
    CanType instanceType(v.getType().castTo<MetaTypeType>()->getInstanceType());
    return emitClassHeapMetadataRefForMetatype(IGF, swiftMeta, instanceType);
  }
  }
}

static void emitBuiltinApplyInst(IRGenSILFunction &IGF,
                                 Identifier builtin,
                                 ApplyInst *i,
                                 ArrayRef<Substitution> substitutions) {
  auto argValues = i->getArgumentsWithoutIndirectResult();
  auto params = i->getOrigCalleeType()->getParametersWithoutIndirectResult();
  assert(argValues.size() == params.size());
  auto subs = i->getSubstitutions();
  
  Explosion args(ExplosionKind::Maximal);
  for (auto index : indices(argValues)) {
    emitApplyArgument(IGF, argValues[index], params[index], subs, args);
  }
  
  if (i->hasIndirectResult()) {
    Address indirectResult = IGF.getLoweredAddress(i->getIndirectResult());
    emitBuiltinCall(IGF, builtin, i->getSubstCalleeType(),
                    args, nullptr, indirectResult, substitutions);
  } else {
    Explosion result(ExplosionKind::Maximal);
    emitBuiltinCall(IGF, builtin, i->getSubstCalleeType(),
                    args, &result, Address(), substitutions);
    IGF.setLoweredExplosion(SILValue(i,0), result);
  }
}

void IRGenSILFunction::visitApplyInst(swift::ApplyInst *i) {
  SILValue v(i, 0);
  
  const LoweredValue &calleeLV = getLoweredValue(i->getCallee());
  
  // Handle builtin calls separately.
  if (calleeLV.kind == LoweredValue::Kind::BuiltinValue) {
    auto &builtin = calleeLV.getBuiltinValue();
    return emitBuiltinApplyInst(*this, builtin.getId(), i,
                                i->getSubstitutions());
  }

  auto origCalleeType = i->getOrigCalleeType();
  auto substCalleeType = i->getSubstCalleeType();
  
  CallEmission emission =
    getCallEmissionForLoweredValue(*this, origCalleeType, substCalleeType,
                                   calleeLV, i->getSubstitutions());
  
  auto params = origCalleeType->getParametersWithoutIndirectResult();
  auto args = i->getArgumentsWithoutIndirectResult();
  assert(params.size() == args.size());

  // Save off the indirect return argument, if any.
  SILValue indirectResult;
  if (i->hasIndirectResult()) {
    indirectResult = i->getIndirectResult();
  }

  // Lower the SIL arguments to IR arguments.
  Explosion llArgs(emission.getCurExplosionLevel());
  
  // ObjC message sends need special handling for the 'self' argument, which in
  // SIL gets curried to the end of the argument list but in IR is passed as the
  // first argument. It additionally may need to be wrapped in an objc_super
  // struct, and the '_cmd' argument needs to be passed alongside it.
  if (calleeLV.kind == LoweredValue::Kind::ObjCMethod) {
    SILValue selfValue = args.back();
    args = args.slice(0, args.size() - 1);
    params = params.slice(0, params.size() - 1);

    llvm::Value *selfArg;
    // Convert a metatype 'self' argument to the ObjC Class pointer.
    if (selfValue.getType().is<MetaTypeType>()) {
      selfArg = getObjCClassForValue(*this, selfValue);
    } else {
      Explosion selfExplosion = getLoweredExplosion(selfValue);
      selfArg = selfExplosion.claimNext();
    }

    addObjCMethodCallImplicitArguments(*this, llArgs,
                                 calleeLV.getObjCMethod().getMethod(),
                                 selfArg,
                                 calleeLV.getObjCMethod().getSuperSearchType());
  }

  // Turn the formal SIL parameters into IR-gen things.
  for (auto index : indices(args)) {
    emitApplyArgument(*this, args[index], params[index],
                      i->getSubstitutions(), llArgs);
  }

  // Pass the generic arguments first.
  if (origCalleeType->isPolymorphic()) {
    emitPolymorphicArguments(*this, origCalleeType, substCalleeType,
                             i->getSubstitutions(), llArgs);
  }

  // Add all those arguments.
  emission.addArg(llArgs);
  
  // If the SIL function takes an indirect-result argument, emit into it.
  if (indirectResult) {
    Address a = getLoweredAddress(indirectResult);
    auto &retTI = getTypeInfo(indirectResult.getType());
    emission.emitToMemory(a, retTI);
    return;
  }
  
  // FIXME: handle the result being an address. This doesn't happen normally
  // in Swift but is how SIL currently models global accessors, and could also
  // be how we model "address" properties in the future.
  
  // If the result is a non-address value, emit to an explosion.
  Explosion result(emission.getCurExplosionLevel());
  emission.emitToExplosion(result);
  setLoweredExplosion(SILValue(i, 0), result);
}

static std::tuple<llvm::Value*, llvm::Value*, CanSILFunctionType>
getPartialApplicationFunction(IRGenSILFunction &IGF,
                              SILValue v) {
  LoweredValue &lv = IGF.getLoweredValue(v);

  switch (lv.kind) {
  case LoweredValue::Kind::Address:
    llvm_unreachable("can't partially apply an address");
  case LoweredValue::Kind::ObjCMethod:
    llvm_unreachable("objc method partial application shouldn't get here");

  case LoweredValue::Kind::StaticFunction:
    switch (lv.getStaticFunction().getAbstractCC()) {
    case AbstractCC::C:
    case AbstractCC::ObjCMethod:
      assert(false && "partial_apply of foreign functions not implemented");
      break;
        
    case AbstractCC::WitnessMethod:
      assert(false && "partial_apply of witness functions not implemented");
      break;
      
    case AbstractCC::Freestanding:
    case AbstractCC::Method:
      break;
    }
    return std::make_tuple(lv.getStaticFunction().getFunction(),
                           nullptr, v.getType().castTo<SILFunctionType>());
  case LoweredValue::Kind::Explosion:
  case LoweredValue::Kind::MetatypeValue:
  case LoweredValue::Kind::BuiltinValue: {
    Explosion ex = lv.getExplosion(IGF);
    llvm::Value *fn = ex.claimNext();
    llvm::Value *context = nullptr;
    auto fnType = v.getType().castTo<SILFunctionType>();
    if (!fnType->isThin())
      context = ex.claimNext();

    return std::make_tuple(fn, context, fnType);
  }
  }
}

void IRGenSILFunction::visitPartialApplyInst(swift::PartialApplyInst *i) {
  SILValue v(i, 0);

  // Apply the closure up to the next-to-last uncurry level to gather the
  // context arguments.

  // Note that we collect the arguments under the substituted type.
  auto args = i->getArguments();
  auto params = i->getSubstCalleeType()->getParameters();
  params = params.slice(params.size() - args.size(), args.size());
  
  Explosion llArgs(ExplosionKind::Maximal);
  SmallVector<SILType, 8> argTypes;

  for (auto index : indices(args)) {
    assert(args[index].getType() = params[index].getSILType());
    emitApplyArgument(*this, args[index], params[index], {}, llArgs);
    // FIXME: Need to carry the address-ness of each argument alongside
    // the object type's TypeInfo.
    argTypes.push_back(args[index].getType());
  }
  
  auto &lv = getLoweredValue(i->getCallee());
  if (lv.kind == LoweredValue::Kind::ObjCMethod) {
    // Objective-C partial applications require a different path. There's no
    // actual function pointer to capture, and we semantically can't cache
    // dispatch, so we need to perform the message send in the partial
    // application thunk.
    auto &objcMethod = lv.getObjCMethod();
    assert(i->getArguments().size() == 1 &&
           "only partial application of objc method to self implemented");
    assert(llArgs.size() == 1 &&
           "objc partial_apply argument is not a single retainable pointer?!");
    llvm::Value *selfVal = llArgs.claimNext();
    
    Explosion function(ExplosionKind::Maximal);
    emitObjCPartialApplication(*this,
                               objcMethod.getMethod(),
                               i->getOrigCalleeType(),
                               i->getType().castTo<SILFunctionType>(),
                               selfVal,
                               i->getArguments()[0].getType(),
                               function);
    setLoweredExplosion(SILValue(i, 0), function);
    return;
  }
  
  // Get the function value.
  llvm::Value *calleeFn = nullptr;
  llvm::Value *innerContext = nullptr;
  CanSILFunctionType origCalleeTy;
  
  std::tie(calleeFn, innerContext, origCalleeTy)
    = getPartialApplicationFunction(*this, i->getCallee());
  
  // Create the thunk and function value.
  Explosion function(ExplosionKind::Maximal);
  emitFunctionPartialApplication(*this, calleeFn, innerContext, llArgs,
                                 argTypes, i->getSubstitutions(),
                                 origCalleeTy, i->getSubstCalleeType(),
                                 i->getType().castTo<SILFunctionType>(),
                                 function);
  setLoweredExplosion(v, function);
}

void IRGenSILFunction::visitIntegerLiteralInst(swift::IntegerLiteralInst *i) {
  APInt value = i->getValue();
  BuiltinIntegerWidth width
    = i->getType().castTo<BuiltinIntegerType>()->getWidth();
  
  // The value may need truncation if its type had an abstract size.
  if (width.isFixedWidth()) {
    // nothing to do
  } else if (width.isPointerWidth()) {
    unsigned pointerWidth = IGM.getPointerSize().getValueInBits();
    assert(pointerWidth <= value.getBitWidth()
           && "lost precision at AST/SIL level?!");
    if (pointerWidth < value.getBitWidth())
      value = value.trunc(pointerWidth);
  } else {
    llvm_unreachable("impossible width value");
  }
  
  llvm::Value *constant = llvm::ConstantInt::get(IGM.LLVMContext, value);
  
  Explosion e(ExplosionKind::Maximal);
  e.add(constant);
  setLoweredExplosion(SILValue(i, 0), e);
}

void IRGenSILFunction::visitFloatLiteralInst(swift::FloatLiteralInst *i) {
  llvm::Value *constant = llvm::ConstantFP::get(IGM.LLVMContext,
                                                i->getValue());
  Explosion e(ExplosionKind::Maximal);
  e.add(constant);
  setLoweredExplosion(SILValue(i, 0), e);
}

void IRGenSILFunction::visitStringLiteralInst(swift::StringLiteralInst *i) {
  {
    Explosion e(ExplosionKind::Maximal);
    e.add(IGM.getAddrOfGlobalString(i->getValue()));
    setLoweredExplosion(SILValue(i, 0), e);
  }
  {
    Explosion e(ExplosionKind::Maximal);
    e.add(Builder.getInt64(i->getValue().size()));
    setLoweredExplosion(SILValue(i, 1), e);
  }
  
  // Determine whether this is an ASCII string.
  bool isASCII = true;
  for (unsigned char c : i->getValue()) {
    if (c > 127) {
      isASCII = false;
      break;
    }
  }

  Explosion e(ExplosionKind::Maximal);
  e.add(Builder.getInt1(isASCII));
  setLoweredExplosion(SILValue(i, 2), e);
}

void IRGenSILFunction::visitUnreachableInst(swift::UnreachableInst *i) {
  // TODO: When we have "checked"/"unchecked" mode support, drop the trap.
  llvm::Function *trapIntrinsic = llvm::Intrinsic::getDeclaration(&IGM.Module,
                                                    llvm::Intrinsic::ID::trap);
  Builder.CreateCall(trapIntrinsic);
  Builder.CreateUnreachable();
}

static void emitReturnInst(IRGenSILFunction &IGF,
                           SILType resultTy,
                           Explosion &result) {
  // Even if SIL has a direct return, the IR-level calling convention may
  // require an indirect return.
  if (IGF.IndirectReturn.isValid()) {
    auto &retTI = cast<LoadableTypeInfo>(IGF.getTypeInfo(resultTy));
    retTI.initialize(IGF, result, IGF.IndirectReturn);
    IGF.Builder.CreateRetVoid();
  } else {
    IGF.emitScalarReturn(result);
  }
}

void IRGenSILFunction::visitReturnInst(swift::ReturnInst *i) {
  Explosion result = getLoweredExplosion(i->getOperand());
  emitReturnInst(*this, i->getOperand().getType(), result);
}

void IRGenSILFunction::visitAutoreleaseReturnInst(AutoreleaseReturnInst *i) {
  Explosion result = getLoweredExplosion(i->getOperand());
  assert(result.size() == 1 &&
         "should have one objc pointer value for autorelease_return");
  emitObjCAutoreleaseReturnValue(*this, result.getAll()[0]);
  emitReturnInst(*this, i->getOperand().getType(), result);
}

void IRGenSILFunction::visitSwitchIntInst(SwitchIntInst *i) {
  llvm_unreachable("not implemented");
}

// Bind an incoming explosion value to a SILArgument's LLVM phi node(s).
static void addIncomingExplosionToPHINodes(IRGenSILFunction &IGF,
                                           LoweredBB &lbb,
                                           unsigned &phiIndex,
                                           Explosion &argValue) {
  llvm::BasicBlock *curBB = IGF.Builder.GetInsertBlock();
  while (!argValue.empty())
    lbb.phis[phiIndex++]->addIncoming(argValue.claimNext(), curBB);
}

// Bind an incoming address value to a SILArgument's LLVM phi node(s).
static void addIncomingAddressToPHINodes(IRGenSILFunction &IGF,
                                         LoweredBB &lbb,
                                         unsigned &phiIndex,
                                         Address argValue) {
  llvm::BasicBlock *curBB = IGF.Builder.GetInsertBlock();
  lbb.phis[phiIndex++]->addIncoming(argValue.getAddress(), curBB);
}

// Add branch arguments to destination phi nodes.
static void addIncomingSILArgumentsToPHINodes(IRGenSILFunction &IGF,
                                              LoweredBB &lbb,
                                              OperandValueArrayRef args) {
  unsigned phiIndex = 0;
  for (SILValue arg : args) {
    const LoweredValue &lv = IGF.getLoweredValue(arg);
  
    if (lv.isAddress()) {
      addIncomingAddressToPHINodes(IGF, lbb, phiIndex, lv.getAddress());
      continue;
    }
    
    Explosion argValue = lv.getExplosion(IGF);
    addIncomingExplosionToPHINodes(IGF, lbb, phiIndex, argValue);
  }
}

static llvm::BasicBlock *emitBBMapForSwitchEnum(
        IRGenSILFunction &IGF,
        SmallVectorImpl<std::pair<EnumElementDecl*, llvm::BasicBlock*>> &dests,
        SwitchEnumInstBase *inst) {
  for (unsigned i = 0, e = inst->getNumCases(); i < e; ++i) {
    auto casePair = inst->getCase(i);
    
    // If the destination BB accepts the case argument, set up a waypoint BB so
    // we can feed the values into the argument's PHI node(s).
    //
    // FIXME: This is cheesy when the destination BB has only the switch
    // as a predecessor.
    if (!casePair.second->bbarg_empty())
      dests.push_back({casePair.first,
        llvm::BasicBlock::Create(IGF.IGM.getLLVMContext())});
    else
      dests.push_back({casePair.first, IGF.getLoweredBB(casePair.second).bb});
  }
  
  llvm::BasicBlock *defaultDest = nullptr;
  if (inst->hasDefault())
    defaultDest = IGF.getLoweredBB(inst->getDefaultBB()).bb;
  return defaultDest;
}

void IRGenSILFunction::visitSwitchEnumInst(SwitchEnumInst *inst) {
  Explosion value = getLoweredExplosion(inst->getOperand());
  
  // Map the SIL dest bbs to their LLVM bbs.
  SmallVector<std::pair<EnumElementDecl*, llvm::BasicBlock*>, 4> dests;
  llvm::BasicBlock *defaultDest
    = emitBBMapForSwitchEnum(*this, dests, inst);
  
  // Emit the dispatch.
  emitSwitchLoadableEnumDispatch(*this, inst->getOperand().getType(),
                                  value, dests, defaultDest);
  
  // Bind arguments for cases that want them.
  for (unsigned i = 0, e = inst->getNumCases(); i < e; ++i) {
    auto casePair = inst->getCase(i);
    
    if (!casePair.second->bbarg_empty()) {
      auto waypointBB = dests[i].second;
      auto &destLBB = getLoweredBB(casePair.second);
      
      Builder.emitBlock(waypointBB);
      
      Explosion inValue = getLoweredExplosion(inst->getOperand());
      Explosion projected(ExplosionKind::Minimal);
      emitProjectLoadableEnum(*this, inst->getOperand().getType(),
                               inValue, casePair.first, projected);
      
      unsigned phiIndex = 0;
      addIncomingExplosionToPHINodes(*this, destLBB, phiIndex, projected);
      
      Builder.CreateBr(destLBB.bb);
    }
  }
}

void
IRGenSILFunction::visitDestructiveSwitchEnumAddrInst(
                                        DestructiveSwitchEnumAddrInst *inst) {
  Address value = getLoweredAddress(inst->getOperand());
  
  // Map the SIL dest bbs to their LLVM bbs.
  SmallVector<std::pair<EnumElementDecl*, llvm::BasicBlock*>, 4> dests;
  llvm::BasicBlock *defaultDest
    = emitBBMapForSwitchEnum(*this, dests, inst);
  
  // Emit the dispatch.
  emitSwitchAddressOnlyEnumDispatch(*this, inst->getOperand().getType(),
                                     value, dests, defaultDest);

  // Bind arguments for cases that want them.
  for (unsigned i = 0, e = inst->getNumCases(); i < e; ++i) {
    auto casePair = inst->getCase(i);
    if (!casePair.second->bbarg_empty()) {
      auto waypointBB = dests[i].second;
      auto &destLBB = getLoweredBB(casePair.second);
      
      Builder.emitBlock(waypointBB);

      Address data
        = emitDestructiveProjectEnumAddressForLoad(*this,
                                                  inst->getOperand().getType(),
                                                  value, casePair.first);
      unsigned phiIndex = 0;
      addIncomingAddressToPHINodes(*this, destLBB, phiIndex, data);
      Builder.CreateBr(destLBB.bb);
    }
  }
}

void IRGenSILFunction::visitDynamicMethodBranchInst(DynamicMethodBranchInst *i){
  LoweredBB &hasMethodBB = getLoweredBB(i->getHasMethodBB());
  LoweredBB &noMethodBB = getLoweredBB(i->getNoMethodBB());

  // Emit the swift_objcRespondsToSelector() call.
  // FIXME: Make this work for subscripts as well.
  StringRef selector;
  llvm::SmallString<64> selectorBuffer;
  if (auto fnDecl = dyn_cast<FuncDecl>(i->getMember().getDecl()))
    selector = fnDecl->getObjCSelector(selectorBuffer);
  else if (auto var = dyn_cast<VarDecl>(i->getMember().getDecl()))
    selector = var->getObjCGetterSelector(selectorBuffer);
  else if (auto subscript = dyn_cast<SubscriptDecl>(i->getMember().getDecl())) {
    selector = subscript->getObjCGetterSelector();
  } else {
    llvm_unreachable("Unhandled dynamic method branch query");
  }

  llvm::Value *object = getLoweredExplosion(i->getOperand()).claimNext();
  if (object->getType() != IGM.ObjCPtrTy)
    object = Builder.CreateBitCast(object, IGM.ObjCPtrTy);
  llvm::Value *loadSel = emitObjCSelectorRefLoad(selector);
  llvm::CallInst *call = Builder.CreateCall2(IGM.getObjCRespondsToSelectorFn(),
                                             object, loadSel);
  call->setDoesNotThrow();

  // FIXME: Assume (probably safely) that the hasMethodBB has only us as a
  // predecessor, and cannibalize its bb argument so we can represent is as an
  // ObjCMethod lowered value. This is hella gross but saves us having to
  // implement ObjCMethod-to-Explosion lowering and creating a thunk we don't
  // want.
  assert(std::next(i->getHasMethodBB()->pred_begin())
           == i->getHasMethodBB()->pred_end()
         && "lowering dynamic_method_br with multiple preds for destination "
            "not implemented");
  // Kill the existing lowered value for the bb arg and its phi nodes.
  SILValue methodArg = i->getHasMethodBB()->bbarg_begin()[0];
  Explosion formerLLArg = getLoweredExplosion(methodArg);
  for (llvm::Value *val : formerLLArg.claimAll()) {
    auto phi = cast<llvm::PHINode>(val);
    assert(phi->getNumIncomingValues() == 0 && "phi already used");
    phi->removeFromParent();
    delete phi;
  }
  LoweredValues.erase(methodArg);
  
  // Replace the lowered value with an ObjCMethod lowering.
  setLoweredObjCMethod(methodArg, i->getMember());
  
  // Create the branch.
  Builder.CreateCondBr(call, hasMethodBB.bb, noMethodBB.bb);
}

void IRGenSILFunction::visitBranchInst(swift::BranchInst *i) {
  LoweredBB &lbb = getLoweredBB(i->getDestBB());
  addIncomingSILArgumentsToPHINodes(*this, lbb, i->getArgs());
  Builder.CreateBr(lbb.bb);
}

void IRGenSILFunction::visitCondBranchInst(swift::CondBranchInst *i) {
  LoweredBB &trueBB = getLoweredBB(i->getTrueBB());
  LoweredBB &falseBB = getLoweredBB(i->getFalseBB());
  llvm::Value *condValue =
    getLoweredExplosion(i->getCondition()).claimNext();

  addIncomingSILArgumentsToPHINodes(*this, trueBB, i->getTrueArgs());
  addIncomingSILArgumentsToPHINodes(*this, falseBB, i->getFalseArgs());
  
  Builder.CreateCondBr(condValue, trueBB.bb, falseBB.bb);
}

void IRGenSILFunction::visitCopyValueInst(swift::CopyValueInst *i) {
  Explosion in = getLoweredExplosion(i->getOperand());
  Explosion out(in.getKind());
  cast<LoadableTypeInfo>(getTypeInfo(i->getType())).copy(*this, in, out);
  setLoweredExplosion(SILValue(i, 0), out);
}

void IRGenSILFunction::visitDestroyValueInst(swift::DestroyValueInst *i) {
  Explosion in = getLoweredExplosion(i->getOperand());
  cast<LoadableTypeInfo>(getTypeInfo(i->getOperand().getType()))
    .consume(*this, in);
}

void IRGenSILFunction::visitStructInst(swift::StructInst *i) {
  Explosion out(ExplosionKind::Maximal);
  for (SILValue elt : i->getElements())
    out.add(getLoweredExplosion(elt).claimAll());
  setLoweredExplosion(SILValue(i, 0), out);
}

void IRGenSILFunction::visitTupleInst(swift::TupleInst *i) {
  Explosion out(ExplosionKind::Maximal);
  for (SILValue elt : i->getElements())
    out.add(getLoweredExplosion(elt).claimAll());
  setLoweredExplosion(SILValue(i, 0), out);
}

void IRGenSILFunction::visitEnumInst(swift::EnumInst *i) {
  Explosion data = (i->hasOperand())
    ? getLoweredExplosion(i->getOperand())
    : Explosion(ExplosionKind::Minimal);
  Explosion out(ExplosionKind::Maximal);
  emitInjectLoadableEnum(*this, i->getType(), i->getElement(), data, out);
  setLoweredExplosion(SILValue(i, 0), out);
}

void IRGenSILFunction::visitEnumDataAddrInst(swift::EnumDataAddrInst *i) {
  Address enumAddr = getLoweredAddress(i->getOperand());
  Address dataAddr = emitProjectEnumAddressForStore(*this,
                                                     i->getOperand().getType(),
                                                     enumAddr,
                                                     i->getElement());
  setLoweredAddress(SILValue(i, 0), dataAddr);
}

void IRGenSILFunction::visitInjectEnumAddrInst(swift::InjectEnumAddrInst *i) {
  Address enumAddr = getLoweredAddress(i->getOperand());
  emitStoreEnumTagToAddress(*this, i->getOperand().getType(),
                             enumAddr, i->getElement());
}

void IRGenSILFunction::visitBuiltinZeroInst(swift::BuiltinZeroInst *i) {
  auto &ti = getTypeInfo(i->getType());
  llvm::Value *zeroValue = llvm::Constant::getNullValue(ti.getStorageType());
  Explosion out(ExplosionKind::Maximal);
  out.add(zeroValue);
  setLoweredExplosion(SILValue(i, 0), out);
}

void IRGenSILFunction::visitTupleExtractInst(swift::TupleExtractInst *i) {
  SILValue v(i, 0);
  Explosion fullTuple = getLoweredExplosion(i->getOperand());
  Explosion output(fullTuple.getKind());
  SILType baseType = i->getOperand().getType();
  
  projectTupleElementFromExplosion(*this,
                                   baseType,
                                   fullTuple,
                                   i->getFieldNo(),
                                   output);
  fullTuple.claimAll();
  setLoweredExplosion(v, output);
}

void IRGenSILFunction::visitTupleElementAddrInst(swift::TupleElementAddrInst *i)
{
  Address base = getLoweredAddress(i->getOperand());
  SILType baseType = i->getOperand().getType();

  Address field = projectTupleElementAddress(*this, base, baseType,
                                             i->getFieldNo());
  setLoweredAddress(SILValue(i, 0), field);
}

void IRGenSILFunction::visitStructExtractInst(swift::StructExtractInst *i) {
  SILValue v(i, 0);
  Explosion operand = getLoweredExplosion(i->getOperand());
  Explosion lowered(operand.getKind());
  SILType baseType = i->getOperand().getType();
  
  projectPhysicalStructMemberFromExplosion(*this,
                                           baseType,
                                           operand,
                                           i->getField(),
                                           lowered);

  operand.claimAll();
  setLoweredExplosion(v, lowered);
}

void IRGenSILFunction::visitStructElementAddrInst(
                                              swift::StructElementAddrInst *i) {
  Address base = getLoweredAddress(i->getOperand());
  SILType baseType = i->getOperand().getType();

  Address field = projectPhysicalStructMemberAddress(*this, base, baseType,
                                                     i->getField());
  setLoweredAddress(SILValue(i, 0), field);
}

void IRGenSILFunction::visitRefElementAddrInst(swift::RefElementAddrInst *i) {
  Explosion base = getLoweredExplosion(i->getOperand());
  llvm::Value *value = base.claimNext();
  
  SILType baseTy = i->getOperand().getType();
  Address field = projectPhysicalClassMemberAddress(*this,
                                                    value,
                                                    baseTy,
                                                    i->getField())
    .getAddress();
  setLoweredAddress(SILValue(i, 0), field);
}

void IRGenSILFunction::visitLoadInst(swift::LoadInst *i) {
  Explosion lowered(ExplosionKind::Maximal);
  Address source = getLoweredAddress(i->getOperand());
  const TypeInfo &type = getTypeInfo(i->getType().getObjectType());
  cast<LoadableTypeInfo>(type).loadAsTake(*this, source, lowered);
  setLoweredExplosion(SILValue(i, 0), lowered);
}

void IRGenSILFunction::visitStoreInst(swift::StoreInst *i) {
  Explosion source = getLoweredExplosion(i->getSrc());
  Address dest = getLoweredAddress(i->getDest());
  auto &type = getTypeInfo(i->getSrc().getType().getObjectType());
  cast<LoadableTypeInfo>(type).initialize(*this, source, dest);
}


void IRGenSILFunction::visitLoadWeakInst(swift::LoadWeakInst *i) {
  Address source = getLoweredAddress(i->getOperand());
  auto &weakTI = cast<WeakTypeInfo>(getTypeInfo(i->getOperand().getType()));

  Explosion result(ExplosionKind::Maximal);
  if (i->isTake()) {
    weakTI.weakTakeStrong(*this, source, result);
  } else {
    weakTI.weakLoadStrong(*this, source, result);
  }

  setLoweredExplosion(SILValue(i, 0), result);
}

void IRGenSILFunction::visitStoreWeakInst(swift::StoreWeakInst *i) {
  Explosion source = getLoweredExplosion(i->getSrc());
  Address dest = getLoweredAddress(i->getDest());

  auto &weakTI = cast<WeakTypeInfo>(getTypeInfo(i->getDest().getType()));
  if (i->isInitializationOfDest()) {
    weakTI.weakInit(*this, source, dest);
  } else {
    weakTI.weakAssign(*this, source, dest);
  }
}

void IRGenSILFunction::visitStrongRetainInst(swift::StrongRetainInst *i) {
  Explosion lowered = getLoweredExplosion(i->getOperand());
  auto &ti = cast<ReferenceTypeInfo>(getTypeInfo(i->getOperand().getType()));
  ti.retain(*this, lowered);
}

void IRGenSILFunction::visitStrongReleaseInst(swift::StrongReleaseInst *i) {
  Explosion lowered = getLoweredExplosion(i->getOperand());
  auto &ti = cast<ReferenceTypeInfo>(getTypeInfo(i->getOperand().getType()));
  ti.release(*this, lowered);
}

void IRGenSILFunction::
visitStrongRetainAutoreleasedInst(swift::StrongRetainAutoreleasedInst *i) {
  Explosion lowered = getLoweredExplosion(i->getOperand());
  llvm::Value *value = lowered.claimNext();
  emitObjCRetainAutoreleasedReturnValue(*this, value);
}

/// Given a SILType which is a ReferenceStorageType, return the type
/// info for the underlying reference type.
static const ReferenceTypeInfo &getReferentTypeInfo(IRGenFunction &IGF,
                                                    SILType silType) {
  assert(silType.isObject());
  auto type = silType.castTo<ReferenceStorageType>().getReferentType();
  return cast<ReferenceTypeInfo>(IGF.getTypeInfoForLowered(type));
}

void IRGenSILFunction::
visitStrongRetainUnownedInst(swift::StrongRetainUnownedInst *i) {
  Explosion lowered = getLoweredExplosion(i->getOperand());
  auto &ti = getReferentTypeInfo(*this, i->getOperand().getType());
  ti.retainUnowned(*this, lowered);
}

void IRGenSILFunction::visitUnownedRetainInst(swift::UnownedRetainInst *i) {
  Explosion lowered = getLoweredExplosion(i->getOperand());
  auto &ti = getReferentTypeInfo(*this, i->getOperand().getType());
  ti.unownedRetain(*this, lowered);
}


void IRGenSILFunction::visitUnownedReleaseInst(swift::UnownedReleaseInst *i) {
  Explosion lowered = getLoweredExplosion(i->getOperand());
  auto &ti = getReferentTypeInfo(*this, i->getOperand().getType());
  ti.unownedRelease(*this, lowered);
}

void IRGenSILFunction::visitAllocStackInst(swift::AllocStackInst *i) {
  const TypeInfo &type = getTypeInfo(i->getElementType());

  // Derive name from SIL location.
  VarDecl *Decl = i->getDecl();
  StringRef dbgname =
# ifndef NDEBUG
    // If this is a DEBUG build, use pretty names for the LLVM IR.
    Decl ? Decl->getName().str() :
# endif
    "";

  auto addr = type.allocateStack(*this, dbgname);
  if (IGM.DebugInfo && Decl) {
    auto AddrIR = addr.getAddressPointer();
    auto DTI = DebugTypeInfo(Decl, type, i->getDebugScope());
    auto Name = Decl->getName().str();
    auto N = ArgNo.find(cast<VarDecl>(Decl));
    if (N != ArgNo.end())
      IGM.DebugInfo->emitArgVariableDeclaration(Builder, AddrIR, DTI, Name,
                                                N->second, DirectValue);
    else
      IGM.DebugInfo->emitStackVariableDeclaration(Builder, AddrIR, DTI, Name,
                                                  i, DirectValue);
  }

  setLoweredAddress(i->getContainerResult(), addr.getContainer());
  setLoweredAddress(i->getAddressResult(), addr.getAddress());
}

void IRGenSILFunction::visitAllocRefInst(swift::AllocRefInst *i) {
  llvm::Value *alloced = emitClassAllocation(*this, i->getType());
  Explosion e(ExplosionKind::Maximal);
  e.add(alloced);
  setLoweredExplosion(SILValue(i, 0), e);
}

void IRGenSILFunction::visitDeallocStackInst(swift::DeallocStackInst *i) {
  const TypeInfo &type = getTypeInfo(i->getOperand().getType());
  Address addr = getLoweredAddress(i->getOperand());
  type.deallocateStack(*this, addr);
}

void IRGenSILFunction::visitDeallocRefInst(swift::DeallocRefInst *i) {
  //llvm_unreachable("not implemented");
}

void IRGenSILFunction::visitDeallocBoxInst(swift::DeallocBoxInst *i) {
  //llvm_unreachable("not implemented");
}

void IRGenSILFunction::visitAllocBoxInst(swift::AllocBoxInst *i) {
  const TypeInfo &type = getTypeInfo(i->getElementType());

  // Derive name from SIL location.
  VarDecl *Decl = i->getDecl();
  StringRef Name = Decl ? Decl->getName().str() : "";
  StringRef DbgName =
# ifndef NDEBUG
    // If this is a DEBUG build, use pretty names for the LLVM IR.
    Name;
# else
    "";
# endif
  OwnedAddress addr = type.allocateBox(*this, DbgName);
  
  Explosion box(ExplosionKind::Maximal);
  box.add(addr.getOwner());
  setLoweredExplosion(SILValue(i, 0), box);
  setLoweredAddress(SILValue(i, 1), addr.getAddress());

  if (IGM.DebugInfo) {
    auto Indirection = IndirectValue;
    // LValues (@inout) are implicitly indirect because of their type.
    if (Decl && Decl->getType()->getKind() == TypeKind::LValue)
      Indirection = DirectValue;
    // FIXME: @inout arguments that are not promoted are emitted as
    // arguments and also boxed and thus may show up twice. This may
    // or may not be bad.
    IGM.DebugInfo->emitStackVariableDeclaration
      (Builder,
       emitShadowCopy(addr.getAddress(), Name),
       Decl ? DebugTypeInfo(Decl, type)
            : DebugTypeInfo(i->getElementType().getSwiftType(), type),
       Name, i, Indirection);
  }
}

void IRGenSILFunction::visitAllocArrayInst(swift::AllocArrayInst *i) {
  SILValue boxValue(i, 0);
  SILValue ptrValue(i, 1);
  
  Explosion lengthEx = getLoweredExplosion(i->getNumElements());
  llvm::Value *lengthValue = lengthEx.claimNext();
  HeapArrayInfo arrayInfo(*this, i->getElementType().getSwiftType());
  Address ptr;
  llvm::Value *box = arrayInfo.emitUnmanagedAlloc(*this, lengthValue, ptr, "");
  Explosion boxEx(ExplosionKind::Maximal);
  boxEx.add(box);
  setLoweredExplosion(boxValue, boxEx);
  setLoweredAddress(ptrValue, ptr);
}

void IRGenSILFunction::visitConvertFunctionInst(swift::ConvertFunctionInst *i) {
  // This instruction is specified to be a no-op.
  Explosion temp = getLoweredExplosion(i->getOperand());
  setLoweredExplosion(SILValue(i, 0), temp);
}

void IRGenSILFunction::visitAddressToPointerInst(swift::AddressToPointerInst *i)
{
  Explosion to(ExplosionKind::Maximal);
  llvm::Value *addrValue = getLoweredAddress(i->getOperand()).getAddress();
  if (addrValue->getType() != IGM.Int8PtrTy)
    addrValue = Builder.CreateBitCast(addrValue, IGM.Int8PtrTy);
  to.add(addrValue);
  setLoweredExplosion(SILValue(i, 0), to);
}

void IRGenSILFunction::visitPointerToAddressInst(swift::PointerToAddressInst *i)
{
  Explosion from = getLoweredExplosion(i->getOperand());
  llvm::Value *ptrValue = from.claimNext();

  auto &ti = getTypeInfo(i->getType());
  
  llvm::Type *destType = ti.getStorageType()->getPointerTo();
  ptrValue = Builder.CreateBitCast(ptrValue, destType);
  
  setLoweredAddress(SILValue(i, 0),
                    ti.getAddressForPointer(ptrValue));
}

static void emitPointerCastInst(IRGenSILFunction &IGF,
                                SILValue src,
                                SILValue dest,
                                llvm::Type *castToType) {
  Explosion from = IGF.getLoweredExplosion(src);
  llvm::Value *ptrValue = from.claimNext();
  
  ptrValue = IGF.Builder.CreateBitCast(ptrValue, castToType);
  
  Explosion to(ExplosionKind::Maximal);
  to.add(ptrValue);
  IGF.setLoweredExplosion(dest, to);
}

void IRGenSILFunction::visitRefToObjectPointerInst(
                                             swift::RefToObjectPointerInst *i) {
  auto &ti = getTypeInfo(i->getType());
  llvm::Type *destType = ti.getStorageType();
  emitPointerCastInst(*this, i->getOperand(), SILValue(i, 0), destType);
}

void IRGenSILFunction::visitObjectPointerToRefInst(
                                             swift::ObjectPointerToRefInst *i) {
  auto &ti = getTypeInfo(i->getType());
  llvm::Type *destType = ti.getStorageType();
  emitPointerCastInst(*this, i->getOperand(), SILValue(i, 0), destType);
}

void IRGenSILFunction::visitRefToRawPointerInst(
                                             swift::RefToRawPointerInst *i) {
  emitPointerCastInst(*this, i->getOperand(), SILValue(i, 0),
                      IGM.Int8PtrTy);
}

void IRGenSILFunction::visitRawPointerToRefInst(swift::RawPointerToRefInst *i) {
  auto &ti = getTypeInfo(i->getType());
  llvm::Type *destType = ti.getStorageType();
  emitPointerCastInst(*this, i->getOperand(), SILValue(i, 0),
                      destType);
}

void IRGenSILFunction::visitUnownedToRefInst(swift::UnownedToRefInst *i) {
  // This instruction is specified to be a no-op.
  Explosion temp = getLoweredExplosion(i->getOperand());
  setLoweredExplosion(SILValue(i, 0), temp);
}

void IRGenSILFunction::visitRefToUnownedInst(swift::RefToUnownedInst *i) {
  // This instruction is specified to be a no-op.
  Explosion temp = getLoweredExplosion(i->getOperand());
  setLoweredExplosion(SILValue(i, 0), temp);
}

void IRGenSILFunction::visitThinToThickFunctionInst(
                                            swift::ThinToThickFunctionInst *i) {
  // Take the incoming function pointer and add a null context pointer to it.
  Explosion from = getLoweredExplosion(i->getOperand());
  Explosion to(ExplosionKind::Maximal);
  to.add(from.claimNext());
  to.add(IGM.RefCountedNull);
  setLoweredExplosion(SILValue(i, 0), to);
}

void IRGenSILFunction::visitBridgeToBlockInst(swift::BridgeToBlockInst *i) {
  Explosion from = getLoweredExplosion(i->getOperand());
  Explosion to(ExplosionKind::Maximal);
  emitBridgeToBlock(*this, i->getType(), from, to);
  setLoweredExplosion(SILValue(i, 0), to);
}

void IRGenSILFunction::visitArchetypeRefToSuperInst(
                                              swift::ArchetypeRefToSuperInst *i) {
  // Get the archetype value.
  Explosion archetype = getLoweredExplosion(i->getOperand());
  llvm::Value *in = archetype.claimNext();
  
  Explosion out(ExplosionKind::Maximal);
  const TypeInfo &baseTypeInfo = getTypeInfo(i->getType());
  llvm::Type *baseTy = baseTypeInfo.StorageType;
  llvm::Value *cast = Builder.CreateBitCast(in, baseTy);
  out.add(cast);
  setLoweredExplosion(SILValue(i, 0), out);
}

/// Emit a checked cast sequence. Returns an Address; this may be either
/// a proper address or a class reference pointer, depending on the address-
/// or object-ness of the cast.
static Address emitCheckedCast(IRGenSILFunction &IGF,
                               SILValue operand,
                               SILType destTy,
                               CheckedCastKind kind,
                               CheckedCastMode mode) {
  switch (kind) {
  case CheckedCastKind::Unresolved:
  case CheckedCastKind::Coercion:
    llvm_unreachable("invalid for sil");
    
  case CheckedCastKind::Downcast: {
    Explosion from = IGF.getLoweredExplosion(operand);
    llvm::Value *fromValue = from.claimNext();
    llvm::Value *cast = IGF.emitDowncast(fromValue, destTy, mode);
    return Address(cast, Alignment(1));
  }
    
  case CheckedCastKind::SuperToArchetype: {
    Explosion super = IGF.getLoweredExplosion(operand);
    llvm::Value *in = super.claimNext();
    Explosion out(ExplosionKind::Maximal);
    llvm::Value *cast
      = IGF.emitSuperToClassArchetypeConversion(in, destTy, mode);
    return Address(cast, Alignment(1));
  }
      
  case CheckedCastKind::ArchetypeToArchetype:
  case CheckedCastKind::ArchetypeToConcrete:
  case CheckedCastKind::ConcreteToArchetype: {
    if (operand.getType().isAddress()) {
      Address archetype = IGF.getLoweredAddress(operand);
      return emitOpaqueArchetypeDowncast(IGF, archetype,
                                         operand.getType(),
                                         destTy,
                                         mode);
    } else {
      Explosion archetype = IGF.getLoweredExplosion(operand);
      llvm::Value *fromValue = archetype.claimNext();
      llvm::Value *toValue = IGF.emitDowncast(fromValue, destTy, mode);
      return Address(toValue, Alignment(1));
    }
  }
      
  case CheckedCastKind::ExistentialToArchetype:
  case CheckedCastKind::ExistentialToConcrete: {
    if (operand.getType().isAddress()) {
      Address existential = IGF.getLoweredAddress(operand);
      return emitIndirectExistentialDowncast(IGF, existential,
                                             operand.getType(),
                                             destTy,
                                             mode);
    } else {
      Explosion existential = IGF.getLoweredExplosion(operand);
      llvm::Value *instance
        = emitClassExistentialProjection(IGF, existential,
                                         operand.getType());
      
      llvm::Value *toValue = IGF.emitDowncast(instance, destTy, mode);
      return Address(toValue, Alignment(1));
    }
  }
  case CheckedCastKind::ConcreteToUnrelatedExistential: {
    Explosion from = IGF.getLoweredExplosion(operand);
    llvm::Value *fromValue = from.claimNext();
    llvm::Value *cast = emitObjCExistentialDowncast(IGF, fromValue,
                                                    operand.getType(),
                                                    destTy, mode);
    return Address(cast, Alignment(1));
  }
  }
}

void IRGenSILFunction::visitUnconditionalCheckedCastInst(
                                       swift::UnconditionalCheckedCastInst *i) {
  Address val = emitCheckedCast(*this, i->getOperand(), i->getType(),
                                i->getCastKind(),
                                CheckedCastMode::Unconditional);
  
  if (i->getType().isAddress()) {
    setLoweredAddress(SILValue(i,0), val);
  } else {
    Explosion ex(ExplosionKind::Maximal);
    ex.add(val.getAddress());
    setLoweredExplosion(SILValue(i,0), ex);
  }
}

void IRGenSILFunction::visitCheckedCastBranchInst(
                                              swift::CheckedCastBranchInst *i) {
  // Emit the cast operation.
  Address val = emitCheckedCast(*this, i->getOperand(), i->getCastType(),
                                i->getCastKind(),
                                CheckedCastMode::Conditional);
  
  // Branch on the success of the cast.
  // All cast operations currently return null on failure.
  llvm::Value *isNonnull = Builder.CreateICmpNE(val.getAddress(),
     llvm::ConstantPointerNull::get(val.getType()));
  
  auto &successBB = getLoweredBB(i->getSuccessBB());
  
  Builder.CreateCondBr(isNonnull,
                       successBB.bb,
                       getLoweredBB(i->getFailureBB()).bb);
  
  // Feed the cast result into the nonnull branch.
  unsigned phiIndex = 0;
  if (i->getCastType().isAddress())
    addIncomingAddressToPHINodes(*this, successBB, phiIndex, val);
  else {
    Explosion ex(ExplosionKind::Maximal);
    ex.add(val.getAddress());
    addIncomingExplosionToPHINodes(*this, successBB, phiIndex, ex);
  }
}

void IRGenSILFunction::visitIsNonnullInst(swift::IsNonnullInst *i) {
  // Get the value we're testing, which may be an address or an instance
  // pointer.
  llvm::Value *val;
  const LoweredValue &lv = getLoweredValue(i->getOperand());
  if (lv.isAddress()) {
    val = lv.getAddress().getAddress();
  } else {
    Explosion values = lv.getExplosion(*this);
    val = values.claimNext();
  }
  
  // Check that the result isn't null.
  auto *valTy = cast<llvm::PointerType>(val->getType());
  llvm::Value *result = Builder.CreateICmp(llvm::CmpInst::ICMP_NE,
                                    val, llvm::ConstantPointerNull::get(valTy));
  
  Explosion out(ExplosionKind::Maximal);
  out.add(result);
  setLoweredExplosion(SILValue(i, 0), out);
}

void IRGenSILFunction::visitCoerceInst(swift::CoerceInst *i) {
  Explosion from = getLoweredExplosion(i->getOperand());
  setLoweredExplosion(SILValue(i, 0), from);
}

void IRGenSILFunction::visitUpcastInst(swift::UpcastInst *i) {
  Explosion from = getLoweredExplosion(i->getOperand());
  Explosion to(from.getKind());
  assert(from.size() == 1 && "class should explode to single value");
  const TypeInfo &toTI = getTypeInfo(i->getType());
  llvm::Value *fromValue = from.claimNext();
  to.add(Builder.CreateBitCast(fromValue, toTI.getStorageType()));
  setLoweredExplosion(SILValue(i, 0), to);
}

void IRGenSILFunction::visitIndexAddrInst(swift::IndexAddrInst *i) {
  Address base = getLoweredAddress(i->getBase());
  Explosion indexValues = getLoweredExplosion(i->getIndex());
  llvm::Value *index = indexValues.claimNext();
  
  // We don't expose a non-inbounds GEP operation.
  llvm::Value *destValue = Builder.CreateInBoundsGEP(base.getAddress(),
                                                     index);
  
  setLoweredAddress(SILValue(i, 0), Address(destValue, base.getAlignment()));
}

void IRGenSILFunction::visitIndexRawPointerInst(swift::IndexRawPointerInst *i) {
  Explosion baseValues = getLoweredExplosion(i->getBase());
  llvm::Value *base = baseValues.claimNext();
  
  Explosion indexValues = getLoweredExplosion(i->getIndex());
  llvm::Value *index = indexValues.claimNext();
  
  // We don't expose a non-inbounds GEP operation.
  llvm::Value *destValue = Builder.CreateInBoundsGEP(base, index);
  
  Explosion result(ExplosionKind::Maximal);
  result.add(destValue);
  setLoweredExplosion(SILValue(i, 0), result);
}

void IRGenSILFunction::visitInitExistentialInst(swift::InitExistentialInst *i) {
  Address container = getLoweredAddress(i->getOperand());
  SILType destType = i->getOperand().getType();
  SILType srcType = i->getConcreteType();
  Address buffer = emitOpaqueExistentialContainerInit(*this,
                                                container,
                                                destType, srcType,
                                                i->getConformances());
  setLoweredAddress(SILValue(i, 0), buffer);
}

void IRGenSILFunction::visitInitExistentialRefInst(InitExistentialRefInst *i) {
  Explosion instance = getLoweredExplosion(i->getOperand());
  Explosion result(ExplosionKind::Maximal);
  emitClassExistentialContainer(*this,
                               result, i->getType(),
                               instance.claimNext(), i->getOperand().getType(),
                               i->getConformances());
  setLoweredExplosion(SILValue(i, 0), result);
}

void IRGenSILFunction::visitUpcastExistentialInst(
                                              swift::UpcastExistentialInst *i) {
  /// FIXME: Handle source existential being class existential.
  Address src = getLoweredAddress(i->getSrcExistential());
  Address dest = getLoweredAddress(i->getDestExistential());
  SILType srcType = i->getSrcExistential().getType();
  SILType destType = i->getDestExistential().getType();
  emitOpaqueExistentialContainerUpcast(*this, dest, destType, src, srcType,
                                       i->isTakeOfSrc());
}

void IRGenSILFunction::visitUpcastExistentialRefInst(
                                           swift::UpcastExistentialRefInst *i) {
  Explosion src = getLoweredExplosion(i->getOperand());
  Explosion dest(src.getKind());
  SILType srcType = i->getOperand().getType();
  SILType destType = i->getType();
  
  emitClassExistentialContainerUpcast(*this, dest, destType,
                                             src, srcType);
  
  setLoweredExplosion(SILValue(i, 0), dest);
}

void IRGenSILFunction::visitDeinitExistentialInst(
                                              swift::DeinitExistentialInst *i) {
  Address container = getLoweredAddress(i->getOperand());
  emitOpaqueExistentialContainerDeinit(*this, container,
                                       i->getOperand().getType());
}

void IRGenSILFunction::visitProjectExistentialInst(
                                             swift::ProjectExistentialInst *i) {
  SILType baseTy = i->getOperand().getType();
  Address base = getLoweredAddress(i->getOperand());
  Address object = emitOpaqueExistentialProjection(*this, base, baseTy);
  
  setLoweredAddress(SILValue(i, 0), object);
}

void IRGenSILFunction::visitProjectExistentialRefInst(
                                          swift::ProjectExistentialRefInst *i) {
  SILType baseTy = i->getOperand().getType();
  Explosion base = getLoweredExplosion(i->getOperand());
  
  Explosion result(ExplosionKind::Maximal);
  llvm::Value *instance
    = emitClassExistentialProjection(*this, base, baseTy);
  result.add(instance);
  setLoweredExplosion(SILValue(i, 0), result);
}

void IRGenSILFunction::visitProtocolMethodInst(swift::ProtocolMethodInst *i) {
  // For Objective-C classes we need to arrange for a msgSend
  // to happen when the method is called.
  if (i->getMember().isForeign) {
    setLoweredObjCMethod(SILValue(i, 0), i->getMember());
    return;
  }

  SILType baseTy = i->getOperand().getType();
  SILDeclRef member = i->getMember();
  
  Explosion lowered(ExplosionKind::Maximal);
  if (baseTy.isClassExistentialType()) {
    Explosion base = getLoweredExplosion(i->getOperand());
    emitClassProtocolMethodValue(*this, base, baseTy, member, lowered);
  } else {
    Address base = getLoweredAddress(i->getOperand());
    emitOpaqueProtocolMethodValue(*this, base, baseTy, member, lowered);
  }
  
  setLoweredExplosion(SILValue(i, 0), lowered);
}

void IRGenSILFunction::visitDynamicMethodInst(DynamicMethodInst *i) {
  assert(i->getMember().isForeign && "dynamic_method requires [objc] method");
  setLoweredObjCMethod(SILValue(i, 0), i->getMember());
  return;
}

void IRGenSILFunction::visitArchetypeMethodInst(swift::ArchetypeMethodInst *i) {
  // For Objective-C classes we need to arrange for a msgSend
  // to happen when the method is called.
  if (i->getMember().isForeign) {
    setLoweredObjCMethod(SILValue(i, 0), i->getMember());
    return;
  }

  SILType baseTy = i->getLookupArchetype();
  SILDeclRef member = i->getMember();

  Explosion lowered(ExplosionKind::Maximal);
  emitArchetypeMethodValue(*this, baseTy, member, lowered);
  
  setLoweredExplosion(SILValue(i, 0), lowered);
}

void IRGenSILFunction::visitInitializeVarInst(swift::InitializeVarInst *i) {
  SILType ty = i->getOperand().getType();
  const TypeInfo &ti = getTypeInfo(ty);
  Address dest = getLoweredAddress(i->getOperand());
  Builder.CreateMemSet(Builder.CreateBitCast(dest.getAddress(),
                                             IGM.Int8PtrTy),
                       Builder.getInt8(0),
                       ti.getSize(*this),
                       dest.getAlignment().getValue(),
                       /*isVolatile=*/ false);
}

void IRGenSILFunction::visitCopyAddrInst(swift::CopyAddrInst *i) {
  SILType addrTy = i->getSrc().getType();
  Address src = getLoweredAddress(i->getSrc());
  Address dest = getLoweredAddress(i->getDest());
  const TypeInfo &addrTI = getTypeInfo(addrTy);

  unsigned takeAndOrInitialize =
    (i->isTakeOfSrc() << 1U) | i->isInitializationOfDest();
  static const unsigned COPY = 0, TAKE = 2, ASSIGN = 0, INITIALIZE = 1;
  
  switch (takeAndOrInitialize) {
  case ASSIGN | COPY:
    addrTI.assignWithCopy(*this, dest, src);
    break;
  case INITIALIZE | COPY:
    addrTI.initializeWithCopy(*this, dest, src);
    break;
  case ASSIGN | TAKE:
    addrTI.assignWithTake(*this, dest, src);
    break;
  case INITIALIZE | TAKE:
    addrTI.initializeWithTake(*this, dest, src);
    break;
  default:
    llvm_unreachable("unexpected take/initialize attribute combination?!");
  }
}

void IRGenSILFunction::visitDestroyAddrInst(swift::DestroyAddrInst *i) {
  SILType addrTy = i->getOperand().getType();
  Address base = getLoweredAddress(i->getOperand());
  const TypeInfo &addrTI = getTypeInfo(addrTy);
  addrTI.destroy(*this, base);
}

void IRGenSILFunction::visitCondFailInst(swift::CondFailInst *i) {
  Explosion e = getLoweredExplosion(i->getOperand());
  llvm::Value *cond = e.claimNext();
  Builder.CreateCall(IGM.getConditionalFailureFn(), cond);
}

void IRGenSILFunction::visitSuperMethodInst(swift::SuperMethodInst *i) {
  assert(i->getMember().isForeign && "super_method to non_objc callee");
  setLoweredObjCMethod(SILValue(i, 0), i->getMember(),
                       i->getOperand().getType());
}

void IRGenSILFunction::visitClassMethodInst(swift::ClassMethodInst *i) {
  // For Objective-C classes we need to arrange for a msgSend
  // to happen when the method is called.
  if (i->getMember().isForeign) {
    setLoweredObjCMethod(SILValue(i, 0), i->getMember());
    return;
  }
  
  Explosion base = getLoweredExplosion(i->getOperand());
  llvm::Value *baseValue = base.claimNext();
  
  SILDeclRef method = i->getMember();
  auto methodType = i->getType().castTo<SILFunctionType>();
 
  // For Swift classes, get the method implementation from the vtable.
  // FIXME: better explosion kind, map as static.
  llvm::Value *fnValue = emitVirtualMethodValue(*this, baseValue,
                                                i->getOperand().getType(),
                                                method, methodType,
                                                ExplosionKind::Minimal);
  fnValue = Builder.CreateBitCast(fnValue, IGM.Int8PtrTy);
  Explosion e(ExplosionKind::Maximal);
  e.add(fnValue);
  setLoweredExplosion(SILValue(i, 0), e);
}
