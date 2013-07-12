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
#include "llvm/Support/SourceMgr.h"
#include "swift/Basic/Range.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/STLExtras.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/Stmt.h"
#include "swift/SIL/SILConstant.h"
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
#include "GenProto.h"
#include "GenStruct.h"
#include "GenTuple.h"
#include "IRGenDebugInfo.h"
#include "IRGenModule.h"
#include "Linking.h"
#include "ReferenceTypeInfo.h"

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
  /// The SILConstant declaring the method.
  SILConstant method;
  /// For a super call, the type to pass to msgSendSuper2 dispatch.
  /// Null for non-super calls.
  SILType superSearchType;

public:
  ObjCMethod(SILConstant method, SILType superSearchType)
    : method(method), superSearchType(superSearchType)
  {}
  
  SILConstant getMethod() const { return method; }
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

/// Represents the application of a SpecializeInst to a value.
class SpecializedValue {
  // The SILValue of the unspecialized generic function.
  SILValue unspecializedValue;
  // The substitutions applied to the value.
  ArrayRef<Substitution> substitutions;

public:
  SpecializedValue(SILValue unspecializedValue,
                   ArrayRef<Substitution> substitutions)
    : unspecializedValue(unspecializedValue),
      substitutions(substitutions)
  {}
  
  SILType getUnspecializedType() const {
    return unspecializedValue.getType();
  }
  
  SILValue getUnspecializedValue() const {
    return unspecializedValue;
  }
  
  ArrayRef<Substitution> getSubstitutions() const {
    return substitutions;
  }
};
  
/// Represents a builtin function.
class BuiltinValue {
  FuncDecl *decl;
  ArrayRef<Substitution> substitutions;
  
public:
  BuiltinValue(FuncDecl *decl, ArrayRef<Substitution> substitutions)
    : decl(decl), substitutions(substitutions)
  {}
  
  FuncDecl *getDecl() const { return decl; }
  
  ArrayRef<Substitution> getSubstitutions() const { return substitutions; }
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
    
      /// A SpecializedValue.
      SpecializedValue,
    
      /// A builtin function.
      BuiltinValue,
    Value_Last = SpecializedValue
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
    MetatypeValue metatypeValue;
    SpecializedValue specializedValue;
    BuiltinValue builtinValue;
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
  
  LoweredValue(MetatypeValue &&metatypeValue)
    : kind(Kind::MetatypeValue), metatypeValue(std::move(metatypeValue))
  {}
  
  LoweredValue(SpecializedValue &&specializedValue)
    : kind(Kind::SpecializedValue), specializedValue(std::move(specializedValue))
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
    case Kind::SpecializedValue:
      ::new (&specializedValue) SpecializedValue(std::move(lv.specializedValue));
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
  
  StaticFunction const &getStaticFunction() const {
    assert(kind == Kind::StaticFunction && "not a static function");
    return staticFunction;
  }
  
  ObjCMethod const &getObjCMethod() const {
    assert(kind == Kind::ObjCMethod && "not an objc method");
    return objcMethod;
  }
  
  MetatypeValue const &getMetatypeValue() const {
    assert(kind == Kind::MetatypeValue && "not a metatype value");
    return metatypeValue;
  }
  
  SpecializedValue const &getSpecializedValue() const {
    assert(kind == Kind::SpecializedValue && "not a specialized value");
    return specializedValue;
  }
  
  BuiltinValue const &getBuiltinValue() const {
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
    case Kind::SpecializedValue:
      specializedValue.~SpecializedValue();
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
  llvm::MapVector<SILBasicBlock *, LoweredBB> LoweredBBs;
  
  SILFunction *CurSILFn;
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
  void setLoweredAddress(SILValue v, Address const &address) {
    assert(v.getType().isAddress() && "address for non-address value?!");
    setLoweredValue(v, address);
  }
  
  /// Create a new Explosion corresponding to the given SIL value.
  void setLoweredExplosion(SILValue v, Explosion &e) {
    assert(!v.getType().isAddress() && "explosion for address value?!");
    setLoweredValue(v, LoweredValue(e));
  }
  
  /// Create a new StaticFunction corresponding to the given SIL value.
  void setLoweredStaticFunction(SILValue v,
                                llvm::Function *f,
                                AbstractCC cc,
                                ExplosionKind explosionLevel) {
    assert(!v.getType().isAddress() && "function for address value?!");
    assert(v.getType().is<AnyFunctionType>() &&
           "function for non-function value?!");
    setLoweredValue(v, StaticFunction{f, cc, explosionLevel});
  }
  
  void setLoweredObjCMethod(SILValue v, SILConstant method,
                            SILType superSearchType = SILType()) {
    assert(!v.getType().isAddress() && "function for address value?!");
    assert(v.getType().is<AnyFunctionType>() &&
           "function for non-function value?!");
    setLoweredValue(v, ObjCMethod{method, superSearchType});
  }
  
  void setLoweredMetatypeValue(SILValue v,
                               llvm::Value /*nullable*/ *swiftMetatype,
                               llvm::Value /*nullable*/ *objcMetatype) {
    setLoweredValue(v, MetatypeValue{swiftMetatype, objcMetatype});
  }
  
  void setLoweredSpecializedValue(SILValue v,
                                  SILValue unspecializedValue,
                                  ArrayRef<Substitution> substitutions) {
    setLoweredValue(v, SpecializedValue{unspecializedValue, substitutions});
  }
  
  void setLoweredBuiltinValue(SILValue v,
                              FuncDecl *builtin,
                              ArrayRef<Substitution> substitutions) {
    assert(isa<BuiltinModule>(builtin->getDeclContext())
           && "not a builtin");
    setLoweredValue(v, BuiltinValue{builtin, substitutions});
  }
  
  /// Get the LoweredValue corresponding to the given SIL value, which must
  /// have been lowered.
  LoweredValue &getLoweredValue(SILValue v) {
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
  
  //===--------------------------------------------------------------------===//
  // SIL instruction lowering
  //===--------------------------------------------------------------------===//

  void visitSILBasicBlock(SILBasicBlock *BB);
  
  void visitSILInstruction(SILInstruction *i) {
    i->dump();
    llvm_unreachable("irgen for SIL instruction not yet implemented");
  }
  
  void visitAllocVarInst(AllocVarInst *i);
  void visitAllocRefInst(AllocRefInst *i);
  void visitAllocBoxInst(AllocBoxInst *i);
  void visitAllocArrayInst(AllocArrayInst *i);

  void visitApplyInst(ApplyInst *i);
  void visitPartialApplyInst(PartialApplyInst *i);
  void visitSpecializeInst(SpecializeInst *i);

  void visitBuiltinFunctionRefInst(BuiltinFunctionRefInst *i);
  void visitFunctionRefInst(FunctionRefInst *i);
  void visitGlobalAddrInst(GlobalAddrInst *i);

  void visitIntegerLiteralInst(IntegerLiteralInst *i);
  void visitFloatLiteralInst(FloatLiteralInst *i);
  void visitStringLiteralInst(StringLiteralInst *i);

  void visitLoadInst(LoadInst *i);
  void visitStoreInst(StoreInst *i);
  void visitStructInst(StructInst *i);
  void visitTupleInst(TupleInst *i);
  void visitBuiltinZeroInst(BuiltinZeroInst *i);
  void visitMetatypeInst(MetatypeInst *i);
  void visitClassMetatypeInst(ClassMetatypeInst *i);
  void visitArchetypeMetatypeInst(ArchetypeMetatypeInst *i);
  void visitProtocolMetatypeInst(ProtocolMetatypeInst *i);
  void visitAssociatedMetatypeInst(AssociatedMetatypeInst *i);
  void visitTupleExtractInst(TupleExtractInst *i);
  void visitTupleElementAddrInst(TupleElementAddrInst *i);
  void visitStructExtractInst(StructExtractInst *i);
  void visitStructElementAddrInst(StructElementAddrInst *i);
  void visitRefElementAddrInst(RefElementAddrInst *i);
  void visitModuleInst(ModuleInst *i);

  void visitClassMethodInst(ClassMethodInst *i);
  void visitSuperMethodInst(SuperMethodInst *i);
  void visitArchetypeMethodInst(ArchetypeMethodInst *i);
  void visitProtocolMethodInst(ProtocolMethodInst *i);
  
  void visitProjectExistentialInst(ProjectExistentialInst *i);
  void visitProjectExistentialRefInst(ProjectExistentialRefInst *i);
  void visitInitExistentialInst(InitExistentialInst *i);
  void visitInitExistentialRefInst(InitExistentialRefInst *i);
  void visitUpcastExistentialInst(UpcastExistentialInst *i);
  void visitUpcastExistentialRefInst(UpcastExistentialRefInst *i);
  void visitDeinitExistentialInst(DeinitExistentialInst *i);

  void visitRetainInst(RetainInst *i);
  void visitReleaseInst(ReleaseInst *i);
  void visitRetainAutoreleasedInst(RetainAutoreleasedInst *i);
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
  void visitPointerToAddressInst(PointerToAddressInst *i);
  void visitRefToObjectPointerInst(RefToObjectPointerInst *i);
  void visitObjectPointerToRefInst(ObjectPointerToRefInst *i);
  void visitRefToRawPointerInst(RefToRawPointerInst *i);
  void visitRawPointerToRefInst(RawPointerToRefInst *i);
  void visitThinToThickFunctionInst(ThinToThickFunctionInst *i);
  void visitBridgeToBlockInst(BridgeToBlockInst *i);
  void visitArchetypeRefToSuperInst(ArchetypeRefToSuperInst *i);
  void visitSuperToArchetypeRefInst(SuperToArchetypeRefInst *i);
  void visitDowncastArchetypeRefInst(DowncastArchetypeRefInst *i);
  void visitDowncastExistentialRefInst(DowncastExistentialRefInst *i);
  void visitDowncastArchetypeAddrInst(DowncastArchetypeAddrInst *i);
  void visitProjectDowncastExistentialAddrInst(
                                         ProjectDowncastExistentialAddrInst *i);

  void visitIsNonnullInst(IsNonnullInst *i);

  void visitIndexAddrInst(IndexAddrInst *i);
  void visitIndexRawPointerInst(IndexRawPointerInst *i);
  
  void visitUnreachableInst(UnreachableInst *i);
  void visitBranchInst(BranchInst *i);
  void visitCondBranchInst(CondBranchInst *i);
  void visitReturnInst(ReturnInst *i);
  void visitAutoreleaseReturnInst(AutoreleaseReturnInst *i);
};

}

llvm::Value *StaticFunction::getExplosionValue(IRGenFunction &IGF) const {
  switch (cc) {
  case AbstractCC::C:
  case AbstractCC::ObjCMethod:
    // FIXME: Thunk foreign functions to Swift's CC when producing function
    // values.
    assert(false && "thunking C functions not yet implemented");
    return nullptr;

  case AbstractCC::Method:
  case AbstractCC::Freestanding:
    return IGF.Builder.CreateBitCast(function, IGF.IGM.Int8PtrTy);
  }
  
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
  
  case Kind::SpecializedValue:
    llvm_unreachable("thunking generic function not yet supported");

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
  case Kind::SpecializedValue:
  case Kind::BuiltinValue:
    return ExplosionKind::Minimal;
  }
}

IRGenSILFunction::IRGenSILFunction(IRGenModule &IGM,
                                   SILFunction *f,
                                   ExplosionKind explosionLevel)
  : IRGenFunction(IGM, explosionLevel,
                  IGM.getAddrOfSILFunction(f, explosionLevel),
                  f->getDebugScope()),
    CurSILFn(f)
{}

IRGenSILFunction::~IRGenSILFunction() {
  DEBUG(CurFn->print(llvm::dbgs()));
}

static std::vector<llvm::PHINode*>
emitPHINodesForBBArgs(IRGenSILFunction &IGF,
                      SILBasicBlock *silBB,
                      llvm::BasicBlock *llBB) {
  std::vector<llvm::PHINode*> phis;
  unsigned predecessors = std::count_if(silBB->pred_begin(), silBB->pred_end(),
                                        [](...){ return true; });
  
  IGF.Builder.SetInsertPoint(llBB);
  for (SILArgument *arg : make_range(silBB->bbarg_begin(), silBB->bbarg_end())) {
    size_t first = phis.size();
    
    const TypeInfo &ti = IGF.getFragileTypeInfo(arg->getType());
    
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
  
  return phis;
}

static ArrayRef<SILArgument*> emitEntryPointIndirectReturn(
                                 IRGenSILFunction &IGF,
                                 SILBasicBlock *entry,
                                 Explosion &params,
                                 SILFunctionTypeInfo *funcTI,
                                 std::function<bool()> requiresIndirectResult) {
  // Map the indirect return if present.
  if (funcTI->hasIndirectReturn()) {
    SILArgument *ret = entry->bbarg_begin()[0];
    SILValue retv(ret, 0);
    TypeInfo const &retType = IGF.IGM.getFragileTypeInfo(ret->getType());
    
    IGF.setLoweredAddress(retv,
                          retType.getAddressForPointer(params.claimNext()));
    return entry->getBBArgs().slice(1);
  } else {
    // Map an indirect return for a type SIL considers loadable but still
    // requires an indirect return at the IR level.
    if (requiresIndirectResult()) {
      TypeInfo const &retType
        = IGF.IGM.getFragileTypeInfo(funcTI->getResultType());
      IGF.IndirectReturn = retType.getAddressForPointer(params.claimNext());
    }
    return entry->getBBArgs();
  }  
}

/// Emit entry point arguments for a SILFunction with the Swift calling
/// convention.
static void emitEntryPointArgumentsNativeCC(IRGenSILFunction &IGF,
                                            SILBasicBlock *entry,
                                            Explosion &allParamValues,
                                            SILType funcTy) {
  SILFunctionTypeInfo *funcTI = funcTy.getFunctionTypeInfo(*IGF.IGM.SILMod);
  
  // Map the indirect return if present.
  ArrayRef<SILArgument*> params
    = emitEntryPointIndirectReturn(IGF, entry, allParamValues, funcTI,
      [&]() -> bool {
        auto retType = funcTI->getResultType().getSwiftRValueType();
        return IGF.IGM.requiresIndirectResult(retType, IGF.CurExplosionLevel);
      });

  // Map the remaining SIL parameters to LLVM parameters.
  for (SILArgument *param : params) {
    // Pull out the parameter value and its formal type.
    auto &paramTI = IGF.getFragileTypeInfo(param->getType());

    // If the SIL parameter isn't passed indirectly, we need to map it
    // to an explosion.  Fortunately, in this case we have a guarantee
    // that it's passed directly in IR.
    if (!param->getType().isAddress()) {
      Explosion paramValues(IGF.CurExplosionLevel);
      paramTI.reexplode(IGF, allParamValues, paramValues);
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
  if (auto polyFn = funcTy.getAs<PolymorphicFunctionType>())
    emitPolymorphicParameters(IGF, polyFn, allParamValues);
}

/// Emit entry point arguments for the parameters of a C function, or the
/// method parameters of an ObjC method.
static void emitEntryPointArgumentsCOrObjC(IRGenSILFunction &IGF,
                                           SILBasicBlock *entry,
                                           Explosion &params,
                                           ArrayRef<SILArgument*> args) {
  for (SILArgument *arg : args) {
    TypeInfo const &argType = IGF.getFragileTypeInfo(arg->getType());
    if (arg->getType().isAddress()) {
      IGF.setLoweredAddress(arg,
                            argType.getAddressForPointer(params.claimNext()));
      continue;
    }
    
    Explosion argExplosion(IGF.CurExplosionLevel);
    
    // Load and explode an argument that is 'byval' in the C calling convention.
    if (requiresExternalByvalArgument(IGF.IGM, arg->getType())) {
      Address byval = argType.getAddressForPointer(params.claimNext());
      argType.load(IGF, byval, argExplosion);
    } else {
      argType.reexplode(IGF, params, argExplosion);
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
                                                SILType funcTy) {
  SILFunctionTypeInfo *funcTI = funcTy.getFunctionTypeInfo(*IGF.IGM.SILMod);

  // Map the indirect return if present.
  ArrayRef<SILArgument*> args
    = emitEntryPointIndirectReturn(IGF, entry, params, funcTI, [&] {
      return requiresExternalIndirectResult(IGF.IGM, funcTI->getResultType());
    });
  
  // Map the self argument. This should always be an ObjC pointer type so
  // should never need to be loaded from a byval.
  SILArgument *selfArg = args[0];
  TypeInfo const &selfType = IGF.getFragileTypeInfo(selfArg->getType());
  Explosion self(IGF.CurExplosionLevel);
  selfType.reexplode(IGF, params, self);
  IGF.setLoweredExplosion(selfArg, self);
  
  // Discard the implicit _cmd argument.
  params.claimNext();
  
  // Map the rest of the arguments as in the C calling convention.
  emitEntryPointArgumentsCOrObjC(IGF, entry, params, args.slice(1));
}

/// Emit entry point arguments for a SILFunction with the C calling
/// convention.
static void emitEntryPointArgumentsCCC(IRGenSILFunction &IGF,
                                       SILBasicBlock *entry,
                                       Explosion &params,
                                       SILType funcTy) {
  SILFunctionTypeInfo *funcTI = funcTy.getFunctionTypeInfo(*IGF.IGM.SILMod);

  // Map the indirect return if present.
  ArrayRef<SILArgument*> args
    = emitEntryPointIndirectReturn(IGF, entry, params, funcTI, [&] {
      return requiresExternalIndirectResult(IGF.IGM, funcTI->getResultType());
    });
  emitEntryPointArgumentsCOrObjC(IGF, entry, params, args);
}

/// Emit the definition for the given SIL constant.
void IRGenModule::emitSILFunction(SILFunction *f) {
  if (f->isExternalDeclaration())
    return;
    
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
  Explosion params = collectParameters();
  SILType funcTy = CurSILFn->getLoweredType();

  switch (CurSILFn->getAbstractCC()) {
  case AbstractCC::Freestanding:
  case AbstractCC::Method:
    emitEntryPointArgumentsNativeCC(*this, entry->first, params, funcTy);
    break;
  case AbstractCC::ObjCMethod:
    emitEntryPointArgumentsObjCMethodCC(*this, entry->first, params, funcTy);
    break;
  case AbstractCC::C:
    emitEntryPointArgumentsCCC(*this, entry->first, params, funcTy);
    break;
  }
  
  assert(params.empty() && "did not map all llvm params to SIL params?!");

  // Emit the function body.
  for (SILBasicBlock &bb : *CurSILFn)
    visitSILBasicBlock(&bb);
}

void IRGenSILFunction::visitSILBasicBlock(SILBasicBlock *BB) {
  // Insert into the lowered basic block.
  llvm::BasicBlock *llBB = getLoweredBB(BB).bb;
  Builder.SetInsertPoint(llBB);

  // FIXME: emit a phi node to bind the bb arguments from all the predecessor
  // branches.
  
  // Generate the body.
  for (auto &I : *BB) {
    // Set the debug info location for I, if applicable.
    if (IGM.DebugInfo) {
      if (SILDebugScope *DS = I.getDebugScope())
        IGM.DebugInfo->setCurrentLoc(Builder, DS, I.getLoc());
      else {
        assert( CurSILFn->getDebugScope() && "function without a debug scope");
        IGM.DebugInfo->setCurrentLoc(Builder, CurSILFn->getDebugScope());
      }
    }
    visit(&I);
  }
  
  assert(Builder.hasPostTerminatorIP() && "SIL bb did not terminate block?!");
}

/// Find the entry point, natural curry level, and calling convention for a
/// SILConstant function.
llvm::Function *IRGenModule::getAddrOfSILFunction(SILFunction *f,
                                                  ExplosionKind level) {
  // Check whether we've created the function already.
  // FIXME: We should integrate this into the LinkEntity cache more cleanly.
  llvm::Function *fn = Module.getFunction(f->getMangledName());
  if (fn) return fn;
    
  LinkEntity entity = LinkEntity::forSILFunction(f, level);
  
  llvm::AttributeSet attrs;
  llvm::FunctionType *fnType = getFunctionType(f->getLoweredType(),
                                               level,
                                               ExtraData::None,
                                               attrs);
  
  auto cc = expandAbstractCC(*this, f->getAbstractCC());
  LinkInfo link = LinkInfo::get(*this, entity);

  fn = link.createFunction(*this, fnType, cc, attrs);
  if (DebugInfo)
    DebugInfo->createFunction(f->getDebugScope(), fn,
                              f->getAbstractCC(),
                              f->getLoweredType());

  return fn;
}

void IRGenSILFunction::visitBuiltinFunctionRefInst(
                                             swift::BuiltinFunctionRefInst *i) {
  setLoweredBuiltinValue(SILValue(i, 0), cast<FuncDecl>(i->getFunction()),
                         /*substitutions*/ {});
}

void IRGenSILFunction::visitFunctionRefInst(swift::FunctionRefInst *i) {
  // FIXME: pick the best available explosion level
  ExplosionKind explosionLevel = ExplosionKind::Minimal;
  llvm::Function *fnptr =
    IGM.getAddrOfSILFunction(i->getFunction(), explosionLevel);
  
  // Store the function constant and calling
  // convention as a StaticFunction so we can avoid bitcasting or thunking if
  // we don't need to.
  setLoweredStaticFunction(SILValue(i, 0), fnptr,
                           i->getFunction()->getAbstractCC(),
                           explosionLevel);
}

void IRGenSILFunction::visitGlobalAddrInst(GlobalAddrInst *i) {
  VarDecl *global = i->getGlobal();
  TypeInfo const &type = getFragileTypeInfo(global->getType());
  
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

/// Determine whether a metatype value is used as a Swift metatype, ObjC class,
/// or both.
static void getMetatypeUses(ValueBase *i,
                            bool &isUsedAsSwiftMetatype,
                            bool &isUsedAsObjCClass) {
  isUsedAsSwiftMetatype = isUsedAsObjCClass = false;
  for (auto *use : i->getUses()) {
    // Ignore retains or releases of metatypes.
    if (isa<RetainInst>(use->getUser()) || isa<ReleaseInst>(use->getUser()))
      continue;
    
    // If a class_method lookup of an ObjC method is done on us, we'll need the
    // objc class.
    if (auto *cm = dyn_cast<ClassMethodInst>(use->getUser())) {
      if (cm->getMember().getDecl()->isObjC()) {
        isUsedAsObjCClass = true;
        continue;
      }
    }
    
    // If we're applied as the 'this' argument to a class_method of an objc
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

void IRGenSILFunction::visitAssociatedMetatypeInst(
                                             swift::AssociatedMetatypeInst *i) {
  CanType instanceType(i->getType().castTo<MetaTypeType>()->getInstanceType());
  emitMetatypeInst(*this, i, instanceType);  
}

static void emitApplyArgument(IRGenSILFunction &IGF,
                              Explosion &args,
                              SILValue newArg) {
  if (newArg.getType().isAddress()) {
    args.add(IGF.getLoweredAddress(newArg).getAddress());
    return;
  }

  // Otherwise, it's an explosion, which we may need to translate.
  if (args.getKind() == IGF.getExplosionKind(newArg)) {
    IGF.getLoweredExplosion(newArg, args);
  } else {
    Explosion temp = IGF.getLoweredExplosion(newArg);
    IGF.getFragileTypeInfo(newArg.getType()).reexplode(IGF, temp, args);
  }
}

static CallEmission getCallEmissionForLoweredValue(IRGenSILFunction &IGF,
                                         SILType calleeTy,
                                         SILType resultTy,
                                         LoweredValue const &lv,
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
                                     calleeTy,
                                     resultTy,
                                     substitutions,
                                     ExplosionKind::Minimal,
                                     bool(objcMethod.getSuperSearchType()));
  }
      
  case LoweredValue::Kind::Explosion: {
    Explosion calleeValues = lv.getExplosion(IGF);
    
    calleeFn = calleeValues.claimNext();
    if (!calleeTy.castTo<AnyFunctionType>()->isThin())
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
    auto fnPtrTy = IGF.IGM.getFunctionType(calleeTy, explosionLevel,
                                           extraData, attrs)->getPointerTo();
    calleeFn = IGF.Builder.CreateBitCast(calleeFn, fnPtrTy);
    break;
  }
      
  case LoweredValue::Kind::MetatypeValue:
    llvm_unreachable("metatype isn't a valid callee");
    
  case LoweredValue::Kind::Address:
    llvm_unreachable("sil address isn't a valid callee");
  
  case LoweredValue::Kind::SpecializedValue:
    llvm_unreachable("specialized value should be handled before reaching here");
      
  case LoweredValue::Kind::BuiltinValue:
    llvm_unreachable("builtins should be handled before reaching here");
  }
  
  Callee callee = Callee::forKnownFunction(calleeTy,
                                           resultTy,
                                           substitutions, calleeFn, calleeData,
                                           explosionLevel);
  return CallEmission(IGF, callee);
}

static CallEmission getCallEmissionForLoweredValue(IRGenSILFunction &IGF,
                                                   SILType calleeTy,
                                                   SILType resultTy,
                                                   LoweredValue const &lv) {
  switch (lv.kind) {
  case LoweredValue::Kind::SpecializedValue: {
    LoweredValue const &unspecializedValue
      = IGF.getLoweredValue(lv.getSpecializedValue().getUnspecializedValue());
    return getCallEmissionForLoweredValue(IGF,
                              lv.getSpecializedValue().getUnspecializedType(),
                              resultTy,
                              unspecializedValue,
                              lv.getSpecializedValue().getSubstitutions());
  }
  case LoweredValue::Kind::ObjCMethod:
  case LoweredValue::Kind::StaticFunction:
  case LoweredValue::Kind::Explosion:
    // No substitutions.
    return getCallEmissionForLoweredValue(IGF, calleeTy, resultTy, lv,
                                          /*substitutions=*/ {});

  case LoweredValue::Kind::MetatypeValue:
    llvm_unreachable("metatype isn't a valid callee");
    
  case LoweredValue::Kind::Address:
    llvm_unreachable("sil address isn't a valid callee");
      
  case LoweredValue::Kind::BuiltinValue:
    llvm_unreachable("builtins should be handled before reaching here");
  }
}

static llvm::Value *getObjCClassForValue(IRGenSILFunction &IGF,
                                         SILValue v) {
  LoweredValue const &lv = IGF.getLoweredValue(v);
  switch (lv.kind) {
  case LoweredValue::Kind::Address:
    llvm_unreachable("address isn't a valid metatype");
  
  case LoweredValue::Kind::ObjCMethod:
  case LoweredValue::Kind::StaticFunction:
  case LoweredValue::Kind::SpecializedValue:
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
                                 FuncDecl *builtin,
                                 ApplyInst *i,
                                 ArrayRef<Substitution> substitutions) {
  Explosion args(ExplosionKind::Maximal);
  
  auto argValues = i->getArguments();
  
  Address indirectResult;
  if (i->hasIndirectReturn(*IGF.IGM.SILMod)) {
    indirectResult =IGF.getLoweredAddress(i->getIndirectReturn(*IGF.IGM.SILMod));
    argValues = argValues.slice(0, argValues.size() - 1);
  }
  
  for (SILValue arg : argValues)
    emitApplyArgument(IGF, args, arg);
  
  if (indirectResult.isValid()) {
    emitBuiltinCall(IGF, builtin, args, nullptr, indirectResult, substitutions);
  } else {
    Explosion result(ExplosionKind::Maximal);
    emitBuiltinCall(IGF, builtin, args, &result, Address(), substitutions);
    IGF.setLoweredExplosion(SILValue(i,0), result);
  }
}

void IRGenSILFunction::visitApplyInst(swift::ApplyInst *i) {
  SILValue v(i, 0);
  
  LoweredValue const &calleeLV = getLoweredValue(i->getCallee());
  
  // Handle builtin calls separately.
  if (calleeLV.kind == LoweredValue::Kind::BuiltinValue) {
    auto &builtin = calleeLV.getBuiltinValue();
    return emitBuiltinApplyInst(*this, builtin.getDecl(), i,
                                builtin.getSubstitutions());
  }

  SILType calleeTy = i->getCallee().getType();
  SILType resultTy
    = calleeTy.getFunctionTypeInfo(*IGM.SILMod)->getSemanticResultType();
  
  CallEmission emission = getCallEmissionForLoweredValue(*this,
                                                     i->getCallee().getType(),
                                                     resultTy, calleeLV);
  
  // Lower the SIL arguments to IR arguments.
  Explosion llArgs(emission.getCurExplosionLevel());
  
  // Save off the indirect return argument, if any.
  OperandValueArrayRef args = i->getArgumentsWithoutIndirectReturn(*IGM.SILMod);
  SILValue indirectReturn;
  if (i->hasIndirectReturn(*IGM.SILMod)) {
    indirectReturn = i->getIndirectReturn(*IGM.SILMod);
  }
  
  // ObjC message sends need special handling for the 'this' argument. It may
  // need to be wrapped in an objc_super struct, and the '_cmd' argument needs
  // to be passed alongside it.
  if (calleeLV.kind == LoweredValue::Kind::ObjCMethod) {
    SILValue thisValue = i->getArguments()[0];
    llvm::Value *selfArg;
    // Convert a metatype 'this' argument to the ObjC Class pointer.
    if (thisValue.getType().is<MetaTypeType>()) {
      selfArg = getObjCClassForValue(*this, thisValue);
    } else {
      Explosion selfExplosion(getExplosionKind(thisValue));
      getLoweredExplosion(thisValue, selfExplosion);
      selfArg = selfExplosion.claimNext();
    }

    addObjCMethodCallImplicitArguments(*this, llArgs,
                                 calleeLV.getObjCMethod().getMethod(),
                                 selfArg,
                                 calleeLV.getObjCMethod().getSuperSearchType());
    
    args = args.slice(1);
  }

  for (SILValue arg : args)
    emitApplyArgument(*this, llArgs, arg);
  
  emission.addSubstitutedArg(CanType(calleeTy.castTo<FunctionType>()->getInput()),
                             llArgs);
  
  // If the function takes an indirect return argument, emit into it.
  if (indirectReturn) {
    Address a = getLoweredAddress(indirectReturn);
    TypeInfo const &ti = getFragileTypeInfo(indirectReturn.getType());
    emission.emitToMemory(a, ti);
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

static std::tuple<llvm::Function*, SILType, ArrayRef<Substitution>>
getPartialApplicationFunction(IRGenSILFunction &IGF,
                              SILValue v) {
  LoweredValue &lv = IGF.getLoweredValue(v);

  switch (lv.kind) {
  case LoweredValue::Kind::Address:
    llvm_unreachable("can't partially apply an address");

  case LoweredValue::Kind::StaticFunction:
    switch (lv.getStaticFunction().getAbstractCC()) {
    case AbstractCC::C:
    case AbstractCC::ObjCMethod:
      assert(false && "partial_apply of foreign functions not implemented");
      break;
      
    case AbstractCC::Freestanding:
    case AbstractCC::Method:
      break;
    }
    return {lv.getStaticFunction().getFunction(),
            v.getType(),
            ArrayRef<Substitution>{}};
  case LoweredValue::Kind::SpecializedValue: {
    const SpecializedValue &specialized = lv.getSpecializedValue();
    SILValue unspecialized = specialized.getUnspecializedValue();
    auto res = getPartialApplicationFunction(IGF, unspecialized);
    return {std::get<0>(res),
            std::get<1>(res),
            specialized.getSubstitutions()};
  }
  case LoweredValue::Kind::Explosion:
  case LoweredValue::Kind::ObjCMethod:
  case LoweredValue::Kind::MetatypeValue:
  case LoweredValue::Kind::BuiltinValue:
    llvm_unreachable("partial application not yet supported");
  }
}

void IRGenSILFunction::visitPartialApplyInst(swift::PartialApplyInst *i) {
  SILValue v(i, 0);

  // Get the static function value.
  // FIXME: We'll need to be able to close over runtime function values
  // too, by including the function pointer and context data into the new
  // closure context.
  llvm::Function *calleeFn = nullptr;
  SILType origCalleeTy;
  ArrayRef<Substitution> substitutions;

  std::tie(calleeFn, origCalleeTy, substitutions)
    = getPartialApplicationFunction(*this, i->getCallee());
  
  // Apply the closure up to the next-to-last uncurry level to gather the
  // context arguments.

  // FIXME: We need to close over fat function values to be able to curry
  // specialized 
  assert(i->getCallee().getType().castTo<FunctionType>()->isThin() &&
         "can't closure a function that already has context");
  
  Explosion llArgs(ExplosionKind::Maximal);
  SmallVector<SILType, 8> argTypes;
  for (SILValue arg : i->getArguments()) {
    emitApplyArgument(*this, llArgs, arg);
    // FIXME: Need to carry the address-ness of each argument alongside
    // the object type's TypeInfo.
    argTypes.push_back(arg.getType());
  }
  
  // Create the thunk and function value.
  Explosion function(ExplosionKind::Maximal);
  emitFunctionPartialApplication(*this, calleeFn, llArgs,
                                 argTypes, substitutions,
                                 origCalleeTy, i->getCallee().getType(),
                                 i->getType(), function);
  setLoweredExplosion(v, function);
}

void IRGenSILFunction::visitIntegerLiteralInst(swift::IntegerLiteralInst *i) {
  llvm::Value *constant = llvm::ConstantInt::get(IGM.LLVMContext,
                                                 i->getValue());
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
  Explosion e(ExplosionKind::Maximal);
  emitStringLiteral(*this, i->getValue(), e);
  setLoweredExplosion(SILValue(i, 0), e);
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
    TypeInfo const &retType = IGF.getFragileTypeInfo(resultTy);
    retType.initialize(IGF, result, IGF.IndirectReturn);
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

// Add branch arguments to destination phi nodes.
static void addIncomingSILArgumentsToPHINodes(IRGenSILFunction &IGF,
                                             LoweredBB &lbb,
                                             OperandValueArrayRef args) {
  llvm::BasicBlock *curBB = IGF.Builder.GetInsertBlock();
  ArrayRef<llvm::PHINode*> phis = lbb.phis;
  size_t phiIndex = 0;
  for (SILValue arg : args) {
    const LoweredValue &lv = IGF.getLoweredValue(arg);
    
    if (lv.isAddress()) {
      phis[phiIndex++]->addIncoming(lv.getAddress().getAddress(), curBB);
      continue;
    }
    
    Explosion argValue = lv.getExplosion(IGF);
    while (!argValue.empty())
      phis[phiIndex++]->addIncoming(argValue.claimNext(), curBB);
  }
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

void IRGenSILFunction::visitBuiltinZeroInst(swift::BuiltinZeroInst *i) {
  auto &ti = getFragileTypeInfo(i->getType());
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

  Address field = projectTupleElementAddress(*this,
                                             OwnedAddress(base, nullptr),
                                             baseType,
                                             i->getFieldNo()).getAddress();
  setLoweredAddress(SILValue(i, 0), field);
}

void IRGenSILFunction::visitStructExtractInst(swift::StructExtractInst *i) {
  SILValue v(i, 0);
  Explosion lowered(ExplosionKind::Maximal);
  Explosion operand = getLoweredExplosion(i->getOperand());
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

  Address field = projectPhysicalStructMemberAddress(*this,
                                                   OwnedAddress(base, nullptr),
                                                   baseType,
                                                   i->getField()).getAddress();
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

void IRGenSILFunction::visitModuleInst(swift::ModuleInst *i) {
  // Currently, module values are always empty.
  Explosion empty(ExplosionKind::Maximal);
  setLoweredExplosion(SILValue(i, 0), empty);
}

void IRGenSILFunction::visitLoadInst(swift::LoadInst *i) {
  Explosion lowered(ExplosionKind::Maximal);
  Address source = getLoweredAddress(i->getOperand());
  const TypeInfo &type = getFragileTypeInfo(i->getType().getObjectType());
  type.loadAsTake(*this, source, lowered);
  setLoweredExplosion(SILValue(i, 0), lowered);
}

void IRGenSILFunction::visitStoreInst(swift::StoreInst *i) {
  Explosion source = getLoweredExplosion(i->getSrc());
  Address dest = getLoweredAddress(i->getDest());
  const TypeInfo &type = getFragileTypeInfo(
                              i->getSrc().getType().getObjectType());

  type.initialize(*this, source, dest);
}

void IRGenSILFunction::visitRetainInst(swift::RetainInst *i) {
  // FIXME: Specialization thunks may eventually require retaining. For now,
  // since we don't yet thunk specialized function values, ignore retains
  // of lowered SpecializedValues.
  if (getLoweredValue(i->getOperand()).kind
        == LoweredValue::Kind::SpecializedValue) {
    return;
  }
  
  Explosion lowered = getLoweredExplosion(i->getOperand());
  auto &ti =
    cast<ReferenceTypeInfo>(getFragileTypeInfo(i->getOperand().getType()));
  ti.retain(*this, lowered);
}

void IRGenSILFunction::visitReleaseInst(swift::ReleaseInst *i) {
  // FIXME: Specialization thunks may eventually require retaining. For now,
  // since we don't yet thunk specialized function values, ignore retains
  // of lowered SpecializedValues.
  if (getLoweredValue(i->getOperand()).kind
        == LoweredValue::Kind::SpecializedValue) {
    return;
  }
  
  Explosion lowered = getLoweredExplosion(i->getOperand());
  auto &ti =
    cast<ReferenceTypeInfo>(getFragileTypeInfo(i->getOperand().getType()));
  ti.release(*this, lowered);
}

void IRGenSILFunction::visitRetainAutoreleasedInst(
                                             swift::RetainAutoreleasedInst *i) {
  Explosion lowered = getLoweredExplosion(i->getOperand());
  llvm::Value *value = lowered.claimNext();
  emitObjCRetainAutoreleasedReturnValue(*this, value);
}

void IRGenSILFunction::visitAllocVarInst(swift::AllocVarInst *i) {
  const TypeInfo &type = getFragileTypeInfo(i->getElementType());
  SILValue v(i, 0);

  OnHeap_t isOnHeap = NotOnHeap;
  switch (i->getAllocKind()) {
  case AllocKind::Heap:
    llvm_unreachable("heap alloc_var not implemented");
  case AllocKind::Stack:
    isOnHeap = NotOnHeap;
    break;
  case AllocKind::Pseudo:
    llvm_unreachable("pseudo allocation not implemented");
  }

  // Derive name from SIL location.
  VarDecl *Decl = i->getDecl();
  StringRef dbgname =
# ifndef NDEBUG
    // If this is a DEBUG build, use pretty names for the LLVM IR.
    Decl ? Decl->getName().str() :
# endif
    "";

  OwnedAddress addr = type.allocate(*this, isOnHeap, dbgname);
  if (IGM.DebugInfo && Decl)
    IGM.DebugInfo->emitStackVariableDeclaration(Builder,
                                                addr.getAddressPointer(),
                                                DebugTypeInfo(*Decl, type),
                                                Decl->getName().str());

  setLoweredAddress(v, addr.getAddress());
}

void IRGenSILFunction::visitAllocRefInst(swift::AllocRefInst *i) {
  llvm::Value *alloced = emitClassAllocation(*this, i->getType());
  Explosion e(ExplosionKind::Maximal);
  e.add(alloced);
  setLoweredExplosion(SILValue(i, 0), e);
}

void IRGenSILFunction::visitDeallocVarInst(swift::DeallocVarInst *i) {
  switch (i->getAllocKind()) {
  case AllocKind::Heap:
    llvm_unreachable("FIXME: heap dealloc_var not implemented");
  case AllocKind::Stack:
    // Nothing to do. We could emit a lifetime.end here maybe.
    break;
  case AllocKind::Pseudo:
    llvm_unreachable("pseudo allocation not implemented");
  }
}

void IRGenSILFunction::visitAllocBoxInst(swift::AllocBoxInst *i) {
  SILValue boxValue(i, 0);
  SILValue ptrValue(i, 1);
  const TypeInfo &type = getFragileTypeInfo(i->getElementType());
  OwnedAddress addr = type.allocate(*this,
                                    OnHeap,
                                    // FIXME: derive name from SIL location
                                    "");
  
  Explosion box(ExplosionKind::Maximal);
  box.add(addr.getOwner());
  setLoweredExplosion(boxValue, box);
  setLoweredAddress(ptrValue, addr.getAddress());
}

void IRGenSILFunction::visitAllocArrayInst(swift::AllocArrayInst *i) {
  SILValue boxValue(i, 0);
  SILValue ptrValue(i, 1);
  
  Explosion lengthEx = getLoweredExplosion(i->getNumElements());
  llvm::Value *lengthValue = lengthEx.claimNext();
  HeapArrayInfo arrayInfo(*this, i->getElementType()->getCanonicalType());
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

  auto &ti = getFragileTypeInfo(i->getType());
  
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
  emitPointerCastInst(*this, i->getOperand(), SILValue(i, 0),
                      IGM.RefCountedPtrTy);
}

void IRGenSILFunction::visitObjectPointerToRefInst(
                                             swift::ObjectPointerToRefInst *i) {
  auto &ti = getFragileTypeInfo(i->getType());
  llvm::Type *destType = ti.getStorageType();
  emitPointerCastInst(*this, i->getOperand(), SILValue(i, 0),
                      destType);
}

void IRGenSILFunction::visitRefToRawPointerInst(
                                             swift::RefToRawPointerInst *i) {
  emitPointerCastInst(*this, i->getOperand(), SILValue(i, 0),
                      IGM.Int8PtrTy);
}

void IRGenSILFunction::visitRawPointerToRefInst(swift::RawPointerToRefInst *i) {
  auto &ti = getFragileTypeInfo(i->getType());
  llvm::Type *destType = ti.getStorageType();
  emitPointerCastInst(*this, i->getOperand(), SILValue(i, 0),
                      destType);
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
  const TypeInfo &baseTypeInfo = getFragileTypeInfo(i->getType());
  llvm::Type *baseTy = baseTypeInfo.StorageType;
  llvm::Value *cast = Builder.CreateBitCast(in, baseTy);
  out.add(cast);
  setLoweredExplosion(SILValue(i, 0), out);
}

void IRGenSILFunction::visitSuperToArchetypeRefInst(
                                             swift::SuperToArchetypeRefInst *i) {
  Explosion super = getLoweredExplosion(i->getOperand());
  llvm::Value *in = super.claimNext();
  Explosion out(ExplosionKind::Maximal);
  llvm::Value *cast
    = emitSuperToClassArchetypeConversion(in, i->getType(), i->getMode());
  out.add(cast);
  setLoweredExplosion(SILValue(i, 0), out);
}

void IRGenSILFunction::visitDowncastArchetypeRefInst(
                                           swift::DowncastArchetypeRefInst *i) {
  Explosion archetype = getLoweredExplosion(i->getOperand());
  llvm::Value *fromValue = archetype.claimNext();
  llvm::Value *toValue = emitDowncast(fromValue, i->getType(), i->getMode());
  Explosion to(archetype.getKind());
  to.add(toValue);
  setLoweredExplosion(SILValue(i,0), to);
}

void IRGenSILFunction::visitDowncastExistentialRefInst(
                                         swift::DowncastExistentialRefInst *i) {
  Explosion existential = getLoweredExplosion(i->getOperand());
  llvm::Value *instance
    = emitClassExistentialProjection(*this, existential,
                                     i->getOperand().getType());

  llvm::Value *toValue = emitDowncast(instance, i->getType(), i->getMode());
  Explosion to(existential.getKind());
  to.add(toValue);
  setLoweredExplosion(SILValue(i,0), to);
}

void IRGenSILFunction::visitDowncastArchetypeAddrInst(
                                          swift::DowncastArchetypeAddrInst *i) {
  Address archetype = getLoweredAddress(i->getOperand());
  Address cast = emitOpaqueArchetypeDowncast(*this, archetype,
                                             i->getOperand().getType(),
                                             i->getType(),
                                             i->getMode());
  setLoweredAddress(SILValue(i,0), cast);
}

void IRGenSILFunction::visitProjectDowncastExistentialAddrInst(
                                 swift::ProjectDowncastExistentialAddrInst *i) {
  Address existential = getLoweredAddress(i->getOperand());
  Address cast = emitOpaqueExistentialDowncast(*this, existential,
                                               i->getOperand().getType(),
                                               i->getType(),
                                               i->getMode());
  setLoweredAddress(SILValue(i,0), cast);
}

void IRGenSILFunction::visitIsNonnullInst(swift::IsNonnullInst *i) {
  // Get the value we're testing, which may be an address or an instance
  // pointer.
  llvm::Value *val;
  LoweredValue const &lv = getLoweredValue(i->getOperand());
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
  const TypeInfo &toTI = getFragileTypeInfo(i->getType());
  llvm::Value *fromValue = from.claimNext();
  to.add(Builder.CreateBitCast(fromValue, toTI.getStorageType()));
  setLoweredExplosion(SILValue(i, 0), to);
}

void IRGenSILFunction::visitDowncastInst(swift::DowncastInst *i) {
  Explosion from = getLoweredExplosion(i->getOperand());
  Explosion to(from.getKind());
  llvm::Value *fromValue = from.claimNext();
  llvm::Value *castValue = emitDowncast(fromValue, i->getType(), i->getMode());
  to.add(castValue);
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
  Explosion lowered(ExplosionKind::Maximal);
  lowered.add(object.getAddress());
  setLoweredExplosion(SILValue(i, 0), lowered);
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
  if (i->getMember().isObjC) {
    setLoweredObjCMethod(SILValue(i, 0), i->getMember());
    return;
  }

  SILType baseTy = i->getOperand().getType();
  SILConstant member = i->getMember();
  
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

void IRGenSILFunction::visitArchetypeMethodInst(swift::ArchetypeMethodInst *i) {
  // For Objective-C classes we need to arrange for a msgSend
  // to happen when the method is called.
  if (i->getMember().isObjC) {
    setLoweredObjCMethod(SILValue(i, 0), i->getMember());
    return;
  }

  SILType baseTy = i->getLookupArchetype();
  SILConstant member = i->getMember();

  Explosion lowered(ExplosionKind::Maximal);
  emitArchetypeMethodValue(*this, baseTy, member, lowered);
  
  setLoweredExplosion(SILValue(i, 0), lowered);
}

void IRGenSILFunction::visitInitializeVarInst(swift::InitializeVarInst *i) {
  SILType ty = i->getOperand().getType();
  TypeInfo const &ti = getFragileTypeInfo(ty);
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
  TypeInfo const &addrTI = getFragileTypeInfo(addrTy);

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
  TypeInfo const &addrTI = getFragileTypeInfo(addrTy);
  addrTI.destroy(*this, base);
}

void IRGenSILFunction::visitSuperMethodInst(swift::SuperMethodInst *i) {
  assert(i->getMember().isObjC && "super_method to non_objc callee");
  setLoweredObjCMethod(SILValue(i, 0), i->getMember(),
                       i->getOperand().getType());
}

void IRGenSILFunction::visitClassMethodInst(swift::ClassMethodInst *i) {
  // For Objective-C classes we need to arrange for a msgSend
  // to happen when the method is called.
  if (i->getMember().isObjC) {
    setLoweredObjCMethod(SILValue(i, 0), i->getMember());
    return;
  }
  
  Explosion base = getLoweredExplosion(i->getOperand());
  llvm::Value *baseValue = base.claimNext();
  
  SILConstant method = i->getMember();
  
  // For Swift classes, get the method implementation from the vtable.
  // FIXME: better explosion kind, map as static.
  llvm::Value *fnValue = emitVirtualMethodValue(*this, baseValue,
                                                i->getOperand().getType(),
                                                method, i->getType(),
                                                ExplosionKind::Minimal);
  fnValue = Builder.CreateBitCast(fnValue, IGM.Int8PtrTy);
  Explosion e(ExplosionKind::Maximal);
  e.add(fnValue);
  setLoweredExplosion(SILValue(i, 0), e);
}

void IRGenSILFunction::visitSpecializeInst(swift::SpecializeInst *i) {
  // If we're specializing a builtin, store the substitutions directly with the
  // builtin.
  LoweredValue const &operand = getLoweredValue(i->getOperand());
  if (operand.kind == LoweredValue::Kind::BuiltinValue) {
    assert(operand.getBuiltinValue().getSubstitutions().empty() &&
           "builtin already specialized");
    return setLoweredBuiltinValue(SILValue(i, 0),
                                  operand.getBuiltinValue().getDecl(),
                                  i->getSubstitutions());
  }
  
  // If the specialization is used as a value and not just called, we need to
  // emit the thunk.
  for (auto *use : i->getUses())
    if (!isa<ApplyInst>(use->getUser())
        && !isa<PartialApplyInst>(use->getUser())) {
      assert(operand.kind == LoweredValue::Kind::StaticFunction &&
         "specialization thunks for dynamic function values not yet supported");

      // FIXME: better explosion level, preserve!
      llvm::Function *thunk = emitFunctionSpecialization(IGM,
                                 operand.getStaticFunction().getFunction(),
                                 i->getOperand().getType(),
                                 i->getType(),
                                 i->getSubstitutions(),
                                 ExplosionKind::Minimal);
      
      Explosion result(ExplosionKind::Maximal);
      result.add(Builder.CreateBitCast(thunk, IGM.Int8PtrTy));
      return setLoweredExplosion(SILValue(i, 0), result);
    }
  
  // If it's only called, we can just emit calls to the generic inline.
  setLoweredSpecializedValue(SILValue(i, 0),
                             i->getOperand(),
                             i->getSubstitutions());
}
