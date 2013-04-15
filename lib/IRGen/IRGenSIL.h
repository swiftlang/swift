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
#include "swift/AST/Module.h"
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

class LoweredValue;
  
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
  llvm::DenseMap<SILValue, LoweredValue> loweredValues;
  llvm::MapVector<SILBasicBlock *, LoweredBB> loweredBBs;
  
  SILConstant CurConstant;
  SILFunction *CurSILFn;
  Address IndirectReturn;
  
  IRGenSILFunction(IRGenModule &IGM,
                   CanType t,
                   ExplosionKind explosionLevel,
                   llvm::Function *fn);
  ~IRGenSILFunction();
  
  /// Generate IR for the given SIL Function.
  void emitSILFunction(SILConstant c, SILFunction *f);

  /// Generate code from the global toplevel. This will emit all the
  /// declarations in the given translation unit along with the toplevel
  /// of the given SILModule.
  void emitGlobalTopLevel(TranslationUnit *TU,
                          SILModule *SILMod);
  
  /// Generate local decls in the given function body. This skips VarDecls and
  /// other locals that are consumed by SIL.
  void emitLocalDecls(BraceStmt *body);
  
  void newLoweredValue(SILValue v, LoweredValue &&lv) {
    auto inserted = loweredValues.insert({v, std::move(lv)});
    assert(inserted.second && "already had lowered value for sil value?!");
    (void)inserted;
  }
  
  /// Create a new Address corresponding to the given SIL address value.
  void newLoweredAddress(SILValue v, Address const &address) {
    assert(v.getType().isAddress() && "address for non-address value?!");
    newLoweredValue(v, address);
  }
  
  /// Create a new Explosion corresponding to the given SIL value.
  void newLoweredExplosion(SILValue v, Explosion &e) {
    assert(!v.getType().isAddress() && "explosion for address value?!");
    newLoweredValue(v, LoweredValue(e));
  }
  
  /// Create a new Explosion corresponding to the given SIL value, disabling
  /// cleanups on the input Explosion if necessary.
  void newLoweredExplosion(SILValue v, Explosion &e, IRGenFunction &IGF) {
    assert(!v.getType().isAddress() && "explosion for address value?!");
    newLoweredValue(v, LoweredValue(e, IGF));
  }
  
  /// Create a new StaticFunction corresponding to the given SIL value.
  void newLoweredStaticFunction(SILValue v,
                                llvm::Function *f,
                                AbstractCC cc) {
    assert(!v.getType().isAddress() && "function for address value?!");
    assert(v.getType().is<AnyFunctionType>() &&
           "function for non-function value?!");
    newLoweredValue(v, StaticFunction{f, cc});
  }
  
  void newLoweredObjCMethod(SILValue v, ValueDecl *method,
                            CanType superSearchType = CanType()) {
    assert(!v.getType().isAddress() && "function for address value?!");
    assert(v.getType().is<AnyFunctionType>() &&
           "function for non-function value?!");
    newLoweredValue(v, ObjCMethod{method, superSearchType});
  }
  
  void newLoweredMetatypeValue(SILValue v,
                               llvm::Value /*nullable*/ *swiftMetatype,
                               llvm::Value /*nullable*/ *objcMetatype) {
    newLoweredValue(v, MetatypeValue{swiftMetatype, objcMetatype});
  }
  
  void newLoweredSpecializedValue(SILValue v,
                                  SILValue unspecializedValue,
                                  ArrayRef<Substitution> substitutions) {
    newLoweredValue(v, SpecializedValue{unspecializedValue, substitutions});
  }
  
  void newLoweredBuiltinValue(SILValue v,
                              FuncDecl *builtin,
                              ArrayRef<Substitution> substitutions) {
    assert(isa<BuiltinModule>(builtin->getDeclContext())
           && "not a builtin");
    newLoweredValue(v, BuiltinValue{builtin, substitutions});
  }
  
  /// Get the LoweredValue corresponding to the given SIL value, which must
  /// have been lowered.
  LoweredValue &getLoweredValue(SILValue v) {
    auto foundValue = loweredValues.find(v);
    assert(foundValue != loweredValues.end() &&
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
  
  /// Add LLVM values managed by cleanups lowered from a SIL value to an
  /// explosion.
  /// FIXME: This is used to lower ApplyInsts that call foreign methods with
  /// non-standard ownership conventions. In normal circumstances SIL values
  /// should not be managed.
  void getLoweredManagedExplosion(SILValue v, Explosion &out);

  LoweredBB &getLoweredBB(SILBasicBlock *bb) {
    auto foundBB = loweredBBs.find(bb);
    assert(foundBB != loweredBBs.end() && "no llvm bb for sil bb?!");
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
  void visitModuleInst(ModuleInst *i);

  void visitClassMethodInst(ClassMethodInst *i);
  void visitSuperMethodInst(SuperMethodInst *i);
  void visitArchetypeMethodInst(ArchetypeMethodInst *i);
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
  void visitBridgeToBlockInst(BridgeToBlockInst *i);
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
