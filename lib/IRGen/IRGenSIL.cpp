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

#define DEBUG_TYPE "irgen"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/SmallBitVector.h"
#include "llvm/Support/Debug.h"
#include "clang/AST/ASTContext.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/Basic/Range.h"
#include "swift/Basic/STLExtras.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/Pattern.h"
#include "swift/SIL/PrettyStackTrace.h"
#include "swift/SIL/SILDebugScope.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILLinkage.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILType.h"
#include "swift/SIL/SILVisitor.h"
#include "clang/CodeGen/CodeGenABITypes.h"

#include "CallEmission.h"
#include "Explosion.h"
#include "GenCast.h"
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
#include "ReferenceTypeInfo.h"
#include "GenType.h"
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
  
public:
  StaticFunction(llvm::Function *function, AbstractCC cc)
    : function(function), cc(cc)
  {}
  
  llvm::Function *getFunction() const { return function; }
  AbstractCC getAbstractCC() const { return cc; }
  
  llvm::Value *getExplosionValue(IRGenFunction &IGF) const;
};
  
/// Represents an ObjC method reference that will be invoked by a form of
/// objc_msgSend.
class ObjCMethod {
  /// The SILDeclRef declaring the method.
  SILDeclRef method;
  /// For a bounded call, the static type that provides the lower bound for
  /// the search. Null for unbounded calls that will look for the method in
  /// the dynamic type of the object.
  llvm::PointerIntPair<SILType, 1, bool> searchTypeAndSuper;

public:
  ObjCMethod(SILDeclRef method, SILType searchType, bool startAtSuper)
    : method(method), searchTypeAndSuper(searchType, startAtSuper)
  {}
  
  SILDeclRef getMethod() const { return method; }
  SILType getSearchType() const { return searchTypeAndSuper.getPointer(); }
  bool shouldStartAtSuper() const { return searchTypeAndSuper.getInt(); }
  
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
    Value_Last = ObjCMethod,
  };
  
  Kind kind;
  
private:
  using ExplosionVector = SmallVector<llvm::Value *, 4>;
  
  union {
    Address address;
    struct {
      ExplosionVector values;
    } explosion;
    StaticFunction staticFunction;
    ObjCMethod objcMethod;
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
  
  LoweredValue(Explosion &e)
    : kind(Kind::Explosion), explosion{{}} {
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
      ::new (&explosion.values) ExplosionVector(std::move(lv.explosion.values));
      break;
    case Kind::StaticFunction:
      ::new (&staticFunction) StaticFunction(std::move(lv.staticFunction));
      break;
    case Kind::ObjCMethod:
      ::new (&objcMethod) ObjCMethod(std::move(lv.objcMethod));
      break;
    }
  }

  LoweredValue &operator=(LoweredValue &&lv) {
    assert(this != &lv);
    this->~LoweredValue();
    ::new (this) LoweredValue(std::move(lv));
    return *this;
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
  
  Explosion getExplosion(IRGenFunction &IGF) const {
    Explosion e;
    getExplosion(IGF, e);
    return e;
  }

  llvm::Value *getSingletonExplosion(IRGenFunction &IGF) const;
  
  const StaticFunction &getStaticFunction() const {
    assert(kind == Kind::StaticFunction && "not a static function");
    return staticFunction;
  }
  
  const ObjCMethod &getObjCMethod() const {
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
  llvm::SmallDenseMap<const VarDecl *, unsigned, 8> ArgNo;
  llvm::SmallBitVector DidEmitDebugInfoForArg;
  
  // Destination basic blocks for condfail traps.
  llvm::SmallVector<llvm::BasicBlock *, 8> FailBBs;

  SILFunction *CurSILFn;
  Address IndirectReturn;
  
  IRGenSILFunction(IRGenModule &IGM, SILFunction *f);
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

  void overwriteLoweredExplosion(SILValue v, Explosion &e) {
    assert(v.getType().isObject() && "explosion for address value?!");
    auto it = LoweredValues.find(v);
    assert(it != LoweredValues.end() && "no existing entry for overwrite?");
    it->second = LoweredValue(e);
  }

  void setLoweredSingleValue(SILValue v, llvm::Value *scalar) {
    Explosion e;
    e.add(scalar);
    setLoweredExplosion(v, e);
  }
  
  /// Create a new StaticFunction corresponding to the given SIL value.
  void setLoweredStaticFunction(SILValue v,
                                llvm::Function *f,
                                AbstractCC cc) {
    assert(v.getType().isObject() && "function for address value?!");
    assert(v.getType().is<SILFunctionType>() &&
           "function for non-function value?!");
    setLoweredValue(v, StaticFunction{f, cc});
  }

  /// Create a new Objective-C method corresponding to the given SIL value.
  void setLoweredObjCMethod(SILValue v, SILDeclRef method) {
    assert(v.getType().isObject() && "function for address value?!");
    assert(v.getType().is<SILFunctionType>() &&
           "function for non-function value?!");
    setLoweredValue(v, ObjCMethod{method, SILType(), false});
  }

  /// Create a new Objective-C method corresponding to the given SIL value that
  /// starts its search from the given search type.
  ///
  /// Unlike \c setLoweredObjCMethod, which finds the method in the actual
  /// runtime type of the object, this routine starts at the static type of the
  /// object and searches up the the class hierarchy (toward superclasses).
  ///
  /// \param searchType The class from which the Objective-C runtime will start
  /// its search for a method.
  ///
  /// \param startAtSuper Whether we want to start at the superclass of the
  /// static type (vs. the static type itself).
  void setLoweredObjCMethodBounded(SILValue v, SILDeclRef method,
                                   SILType searchType, bool startAtSuper) {
    assert(v.getType().isObject() && "function for address value?!");
    assert(v.getType().is<SILFunctionType>() &&
           "function for non-function value?!");
    setLoweredValue(v, ObjCMethod{method, searchType, startAtSuper});
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
      auto schema = ti.getSchema();
      Explosion e;
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

  /// Return the single member of the lowered explosion for the
  /// given SIL value.
  llvm::Value *getLoweredSingletonExplosion(SILValue v) {
    return getLoweredValue(v).getSingletonExplosion(*this);
  }
  
  LoweredBB &getLoweredBB(SILBasicBlock *bb) {
    auto foundBB = LoweredBBs.find(bb);
    assert(foundBB != LoweredBBs.end() && "no llvm bb for sil bb?!");
    return foundBB->second;
  }

  /// At -O0, emit a shadow copy of an Address in an alloca, so the
  /// register allocator doesn't elide the dbg.value intrinsic when
  /// register pressure is high.  There is a trade-off to this: With
  /// shadow copies, we loose the precise lifetime.
  llvm::Value *emitShadowCopy(llvm::Value *Storage,
                              StringRef Name,
                              Alignment Align = Alignment(0)) {
    auto Ty = Storage->getType();
    if (IGM.Opts.Optimize ||
        isa<llvm::AllocaInst>(Storage) ||
        isa<llvm::UndefValue>(Storage) ||
        Ty == IGM.RefCountedPtrTy) // No debug info is emitted for refcounts.
      return Storage;

    if (Align.isZero())
      Align = IGM.getPointerAlignment();

    auto Alloca = createAlloca(Ty, Align, Name+".addr");
    Builder.CreateAlignedStore(Storage, Alloca.getAddress(), Align.getValue());
    return Alloca.getAddress();
  }

  llvm::Value *emitShadowCopy(const Address &Storage, StringRef Name) {
    return emitShadowCopy(Storage.getAddress(), Name, Storage.getAlignment());
  }

  void emitShadowCopy(ArrayRef<llvm::Value *> Vals, StringRef Name,
                      llvm::SmallVectorImpl<llvm::Value *> &Copy) {
    // Only do this at -O0.
    if (IGM.Opts.Optimize) {
      for (auto val : Vals)
        Copy.push_back(val);
      return;
    }

    // Single or empty values.
    if (Vals.size() <= 1) {
      for (auto val : Vals)
        Copy.push_back(emitShadowCopy(val, Name));
      return;
    }

    // Create a single aggregate alloca for explosions.
    SmallVector<llvm::Type *, 8> Eltypes;
    for (auto val : Vals)
      Eltypes.push_back(val->getType());
    auto AggregateType = llvm::StructType::get(Builder.getContext(), Eltypes);
    auto Align = IGM.getPointerAlignment();
    auto Alloca = createAlloca(AggregateType, Align, Name+".coerce");
    unsigned i = 0;
    for (auto val : Vals) {
      auto addr = Builder.CreateConstGEP2_32(Alloca.getAddress(), 0, i++);
      Builder.CreateStore(val, addr);
    }
    Copy.push_back(Alloca.getAddress());
  }

  /// Emit debug info for a function argument or a local variable.
  template <typename StorageType>
  void emitDebugVariableDeclaration(IRBuilder &Builder,
                                    StorageType Storage,
                                    DebugTypeInfo Ty,
                                    SILDebugScope *DS,
                                    StringRef Name) {
    if (!IGM.DebugInfo) return;
    auto N = ArgNo.find(cast<VarDecl>(Ty.getDecl()));
    if (N != ArgNo.end()) {
      if (DidEmitDebugInfoForArg[N->second])
        return;

      PrologueLocation AutoRestore(IGM.DebugInfo, Builder);
      IGM.DebugInfo->
        emitArgVariableDeclaration(Builder, Storage,
                                   Ty, DS, Name, N->second, DirectValue);
      DidEmitDebugInfoForArg.set(N->second);
    } else
      IGM.DebugInfo->
        emitStackVariableDeclaration(Builder, Storage,
                                     Ty, DS, Name, DirectValue);
  }

  
  void emitFailBB() {
    if (!FailBBs.empty()) {
      // Move the trap basic blocks to the end of the function.
      for (auto *FailBB : FailBBs) {
        auto &BlockList = CurFn->getBasicBlockList();
        BlockList.splice(BlockList.end(), BlockList, FailBB);
      }
    }
  }
  
  //===--------------------------------------------------------------------===//
  // SIL instruction lowering
  //===--------------------------------------------------------------------===//

  void visitSILBasicBlock(SILBasicBlock *BB);
  IndirectionKind getLoweredArgValue(llvm::SmallVectorImpl<llvm::Value *> &Vals,
                                     SILArgument *Arg, StringRef Name);

  void emitFunctionArgDebugInfo(SILBasicBlock *BB);

  void visitAllocStackInst(AllocStackInst *i);
  void visitAllocRefInst(AllocRefInst *i);
  void visitAllocRefDynamicInst(AllocRefDynamicInst *i);
  void visitAllocBoxInst(AllocBoxInst *i);

  void visitApplyInst(ApplyInst *i);
  void visitTryApplyInst(TryApplyInst *i);
  void visitFullApplySite(FullApplySite i);
  void visitPartialApplyInst(PartialApplyInst *i);
  void visitBuiltinInst(BuiltinInst *i);

  void visitFunctionRefInst(FunctionRefInst *i);
  void visitGlobalAddrInst(GlobalAddrInst *i);

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
  void visitDebugValueInst(DebugValueInst *i) {
    if (!IGM.DebugInfo)
      return;

    VarDecl *Decl = i->getDecl();
    if (!Decl)
      return;

    auto N = ArgNo.find(Decl);
    if (N != ArgNo.end() && DidEmitDebugInfoForArg[N->second])
      return;

    StringRef Name = Decl->getNameStr();
    auto SILVal = i->getOperand();
    Explosion e = getLoweredExplosion(SILVal);
    DebugTypeInfo DbgTy(Decl, SILVal.getType().getSwiftRValueType(),
                        getTypeInfo(SILVal.getType()));
    // Emit an -O0 shadow copy for the explosion.
    llvm::SmallVector<llvm::Value *, 8> Copy;
    emitShadowCopy(e.claimAll(), Name, Copy);
    emitDebugVariableDeclaration(Builder, Copy, DbgTy,
                                 i->getDebugScope(), Name);
  }
  void visitDebugValueAddrInst(DebugValueAddrInst *i) {
    if (!IGM.DebugInfo) return;
    VarDecl *Decl = i->getDecl();
    if (!Decl) return;
    StringRef Name = Decl->getName().str();
    auto SILVal = i->getOperand();
    auto Val = getLoweredAddress(SILVal).getAddress();
    emitDebugVariableDeclaration
      (Builder, Val,
       DebugTypeInfo(Decl, SILVal.getType().getSwiftRValueType(),
                     getTypeInfo(SILVal.getType())),
       i->getDebugScope(), Name);
  }
  void visitLoadWeakInst(LoadWeakInst *i);
  void visitStoreWeakInst(StoreWeakInst *i);
  void visitRetainValueInst(RetainValueInst *i);
  void visitReleaseValueInst(ReleaseValueInst *i);
  void visitAutoreleaseValueInst(AutoreleaseValueInst *i);
  void visitStructInst(StructInst *i);
  void visitTupleInst(TupleInst *i);
  void visitEnumInst(EnumInst *i);
  void visitInitEnumDataAddrInst(InitEnumDataAddrInst *i);
  void visitSelectEnumInst(SelectEnumInst *i);
  void visitSelectEnumAddrInst(SelectEnumAddrInst *i);
  void visitSelectValueInst(SelectValueInst *i);
  void visitUncheckedEnumDataInst(UncheckedEnumDataInst *i);
  void visitUncheckedTakeEnumDataAddrInst(UncheckedTakeEnumDataAddrInst *i);
  void visitInjectEnumAddrInst(InjectEnumAddrInst *i);
  void visitObjCProtocolInst(ObjCProtocolInst *i);
  void visitMetatypeInst(MetatypeInst *i);
  void visitValueMetatypeInst(ValueMetatypeInst *i);
  void visitExistentialMetatypeInst(ExistentialMetatypeInst *i);
  void visitTupleExtractInst(TupleExtractInst *i);
  void visitTupleElementAddrInst(TupleElementAddrInst *i);
  void visitStructExtractInst(StructExtractInst *i);
  void visitStructElementAddrInst(StructElementAddrInst *i);
  void visitRefElementAddrInst(RefElementAddrInst *i);

  void visitClassMethodInst(ClassMethodInst *i);
  void visitSuperMethodInst(SuperMethodInst *i);
  void visitWitnessMethodInst(WitnessMethodInst *i);
  void visitDynamicMethodInst(DynamicMethodInst *i);

  void visitAllocValueBufferInst(AllocValueBufferInst *i);
  void visitProjectValueBufferInst(ProjectValueBufferInst *i);
  void visitDeallocValueBufferInst(DeallocValueBufferInst *i);

  void visitOpenExistentialAddrInst(OpenExistentialAddrInst *i);
  void visitOpenExistentialMetatypeInst(OpenExistentialMetatypeInst *i);
  void visitOpenExistentialRefInst(OpenExistentialRefInst *i);
  void visitInitExistentialAddrInst(InitExistentialAddrInst *i);
  void visitInitExistentialMetatypeInst(InitExistentialMetatypeInst *i);
  void visitInitExistentialRefInst(InitExistentialRefInst *i);
  void visitDeinitExistentialAddrInst(DeinitExistentialAddrInst *i);
  
  void visitAllocExistentialBoxInst(AllocExistentialBoxInst *i);
  void visitOpenExistentialBoxInst(OpenExistentialBoxInst *i);
  void visitDeallocExistentialBoxInst(DeallocExistentialBoxInst *i);
  
  void visitProjectBlockStorageInst(ProjectBlockStorageInst *i);
  void visitInitBlockStorageHeaderInst(InitBlockStorageHeaderInst *i);
  
  void visitFixLifetimeInst(FixLifetimeInst *i);
  void visitMarkDependenceInst(MarkDependenceInst *i);
  void visitCopyBlockInst(CopyBlockInst *i);
  void visitStrongPinInst(StrongPinInst *i);
  void visitStrongUnpinInst(StrongUnpinInst *i);
  void visitStrongRetainInst(StrongRetainInst *i);
  void visitStrongReleaseInst(StrongReleaseInst *i);
  void visitStrongRetainAutoreleasedInst(StrongRetainAutoreleasedInst *i);
  void visitStrongRetainUnownedInst(StrongRetainUnownedInst *i);
  void visitUnownedRetainInst(UnownedRetainInst *i);
  void visitUnownedReleaseInst(UnownedReleaseInst *i);
  void visitDeallocStackInst(DeallocStackInst *i);
  void visitDeallocBoxInst(DeallocBoxInst *i);
  void visitDeallocRefInst(DeallocRefInst *i);

  void visitCopyAddrInst(CopyAddrInst *i);
  void visitDestroyAddrInst(DestroyAddrInst *i);

  void visitCondFailInst(CondFailInst *i);
  
  void visitConvertFunctionInst(ConvertFunctionInst *i);
  void visitThinFunctionToPointerInst(ThinFunctionToPointerInst *i);
  void visitPointerToThinFunctionInst(PointerToThinFunctionInst *i);
  void visitUpcastInst(UpcastInst *i);
  void visitAddressToPointerInst(AddressToPointerInst *i);
  void visitPointerToAddressInst(PointerToAddressInst *i);
  void visitUncheckedRefCastInst(UncheckedRefCastInst *i);
  void visitUncheckedAddrCastInst(UncheckedAddrCastInst *i);
  void visitUncheckedRefBitCastInst(UncheckedRefBitCastInst *i);
  void visitUncheckedTrivialBitCastInst(UncheckedTrivialBitCastInst *i);
  void visitRefToRawPointerInst(RefToRawPointerInst *i);
  void visitRawPointerToRefInst(RawPointerToRefInst *i);
  void visitRefToUnownedInst(RefToUnownedInst *i);
  void visitUnownedToRefInst(UnownedToRefInst *i);
  void visitRefToUnmanagedInst(RefToUnmanagedInst *i);
  void visitUnmanagedToRefInst(UnmanagedToRefInst *i);
  void visitThinToThickFunctionInst(ThinToThickFunctionInst *i);
  void visitThickToObjCMetatypeInst(ThickToObjCMetatypeInst *i);
  void visitObjCToThickMetatypeInst(ObjCToThickMetatypeInst *i);
  void visitUnconditionalCheckedCastInst(UnconditionalCheckedCastInst *i);
  void visitUnconditionalCheckedCastAddrInst(UnconditionalCheckedCastAddrInst *i);
  void visitObjCMetatypeToObjectInst(ObjCMetatypeToObjectInst *i);
  void visitObjCExistentialMetatypeToObjectInst(
                                        ObjCExistentialMetatypeToObjectInst *i);
  void visitRefToBridgeObjectInst(RefToBridgeObjectInst *i);
  void visitBridgeObjectToRefInst(BridgeObjectToRefInst *i);
  void visitBridgeObjectToWordInst(BridgeObjectToWordInst *i);

  void visitIsNonnullInst(IsNonnullInst *i);

  void visitIndexAddrInst(IndexAddrInst *i);
  void visitIndexRawPointerInst(IndexRawPointerInst *i);
  
  void visitUnreachableInst(UnreachableInst *i);
  void visitBranchInst(BranchInst *i);
  void visitCondBranchInst(CondBranchInst *i);
  void visitReturnInst(ReturnInst *i);
  void visitAutoreleaseReturnInst(AutoreleaseReturnInst *i);
  void visitThrowInst(ThrowInst *i);
  void visitSwitchValueInst(SwitchValueInst *i);
  void visitSwitchEnumInst(SwitchEnumInst *i);
  void visitSwitchEnumAddrInst(SwitchEnumAddrInst *i);
  void visitDynamicMethodBranchInst(DynamicMethodBranchInst *i);
  void visitCheckedCastBranchInst(CheckedCastBranchInst *i);
  void visitCheckedCastAddrBranchInst(CheckedCastAddrBranchInst *i);
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
    for (auto *value : explosion.values)
      ex.add(value);
    break;

  case Kind::StaticFunction:
    ex.add(staticFunction.getExplosionValue(IGF));
    break;
      
  case Kind::ObjCMethod:
    ex.add(objcMethod.getExplosionValue(IGF));
    break;
  }
}

llvm::Value *LoweredValue::getSingletonExplosion(IRGenFunction &IGF) const {
  switch (kind) {
  case Kind::Address:
    llvm_unreachable("not a value");

  case Kind::Explosion:
    assert(explosion.values.size() == 1);
    return explosion.values[0];

  case Kind::StaticFunction:
    return staticFunction.getExplosionValue(IGF);

  case Kind::ObjCMethod:
    return objcMethod.getExplosionValue(IGF);
  }
  llvm_unreachable("bad lowered value kind!");
}

IRGenSILFunction::IRGenSILFunction(IRGenModule &IGM,
                                   SILFunction *f)
  : IRGenFunction(IGM, IGM.getAddrOfSILFunction(f, ForDefinition),
                  f->getDebugScope(), f->getLocation()),
    CurSILFn(f)
{}

IRGenSILFunction::~IRGenSILFunction() {
  assert(Builder.hasPostTerminatorIP() && "did not terminate BB?!");
  // Emit the fail BB if we have one.
  if (!FailBBs.empty())
    emitFailBB();
  DEBUG(CurFn->print(llvm::dbgs()));
}

template<typename ValueVector>
static void emitPHINodesForType(IRGenSILFunction &IGF, SILType type,
                                const TypeInfo &ti, unsigned predecessors,
                                ValueVector &phis) {
  if (type.isAddress()) {
    phis.push_back(IGF.Builder.CreatePHI(ti.getStorageType()->getPointerTo(),
                                         predecessors));
  } else {
    // PHIs are always emitted with maximal explosion.
    ExplosionSchema schema = ti.getSchema();
    for (auto &elt : schema) {
      if (elt.isScalar())
        phis.push_back(
                   IGF.Builder.CreatePHI(elt.getScalarType(), predecessors));
      else
        phis.push_back(
                   IGF.Builder.CreatePHI(elt.getAggregateType()->getPointerTo(),
                   predecessors));
    }
  }
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
      // FIXME: This should be an assertion.
      if (!DS || (DS->SILFn != IGF.CurSILFn && !DS->InlinedCallSite))
        DS = IGF.CurSILFn->getDebugScope();
      IGF.IGM.DebugInfo->setCurrentLoc(IGF.Builder, DS, I.getLoc());
    }
  }

  for (SILArgument *arg : make_range(silBB->bbarg_begin(), silBB->bbarg_end())) {
    size_t first = phis.size();
    
    const TypeInfo &ti = IGF.getTypeInfo(arg->getType());
    
    emitPHINodesForType(IGF, arg->getType(), ti, predecessors, phis);
    if (arg->getType().isAddress()) {
      IGF.setLoweredAddress(SILValue(arg,0),
                            ti.getAddressForPointer(phis.back()));
    } else {
      Explosion argValue;
      for (llvm::PHINode *phi :
               swift::make_range(phis.begin()+first, phis.end()))
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
      auto retTy = IGF.CurSILFn->mapTypeIntoContext(funcTy->getResult()
                                                          .getSILType());
      auto &retTI = IGF.IGM.getTypeInfo(retTy);
      IGF.IndirectReturn = retTI.getAddressForPointer(params.claimNext());
    }
    return entry->getBBArgs();
  }  
}

/// Emit a direct parameter that was passed under a native CC.
static void emitDirectExternalParameter(IRGenSILFunction &IGF,
                                        Explosion &in,
                                        llvm::Type *coercionTy,
                                        Explosion &out,
                                        SILType paramType,
                                        const LoadableTypeInfo &paramTI) {
  // The ABI IR types for the entrypoint might differ from the
  // Swift IR types for the body of the function.

  ArrayRef<llvm::Type*> expandedTys;
  if (auto expansionTy = dyn_cast<llvm::StructType>(coercionTy)) {
    expandedTys = makeArrayRef(expansionTy->element_begin(),
                               expansionTy->getNumElements());

  // Fast-path a really common case.  This check assumes that either
  // the storage type of a type is an llvm::StructType or it has a
  // single-element explosion.
  } else if (coercionTy == paramTI.StorageType) {
    out.add(in.claimNext());
    return;
  } else {
    expandedTys = coercionTy;
  }

  auto outputSchema = paramTI.getSchema();

  // Check to see if we can pairwise-coerce Swift's exploded scalars
  // to Clang's expanded elements.
  if (canCoerceToSchema(IGF.IGM, expandedTys, outputSchema)) {
    for (auto &outputElt : outputSchema) {
      llvm::Value *param = in.claimNext();
      llvm::Type *outputTy = outputElt.getScalarType();
      if (param->getType() != outputTy)
        param = IGF.coerceValue(param, outputTy, IGF.IGM.DataLayout);
      out.add(param);
    }
    return;
  }

  // Otherwise, we need to traffic through memory.
  assert((IGF.IGM.DataLayout.getTypeSizeInBits(coercionTy) ==
          IGF.IGM.DataLayout.getTypeSizeInBits(paramTI.StorageType))
         && "Coerced types should not differ in size!");

  // Create a temporary.
  Address temporary = paramTI.allocateStack(IGF, paramType,
                                            "coerced-param").getAddress();

  // Write the input parameters into the temporary:
  Address coercedAddr =
    IGF.Builder.CreateBitCast(temporary, coercionTy->getPointerTo());

  // Break down a struct expansion if necessary.
  if (auto expansionTy = dyn_cast<llvm::StructType>(coercionTy)) {
    auto layout = IGF.IGM.DataLayout.getStructLayout(expansionTy);
    for (unsigned i = 0, e = expansionTy->getNumElements(); i != e; ++i) {
      auto fieldOffset = Size(layout->getElementOffset(i));
      auto fieldAddr = IGF.Builder.CreateStructGEP(coercedAddr, i, fieldOffset);
      IGF.Builder.CreateStore(in.claimNext(), fieldAddr);
    }

  // Otherwise, store the single scalar.
  } else {
    IGF.Builder.CreateStore(in.claimNext(), coercedAddr);
  }

  // Pull out the elements.
  paramTI.loadAsTake(IGF, temporary, out);

  // Deallocate the temporary.
  paramTI.deallocateStack(IGF, temporary, paramType);
}

static void bindParameter(IRGenSILFunction &IGF,
                          SILArgument *param,
                          Explosion &allParamValues) {
  // Pull out the parameter value and its formal type.
  auto &paramTI = IGF.getTypeInfo(param->getType());

  // If the SIL parameter isn't passed indirectly, we need to map it
  // to an explosion.  Fortunately, in this case we have a guarantee
  // that it's passed directly in IR.
  if (param->getType().isObject()) {
    Explosion paramValues;
    cast<LoadableTypeInfo>(paramTI).reexplode(IGF, allParamValues, paramValues);
    IGF.setLoweredExplosion(SILValue(param, 0), paramValues);
    return;
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

/// Emit entry point arguments for a SILFunction with the Swift calling
/// convention.
static void emitEntryPointArgumentsNativeCC(IRGenSILFunction &IGF,
                                            SILBasicBlock *entry,
                                            Explosion &allParamValues) {
  auto funcTy = IGF.CurSILFn->getLoweredFunctionType();
  
  // Map the indirect return if present.
  ArrayRef<SILArgument*> params
    = emitEntryPointIndirectReturn(IGF, entry, allParamValues, funcTy,
      [&]() -> bool {
        auto retType
          = IGF.CurSILFn->mapTypeIntoContext(funcTy->getResult()
                                                    .getSILType());
        return IGF.IGM.requiresIndirectResult(retType);
      });

  // The witness method CC passes Self as a final argument.
  WitnessMetadata witnessMetadata;
  if (funcTy->getAbstractCC() == AbstractCC::WitnessMethod) {
    collectTrailingWitnessMetadata(IGF, *IGF.CurSILFn, allParamValues,
                                   witnessMetadata);
  }

  // Bind the error result by popping it off the parameter list.
  if (funcTy->hasErrorResult()) {
    IGF.setErrorResultSlot(allParamValues.takeLast());
  }

  // The 'self' argument might be in the context position, which is
  // now the end of the parameter list.  Bind it now.
  if (funcTy->hasSelfArgument() &&
      isSelfContextParameter(funcTy->getSelfParameter())) {
    SILArgument *selfParam = params.back();
    params = params.drop_back();

    Explosion selfTemp;
    selfTemp.add(allParamValues.takeLast());
    bindParameter(IGF, selfParam, selfTemp);

  // Even if we don't have a 'self', if we have an error result, we
  // should have a placeholder argument here.
  } else if (funcTy->hasErrorResult() ||
             funcTy->hasThickRepresentation()) {
    llvm::Value *contextPtr = allParamValues.takeLast(); (void) contextPtr;
    assert(contextPtr->getType() == IGF.IGM.RefCountedPtrTy);
  }

  // Map the remaining SIL parameters to LLVM parameters.
  for (SILArgument *param : params) {
    bindParameter(IGF, param, allParamValues);
  }

  // Bind polymorphic arguments.  This can only be done after binding
  // all the value parameters.
  if (hasPolymorphicParameters(funcTy)) {
    emitPolymorphicParameters(IGF, *IGF.CurSILFn, allParamValues,
                              &witnessMetadata,
      [&](unsigned paramIndex) -> llvm::Value* {
        SILValue parameter = entry->getBBArgs()[paramIndex];
        return IGF.getLoweredSingletonExplosion(parameter);
      });
  }

  assert(allParamValues.empty() && "didn't claim all parameters!");
}

/// Emit entry point arguments for the parameters of a C function, or the
/// method parameters of an ObjC method.
static void emitEntryPointArgumentsCOrObjC(IRGenSILFunction &IGF,
                                           SILBasicBlock *entry,
                                           Explosion &params,
                                           CanSILFunctionType funcTy) {
  // Map the indirect return if present.
  ArrayRef<SILArgument*> args
    = emitEntryPointIndirectReturn(IGF, entry, params, funcTy, [&] {
        return requiresExternalIndirectResult(IGF.IGM, funcTy);
      });

  SmallVector<clang::CanQualType,4> argTys;
  auto const &clangCtx = IGF.IGM.getClangASTContext();

  const auto &resultInfo = funcTy->getResult();
  auto clangResultTy = IGF.IGM.getClangType(resultInfo.getSILType());
  unsigned nextArgTyIdx = 0;

  if (IGF.CurSILFn->getAbstractCC() == AbstractCC::ObjCMethod) {
    // First include the self argument and _cmd arguments as types to
    // be considered for ABI type selection purposes.
    SILArgument *selfArg = args.back();
    args = args.slice(0, args.size() - 1);
    auto clangTy = IGF.IGM.getClangType(selfArg->getType());
    argTys.push_back(clangTy);
    argTys.push_back(clangCtx.VoidPtrTy);

    // Now set the lowered explosion for the self argument and drop
    // the explosion element for the _cmd argument.
    auto &selfType = IGF.getTypeInfo(selfArg->getType());
    auto &selfTI = cast<LoadableTypeInfo>(selfType);
    auto selfSchema = selfTI.getSchema();
    assert(selfSchema.size() == 1 && "Expected self to be a single element!");

    auto *selfValue = params.claimNext();
    auto *bodyType = selfSchema.begin()->getScalarType();
    if (selfValue->getType() != bodyType)
      selfValue = IGF.coerceValue(selfValue, bodyType, IGF.IGM.DataLayout);

    Explosion self;
    self.add(selfValue);
    IGF.setLoweredExplosion(selfArg, self);

    // Discard the implicit _cmd argument.
    params.claimNext();

    // We've handled the self and _cmd arguments, so when we deal with
    // generating explosions for the remaining arguments we can skip
    // these.
    nextArgTyIdx = 2;
  }

  // Convert each argument to a Clang type.
  for (SILArgument *arg : args) {
    auto clangTy = IGF.IGM.getClangType(arg->getType());
    argTys.push_back(clangTy);
  }

  // Generate the ABI types for this set of result type + argument types.
  auto extInfo = clang::FunctionType::ExtInfo();
  auto &FI = IGF.IGM.ABITypes->arrangeFreeFunctionCall(clangResultTy,
                                                       argTys, extInfo,
                                             clang::CodeGen::RequiredArgs::All);

  assert(FI.arg_size() == argTys.size() &&
         "Expected one ArgInfo for each parameter type!");
  assert(args.size() == (argTys.size() - nextArgTyIdx) &&
         "Number of arguments not equal to number of argument types!");

  // Generate lowered explosions for each explicit argument.
  for (auto i : indices(args)) {
    auto *arg = args[i];
    auto argTyIdx = i + nextArgTyIdx;
    auto &argTI = IGF.getTypeInfo(arg->getType());
    
    // Bitcast indirect argument pointers to the right storage type.
    if (arg->getType().isAddress()) {
      llvm::Value *ptr = params.claimNext();
      ptr = IGF.Builder.CreateBitCast(ptr,
                                      argTI.getStorageType()->getPointerTo());
      IGF.setLoweredAddress(arg, Address(ptr, argTI.getBestKnownAlignment()));
      continue;
    }
    
    auto &loadableArgTI = cast<LoadableTypeInfo>(argTI);
    Explosion argExplosion;

    auto AI = FI.arg_begin()[argTyIdx].info;

    // Drop padding arguments.
    if (AI.getPaddingType())
      params.claimNext();

    switch (AI.getKind()) {
    case clang::CodeGen::ABIArgInfo::Extend:
    case clang::CodeGen::ABIArgInfo::Direct: {
      emitDirectExternalParameter(IGF, params, AI.getCoerceToType(),
                                  argExplosion, arg->getType(), loadableArgTI);
      IGF.setLoweredExplosion(arg, argExplosion);
      continue;
    }
    case clang::CodeGen::ABIArgInfo::Indirect: {
      Address address = loadableArgTI.getAddressForPointer(params.claimNext());
      loadableArgTI.loadAsTake(IGF, address, argExplosion);
      IGF.setLoweredExplosion(arg, argExplosion);
      continue;
    }
    case clang::CodeGen::ABIArgInfo::Expand: {
      emitClangExpandedParameter(IGF, params, argExplosion, argTys[argTyIdx],
                                 arg->getType(), loadableArgTI);
      IGF.setLoweredExplosion(arg, argExplosion);
      continue;
    }

    case clang::CodeGen::ABIArgInfo::Ignore:
    case clang::CodeGen::ABIArgInfo::InAlloca:
      llvm_unreachable("Need to handle InAlloca during signature expansion");
    }
  }
}

/// Get metadata for the dynamic Self type if we have it.
static void emitLocalSelfMetadata(IRGenSILFunction &IGF) {
  // Self is the final SIL argument, if any.
  if (IGF.CurSILFn->begin()->bbarg_empty())
    return;
  
  SILArgument *selfArg = IGF.CurSILFn->begin()->getBBArgs().back();
  
  // If the argument is a class or class metatype value, we can use it for Self's
  // metadata.
  auto selfSILTy = selfArg->getType();
  if (!selfSILTy.isObject())
    return;
  CanType instanceTy = selfSILTy.getSwiftRValueType();
  CanMetatypeType metaTy = dyn_cast<MetatypeType>(instanceTy);
  if (metaTy)
    instanceTy = metaTy.getInstanceType();
  
  if (!instanceTy->getClassOrBoundGenericClass())
    return;

  IRGenFunction::LocalSelfKind selfKind;
  if (!metaTy)
    selfKind = IRGenFunction::ObjectReference;
  else switch (metaTy->getRepresentation()) {
  case MetatypeRepresentation::Thin:
    llvm_unreachable("class metatypes are never thin");
  case MetatypeRepresentation::Thick:
    selfKind = IRGenFunction::SwiftMetatype;
    break;
  case MetatypeRepresentation::ObjC:
    selfKind = IRGenFunction::ObjCMetatype;
    break;
  }
  
  llvm::Value *value = IGF.getLoweredExplosion(selfArg).claimNext();
  IGF.setLocalSelfMetadata(value, selfKind);
}

/// Emit the definition for the given SIL constant.
void IRGenModule::emitSILFunction(SILFunction *f) {
  if (f->isExternalDeclaration())
    return;

  PrettyStackTraceSILFunction stackTrace("emitting IR", f);
  IRGenSILFunction(*this, f).emitSILFunction();
}

void IRGenSILFunction::emitSILFunction() {
  DEBUG(llvm::dbgs() << "emitting SIL function: ";
        CurSILFn->printName(llvm::dbgs());
        llvm::dbgs() << '\n';
        CurSILFn->print(llvm::dbgs()));
  
  assert(!CurSILFn->empty() && "function has no basic blocks?!");
  
  // FIXME: Or if this is a witness. DebugInfo doesn't have an interface to
  // correctly handle the generic parameters of a witness, which can come from
  // both the requirement and witness contexts.
  if (IGM.DebugInfo && CurSILFn->getAbstractCC() != AbstractCC::WitnessMethod) {
    IGM.DebugInfo->emitFunction(*CurSILFn, CurFn);
  }

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
  auto funcTy = CurSILFn->getLoweredFunctionType();

  switch (CurSILFn->getAbstractCC()) {
  case AbstractCC::Freestanding:
  case AbstractCC::Method:
  case AbstractCC::WitnessMethod:
    emitEntryPointArgumentsNativeCC(*this, entry->first, params);
    break;
  case AbstractCC::ObjCMethod:
  case AbstractCC::C:
    emitEntryPointArgumentsCOrObjC(*this, entry->first, params, funcTy);
    break;
  }
  emitLocalSelfMetadata(*this);
  
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
      auto next = std::next(SILFunction::iterator(bb));
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
    for (auto &succ : bb->getSuccessors()) {
      auto succBB = succ.getBB();
      if (visitedBlocks.insert(succBB).second)
        workQueue.push_back(succBB);
    }
  }

  // If there are dead blocks in the SIL function, we might have left
  // invalid blocks in the IR.  Do another pass and kill them off.
  for (SILBasicBlock &bb : *CurSILFn)
    if (!visitedBlocks.count(&bb))
      LoweredBBs[&bb].bb->eraseFromParent();
}

/// Store the lowered IR representation of Arg in the array
/// Vals. Returns true if Arg is a byref argument.
IndirectionKind IRGenSILFunction::
getLoweredArgValue(llvm::SmallVectorImpl<llvm::Value *> &Vals,
                   SILArgument *Arg, StringRef Name) {
  const LoweredValue &LoweredArg = getLoweredValue(Arg);
  if (LoweredArg.isAddress()) {
    Vals.push_back(LoweredArg.getAddress().getAddress());
    return IndirectValue;
  } else if (LoweredArg.kind == LoweredValue::Kind::Explosion) {
    Explosion e = LoweredArg.getExplosion(*this);
    for (auto val : e.claimAll())
      Vals.push_back(val);
  } else
    llvm_unreachable("unhandled argument kind");
  return DirectValue;
}

void IRGenSILFunction::emitFunctionArgDebugInfo(SILBasicBlock *BB) {
  assert(BB->pred_empty());
  if (!IGM.DebugInfo)
    return;

  // This is the prologue of a function. Emit debug info for all
  // trivial arguments and any captured and promoted [inout]
  // variables.
  int N = 0;
  for (auto I = BB->getBBArgs().begin(), E=BB->getBBArgs().end();
       I != E; ++I) {
    SILArgument *Arg = *I;
    ++N;

    if (!Arg->getDecl() || DidEmitDebugInfoForArg[N])
      continue;

    // Generic and existential types were already handled in
    // visitAllocStackInst.
    if (Arg->getType().isExistentialType() ||
        Arg->getType().getSwiftRValueType()->isDependentType() ||
        Arg->getType().is<ArchetypeType>())
      continue;

    auto Name = Arg->getDecl()->getNameStr();
    DebugTypeInfo DTI(const_cast<ValueDecl*>(Arg->getDecl()),
                      getTypeInfo(Arg->getType()));

    llvm::SmallVector<llvm::Value *, 8> Vals, Copy;
    bool Deref = getLoweredArgValue(Vals, Arg, Name);
    // Don't bother emitting swift.refcounted* for now.
    if (Vals.size() && Vals.back()->getType() == IGM.RefCountedPtrTy)
      Vals.pop_back();

    // Consolidate all pieces of an exploded multi-argument into one list.
    for (auto Next = I+1; Next != E; ++Next, ++I) {
      if ((*Next)->getDecl() != Arg->getDecl())
        break;

      Deref |= getLoweredArgValue(Vals, *Next, Name);
    }
    if (DTI.getType()->getKind() == TypeKind::InOut) {
      if (!Deref) {
        // If the value was promoted, unwrap the type.
        DTI = DebugTypeInfo(DTI.getType()->castTo<InOutType>()->getObjectType(),
                            DTI.StorageType, DTI.size, DTI.align,
                            DTI.getDeclContext());
      }

      // In contrast to by-value captures, inout arguments are encoded
      // as reference types, so there is no need to emit a deref
      // operation.
      Deref = DirectValue;
    }

    // Emit -O0 shadow copies for by-value parameters to ensure they
    // are visible until the end of the function.
    emitShadowCopy(Vals, Name, Copy);
    IGM.DebugInfo->emitArgVariableDeclaration
      (Builder, Copy, DTI, getDebugScope(), Name, N,
       IndirectionKind(Deref), RealValue);

    DidEmitDebugInfoForArg.set(N);
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

  // The basic blocks are visited in a random order. Reset the debug location.
  std::unique_ptr<AutoRestoreLocation> ScopedLoc;
  if (InEntryBlock)
    ScopedLoc = llvm::make_unique<PrologueLocation>(IGM.DebugInfo, Builder);
  else
    ScopedLoc = llvm::make_unique<ArtificialLocation>(
        CurSILFn->getDebugScope(), IGM.DebugInfo, Builder);

  if (InEntryBlock) {
    // Establish a mapping from VarDecl -> ArgNo to be used by
    // visitAllocStackInst().
    unsigned N = 1;
    for (auto Arg : BB->getBBArgs()) {
      if (auto VD = dyn_cast_or_null<VarDecl>(Arg->getDecl()))
        ArgNo.insert( {VD, N} );
      ++N;
    }
    DidEmitDebugInfoForArg.resize(N);
  }

  // Generate the body.
  bool InCleanupBlock = false;
  bool KeepCurrentLocation = false;

  for (auto InsnIter = BB->begin(); InsnIter != BB->end(); ++InsnIter) {
    auto &I = *InsnIter;
    if (IGM.DebugInfo) {
      // Set the debug info location for I, if applicable.
      SILLocation ILoc = I.getLoc();
      // Handle cleanup locations.
      if (ILoc.getKind() == SILLocation::CleanupKind) {
        // Cleanup locations point to the decl of the the value that
        // is being destroyed (for diagnostic generation). As far as
        // the linetable is concerned, cleanups at the end of a
        // lexical scope should point to the cleanup location, which
        // is the location of the last instruction in the basic block.
        if (!InCleanupBlock) {
          InCleanupBlock = true;
          // Scan ahead to see if this is the final cleanup block in
          // this basic block.
          auto It = InsnIter;
          do ++It; while (It != BB->end() &&
                          It->getLoc().getKind() == SILLocation::CleanupKind);
          // We are still in the middle of a basic block?
          if (It != BB->end() && !isa<TermInst>(It))
            KeepCurrentLocation = true;
        }

        // Assign the cleanup location to this instruction.
        if (!KeepCurrentLocation) {
          assert(BB->getTerminator());
          ILoc = BB->getTerminator()->getLoc();
        }
      } else if (InCleanupBlock) {
        KeepCurrentLocation = false;
        InCleanupBlock = false;
      }

      auto DS = I.getDebugScope();
      assert((!DS || (DS->SILFn == CurSILFn || DS->InlinedCallSite)) &&
             "insn was not inlined, but belongs to a different function");

      // Until SILDebugScopes are properly serialized, bare functions
      // are allowed to not have a scope.
      if (!DS) {
        if (CurSILFn->isBare())
          DS = CurSILFn->getDebugScope();
        else
          assert(maybeScopeless(I) && "instruction has location, but no scope");
      }

      // Ignore scope-less instructions and have IRBuilder reuse the
      // previous location and scope.
      if (DS && !KeepCurrentLocation && !ILoc.isInPrologue())
        IGM.DebugInfo->setCurrentLoc(Builder, DS, ILoc);

      // Function argument handling.
      if (InEntryBlock && !ArgsEmitted) {
        if (!I.getLoc().isInPrologue()) {
          if (I.getLoc().getSourceLoc().isValid()) {
            // This is the first non-prologue instruction in the entry
            // block.  The function prologue is where the stack frame is
            // set up and storage for local variables and function
            // arguments is initialized.  We need to emit the debug info
            // for the function arguments after the function prologue,
            // after the initialization.
            if (!DS)
              DS = CurSILFn->getDebugScope();
            IGM.DebugInfo->clearLoc(Builder);
            emitFunctionArgDebugInfo(BB);
            IGM.DebugInfo->setCurrentLoc(Builder, DS, ILoc);
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

void IRGenSILFunction::visitFunctionRefInst(FunctionRefInst *i) {
  llvm::Function *fnptr =
    IGM.getAddrOfSILFunction(i->getReferencedFunction(), NotForDefinition);
  
  // Store the function constant and calling
  // convention as a StaticFunction so we can avoid bitcasting or thunking if
  // we don't need to.
  setLoweredStaticFunction(SILValue(i, 0), fnptr,
                           i->getReferencedFunction()->getAbstractCC());
}

void IRGenSILFunction::visitGlobalAddrInst(GlobalAddrInst *i) {
  auto &ti = getTypeInfo(i->getType());
  
  Address addr;
  // If the variable is empty, don't actually emit it; just return undef.
  if (ti.isKnownEmpty()) {
    addr = ti.getUndefAddress();
  } else {
    addr = IGM.getAddrOfSILGlobalVariable(i->getReferencedGlobal(),
                                          NotForDefinition);
  }
  
  setLoweredAddress(SILValue(i, 0), addr);
}

void IRGenSILFunction::visitMetatypeInst(swift::MetatypeInst *i) {
  auto metaTy = i->getType().castTo<MetatypeType>();
  Explosion e;
  emitMetatypeRef(*this, metaTy, e);
  setLoweredExplosion(SILValue(i, 0), e);
}

static llvm::Value *getClassBaseValue(IRGenSILFunction &IGF,
                                      SILValue v) {
  if (v.getType().isAddress()) {
    auto addr = IGF.getLoweredAddress(v);
    return IGF.Builder.CreateLoad(addr);
  }
  
  Explosion e = IGF.getLoweredExplosion(v);
  return e.claimNext();
}

static llvm::Value *getClassMetatype(IRGenFunction &IGF,
                                     llvm::Value *baseValue,
                                     MetatypeRepresentation repr,
                                     SILType instanceType) {
  switch (repr) {
  case MetatypeRepresentation::Thin:
    llvm_unreachable("Class metatypes are never thin");
    
  case MetatypeRepresentation::Thick:
    return emitDynamicTypeOfHeapObject(IGF, baseValue, instanceType);
      
  case MetatypeRepresentation::ObjC:
    return emitHeapMetadataRefForHeapObject(IGF, baseValue, instanceType);
  }
}

void IRGenSILFunction::visitValueMetatypeInst(swift::ValueMetatypeInst *i) {
  SILType instanceTy = i->getOperand().getType();
  auto metaTy = i->getType().castTo<MetatypeType>();
  
  if (metaTy->getRepresentation() == MetatypeRepresentation::Thin) {
    Explosion empty;
    setLoweredExplosion(SILValue(i, 0), empty);
    return;
  }
  
  Explosion e;
  
  if (instanceTy.getClassOrBoundGenericClass()) {
    e.add(getClassMetatype(*this,
                           getClassBaseValue(*this, i->getOperand()),
                           metaTy->getRepresentation(), instanceTy));
  } else if (auto arch = instanceTy.getAs<ArchetypeType>()) {
    if (arch->requiresClass()) {
      e.add(getClassMetatype(*this,
                             getClassBaseValue(*this, i->getOperand()),
                             metaTy->getRepresentation(), instanceTy));
    } else {
      Address base = getLoweredAddress(i->getOperand());
      e.add(emitDynamicTypeOfOpaqueArchetype(*this, base,
                                             i->getOperand().getType()));
      // FIXME: We need to convert this back to an ObjC class for an
      // ObjC metatype representation.
      if (metaTy->getRepresentation() == MetatypeRepresentation::ObjC)
        unimplemented(i->getLoc().getSourceLoc(),
                      "objc metatype of non-class-bounded archetype");
    }
  } else {
    emitMetatypeRef(*this, metaTy, e);
  }
  
  setLoweredExplosion(SILValue(i, 0), e);
}

void IRGenSILFunction::visitExistentialMetatypeInst(
                                            swift::ExistentialMetatypeInst *i) {
  Explosion result;
  if (i->getOperand().getType().isClassExistentialType()) {
    SILValue op = i->getOperand();
    Explosion existential = getLoweredExplosion(op);
    emitMetatypeOfClassExistential(*this, existential, i->getType(),
                                   op.getType(), result);
  } else {
    Address existential = getLoweredAddress(i->getOperand());
    emitMetatypeOfOpaqueExistential(*this, existential,
                                    i->getOperand().getType(), result);
  }
  setLoweredExplosion(SILValue(i, 0), result);
}

static void emitApplyArgument(IRGenSILFunction &IGF,
                              SILValue arg,
                              SILParameterInfo param,
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
  if (!isSubstituted) {
    IGF.getLoweredExplosion(arg, out);
    return;
  }

  Explosion temp = IGF.getLoweredExplosion(arg);
  reemitAsUnsubstituted(IGF, param.getSILType(), arg.getType(),
                        temp, out);
}

static llvm::Value *getObjCClassForValue(IRGenSILFunction &IGF,
                                         llvm::Value *selfValue,
                                         CanAnyMetatypeType selfType) {
  // If we have a Swift metatype, map it to the heap metadata, which
  // will be the Class for an ObjC type.
  switch (selfType->getRepresentation()) {
  case swift::MetatypeRepresentation::ObjC:
    return selfValue;

  case swift::MetatypeRepresentation::Thick:
    // Convert thick metatype to Objective-C metatype.
    return emitClassHeapMetadataRefForMetatype(IGF, selfValue,
                                               selfType.getInstanceType());

  case swift::MetatypeRepresentation::Thin:
    llvm_unreachable("Cannot convert Thin metatype to ObjC metatype");
  }
  llvm_unreachable("bad metatype representation");
}

static CallEmission getCallEmissionForLoweredValue(IRGenSILFunction &IGF,
                                         CanSILFunctionType origCalleeType,
                                         CanSILFunctionType substCalleeType,
                                         const LoweredValue &lv,
                                         llvm::Value *selfValue,
                                         Explosion &args,
                                         ArrayRef<Substitution> substitutions) {
  llvm::Value *calleeFn, *calleeData;
  
  switch (lv.kind) {
  case LoweredValue::Kind::StaticFunction:
    calleeFn = lv.getStaticFunction().getFunction();
    calleeData = selfValue;
    break;
      
  case LoweredValue::Kind::ObjCMethod: {
    assert(selfValue);
    auto &objcMethod = lv.getObjCMethod();
    ObjCMessageKind kind = ObjCMessageKind::Normal;
    if (objcMethod.getSearchType())
      kind = objcMethod.shouldStartAtSuper()? ObjCMessageKind::Super
                                            : ObjCMessageKind::Peer;

    CallEmission emission =
      prepareObjCMethodRootCall(IGF, objcMethod.getMethod(),
                                origCalleeType, substCalleeType,
                                substitutions, kind);

    // Convert a metatype 'self' argument to the ObjC Class pointer.
    // FIXME: Should be represented in SIL.
    if (auto metatype = dyn_cast<AnyMetatypeType>(
                          origCalleeType->getSelfParameter().getType())) {
      selfValue = getObjCClassForValue(IGF, selfValue, metatype);
    }

    addObjCMethodCallImplicitArguments(IGF, args, objcMethod.getMethod(),
                                       selfValue,
                                       objcMethod.getSearchType());
    return emission;
  }
      
  case LoweredValue::Kind::Explosion: {    
    switch (origCalleeType->getRepresentation()) {
    case AnyFunctionType::Representation::Block: {
      assert(!selfValue && "block function with self?");

      // Grab the block pointer and make it the first physical argument.
      llvm::Value *blockPtr = lv.getSingletonExplosion(IGF);
      blockPtr = IGF.Builder.CreateBitCast(blockPtr, IGF.IGM.ObjCBlockPtrTy);
      args.add(blockPtr);

      // Extract the invocation pointer for blocks.
      llvm::Value *invokeAddr = IGF.Builder.CreateStructGEP(blockPtr, 3);
      calleeFn = IGF.Builder.CreateLoad(invokeAddr, IGF.IGM.getPointerAlignment());
      calleeData = nullptr;
      break;
    }
        
    case AnyFunctionType::Representation::Thin:
    case AnyFunctionType::Representation::Thick: {
      Explosion calleeValues = lv.getExplosion(IGF);
      calleeFn = calleeValues.claimNext();

      if (origCalleeType->hasThickRepresentation()) {
        assert(!selfValue);
        calleeData = calleeValues.claimNext();
      } else {
        calleeData = selfValue;
      }
      break;
    }
    }

    // Cast the callee pointer to the right function type.
    llvm::AttributeSet attrs;
    auto fnPtrTy =
      IGF.IGM.getFunctionType(origCalleeType, attrs)->getPointerTo();
    calleeFn = IGF.Builder.CreateBitCast(calleeFn, fnPtrTy);
    break;
  }
      
  case LoweredValue::Kind::Address:
    llvm_unreachable("sil address isn't a valid callee");
  }
  
  Callee callee = Callee::forKnownFunction(origCalleeType, substCalleeType,
                                           substitutions, calleeFn, calleeData);
  CallEmission callEmission(IGF, callee);
  if (IGF.CurSILFn->isThunk())
    callEmission.addAttribute(llvm::AttributeSet::FunctionIndex, llvm::Attribute::NoInline);
  
  return callEmission;
}

void IRGenSILFunction::visitBuiltinInst(swift::BuiltinInst *i) {
  auto argValues = i->getArguments();
  Explosion args;
  for (auto argValue : argValues) {
    // Builtin arguments should never be substituted, so use the value's type
    // as the parameter type.
    emitApplyArgument(*this, argValue,
                      SILParameterInfo(argValue.getType().getSwiftRValueType(),
                                       ParameterConvention::Direct_Unowned),
                      args);
  }
  
  Explosion result;
  emitBuiltinCall(*this, i->getName(), i->getType(),
                  args, result, i->getSubstitutions());
  
  setLoweredExplosion(SILValue(i,0), result);
}

void IRGenSILFunction::visitApplyInst(swift::ApplyInst *i) {
  visitFullApplySite(i);
}

void IRGenSILFunction::visitTryApplyInst(swift::TryApplyInst *i) {
  visitFullApplySite(i);
}

void IRGenSILFunction::visitFullApplySite(FullApplySite site) {
  const LoweredValue &calleeLV = getLoweredValue(site.getCallee());
  
  auto origCalleeType = site.getOrigCalleeType();
  auto substCalleeType = site.getSubstCalleeType();
  
  auto params = origCalleeType->getParametersWithoutIndirectResult();
  auto args = site.getArgumentsWithoutIndirectResult();
  assert(params.size() == args.size());

  // Extract 'self' if it needs to be passed as the context parameter.
  llvm::Value *selfValue = nullptr;
  if (origCalleeType->hasSelfArgument() &&
      isSelfContextParameter(origCalleeType->getSelfParameter())) {
    SILValue selfArg = args.back();
    args = args.drop_back();
    params = params.drop_back();

    if (selfArg.getType().isObject()) {
      selfValue = getLoweredSingletonExplosion(selfArg);
    } else {
      selfValue = getLoweredAddress(selfArg).getAddress();
    }
  }

  Explosion llArgs;    
  CallEmission emission =
    getCallEmissionForLoweredValue(*this, origCalleeType, substCalleeType,
                                   calleeLV, selfValue, llArgs,
                                   site.getSubstitutions());

  // Lower the arguments and return value in the callee's generic context.
  GenericContextScope scope(IGM, origCalleeType->getGenericSignature());

  // Save off the indirect return argument, if any.
  SILValue indirectResult;
  if (site.hasIndirectResult()) {
    indirectResult = site.getIndirectResult();
  }

  // Lower the SIL arguments to IR arguments.
  
  // Turn the formal SIL parameters into IR-gen things.
  for (auto index : indices(args)) {
    emitApplyArgument(*this, args[index], params[index], llArgs);
  }

  // Pass the generic arguments.
  WitnessMetadata witnessMetadata;
  if (hasPolymorphicParameters(origCalleeType)) {
    emitPolymorphicArguments(*this, origCalleeType, substCalleeType,
                             site.getSubstitutions(), &witnessMetadata, llArgs);
  }

  // Add all those arguments.
  emission.setArgs(llArgs, &witnessMetadata);

  SILInstruction *i = site.getInstruction();
  
  // If the SIL function takes an indirect-result argument, emit into it.
  Explosion result;
  if (indirectResult) {
    Address a = getLoweredAddress(indirectResult);
    auto &retTI = getTypeInfo(indirectResult.getType());
    emission.emitToMemory(a, retTI);

    // Leave an empty explosion in 'result'.
  } else {
    // FIXME: handle the result value being an address?
    // If the result is a non-address value, emit to an explosion.
    emission.emitToExplosion(result);
  }

  if (isa<ApplyInst>(i)) {
    setLoweredExplosion(SILValue(i, 0), result);
  }
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
  case LoweredValue::Kind::Explosion: {
    Explosion ex = lv.getExplosion(IGF);
    llvm::Value *fn = ex.claimNext();
    llvm::Value *context = nullptr;
    auto fnType = v.getType().castTo<SILFunctionType>();
    
    switch (fnType->getRepresentation()) {
    case AnyFunctionType::Representation::Thin:
      break;
    case AnyFunctionType::Representation::Thick:
      context = ex.claimNext();
      break;
    case AnyFunctionType::Representation::Block:
      llvm_unreachable("partial application of block not implemented");
    }
    
    return std::make_tuple(fn, context, fnType);
  }
  }
}

void IRGenSILFunction::visitPartialApplyInst(swift::PartialApplyInst *i) {
  SILValue v(i, 0);

  // NB: We collect the arguments under the substituted type.
  auto args = i->getArguments();
  auto params = i->getSubstCalleeType()->getParameters();
  params = params.slice(params.size() - args.size(), args.size());
  
  Explosion llArgs;

  {
    // Lower the parameters in the callee's generic context.
    GenericContextScope scope(IGM, i->getOrigCalleeType()->getGenericSignature());
    for (auto index : indices(args)) {
      assert(args[index].getType() == params[index].getSILType());
      emitApplyArgument(*this, args[index], params[index], llArgs);
    }
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
    
    Explosion function;
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
  Explosion function;
  emitFunctionPartialApplication(*this, calleeFn, innerContext, llArgs,
                                 params, i->getSubstitutions(),
                                 origCalleeTy, i->getSubstCalleeType(),
                                 i->getType().castTo<SILFunctionType>(),
                                 function);
  setLoweredExplosion(v, function);
}

/// Construct a ConstantInt from an IntegerLiteralInst.
static llvm::Constant *getConstantInt(IRGenModule &IGM,
                                      swift::IntegerLiteralInst *i) {
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

  return llvm::ConstantInt::get(IGM.LLVMContext, value);
}

void IRGenSILFunction::visitIntegerLiteralInst(swift::IntegerLiteralInst *i) {
  llvm::Value *constant = getConstantInt(IGM, i);
  
  Explosion e;
  e.add(constant);
  setLoweredExplosion(SILValue(i, 0), e);
}

/// Construct a ConstantFP from a FloatLiteralInst.
static llvm::Constant *getConstantFP(IRGenModule &IGM,
                                     swift::FloatLiteralInst *i) {
  return llvm::ConstantFP::get(IGM.LLVMContext, i->getValue());
}

void IRGenSILFunction::visitFloatLiteralInst(swift::FloatLiteralInst *i) {
  llvm::Value *constant = getConstantFP(IGM, i);
  Explosion e;
  e.add(constant);
  setLoweredExplosion(SILValue(i, 0), e);
}

static llvm::Constant *getAddrOfString(IRGenModule &IGM, StringRef string,
                                       StringLiteralInst::Encoding encoding) {
  switch (encoding) {
  case swift::StringLiteralInst::Encoding::UTF8:
    return IGM.getAddrOfGlobalString(string);

  case swift::StringLiteralInst::Encoding::UTF16:
    // This is always a GEP of a GlobalVariable with a nul terminator.
    auto addr = IGM.getAddrOfGlobalUTF16String(string);

    // Cast to Builtin.RawPointer.
    return llvm::ConstantExpr::getBitCast(addr, IGM.Int8PtrTy);
  }
  llvm_unreachable("bad string encoding");
}

void IRGenSILFunction::visitStringLiteralInst(swift::StringLiteralInst *i) {
  auto addr = getAddrOfString(IGM, i->getValue(), i->getEncoding());

  Explosion e;
  e.add(addr);
  setLoweredExplosion(SILValue(i, 0), e);
}

void IRGenSILFunction::visitUnreachableInst(swift::UnreachableInst *i) {
  Builder.CreateUnreachable();
}

static void emitReturnInst(IRGenSILFunction &IGF,
                           SILType resultTy,
                           Explosion &result) {
  // The invariant on the out-parameter is that it's always zeroed, so
  // there's nothing to do here.

  // Even if SIL has a direct return, the IR-level calling convention may
  // require an indirect return.
  if (IGF.IndirectReturn.isValid()) {
    auto &retTI = cast<LoadableTypeInfo>(IGF.getTypeInfo(resultTy));
    retTI.initialize(IGF, result, IGF.IndirectReturn);
    IGF.Builder.CreateRetVoid();
  } else {
    IGF.emitScalarReturn(resultTy, result);
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
  Explosion temp;
  temp.add(emitObjCAutoreleaseReturnValue(*this, result.claimNext()));
  emitReturnInst(*this, i->getOperand().getType(), temp);
}

void IRGenSILFunction::visitThrowInst(swift::ThrowInst *i) {
  // Store the exception to the error slot.
  llvm::Value *exn = getLoweredSingletonExplosion(i->getOperand());
  Builder.CreateStore(exn, getCallerErrorResultSlot());

  // Create a normal return, but leaving the return value undefined.
  auto fnTy = CurFn->getType()->getPointerElementType();
  auto retTy = cast<llvm::FunctionType>(fnTy)->getReturnType();
  if (retTy->isVoidTy()) {
    Builder.CreateRetVoid();
  } else {
    Builder.CreateRet(llvm::UndefValue::get(retTy));
  }
}

static llvm::BasicBlock *emitBBMapForSwitchValue(
        IRGenSILFunction &IGF,
        SmallVectorImpl<std::pair<SILValue, llvm::BasicBlock*>> &dests,
        SwitchValueInst *inst) {
  for (unsigned i = 0, e = inst->getNumCases(); i < e; ++i) {
    auto casePair = inst->getCase(i);
    dests.push_back({casePair.first, IGF.getLoweredBB(casePair.second).bb});
  }

  llvm::BasicBlock *defaultDest = nullptr;
  if (inst->hasDefault())
    defaultDest = IGF.getLoweredBB(inst->getDefaultBB()).bb;
  return defaultDest;
}

static llvm::ConstantInt *
getSwitchCaseValue(IRGenFunction &IGF, SILValue val) {
  if (auto *IL = dyn_cast<IntegerLiteralInst>(val)) {
    return dyn_cast<llvm::ConstantInt>(getConstantInt(IGF.IGM, IL));
  }
  else {
    llvm_unreachable("Switch value cases should be integers");
  }
}

static void
emitSwitchValueDispatch(IRGenSILFunction &IGF,
                        SILType ty,
                        Explosion &value,
                        ArrayRef<std::pair<SILValue, llvm::BasicBlock*>> dests,
                        llvm::BasicBlock *defaultDest) {
  // Create an unreachable block for the default if the original SIL
  // instruction had none.
  bool unreachableDefault = false;
  if (!defaultDest) {
    unreachableDefault = true;
    defaultDest = llvm::BasicBlock::Create(IGF.IGM.getLLVMContext());
  }

  if (ty.getAs<BuiltinIntegerType>()) {
    auto *discriminator = value.claimNext();
    auto *i = IGF.Builder.CreateSwitch(discriminator, defaultDest,
                                     dests.size());
    for (auto &dest : dests)
      i->addCase(getSwitchCaseValue(IGF, dest.first), dest.second);
  } else {
    // Get the value we're testing, which is a function.
    llvm::Value *val;
    llvm::BasicBlock *nextTest = nullptr;
    if (ty.getSwiftType()->is<SILFunctionType>()) {
      val = value.claimNext();   // Function pointer.
      //values.claimNext();         // Ignore the data pointer.
    } else {
      llvm_unreachable("switch_value operand has an unknown type");
    }

    for (int i = 0, e = dests.size(); i < e; ++i) {
      auto casePair = dests[i];
      llvm::Value *caseval;
      auto casevalue = IGF.getLoweredExplosion(casePair.first);
      if (casePair.first.getType().getSwiftType()->is<SILFunctionType>()) {
        caseval = casevalue.claimNext();   // Function pointer.
        //values.claimNext();         // Ignore the data pointer.
      } else {
        llvm_unreachable("switch_value operand has an unknown type");
      }

      // Compare operand with a case tag value.
      llvm::Value *cond = IGF.Builder.CreateICmp(llvm::CmpInst::ICMP_EQ,
                                                 val, caseval);

      if (i == e -1 && !unreachableDefault) {
        nextTest = nullptr;
        IGF.Builder.CreateCondBr(cond, casePair.second, defaultDest);
      } else {
        nextTest = IGF.createBasicBlock("next-test");
        IGF.Builder.CreateCondBr(cond, casePair.second, nextTest);
        IGF.Builder.emitBlock(nextTest);
        IGF.Builder.SetInsertPoint(nextTest);
      }
    }

    if (nextTest) {
      IGF.Builder.CreateBr(defaultDest);
    }
  }

  if (unreachableDefault) {
    IGF.Builder.emitBlock(defaultDest);
    IGF.Builder.CreateUnreachable();
  }
}

void IRGenSILFunction::visitSwitchValueInst(SwitchValueInst *inst) {
  Explosion value = getLoweredExplosion(inst->getOperand());

  // Map the SIL dest bbs to their LLVM bbs.
  SmallVector<std::pair<SILValue, llvm::BasicBlock*>, 4> dests;
  auto *defaultDest = emitBBMapForSwitchValue(*this, dests, inst);

  emitSwitchValueDispatch(*this, inst->getOperand().getType(),
                                  value, dests, defaultDest);
}

// Bind an incoming explosion value to an explosion of LLVM phi node(s).
static void addIncomingExplosionToPHINodes(IRGenSILFunction &IGF,
                                           ArrayRef<llvm::Value*> phis,
                                           Explosion &argValue) {
  llvm::BasicBlock *curBB = IGF.Builder.GetInsertBlock();
  unsigned phiIndex = 0;
  while (!argValue.empty())
    cast<llvm::PHINode>(phis[phiIndex++])
      ->addIncoming(argValue.claimNext(), curBB);
  assert(phiIndex == phis.size() && "explosion doesn't match number of phis");
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
                                         ArrayRef<llvm::Value*> phis,
                                         Address argValue) {
  llvm::BasicBlock *curBB = IGF.Builder.GetInsertBlock();
  assert(phis.size() == 1 && "more than one phi for address?!");
  cast<llvm::PHINode>(phis[0])->addIncoming(argValue.getAddress(), curBB);
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
  auto &EIS = getEnumImplStrategy(IGM, inst->getOperand().getType());
  EIS.emitValueSwitch(*this, value, dests, defaultDest);
  
  // Bind arguments for cases that want them.
  for (unsigned i = 0, e = inst->getNumCases(); i < e; ++i) {
    auto casePair = inst->getCase(i);
    
    if (!casePair.second->bbarg_empty()) {
      auto waypointBB = dests[i].second;
      auto &destLBB = getLoweredBB(casePair.second);
      
      Builder.emitBlock(waypointBB);
      
      Explosion inValue = getLoweredExplosion(inst->getOperand());
      Explosion projected;
      emitProjectLoadableEnum(*this, inst->getOperand().getType(),
                               inValue, casePair.first, projected);
      
      unsigned phiIndex = 0;
      addIncomingExplosionToPHINodes(*this, destLBB, phiIndex, projected);
      
      Builder.CreateBr(destLBB.bb);
    }
  }
}

void
IRGenSILFunction::visitSwitchEnumAddrInst(SwitchEnumAddrInst *inst) {
  Address value = getLoweredAddress(inst->getOperand());
  
  // Map the SIL dest bbs to their LLVM bbs.
  SmallVector<std::pair<EnumElementDecl*, llvm::BasicBlock*>, 4> dests;
  llvm::BasicBlock *defaultDest
    = emitBBMapForSwitchEnum(*this, dests, inst);
  
  // Emit the dispatch.
  emitSwitchAddressOnlyEnumDispatch(*this, inst->getOperand().getType(),
                                     value, dests, defaultDest);
}

// FIXME: We could lower select_enum directly to LLVM select in a lot of cases.
// For now, just emit a switch and phi nodes, like a chump.
template<class C, class T>
static llvm::BasicBlock *
emitBBMapForSelect(IRGenSILFunction &IGF,
                   Explosion &resultPHI,
                   SmallVectorImpl<std::pair<T, llvm::BasicBlock*>> &BBs,
                   llvm::BasicBlock *&defaultBB,
                   SelectInstBase<C, T> *inst) {
  
  auto origBB = IGF.Builder.GetInsertBlock();

  // Set up a continuation BB and phi nodes to receive the result value.
  llvm::BasicBlock *contBB = IGF.createBasicBlock("select_enum");
  IGF.Builder.SetInsertPoint(contBB);

  // Emit an explosion of phi node(s) to receive the value.
  SmallVector<llvm::Value*, 4> phis;
  auto &ti = IGF.getTypeInfo(inst->getType());
  emitPHINodesForType(IGF, inst->getType(), ti,
                      inst->getNumCases() + inst->hasDefault(),
                      phis);
  resultPHI.add(phis);
  
  IGF.Builder.SetInsertPoint(origBB);
  
  auto addIncoming = [&](SILValue value) {
    if (value.getType().isAddress()) {
      addIncomingAddressToPHINodes(IGF, resultPHI.getAll(),
                                   IGF.getLoweredAddress(value));
    } else {
      Explosion ex = IGF.getLoweredExplosion(value);
      addIncomingExplosionToPHINodes(IGF, resultPHI.getAll(), ex);
    }
  };
  
  for (unsigned i = 0, e = inst->getNumCases(); i < e; ++i) {
    auto casePair = inst->getCase(i);
    
    // Create a basic block destination for this case.
    llvm::BasicBlock *destBB = IGF.createBasicBlock("");
    IGF.Builder.emitBlock(destBB);
    
    // Feed the corresponding result into the phi nodes.
    addIncoming(casePair.second);

    // Jump immediately to the continuation.
    IGF.Builder.CreateBr(contBB);
    BBs.push_back(std::make_pair(casePair.first, destBB));
  }
  
  if (inst->hasDefault()) {
    defaultBB = IGF.createBasicBlock("");
    IGF.Builder.emitBlock(defaultBB);
    
    addIncoming(inst->getDefaultResult());
    
    IGF.Builder.CreateBr(contBB);
  } else {
    defaultBB = nullptr;
  }
  
  IGF.Builder.emitBlock(contBB);
  
  IGF.Builder.SetInsertPoint(origBB);
  return contBB;
}

// Try to map the value of a select_enum directly to an int type with a simple
// cast from the tag value to the result type. Optionally also by adding a
// constant offset.
// This is useful, e.g. for rawValue or hashValue of C-like enums.
static llvm::Value *
mapTriviallyToInt(IRGenSILFunction &IGF, const EnumImplStrategy &EIS, SelectEnumInst *inst) {

  // All cases must be covered
  if (inst->hasDefault())
    return nullptr;
  
  auto &ti = IGF.getTypeInfo(inst->getType());
  ExplosionSchema schema = ti.getSchema();

  // Check if the select_enum's result is a single integer scalar.
  if (schema.size() != 1)
    return nullptr;
  
  if (!schema[0].isScalar())
    return nullptr;
  
  llvm::Type *type = schema[0].getScalarType();
  llvm::IntegerType *resultType = dyn_cast<llvm::IntegerType>(type);
  if (!resultType)
    return nullptr;
  
  // Check if the case values directly map to the tag values, maybe with a
  // constant offset.
  APInt commonOffset;
  bool offsetValid = false;

  for (unsigned i = 0, e = inst->getNumCases(); i < e; ++i) {
    auto casePair = inst->getCase(i);

    int64_t index = EIS.getDiscriminatorIndex(casePair.first);
    if (index < 0)
      return nullptr;
    
    IntegerLiteralInst *intLit = dyn_cast<IntegerLiteralInst>(casePair.second.getDef());
    if (!intLit)
      return nullptr;
    
    APInt caseValue = intLit->getValue();
    APInt offset = caseValue - index;
    if (offsetValid) {
      if (offset != commonOffset)
        return nullptr;
    } else {
      commonOffset = offset;
      offsetValid = true;
    }
  }
  
  // Ask the enum implementation strategy to extract the enum tag as an integer
  // value.
  Explosion enumValue = IGF.getLoweredExplosion(inst->getEnumOperand());
  llvm::Value *result = EIS.emitExtractDiscriminator(IGF, enumValue);
  if (!result) {
    enumValue.claimAll();
    return nullptr;
  }

  // Cast to the result type.
  result = IGF.Builder.CreateIntCast(result, resultType, false);
  if (commonOffset != 0) {
    // The the offset, if any.
    auto *offsetConst = llvm::ConstantInt::get(resultType, commonOffset);
    result = IGF.Builder.CreateAdd(result, offsetConst);
  }
  return result;
}

template <class C, class T>
static LoweredValue
getLoweredValueForSelect(IRGenSILFunction &IGF,
                         Explosion &result, SelectInstBase<C, T> *inst) {
  if (inst->getType().isAddress())
    // FIXME: Loses potentially better alignment info we might have.
    return LoweredValue(Address(result.claimNext(),
                IGF.getTypeInfo(inst->getType()).getBestKnownAlignment()));
  return LoweredValue(result);
}

static void emitSingleEnumMemberSelectResult(IRGenSILFunction &IGF,
                                             SelectEnumInstBase *inst,
                                             llvm::Value *isTrue,
                                             Explosion &result) {
  assert((inst->getNumCases() == 1 && inst->hasDefault()) ||
         (inst->getNumCases() == 2 && !inst->hasDefault()));
  
  // Extract the true values.
  auto trueValue = inst->getCase(0).second;
  SmallVector<llvm::Value*, 4> TrueValues;
  if (trueValue.getType().isAddress()) {
    TrueValues.push_back(IGF.getLoweredAddress(trueValue).getAddress());
  } else {
    Explosion ex = IGF.getLoweredExplosion(trueValue);
    while (!ex.empty())
      TrueValues.push_back(ex.claimNext());
  }
    
  // Extract the false values.
  auto falseValue =
    inst->hasDefault() ? inst->getDefaultResult() : inst->getCase(1).second;
  SmallVector<llvm::Value*, 4> FalseValues;
  if (falseValue.getType().isAddress()) {
    FalseValues.push_back(IGF.getLoweredAddress(falseValue).getAddress());
  } else {
    Explosion ex = IGF.getLoweredExplosion(falseValue);
    while (!ex.empty())
      FalseValues.push_back(ex.claimNext());
  }
  
  assert(TrueValues.size() == FalseValues.size() &&
         "explosions didn't produce same element count?");
  for (unsigned i = 0, e = FalseValues.size(); i != e; ++i) {
    auto *TV = TrueValues[i], *FV = FalseValues[i];
    // It is pretty common to select between zero and 1 as the result of the
    // select.  Instead of emitting an obviously dumb select, emit nothing or
    // a zext.
    if (auto *TC = dyn_cast<llvm::ConstantInt>(TV))
      if (auto *FC = dyn_cast<llvm::ConstantInt>(FV))
        if (TC->isOne() && FC->isZero()) {
          result.add(IGF.Builder.CreateZExtOrBitCast(isTrue, TV->getType()));
          continue;
        }
        
    result.add(IGF.Builder.CreateSelect(isTrue, TV, FalseValues[i]));
  }
}


void IRGenSILFunction::visitSelectEnumInst(SelectEnumInst *inst) {
  auto &EIS = getEnumImplStrategy(IGM, inst->getEnumOperand().getType());
  Explosion result;

  if (llvm::Value *R = mapTriviallyToInt(*this, EIS, inst)) {
    result.add(R);
  } else if ((inst->getNumCases() == 1 && inst->hasDefault()) ||
             (inst->getNumCases() == 2 && !inst->hasDefault())) {
    // If this is testing for one case, do simpler codegen.  This is
    // particularly common when testing optionals.
    Explosion value = getLoweredExplosion(inst->getEnumOperand());
    auto isTrue = EIS.emitValueCaseTest(*this, value, inst->getCase(0).first);
    emitSingleEnumMemberSelectResult(*this, inst, isTrue, result);
  } else {
    Explosion value = getLoweredExplosion(inst->getEnumOperand());

    // Map the SIL dest bbs to their LLVM bbs.
    SmallVector<std::pair<EnumElementDecl*, llvm::BasicBlock*>, 4> dests;
    llvm::BasicBlock *defaultDest;
    llvm::BasicBlock *contBB
      = emitBBMapForSelect(*this, result, dests, defaultDest, inst);
    
    // Emit the dispatch.
    EIS.emitValueSwitch(*this, value, dests, defaultDest);

    // emitBBMapForSelectEnum set up a continuation block and phi nodes to
    // receive the result.
    Builder.SetInsertPoint(contBB);
  }
  setLoweredValue(SILValue(inst, 0),
                  getLoweredValueForSelect(*this, result, inst));
}

void IRGenSILFunction::visitSelectEnumAddrInst(SelectEnumAddrInst *inst) {
  Address value = getLoweredAddress(inst->getEnumOperand());
  Explosion result;

  if ((inst->getNumCases() == 1 && inst->hasDefault()) ||
      (inst->getNumCases() == 2 && !inst->hasDefault())) {
    auto &EIS = getEnumImplStrategy(IGM, inst->getEnumOperand().getType());
    // If this is testing for one case, do simpler codegen.  This is
    // particularly common when testing optionals.
    auto isTrue = EIS.emitIndirectCaseTest(*this,
                                           inst->getEnumOperand().getType(),
                                           value, inst->getCase(0).first);
    emitSingleEnumMemberSelectResult(*this, inst, isTrue, result);
  } else {
      // Map the SIL dest bbs to their LLVM bbs.
    SmallVector<std::pair<EnumElementDecl*, llvm::BasicBlock*>, 4> dests;
    llvm::BasicBlock *defaultDest;
    llvm::BasicBlock *contBB
      = emitBBMapForSelect(*this, result, dests, defaultDest, inst);
    
    // Emit the dispatch.
    emitSwitchAddressOnlyEnumDispatch(*this, inst->getEnumOperand().getType(),
                                      value, dests, defaultDest);
    
    // emitBBMapForSelectEnum set up a phi node to receive the result.
    Builder.SetInsertPoint(contBB);
  }
  
  setLoweredValue(SILValue(inst, 0),
                  getLoweredValueForSelect(*this, result, inst));
}

void IRGenSILFunction::visitSelectValueInst(SelectValueInst *inst) {
  Explosion value = getLoweredExplosion(inst->getOperand());

  // Map the SIL dest bbs to their LLVM bbs.
  SmallVector<std::pair<SILValue, llvm::BasicBlock*>, 4> dests;
  llvm::BasicBlock *defaultDest;
  Explosion result;
  auto *contBB = emitBBMapForSelect(*this, result, dests, defaultDest, inst);

  // Emit the dispatch.
  emitSwitchValueDispatch(*this, inst->getOperand().getType(), value, dests,
                          defaultDest);

  // emitBBMapForSelectEnum set up a continuation block and phi nodes to
  // receive the result.
  Builder.SetInsertPoint(contBB);

  setLoweredValue(SILValue(inst, 0),
                  getLoweredValueForSelect(*this, result, inst));
}

void IRGenSILFunction::visitDynamicMethodBranchInst(DynamicMethodBranchInst *i){
  LoweredBB &hasMethodBB = getLoweredBB(i->getHasMethodBB());
  LoweredBB &noMethodBB = getLoweredBB(i->getNoMethodBB());

  // Emit the swift_objcRespondsToSelector() call.
  StringRef selector;
  llvm::SmallString<64> selectorBuffer;
  if (auto fnDecl = dyn_cast<FuncDecl>(i->getMember().getDecl()))
    selector = fnDecl->getObjCSelector().getString(selectorBuffer);
  else if (auto var = dyn_cast<AbstractStorageDecl>(i->getMember().getDecl()))
    selector = var->getObjCGetterSelector().getString(selectorBuffer);
  else
    llvm_unreachable("Unhandled dynamic method branch query");

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

void IRGenSILFunction::visitRetainValueInst(swift::RetainValueInst *i) {
  Explosion in = getLoweredExplosion(i->getOperand());
  Explosion out;
  cast<LoadableTypeInfo>(getTypeInfo(i->getOperand().getType()))
    .copy(*this, in, out);
  out.claimAll();
}

// TODO: Implement this more generally for arbitrary values. Currently the
// SIL verifier restricts it to single-refcounted-pointer types.
void IRGenSILFunction::visitAutoreleaseValueInst(swift::AutoreleaseValueInst *i)
{
  Explosion in = getLoweredExplosion(i->getOperand());
  auto val = in.claimNext();
  
  emitObjCAutoreleaseCall(val);
}

void IRGenSILFunction::visitReleaseValueInst(swift::ReleaseValueInst *i) {
  Explosion in = getLoweredExplosion(i->getOperand());
  cast<LoadableTypeInfo>(getTypeInfo(i->getOperand().getType()))
    .consume(*this, in);
}

void IRGenSILFunction::visitStructInst(swift::StructInst *i) {
  Explosion out;
  for (SILValue elt : i->getElements())
    out.add(getLoweredExplosion(elt).claimAll());
  setLoweredExplosion(SILValue(i, 0), out);
}

void IRGenSILFunction::visitTupleInst(swift::TupleInst *i) {
  Explosion out;
  for (SILValue elt : i->getElements())
    out.add(getLoweredExplosion(elt).claimAll());
  setLoweredExplosion(SILValue(i, 0), out);
}

void IRGenSILFunction::visitEnumInst(swift::EnumInst *i) {
  Explosion data = (i->hasOperand())
    ? getLoweredExplosion(i->getOperand())
    : Explosion();
  Explosion out;
  emitInjectLoadableEnum(*this, i->getType(), i->getElement(), data, out);
  setLoweredExplosion(SILValue(i, 0), out);
}

void IRGenSILFunction::visitInitEnumDataAddrInst(swift::InitEnumDataAddrInst *i) {
  Address enumAddr = getLoweredAddress(i->getOperand());
  Address dataAddr = emitProjectEnumAddressForStore(*this,
                                                     i->getOperand().getType(),
                                                     enumAddr,
                                                     i->getElement());
  setLoweredAddress(SILValue(i, 0), dataAddr);
}

void IRGenSILFunction::visitUncheckedEnumDataInst(swift::UncheckedEnumDataInst *i) {
  Explosion enumVal = getLoweredExplosion(i->getOperand());
  Explosion data;
  emitProjectLoadableEnum(*this, i->getOperand().getType(),
                          enumVal, i->getElement(), data);
  setLoweredExplosion(SILValue(i, 0), data);
}

void IRGenSILFunction::visitUncheckedTakeEnumDataAddrInst(swift::UncheckedTakeEnumDataAddrInst *i) {
  Address enumAddr = getLoweredAddress(i->getOperand());
  Address dataAddr = emitDestructiveProjectEnumAddressForLoad(*this,
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

void IRGenSILFunction::visitTupleExtractInst(swift::TupleExtractInst *i) {
  SILValue v(i, 0);
  Explosion fullTuple = getLoweredExplosion(i->getOperand());
  Explosion output;
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
  Explosion lowered;
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
                                                    i->getType(),
                                                    i->getField())
    .getAddress();
  setLoweredAddress(SILValue(i, 0), field);
}

void IRGenSILFunction::visitLoadInst(swift::LoadInst *i) {
  Explosion lowered;
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

  Explosion result;
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

void IRGenSILFunction::visitFixLifetimeInst(swift::FixLifetimeInst *i) {
  if (i->getOperand().getType().isAddress()) {
    // Just pass in the address to fix lifetime if we have one. We will not do
    // anything to it so nothing bad should happen.
    emitFixLifetime(getLoweredAddress(i->getOperand()).getAddress());
    return;
  }

  // Handle objects.
  Explosion in = getLoweredExplosion(i->getOperand());
  cast<LoadableTypeInfo>(getTypeInfo(i->getOperand().getType()))
    .fixLifetime(*this, in);
}

void IRGenSILFunction::visitMarkDependenceInst(swift::MarkDependenceInst *i) {
  // Dependency-marking is purely for SIL.  Just forward the input as
  // the result.

  SILValue value = i->getValue();
  if (value.getType().isAddress()) {
    setLoweredAddress(i, getLoweredAddress(value));
  } else {
    Explosion temp = getLoweredExplosion(value);
    setLoweredExplosion(i, temp);
  }
}

void IRGenSILFunction::visitCopyBlockInst(CopyBlockInst *i) {
  Explosion lowered = getLoweredExplosion(i->getOperand());
  llvm::Value *copied = emitBlockCopyCall(lowered.claimNext());
  Explosion result;
  result.add(copied);
  setLoweredExplosion(SILValue(i, 0), result);
}

void IRGenSILFunction::visitStrongPinInst(swift::StrongPinInst *i) {
  Explosion lowered = getLoweredExplosion(i->getOperand());
  llvm::Value *object = lowered.claimNext();
  llvm::Value *pinHandle = emitTryPin(object);

  Explosion result;
  result.add(pinHandle);
  setLoweredExplosion(i, result);
}

void IRGenSILFunction::visitStrongUnpinInst(swift::StrongUnpinInst *i) {
  Explosion lowered = getLoweredExplosion(i->getOperand());
  llvm::Value *pinHandle = lowered.claimNext();
  emitUnpin(pinHandle);
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
  value = emitObjCRetainAutoreleasedReturnValue(*this, value);

  // Overwrite the stored explosion value with the result of
  // objc_retainAutoreleasedReturnValue.  This is actually
  // semantically important: if the call result is live across this
  // call, the backend will have to emit instructions that interfere
  // with the reclaim optimization.
  //
  // This is only sound if the retainAutoreleasedReturnValue
  // immediately follows the call, but that should be reliably true.
  //
  // ...the reclaim here should really be implicit in the SIL calling
  // convention.

  Explosion out;
  out.add(value);
  overwriteLoweredExplosion(i->getOperand(), out);
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
    Decl ? Decl->getNameStr() :
# endif
    "";

  auto addr = type.allocateStack(*this,
                                 i->getElementType(),
                                 dbgname);
  if (IGM.DebugInfo && Decl) {
    auto *Pattern = Decl->getParentPattern();
    if (!Pattern || !Pattern->isImplicit()) {
      // Discard any inout or lvalue qualifiers. Since the object itself
      // is stored in the alloca, emitting it as a reference type would
      // be wrong.
      auto DbgTy = DebugTypeInfo(Decl,
                                 Decl->getType()->getLValueOrInOutObjectType(),
                                 type);
      auto Name = Decl->getName().empty() ? "_" : Decl->getName().str();
      auto DS = i->getDebugScope();
      if (!DS) DS = CurSILFn->getDebugScope();
      assert(DS->SILFn == CurSILFn || DS->InlinedCallSite);
      emitDebugVariableDeclaration(Builder, addr.getAddress().getAddress(),
                                   DbgTy, DS, Name);
    }
  }
  
  setLoweredAddress(i->getContainerResult(), addr.getContainer());
  setLoweredAddress(i->getAddressResult(), addr.getAddress());
}

void IRGenSILFunction::visitAllocRefInst(swift::AllocRefInst *i) {
  llvm::Value *alloced = emitClassAllocation(*this, i->getType(), i->isObjC());
  Explosion e;
  e.add(alloced);
  setLoweredExplosion(SILValue(i, 0), e);
}

void IRGenSILFunction::visitAllocRefDynamicInst(swift::AllocRefDynamicInst *i) {
  Explosion metadata = getLoweredExplosion(i->getOperand());
  auto metadataValue = metadata.claimNext();
  llvm::Value *alloced = emitClassAllocationDynamic(*this, metadataValue,
                                                    i->getType(), i->isObjC());
  Explosion e;
  e.add(alloced);
  setLoweredExplosion(SILValue(i, 0), e);
}

void IRGenSILFunction::visitDeallocStackInst(swift::DeallocStackInst *i) {
  const TypeInfo &type = getTypeInfo(i->getOperand().getType());
  Address addr = getLoweredAddress(i->getOperand());
  type.deallocateStack(*this, addr,
                       i->getOperand().getType());
}

void IRGenSILFunction::visitDeallocRefInst(swift::DeallocRefInst *i) {
  // Lower the operand.
  Explosion self = getLoweredExplosion(i->getOperand());
  auto selfValue = self.claimNext();
  auto classType = i->getOperand()->getType(0);
  emitClassDeallocation(*this, classType, selfValue);
}

void IRGenSILFunction::visitDeallocBoxInst(swift::DeallocBoxInst *i) {
  const TypeInfo &type = getTypeInfo(i->getElementType());
  Explosion owner = getLoweredExplosion(i->getOperand());
  llvm::Value *ownerPtr = owner.claimNext();
  type.deallocateBox(*this, ownerPtr, i->getElementType());
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
  OwnedAddress addr = type.allocateBox(*this,
                                       i->getElementType(),
                                       DbgName);
  
  Explosion box;
  box.add(addr.getOwner());
  setLoweredExplosion(SILValue(i, 0), box);
  setLoweredAddress(SILValue(i, 1), addr.getAddress());

  if (IGM.DebugInfo && Decl) {
    auto Indirection = IndirectValue;
    // LValues are implicitly indirect because of their type.
    if (Decl->getType()->getKind() == TypeKind::LValue)
      Indirection = DirectValue;
    // FIXME: inout arguments that are not promoted are emitted as
    // arguments and also boxed and thus may show up twice. This may
    // or may not be bad.
    IGM.DebugInfo->emitStackVariableDeclaration
      (Builder,
       emitShadowCopy(addr.getAddress(), Name),
       DebugTypeInfo(Decl, i->getElementType().getSwiftType(), type),
       i->getDebugScope(), Name, Indirection);
  }
}

void IRGenSILFunction::visitConvertFunctionInst(swift::ConvertFunctionInst *i) {
  // This instruction is specified to be a no-op.
  Explosion temp = getLoweredExplosion(i->getOperand());
  setLoweredExplosion(SILValue(i, 0), temp);
}

void IRGenSILFunction::visitThinFunctionToPointerInst(
                                          swift::ThinFunctionToPointerInst *i) {
  Explosion in = getLoweredExplosion(i->getOperand());
  llvm::Value *fn = in.claimNext();
  fn = Builder.CreateBitCast(fn, IGM.Int8PtrTy);
  Explosion out;
  out.add(fn);
  setLoweredExplosion(i, out);
}

void IRGenSILFunction::visitPointerToThinFunctionInst(
                                          swift::PointerToThinFunctionInst *i) {
  Explosion in = getLoweredExplosion(i->getOperand());
  llvm::Value *fn = in.claimNext();
  fn = Builder.CreateBitCast(fn, IGM.FunctionPtrTy);
  Explosion out;
  out.add(fn);
  setLoweredExplosion(i, out);
}

void IRGenSILFunction::visitAddressToPointerInst(swift::AddressToPointerInst *i)
{
  Explosion to;
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
                                const TypeInfo &ti) {
  Explosion from = IGF.getLoweredExplosion(src);
  llvm::Value *ptrValue = from.claimNext();
  
  auto schema = ti.getSchema();
  assert(schema.size() == 1
         && schema[0].isScalar()
         && "pointer schema is not a single scalar?!");
  auto castToType = schema[0].getScalarType();
  
  ptrValue = IGF.Builder.CreateBitCast(ptrValue, castToType);
  
  Explosion to;
  to.add(ptrValue);
  IGF.setLoweredExplosion(dest, to);
}

void IRGenSILFunction::visitUncheckedRefCastInst(
                                             swift::UncheckedRefCastInst *i) {
  auto &ti = getTypeInfo(i->getType());
  emitPointerCastInst(*this, i->getOperand(), SILValue(i, 0), ti);
}

void IRGenSILFunction::visitUncheckedAddrCastInst(
                                             swift::UncheckedAddrCastInst *i) {
  auto addr = getLoweredAddress(i->getOperand());
  auto &ti = getTypeInfo(i->getType());
  auto result = Builder.CreateBitCast(addr, ti.getStorageType()->getPointerTo());
  setLoweredAddress(SILValue(i, 0), result);
}

static void emitValueBitCast(IRGenSILFunction &IGF,
                             SourceLoc loc,
                             Explosion &in,
                             const LoadableTypeInfo &inTI,
                             Explosion &out,
                             const LoadableTypeInfo &outTI) {
  // Unfortunately, we can't check this invariant until we get to IRGen, since
  // the AST and SIL don't know anything about type layout.
  if (inTI.getFixedSize() != outTI.getFixedSize()) {
    
    // We can hit this case in specialized functions even for correct user code.
    // If the user dynamically checks for correct type sizes in the generic
    // function, a specialized function can contain the (not executed) bitcast
    // with mismatching fixed sizes.
    // Usually llvm can eliminate this code again because the user's safety
    // check should be constant foldable on llvm level.
    llvm::BasicBlock *failBB =
        llvm::BasicBlock::Create(IGF.IGM.getLLVMContext());
    IGF.Builder.CreateBr(failBB);
    IGF.FailBBs.push_back(failBB);

    IGF.Builder.emitBlock(failBB);
    llvm::Function *trapIntrinsic = llvm::Intrinsic::getDeclaration(
        &IGF.IGM.Module, llvm::Intrinsic::ID::trap);
    IGF.Builder.CreateCall(trapIntrinsic);
    IGF.Builder.CreateUnreachable();

    llvm::BasicBlock *contBB = llvm::BasicBlock::Create(IGF.IGM.getLLVMContext());
    IGF.Builder.emitBlock(contBB);
    in.claimAll();
    for (auto schema : outTI.getSchema())
      out.add(llvm::UndefValue::get(schema.getScalarType()));
    return;
  }

  // TODO: We could do bitcasts entirely in the value domain in some cases, but
  // for simplicity, let's just always go through the stack for now.
  
  // Create the allocation.
  auto inStorage = IGF.createAlloca(inTI.getStorageType(),
                                  std::max(inTI.getFixedAlignment(),
                                           outTI.getFixedAlignment()),
                                  "bitcast");
  
  // Store the 'in' value.
  inTI.initialize(IGF, in, inStorage);
  // Load the 'out' value as the destination type.
  auto outStorage = IGF.Builder.CreateBitCast(inStorage,
                                        outTI.getStorageType()->getPointerTo());
  outTI.loadAsTake(IGF, outStorage, out);
  return;
}

void IRGenSILFunction::visitUncheckedTrivialBitCastInst(
                                      swift::UncheckedTrivialBitCastInst *i) {
  Explosion in = getLoweredExplosion(i->getOperand());
  Explosion out;
  
  emitValueBitCast(*this, i->getLoc().getSourceLoc(),
            in,  cast<LoadableTypeInfo>(getTypeInfo(i->getOperand().getType())),
            out, cast<LoadableTypeInfo>(getTypeInfo(i->getType())));
  
  setLoweredExplosion(SILValue(i, 0), out);
}

void IRGenSILFunction::visitUncheckedRefBitCastInst(
                                      swift::UncheckedRefBitCastInst *i) {
  Explosion in = getLoweredExplosion(i->getOperand());
  Explosion out;
  
  emitValueBitCast(*this, i->getLoc().getSourceLoc(),
            in,  cast<LoadableTypeInfo>(getTypeInfo(i->getOperand().getType())),
            out, cast<LoadableTypeInfo>(getTypeInfo(i->getType())));
  
  setLoweredExplosion(SILValue(i, 0), out);
}

void IRGenSILFunction::visitRefToRawPointerInst(
                                             swift::RefToRawPointerInst *i) {
  Explosion from = getLoweredExplosion(i->getOperand());
  llvm::Value *ptrValue = from.claimNext();
  // The input may have witness tables or other additional data, but the class
  // reference is always first.
  from.claimAll();
  
  ptrValue = Builder.CreateBitCast(ptrValue, IGM.Int8PtrTy);
  
  Explosion to;
  to.add(ptrValue);
  setLoweredExplosion(SILValue(i, 0), to);
}

void IRGenSILFunction::visitRawPointerToRefInst(swift::RawPointerToRefInst *i) {
  auto &ti = getTypeInfo(i->getType());
  emitPointerCastInst(*this, i->getOperand(), SILValue(i, 0), ti);
}

// SIL scalar conversions which never change the IR type.
// FIXME: Except for optionals, which get bit-packed into an integer.
static void trivialRefConversion(IRGenSILFunction &IGF,
                                 SILValue input,
                                 SILValue result) {
  Explosion temp = IGF.getLoweredExplosion(input);
  auto &inputTI = IGF.getTypeInfo(input.getType());
  auto &resultTI = IGF.getTypeInfo(result.getType());
  
  // If the types are the same, forward the existing value.
  if (inputTI.getStorageType() == resultTI.getStorageType()) {
    IGF.setLoweredExplosion(result, temp);
    return;
  }
  
  // Otherwise, do the conversion.
  llvm::Value *value = temp.claimNext();
  auto schema = resultTI.getSchema();
  assert(schema.size() == 1 && "not a single scalar type");
  auto resultTy = schema.begin()->getScalarType();
  if (resultTy->isPointerTy())
    value = IGF.Builder.CreateIntToPtr(value, resultTy);
  else
    value = IGF.Builder.CreatePtrToInt(value, resultTy);
  
  Explosion out;
  out.add(value);
  IGF.setLoweredExplosion(result, out);
}

// SIL scalar conversions which never change the IR type.
// FIXME: Except for optionals, which get bit-packed into an integer.
#define NOOP_CONVERSION(KIND)                                     \
void IRGenSILFunction::visit##KIND##Inst(swift::KIND##Inst *i) {  \
  ::trivialRefConversion(*this, i->getOperand(), SILValue(i, 0)); \
}
NOOP_CONVERSION(UnownedToRef)
NOOP_CONVERSION(RefToUnowned)
NOOP_CONVERSION(UnmanagedToRef)
NOOP_CONVERSION(RefToUnmanaged)
#undef NOOP_CONVERSION

void IRGenSILFunction::visitThinToThickFunctionInst(
                                            swift::ThinToThickFunctionInst *i) {
  // Take the incoming function pointer and add a null context pointer to it.
  Explosion from = getLoweredExplosion(i->getOperand());
  Explosion to;
  to.add(from.claimNext());
  to.add(IGM.RefCountedNull);
  setLoweredExplosion(SILValue(i, 0), to);
}

void IRGenSILFunction::visitThickToObjCMetatypeInst(ThickToObjCMetatypeInst *i){
  Explosion from = getLoweredExplosion(i->getOperand());
  llvm::Value *swiftMeta = from.claimNext();
  CanType instanceType(i->getType().castTo<AnyMetatypeType>().getInstanceType());
  Explosion to;
  llvm::Value *classPtr =
    emitClassHeapMetadataRefForMetatype(*this, swiftMeta, instanceType);
  to.add(Builder.CreateBitCast(classPtr, IGM.ObjCClassPtrTy));
  setLoweredExplosion(SILValue(i, 0), to);
}

void IRGenSILFunction::visitObjCToThickMetatypeInst(
                         ObjCToThickMetatypeInst *i) {
  Explosion from = getLoweredExplosion(i->getOperand());
  llvm::Value *classPtr = from.claimNext();
  
  // Fetch the metadata for that class.
  Explosion to;
  auto metadata = emitObjCMetadataRefForMetadata(*this, classPtr);
  to.add(metadata);
  setLoweredExplosion(SILValue(i, 0), to);  
}

/// Emit a checked cast sequence. Returns an Address; this may be either
/// a proper address or a class reference pointer, depending on the address-
/// or object-ness of the cast.
void emitValueCheckedCast(IRGenSILFunction &IGF,
                          SILValue operand,
                          SILType loweredTargetType,
                          CheckedCastMode mode,
                          Explosion &ex) {
  CanType sourceType = operand.getType().getSwiftRValueType();
  CanType targetType = loweredTargetType.getSwiftRValueType();

  if (auto sourceMetaType = dyn_cast<AnyMetatypeType>(sourceType)) {
    llvm::Value *metatypeVal = nullptr;
    auto fromEx = IGF.getLoweredExplosion(operand);
    if (sourceMetaType->getRepresentation() != MetatypeRepresentation::Thin)
      metatypeVal = fromEx.claimNext();
    // If the metatype is existential, there may be witness tables in the
    // value, which we don't need.
    // TODO: In existential-to-existential casts, we should carry over common
    // witness tables from the source to the destination.
    fromEx.claimAll();
    
    SmallVector<ProtocolDecl*, 1> protocols;
    
    if (auto existential = dyn_cast<ExistentialMetatypeType>(targetType))
      emitScalarExistentialDowncast(IGF, metatypeVal,
                                    operand.getType(), loweredTargetType,
                                    mode,
                                    existential->getRepresentation(),
                                    ex);
    else if (auto destMetaType = dyn_cast<MetatypeType>(targetType))
      emitMetatypeDowncast(IGF, metatypeVal, destMetaType, mode, ex);
    else if (targetType->isExistentialType(protocols)) {
      assert(IGF.IGM.ObjCInterop
             && protocols.size() == 1
             && *protocols[0]->getKnownProtocolKind()
                   == KnownProtocolKind::AnyObject
             && "metatypes can only be cast to AnyObject, with ObjC interop");
      emitMetatypeToObjectDowncast(IGF, metatypeVal, sourceMetaType, mode, ex);
    }
    return;
  }

  if ((isa<ArchetypeType>(sourceType) && !targetType.isExistentialType()) ||
      (isa<ArchetypeType>(targetType) && !sourceType.isExistentialType())) {
    Explosion archetype = IGF.getLoweredExplosion(operand);
    llvm::Value *fromValue = archetype.claimNext();
    llvm::Value *toValue =
      emitClassDowncast(IGF, fromValue, loweredTargetType, mode);
    ex.add(toValue);
    return;
  }

  if (sourceType.isExistentialType()) {
    Explosion existential = IGF.getLoweredExplosion(operand);
    llvm::Value *instance
      = emitClassExistentialProjection(IGF, existential,
                                       operand.getType(),
                                       CanArchetypeType());

    llvm::Value *toValue;
    if (loweredTargetType.isExistentialType()) {
      emitScalarExistentialDowncast(IGF, instance,
                                    operand.getType(),
                                    loweredTargetType, mode,
                                    None /*not a metatype*/,
                                    ex);
    } else {
      toValue = emitClassDowncast(IGF, instance, loweredTargetType, mode);
      ex.add(toValue);
    }
    
    return;
  }

  if (targetType.isExistentialType()) {
    Explosion from = IGF.getLoweredExplosion(operand);
    llvm::Value *fromValue = from.claimNext();
    emitScalarExistentialDowncast(IGF, fromValue, operand.getType(),
                                  loweredTargetType, mode,
                                  None /*not a metatype*/,
                                  ex);
    return;
  }

  Explosion from = IGF.getLoweredExplosion(operand);
  llvm::Value *fromValue = from.claimNext();
  llvm::Value *cast
    = emitClassDowncast(IGF, fromValue, loweredTargetType, mode);
  ex.add(cast);
}

void IRGenSILFunction::visitUnconditionalCheckedCastInst(
                                       swift::UnconditionalCheckedCastInst *i) {
  Explosion ex;
  emitValueCheckedCast(*this, i->getOperand(), i->getType(),
                  CheckedCastMode::Unconditional, ex);
  setLoweredExplosion(SILValue(i,0), ex);
}

void IRGenSILFunction::visitObjCMetatypeToObjectInst(
                                                  ObjCMetatypeToObjectInst *i){
  // Bitcast the @objc metatype reference, which is already an ObjC object, to
  // the destination type.
  Explosion from = getLoweredExplosion(i->getOperand());
  llvm::Value *value = from.claimNext();
  value = Builder.CreateBitCast(value, IGM.UnknownRefCountedPtrTy);
  Explosion to;
  to.add(value);
  setLoweredExplosion(SILValue(i,0), to);
}

void IRGenSILFunction::visitObjCExistentialMetatypeToObjectInst(
                                       ObjCExistentialMetatypeToObjectInst *i){
  // Bitcast the @objc metatype reference, which is already an ObjC object, to
  // the destination type. The metatype may carry additional witness tables we
  // can drop.
  Explosion from = getLoweredExplosion(i->getOperand());
  llvm::Value *value = from.claimNext();
  from.claimAll();
  value = Builder.CreateBitCast(value, IGM.UnknownRefCountedPtrTy);
  Explosion to;
  to.add(value);
  setLoweredExplosion(SILValue(i,0), to);
}
void IRGenSILFunction::visitObjCProtocolInst(ObjCProtocolInst *i) {
  // Get the protocol reference.
  llvm::Value *protoRef = emitReferenceToObjCProtocol(*this, i->getProtocol());
  // Bitcast it to the class reference type.
  protoRef = Builder.CreateBitCast(protoRef,
                                   getTypeInfo(i->getType()).getStorageType());
  Explosion ex;
  ex.add(protoRef);
  setLoweredExplosion(SILValue(i,0), ex);
}

void IRGenSILFunction::visitRefToBridgeObjectInst(
                                              swift::RefToBridgeObjectInst *i) {
  Explosion refEx = getLoweredExplosion(i->getConverted());
  llvm::Value *ref = refEx.claimNext();
  
  Explosion bitsEx = getLoweredExplosion(i->getBitsOperand());
  llvm::Value *bits = bitsEx.claimNext();
  
  // Mask the bits into the pointer representation.
  llvm::Value *val = Builder.CreatePtrToInt(ref, IGM.SizeTy);
  val = Builder.CreateOr(val, bits);
  val = Builder.CreateIntToPtr(val, IGM.BridgeObjectPtrTy);
  
  Explosion resultEx;
  resultEx.add(val);
  
  setLoweredExplosion(SILValue(i, 0), resultEx);
}

void IRGenSILFunction::visitBridgeObjectToRefInst(
                                              swift::BridgeObjectToRefInst *i) {
  Explosion boEx = getLoweredExplosion(i->getConverted());
  llvm::Value *bo = boEx.claimNext();
  Explosion resultEx;
  
  auto &refTI = getTypeInfo(i->getType());
  llvm::Type *refType = refTI.getSchema()[0].getScalarType();
  
  // If the value is an ObjC tagged pointer, pass it through verbatim.
  llvm::BasicBlock *taggedCont = nullptr,
    *tagged = nullptr,
    *notTagged = nullptr;
  llvm::Value *taggedRef = nullptr;
  llvm::Value *boBits = nullptr;
  if (IGM.TargetInfo.hasObjCTaggedPointers()) {
    boBits = Builder.CreatePtrToInt(bo, IGM.SizeTy);
    APInt maskValue = IGM.TargetInfo.ObjCPointerReservedBits.asAPInt();
    llvm::Value *mask = llvm::ConstantInt::get(IGM.getLLVMContext(), maskValue);
    llvm::Value *reserved = Builder.CreateAnd(boBits, mask);
    llvm::Value *cond = Builder.CreateICmpEQ(reserved,
                                         llvm::ConstantInt::get(IGM.SizeTy, 0));
    tagged = createBasicBlock("tagged-pointer"),
    notTagged = createBasicBlock("not-tagged-pointer");
    taggedCont = createBasicBlock("tagged-cont");
    
    Builder.CreateCondBr(cond, notTagged, tagged);
    
    Builder.emitBlock(tagged);
    taggedRef = Builder.CreateBitCast(bo, refType);
    Builder.CreateBr(taggedCont);
    
    // If it's not a tagged pointer, mask off the spare bits.
    Builder.emitBlock(notTagged);
  }
  
  // Mask off the spare bits (if they exist).
  auto &spareBits = IGM.getHeapObjectSpareBits();
  llvm::Value *result;
  if (spareBits.any()) {
    APInt maskValue = ~spareBits.asAPInt();
    
    if (!boBits)
      boBits = Builder.CreatePtrToInt(bo, IGM.SizeTy);
    
    llvm::Value *mask = llvm::ConstantInt::get(IGM.getLLVMContext(), maskValue);
    llvm::Value *masked = Builder.CreateAnd(boBits, mask);
    result = Builder.CreateIntToPtr(masked, refType);
  } else {
    result = Builder.CreateBitCast(bo, refType);
  }
  
  if (taggedCont) {
    Builder.CreateBr(taggedCont);
    
    Builder.emitBlock(taggedCont);
    
    auto phi = Builder.CreatePHI(refType, 2);
    phi->addIncoming(taggedRef, tagged);
    phi->addIncoming(result, notTagged);
    
    result = phi;
  }
  
  resultEx.add(result);
  setLoweredExplosion(SILValue(i,0), resultEx);
}

void IRGenSILFunction::visitBridgeObjectToWordInst(
                                             swift::BridgeObjectToWordInst *i) {
  Explosion boEx = getLoweredExplosion(i->getConverted());
  llvm::Value *val = boEx.claimNext();
  val = Builder.CreatePtrToInt(val, IGM.SizeTy);
  Explosion wordEx;
  wordEx.add(val);
  setLoweredExplosion(SILValue(i, 0), wordEx);
}

void IRGenSILFunction::visitUnconditionalCheckedCastAddrInst(
                                   swift::UnconditionalCheckedCastAddrInst *i) {
  Address dest = getLoweredAddress(i->getDest());
  Address src = getLoweredAddress(i->getSrc());
  emitCheckedCast(*this, src, i->getSourceType(), dest, i->getTargetType(),
                  i->getConsumptionKind(), CheckedCastMode::Unconditional);
}

void IRGenSILFunction::visitCheckedCastBranchInst(
                                              swift::CheckedCastBranchInst *i) {
  Explosion ex;
  SILType destTy = i->getCastType();
  if (i->isExact()) {
    auto operand = i->getOperand();
    Explosion source = getLoweredExplosion(operand);
    llvm::Value *result =
      emitClassIdenticalCast(*this, source.claimNext(), operand.getType(),
                             destTy, CheckedCastMode::Conditional);
    ex.add(result);
  } else {
    emitValueCheckedCast(*this, i->getOperand(), i->getCastType(),
                         CheckedCastMode::Conditional, ex);
  }
  
  // Branch on the success of the cast.
  // All cast operations currently return null on failure.
  auto val = ex.claimNext();
  llvm::Value *isNonnull = Builder.CreateICmpNE(val,
     llvm::ConstantPointerNull::get(cast<llvm::PointerType>(val->getType())));

  auto &successBB = getLoweredBB(i->getSuccessBB());
  llvm::Type *toTy = IGM.getTypeInfo(destTy).StorageType;
  if (toTy->isPointerTy())
    val = Builder.CreateBitCast(val, toTy);

  Builder.CreateCondBr(isNonnull,
                       successBB.bb,
                       getLoweredBB(i->getFailureBB()).bb);
  
  // Feed the cast result into the nonnull branch.
  unsigned phiIndex = 0;
  Explosion ex2;
  ex2.add(val);
  ex2.add(ex.claimAll());
  addIncomingExplosionToPHINodes(*this, successBB, phiIndex, ex2);
}

void IRGenSILFunction::visitCheckedCastAddrBranchInst(
                                          swift::CheckedCastAddrBranchInst *i) {
  Address dest = getLoweredAddress(i->getDest());
  Address src = getLoweredAddress(i->getSrc());
  llvm::Value *castSucceeded =
    emitCheckedCast(*this, src, i->getSourceType(), dest, i->getTargetType(),
                    i->getConsumptionKind(), CheckedCastMode::Conditional);
  Builder.CreateCondBr(castSucceeded,
                       getLoweredBB(i->getSuccessBB()).bb,
                       getLoweredBB(i->getFailureBB()).bb);
}

void IRGenSILFunction::visitIsNonnullInst(swift::IsNonnullInst *i) {
  // Get the value we're testing, which may be a function, an address or an
  // instance pointer.
  llvm::Value *val;
  const LoweredValue &lv = getLoweredValue(i->getOperand());
  
  if (i->getOperand().getType().getSwiftType()->is<SILFunctionType>()) {
    Explosion values = lv.getExplosion(*this);
    val = values.claimNext();   // Function pointer.
    values.claimNext();         // Ignore the data pointer.
  } else if (lv.isAddress()) {
    val = lv.getAddress().getAddress();
  } else {
    Explosion values = lv.getExplosion(*this);
    val = values.claimNext();
  }
  
  // Check that the result isn't null.
  auto *valTy = cast<llvm::PointerType>(val->getType());
  llvm::Value *result = Builder.CreateICmp(llvm::CmpInst::ICMP_NE,
                                    val, llvm::ConstantPointerNull::get(valTy));
  
  Explosion out;
  out.add(result);
  setLoweredExplosion(SILValue(i, 0), out);
}

void IRGenSILFunction::visitUpcastInst(swift::UpcastInst *i) {
  auto toTy = getTypeInfo(i->getType()).getStorageType();

  // If we have an address, just bitcast, don't explode.
  if (i->getOperand().getType().isAddress()) {
    Address fromAddr = getLoweredAddress(i->getOperand());
    llvm::Value *toValue = Builder.CreateBitCast(
      fromAddr.getAddress(), toTy->getPointerTo());
    Address Addr(toValue, fromAddr.getAlignment());
    setLoweredAddress(SILValue(i, 0), Addr);
    return;
  }

  Explosion from = getLoweredExplosion(i->getOperand());
  Explosion to;
  assert(from.size() == 1 && "class should explode to single value");
  llvm::Value *fromValue = from.claimNext();
  to.add(Builder.CreateBitCast(fromValue, toTy));
  setLoweredExplosion(SILValue(i, 0), to);
}

void IRGenSILFunction::visitIndexAddrInst(swift::IndexAddrInst *i) {
  Address base = getLoweredAddress(i->getBase());
  Explosion indexValues = getLoweredExplosion(i->getIndex());
  llvm::Value *index = indexValues.claimNext();
  
  auto baseTy = i->getBase().getType();
  auto &ti = getTypeInfo(baseTy);
  
  Address dest = ti.indexArray(*this, base, index, baseTy);
  setLoweredAddress(SILValue(i, 0), dest);
}

void IRGenSILFunction::visitIndexRawPointerInst(swift::IndexRawPointerInst *i) {
  Explosion baseValues = getLoweredExplosion(i->getBase());
  llvm::Value *base = baseValues.claimNext();
  
  Explosion indexValues = getLoweredExplosion(i->getIndex());
  llvm::Value *index = indexValues.claimNext();
  
  // We don't expose a non-inbounds GEP operation.
  llvm::Value *destValue = Builder.CreateInBoundsGEP(base, index);
  
  Explosion result;
  result.add(destValue);
  setLoweredExplosion(SILValue(i, 0), result);
}

void IRGenSILFunction::visitAllocValueBufferInst(
                                          swift::AllocValueBufferInst *i) {
  Address buffer = getLoweredAddress(i->getOperand());
  Address value = emitAllocateBuffer(*this, i->getValueType(), buffer);
  setLoweredAddress(SILValue(i, 0), value);
}

void IRGenSILFunction::visitProjectValueBufferInst(
                                          swift::ProjectValueBufferInst *i) {
  Address buffer = getLoweredAddress(i->getOperand());
  Address value = emitProjectBuffer(*this, i->getValueType(), buffer);
  setLoweredAddress(SILValue(i, 0), value);
}

void IRGenSILFunction::visitDeallocValueBufferInst(
                                          swift::DeallocValueBufferInst *i) {
  Address buffer = getLoweredAddress(i->getOperand());
  emitDeallocateBuffer(*this, i->getValueType(), buffer);
}

void IRGenSILFunction::visitInitExistentialAddrInst(swift::InitExistentialAddrInst *i) {
  Address container = getLoweredAddress(i->getOperand());
  SILType destType = i->getOperand().getType();
  Address buffer = emitOpaqueExistentialContainerInit(*this,
                                                container,
                                                destType,
                                                i->getFormalConcreteType(),
                                                i->getLoweredConcreteType(),
                                                i->getConformances());
  setLoweredAddress(SILValue(i, 0), buffer);
}

void IRGenSILFunction::visitInitExistentialMetatypeInst(
                                              InitExistentialMetatypeInst *i) {
  Explosion metatype = getLoweredExplosion(i->getOperand());
  Explosion result;
  emitExistentialMetatypeContainer(*this,
                                   result, i->getType(),
                                   metatype.claimNext(),
                                   i->getOperand().getType(),
                                   i->getConformances());
  setLoweredExplosion(SILValue(i, 0), result);
}

void IRGenSILFunction::visitInitExistentialRefInst(InitExistentialRefInst *i) {
  Explosion instance = getLoweredExplosion(i->getOperand());
  Explosion result;
  emitClassExistentialContainer(*this,
                               result, i->getType(),
                               instance.claimNext(),
                               i->getFormalConcreteType(),
                               i->getOperand().getType(),
                               i->getConformances());
  setLoweredExplosion(SILValue(i, 0), result);
}

void IRGenSILFunction::visitDeinitExistentialAddrInst(
                                              swift::DeinitExistentialAddrInst *i) {
  Address container = getLoweredAddress(i->getOperand());
  emitOpaqueExistentialContainerDeinit(*this, container,
                                       i->getOperand().getType());
}

void IRGenSILFunction::visitOpenExistentialAddrInst(OpenExistentialAddrInst *i) {
  SILType baseTy = i->getOperand().getType();
  Address base = getLoweredAddress(i->getOperand());

  auto openedArchetype = cast<ArchetypeType>(
                           i->getType().getSwiftRValueType());
  Address object = emitOpaqueExistentialProjection(*this, base, baseTy,
                                                   openedArchetype);

  setLoweredAddress(SILValue(i, 0), object);
}

void IRGenSILFunction::visitOpenExistentialRefInst(OpenExistentialRefInst *i) {

  SILType baseTy = i->getOperand().getType();
  Explosion base = getLoweredExplosion(i->getOperand());
  auto openedArchetype = cast<ArchetypeType>(
                           i->getType().getSwiftRValueType());

  Explosion result;
  llvm::Value *instance
    = emitClassExistentialProjection(*this, base, baseTy,
                                     openedArchetype);
  result.add(instance);
  setLoweredExplosion(SILValue(i, 0), result);
}

void IRGenSILFunction::visitOpenExistentialMetatypeInst(
                                              OpenExistentialMetatypeInst *i) {
  SILType baseTy = i->getOperand().getType();
  Explosion base = getLoweredExplosion(i->getOperand());
  auto openedTy = i->getType().getSwiftRValueType();

  llvm::Value *metatype =
    emitExistentialMetatypeProjection(*this, base, baseTy, openedTy);
  Explosion result;
  result.add(metatype);
  setLoweredExplosion(SILValue(i, 0), result);
}

void IRGenSILFunction::visitProjectBlockStorageInst(ProjectBlockStorageInst *i){
  // TODO
  Address block = getLoweredAddress(i->getOperand());
  Address capture = projectBlockStorageCapture(*this, block,
                       i->getOperand().getType().castTo<SILBlockStorageType>());
  
  setLoweredAddress(SILValue(i, 0), capture);
}

void IRGenSILFunction::visitInitBlockStorageHeaderInst(
                                               InitBlockStorageHeaderInst *i) {
  auto addr = getLoweredAddress(i->getBlockStorage());
  
  // We currently only support static invoke functions.
  auto &invokeVal = getLoweredValue(i->getInvokeFunction());
  llvm::Function *invokeFn = nullptr;
  if (invokeVal.kind != LoweredValue::Kind::StaticFunction) {
    IGM.unimplemented(i->getLoc().getSourceLoc(),
                      "non-static block invoke function");
  } else {
    invokeFn = invokeVal.getStaticFunction().getFunction();
  }
  
  // Initialize the header.
  emitBlockHeader(*this, addr,
          i->getBlockStorage().getType().castTo<SILBlockStorageType>(),
          invokeFn, i->getInvokeFunction().getType().castTo<SILFunctionType>());
  
  // Cast the storage to the block type to produce the result value.
  llvm::Value *asBlock = Builder.CreateBitCast(addr.getAddress(),
                                               IGM.ObjCBlockPtrTy);
  Explosion e;
  e.add(asBlock);
  setLoweredExplosion(SILValue(i, 0), e);
}

void IRGenSILFunction::visitAllocExistentialBoxInst(AllocExistentialBoxInst *i){
  Explosion box;
  auto projectionAddr =
    emitBoxedExistentialContainerAllocation(*this, box, i->getExistentialType(),
                                            i->getFormalConcreteType(),
                                            i->getLoweredConcreteType(),
                                            i->getConformances());
  setLoweredExplosion(i->getExistentialResult(), box);
  setLoweredAddress(i->getValueAddressResult(), projectionAddr);
}

void IRGenSILFunction::visitDeallocExistentialBoxInst(
                                                DeallocExistentialBoxInst *i) {
  Explosion box = getLoweredExplosion(i->getOperand());
  emitBoxedExistentialContainerDeallocation(*this, box,
                                            i->getOperand().getType(),
                                            i->getConcreteType());
}

void IRGenSILFunction::visitOpenExistentialBoxInst(OpenExistentialBoxInst *i) {
  Explosion box = getLoweredExplosion(i->getOperand());
  auto openedArchetype = cast<ArchetypeType>(i->getType().getSwiftRValueType());

  auto addr = emitBoxedExistentialProjection(*this, box,
                                             i->getOperand().getType(),
                                             openedArchetype);
  setLoweredAddress(SILValue(i,0), addr);
}

void IRGenSILFunction::visitDynamicMethodInst(DynamicMethodInst *i) {
  assert(i->getMember().isForeign && "dynamic_method requires [objc] method");
  setLoweredObjCMethod(SILValue(i, 0), i->getMember());
  return;
}

void IRGenSILFunction::visitWitnessMethodInst(swift::WitnessMethodInst *i) {
  // For Objective-C classes we need to arrange for a msgSend
  // to happen when the method is called.
  if (i->getMember().isForeign) {
    setLoweredObjCMethod(SILValue(i, 0), i->getMember());
    return;
  }

  CanType baseTy = i->getLookupType();
  ProtocolConformance *conformance = i->getConformance();
  SILDeclRef member = i->getMember();

  Explosion lowered;
  emitWitnessMethodValue(*this, baseTy, member, conformance, lowered);
  
  setLoweredExplosion(SILValue(i, 0), lowered);
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
    addrTI.assignWithCopy(*this, dest, src, addrTy);
    break;
  case INITIALIZE | COPY:
    addrTI.initializeWithCopy(*this, dest, src, addrTy);
    break;
  case ASSIGN | TAKE:
    addrTI.assignWithTake(*this, dest, src, addrTy);
    break;
  case INITIALIZE | TAKE:
    addrTI.initializeWithTake(*this, dest, src, addrTy);
    break;
  default:
    llvm_unreachable("unexpected take/initialize attribute combination?!");
  }
}

void IRGenSILFunction::visitDestroyAddrInst(swift::DestroyAddrInst *i) {
  SILType addrTy = i->getOperand().getType();
  Address base = getLoweredAddress(i->getOperand());
  const TypeInfo &addrTI = getTypeInfo(addrTy);
  addrTI.destroy(*this, base, addrTy);
}

void IRGenSILFunction::visitCondFailInst(swift::CondFailInst *i) {
  Explosion e = getLoweredExplosion(i->getOperand());
  llvm::Value *cond = e.claimNext();

  // Emit individual fail blocks so that we can map the failure back to a source
  // line.
  llvm::BasicBlock *failBB = llvm::BasicBlock::Create(IGM.getLLVMContext());
  llvm::BasicBlock *contBB = llvm::BasicBlock::Create(IGM.getLLVMContext());
  Builder.CreateCondBr(cond, failBB, contBB);
  Builder.emitBlock(failBB);
  llvm::Function *trapIntrinsic =
      llvm::Intrinsic::getDeclaration(&IGM.Module, llvm::Intrinsic::ID::trap);
  Builder.CreateCall(trapIntrinsic);
  Builder.CreateUnreachable();
  Builder.emitBlock(contBB);
  FailBBs.push_back(failBB);
}

void IRGenSILFunction::visitSuperMethodInst(swift::SuperMethodInst *i) {
  assert(i->getMember().isForeign && "super_method to non_objc callee");
  setLoweredObjCMethodBounded(SILValue(i, 0), i->getMember(),
                              i->getOperand().getType(),
                              /*startAtSuper=*/true);
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
                                                method, methodType);
  fnValue = Builder.CreateBitCast(fnValue, IGM.Int8PtrTy);
  Explosion e;
  e.add(fnValue);
  setLoweredExplosion(SILValue(i, 0), e);
}

/// Generate ConstantStruct for StructInst.
static llvm::Constant *getConstantValue(IRGenModule &IGM, llvm::StructType *STy,
                                        StructInst *SI) {
  SmallVector<llvm::Constant*, 32> Elts;
  assert(SI->getNumOperands() == STy->getNumElements() &&
         "mismatch StructInst with its lowered StructType!");
  for (unsigned i = 0, e = STy->getNumElements(); i != e; ++i) {
    if (auto *Elem = dyn_cast<StructInst>(SI->getOperand(i)))
      Elts.push_back(getConstantValue(IGM,
                     cast<llvm::StructType>(STy->getElementType(i)), Elem));
    else if (auto *ILI = dyn_cast<IntegerLiteralInst>(SI->getOperand(i)))
      Elts.push_back(getConstantInt(IGM, ILI));
    else if (auto *FLI = dyn_cast<FloatLiteralInst>(SI->getOperand(i)))
      Elts.push_back(getConstantFP(IGM, FLI));
    else if (auto *SLI = dyn_cast<StringLiteralInst>(SI->getOperand(i)))
      Elts.push_back(getAddrOfString(IGM, SLI->getValue(), SLI->getEncoding()));
    else
      llvm_unreachable("Unexpected SILInstruction in static initializer!");
  }
  return llvm::ConstantStruct::get(STy, Elts);
}


void IRGenModule::emitSILStaticInitializer() {
  SmallVector<SILFunction*, 8> StaticInitializers;
  for (SILGlobalVariable &v : SILMod->getSILGlobals()) {
    auto *staticInit = v.getInitializer();
    if (!staticInit)
      continue;

    auto *gvar = Module.getGlobalVariable(v.getName(),
                                          /*allowInternal*/true);

    // A check for multi-threaded compilation: Is this the llvm module where the
    // global is defined and not only referenced (or not referenced at all).
    if (!gvar || !gvar->hasInitializer())
      continue;

    auto *STy = dyn_cast<llvm::StructType>(gvar->getInitializer()->getType());
    assert(STy && "We only handle StructType for now!");

    // Get the StructInst that we write to the SILGlobalVariable.
    auto *SI = cast<StructInst>(v.getValueOfStaticInitializer());
    gvar->setInitializer(getConstantValue(*this, STy, SI));
  }
}
