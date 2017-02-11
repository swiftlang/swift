//===--- IRGenSIL.cpp - Swift Per-Function IR Generation ------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements basic setup and teardown for the class which
//  performs IR generation for function bodies.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "irgensil"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/SmallBitVector.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/Support/Debug.h"
#include "clang/AST/ASTContext.h"
#include "clang/Basic/TargetInfo.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/Basic/Range.h"
#include "swift/Basic/STLExtras.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Types.h"
#include "swift/SIL/Dominance.h"
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
#include "GenArchetype.h"
#include "GenBuiltin.h"
#include "GenCall.h"
#include "GenCast.h"
#include "GenClass.h"
#include "GenConstant.h"
#include "GenEnum.h"
#include "GenExistential.h"
#include "GenFunc.h"
#include "GenHeap.h"
#include "GenMeta.h"
#include "GenObjC.h"
#include "GenOpaque.h"
#include "GenPoly.h"
#include "GenProto.h"
#include "GenStruct.h"
#include "GenTuple.h"
#include "GenType.h"
#include "IRGenDebugInfo.h"
#include "IRGenModule.h"
#include "ReferenceTypeInfo.h"
#include "WeakTypeInfo.h"

using namespace swift;
using namespace irgen;

namespace {

class LoweredValue;
  
/// Represents a statically-known function as a SIL thin function value.
class StaticFunction {
  /// The function reference.
  llvm::Function *Function;

  ForeignFunctionInfo ForeignInfo;

  /// The function's native representation.
  SILFunctionTypeRepresentation Rep;
  
public:
  StaticFunction(llvm::Function *function, ForeignFunctionInfo foreignInfo,
                 SILFunctionTypeRepresentation rep)
    : Function(function), ForeignInfo(foreignInfo), Rep(rep)
  {}
  
  llvm::Function *getFunction() const { return Function; }
  SILFunctionTypeRepresentation getRepresentation() const { return Rep; }
  const ForeignFunctionInfo &getForeignInfo() const { return ForeignInfo; }
  
  llvm::Value *getExplosionValue(IRGenFunction &IGF) const;
};
  
/// Represents a SIL value lowered to IR, in one of these forms:
/// - an Address, corresponding to a SIL address value;
/// - an Explosion of (unmanaged) Values, corresponding to a SIL "register"; or
/// - a CallEmission for a partially-applied curried function or method.
class LoweredValue {
public:
  enum class Kind {
    /// The first two LoweredValue kinds correspond to a SIL address value.
    ///
    /// The LoweredValue of an existential alloc_stack keeps an owning container
    /// in addition to the address of the allocated buffer.
    /// Depending on the allocated type, the container may be equal to the
    /// buffer itself (for types with known sizes) or it may be the address
    /// of a fixed-size container which points to the heap-allocated buffer.
    /// In this case the address-part may be null, which means that the buffer
    /// is not allocated yet.
    ContainedAddress,

    /// The LoweredValue of a resilient, generic, or loadable typed alloc_stack
    /// keeps an optional stackrestore point in addition to the address of the
    /// allocated buffer. For all other address values the stackrestore point is
    /// just null.
    /// If the stackrestore point is set (currently, this might happen for
    /// opaque types: generic and resilient) the deallocation of the stack must
    /// reset the stack pointer to this point.
    Address,

    /// The following kinds correspond to SIL non-address values.
    Value_First,
      /// A normal value, represented as an exploded array of llvm Values.
      Explosion = Value_First,

      /// A @box together with the address of the box value.
      BoxWithAddress,

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
    ContainedAddress containedAddress;
    StackAddress address;
    OwnedAddress boxWithAddress;
    struct {
      ExplosionVector values;
    } explosion;
    StaticFunction staticFunction;
    ObjCMethod objcMethod;
  };

public:

  /// Create an address value without a stack restore point.
  LoweredValue(const Address &address)
    : kind(Kind::Address), address(address)
  {}

  /// Create an address value with an optional stack restore point.
  LoweredValue(const StackAddress &address)
    : kind(Kind::Address), address(address)
  {}
  
  enum ContainerForUnallocatedAddress_t { ContainerForUnallocatedAddress };

  /// Create an address value for an alloc_stack, consisting of a container and
  /// a not yet allocated buffer.
  LoweredValue(const Address &container, ContainerForUnallocatedAddress_t)
    : kind(Kind::ContainedAddress), containedAddress(container, Address())
  {}
  
  /// Create an address value for an alloc_stack, consisting of a container and
  /// the address of the allocated buffer.
  LoweredValue(const ContainedAddress &address)
    : kind(Kind::ContainedAddress), containedAddress(address)
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

  LoweredValue(const OwnedAddress &boxWithAddress)
  : kind(Kind::BoxWithAddress), boxWithAddress(boxWithAddress)
  {}

  LoweredValue(LoweredValue &&lv)
    : kind(lv.kind)
  {    
    switch (kind) {
    case Kind::ContainedAddress:
      ::new (&containedAddress) ContainedAddress(std::move(lv.containedAddress));
      break;
    case Kind::Address:
      ::new (&address) StackAddress(std::move(lv.address));
      break;
    case Kind::Explosion:
      ::new (&explosion.values) ExplosionVector(std::move(lv.explosion.values));
      break;
    case Kind::BoxWithAddress:
      ::new (&boxWithAddress) OwnedAddress(std::move(lv.boxWithAddress));
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
  
  bool isAddress() const {
    return kind == Kind::Address && address.getAddress().isValid();
  }
  bool isUnallocatedAddressInBuffer() const {
    return kind == Kind::ContainedAddress &&
           !containedAddress.getAddress().isValid();
  }
  bool isValue() const {
    return kind >= Kind::Value_First && kind <= Kind::Value_Last;
  }
  bool isBoxWithAddress() const {
    return kind == Kind::BoxWithAddress;
  }
  
  Address getAddress() const {
    assert(isAddress() && "not an allocated address");
    return address.getAddress();
  }

  StackAddress getStackAddress() const {
    assert(isAddress() && "not an allocated address");
    return address;
  }
  
  Address getContainerOfAddress() const {
    assert(kind == Kind::ContainedAddress);
    assert(containedAddress.getContainer().isValid() && "address has no container");
    return containedAddress.getContainer();
  }

  Address getAddressInContainer() const {
    assert(kind == Kind::ContainedAddress);
    assert(containedAddress.getContainer().isValid() &&
           "address has no container");
    return containedAddress.getAddress();
  }
  
  void getExplosion(IRGenFunction &IGF, Explosion &ex) const;
  
  Explosion getExplosion(IRGenFunction &IGF) const {
    Explosion e;
    getExplosion(IGF, e);
    return e;
  }

  Address getAddressOfBox() const {
    assert(kind == Kind::BoxWithAddress);
    return boxWithAddress.getAddress();
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
      address.~StackAddress();
      break;
    case Kind::ContainedAddress:
      containedAddress.~ContainedAddress();
      break;
    case Kind::Explosion:
      explosion.values.~ExplosionVector();
      break;
    case Kind::BoxWithAddress:
      boxWithAddress.~OwnedAddress();
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

using PHINodeVector = llvm::TinyPtrVector<llvm::PHINode*>;
  
/// Represents a lowered SIL basic block. This keeps track
/// of SIL branch arguments so that they can be lowered to LLVM phi nodes.
struct LoweredBB {
  llvm::BasicBlock *bb;
  PHINodeVector phis;
  
  LoweredBB() = default;
  explicit LoweredBB(llvm::BasicBlock *bb, PHINodeVector &&phis)
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

  /// All alloc_ref instructions which allocate the object on the stack.
  llvm::SmallPtrSet<SILInstruction *, 8> StackAllocs;

  /// With closure captures it is actually possible to have two function
  /// arguments that both have the same name. Until this is fixed, we need to
  /// also hash the ArgNo here.
  typedef std::pair<unsigned, std::pair<const SILDebugScope *, StringRef>>
      StackSlotKey;
  /// Keeps track of the mapping of source variables to -O0 shadow copy allocas.
  llvm::SmallDenseMap<StackSlotKey, Address, 8> ShadowStackSlots;
  llvm::SmallDenseMap<Decl *, SmallString<4>, 8> AnonymousVariables;
  llvm::SmallDenseMap<llvm::Instruction *, DominancePoint, 8> ValueVariables;
  unsigned NumAnonVars = 0;
  unsigned NumCondFails = 0;

  /// Accumulative amount of allocated bytes on the stack. Used to limit the
  /// size for stack promoted objects.
  /// We calculate it on demand, so that we don't have to do it if the
  /// function does not have any stack promoted allocations.
  int EstimatedStackSize = -1;

  llvm::MapVector<SILBasicBlock *, LoweredBB> LoweredBBs;
  
  // Destination basic blocks for condfail traps.
  llvm::SmallVector<llvm::BasicBlock *, 8> FailBBs;

  SILFunction *CurSILFn;
  Address IndirectReturn;

  // A cached dominance analysis.
  std::unique_ptr<DominanceInfo> Dominance;
  
  IRGenSILFunction(IRGenModule &IGM, SILFunction *f);
  ~IRGenSILFunction();
  
  /// Generate IR for the SIL Function.
  void emitSILFunction();

  /// Calculates EstimatedStackSize.
  void estimateStackSize();

  void setLoweredValue(SILValue v, LoweredValue &&lv) {
    auto inserted = LoweredValues.insert({v, std::move(lv)});
    assert(inserted.second && "already had lowered value for sil value?!");
    (void)inserted;
  }
  
  /// Create a new Address corresponding to the given SIL address value.
  void setLoweredAddress(SILValue v, const Address &address) {
    assert(v->getType().isAddress() && "address for non-address value?!");
    setLoweredValue(v, address);
  }

  void setLoweredStackAddress(SILValue v, const StackAddress &address) {
    assert(v->getType().isAddress() && "address for non-address value?!");
    setLoweredValue(v, address);
  }
  
  void setContainerOfUnallocatedAddress(SILValue v,
                                        const Address &buffer) {
    assert(v->getType().isAddress() && "address for non-address value?!");
    setLoweredValue(v,
      LoweredValue(buffer, LoweredValue::ContainerForUnallocatedAddress));
  }
  
  void overwriteAllocatedAddress(SILValue v, const Address &address) {
    assert(v->getType().isAddress() && "address for non-address value?!");
    auto it = LoweredValues.find(v);
    assert(it != LoweredValues.end() && "no existing entry for overwrite?");
    assert(it->second.isUnallocatedAddressInBuffer() &&
           "not an unallocated address");
    it->second = ContainedAddress(it->second.getContainerOfAddress(), address);
  }

  void setAllocatedAddressForBuffer(SILValue v, const Address &allocedAddress);

  /// Create a new Explosion corresponding to the given SIL value.
  void setLoweredExplosion(SILValue v, Explosion &e) {
    assert(v->getType().isObject() && "explosion for address value?!");
    setLoweredValue(v, LoweredValue(e));
  }

  void setLoweredBox(SILValue v, const OwnedAddress &box) {
    assert(v->getType().isObject() && "box for address value?!");
    setLoweredValue(v, LoweredValue(box));
  }

  /// Create a new StaticFunction corresponding to the given SIL value.
  void setLoweredStaticFunction(SILValue v,
                                llvm::Function *f,
                                SILFunctionTypeRepresentation rep,
                                ForeignFunctionInfo foreignInfo) {
    assert(v->getType().isObject() && "function for address value?!");
    assert(v->getType().is<SILFunctionType>() &&
           "function for non-function value?!");
    setLoweredValue(v, StaticFunction{f, foreignInfo, rep});
  }

  /// Create a new Objective-C method corresponding to the given SIL value.
  void setLoweredObjCMethod(SILValue v, SILDeclRef method) {
    assert(v->getType().isObject() && "function for address value?!");
    assert(v->getType().is<SILFunctionType>() &&
           "function for non-function value?!");
    setLoweredValue(v, ObjCMethod{method, SILType(), false});
  }

  /// Create a new Objective-C method corresponding to the given SIL value that
  /// starts its search from the given search type.
  ///
  /// Unlike \c setLoweredObjCMethod, which finds the method in the actual
  /// runtime type of the object, this routine starts at the static type of the
  /// object and searches up the class hierarchy (toward superclasses).
  ///
  /// \param searchType The class from which the Objective-C runtime will start
  /// its search for a method.
  ///
  /// \param startAtSuper Whether we want to start at the superclass of the
  /// static type (vs. the static type itself).
  void setLoweredObjCMethodBounded(SILValue v, SILDeclRef method,
                                   SILType searchType, bool startAtSuper) {
    assert(v->getType().isObject() && "function for address value?!");
    assert(v->getType().is<SILFunctionType>() &&
           "function for non-function value?!");
    setLoweredValue(v, ObjCMethod{method, searchType, startAtSuper});
  }

  LoweredValue &getUndefLoweredValue(SILType t) {
    auto found = LoweredUndefs.find(t);
    if (found != LoweredUndefs.end())
      return found->second;
    
    auto &ti = getTypeInfo(t);
    switch (t.getCategory()) {
    case SILValueCategory::Address: {
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
      return getUndefLoweredValue(v->getType());
    
    auto foundValue = LoweredValues.find(v);
    assert(foundValue != LoweredValues.end() &&
           "no lowered explosion for sil value!");
    return foundValue->second;
  }
  
  /// Get the Address of a SIL value of address type, which must have been
  /// lowered.
  Address getLoweredAddress(SILValue v) {
    if (getLoweredValue(v).kind == LoweredValue::Kind::Address)
      return getLoweredValue(v).getAddress();
    else
      return getLoweredValue(v).getAddressInContainer();
  }

  StackAddress getLoweredStackAddress(SILValue v) {
    return getLoweredValue(v).getStackAddress();
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

  StringRef getOrCreateAnonymousVarName(VarDecl *Decl) {
    llvm::SmallString<4> &Name = AnonymousVariables[Decl];
    if (Name.empty()) {
      {
        llvm::raw_svector_ostream S(Name);
        S << '_' << NumAnonVars++;
      }
      AnonymousVariables.insert({Decl, Name});
    }
    return Name;
  }

  template <class DebugVarCarryingInst>
  StringRef getVarName(DebugVarCarryingInst *i) {
    StringRef Name = i->getVarInfo().Name;
    // The $match variables generated by the type checker are not
    // guaranteed to be unique within their scope, but they have
    // unique VarDecls.
    if ((Name.empty() || Name == "$match") && i->getDecl())
      return getOrCreateAnonymousVarName(i->getDecl());
    return Name;
  }

  /// At -Onone, forcibly keep all LLVM values that are tracked by
  /// debug variables alive by inserting an empty inline assembler
  /// expression depending on the value in the blocks dominated by the
  /// value.
  void emitDebugVariableRangeExtension(const SILBasicBlock *CurBB) {
    if (IGM.IRGen.Opts.Optimize)
      return;
    for (auto &Variable : ValueVariables) {
      auto VarDominancePoint = Variable.second;
      llvm::Value *Storage = Variable.first;
      if (getActiveDominancePoint() == VarDominancePoint ||
          isActiveDominancePointDominatedBy(VarDominancePoint)) {
        llvm::Type *ArgTys;
        auto *Ty = Storage->getType();
        // Vectors, Pointers and Floats are expected to fit into a register.
        if (Ty->isPointerTy() || Ty->isFloatingPointTy() || Ty->isVectorTy())
          ArgTys = { Ty };
        else {
          // If this is not a scalar or vector type, we can't handle it.
          if (isa<llvm::CompositeType>(Ty))
            continue;
          // The storage is guaranteed to be no larger than the register width.
          // Extend the storage so it would fit into a register.
          llvm::Type *IntTy;
          switch (IGM.getClangASTContext().getTargetInfo().getRegisterWidth()) {
          case 64: IntTy = IGM.Int64Ty; break;
          case 32: IntTy = IGM.Int32Ty; break;
          default: llvm_unreachable("unsupported register width");
          }
          ArgTys = { IntTy };
          Storage = Builder.CreateZExtOrBitCast(Storage, IntTy);
        }
        // Emit an empty inline assembler expression depending on the register.
        auto *AsmFnTy = llvm::FunctionType::get(IGM.VoidTy, ArgTys, false);
        auto *InlineAsm = llvm::InlineAsm::get(AsmFnTy, "", "r", true);
        Builder.CreateCall(InlineAsm, Storage);
        // Propagate the dbg.value intrinsics into the later basic blocks.  Note
        // that this shouldn't be necessary. LiveDebugValues should be doing
        // this but can't in general because it currently only tracks register
        // locations.
        llvm::Instruction *Value = Variable.first;
        auto It = llvm::BasicBlock::iterator(Value);
        auto *BB = Value->getParent();
        auto *CurBB = Builder.GetInsertBlock();
        if (BB != CurBB)
          for (auto I = std::next(It), E = BB->end(); I != E; ++I) {
            auto *DVI = dyn_cast<llvm::DbgValueInst>(I);
            if (DVI && DVI->getValue() == Value)
              IGM.DebugInfo->getBuilder().insertDbgValueIntrinsic(
                  DVI->getValue(), 0, DVI->getVariable(), DVI->getExpression(),
                  DVI->getDebugLoc(), &*CurBB->getFirstInsertionPt());
            else
              // Found all dbg.value intrinsics describing this location.
              break;
        }
      }
    }
  }

  /// Account for bugs in LLVM.
  ///
  /// - The LLVM type legalizer currently doesn't update debug
  ///   intrinsics when a large value is split up into smaller
  ///   pieces. Note that this heuristic as a bit too conservative
  ///   on 32-bit targets as it will also fire for doubles.
  ///
  /// - CodeGen Prepare may drop dbg.values pointing to PHI instruction.
  bool needsShadowCopy(llvm::Value *Storage) {
    return (IGM.DataLayout.getTypeSizeInBits(Storage->getType()) >
            IGM.getClangASTContext().getTargetInfo().getRegisterWidth()) ||
           isa<llvm::PHINode>(Storage);
  }

  /// At -Onone, emit a shadow copy of an Address in an alloca, so the
  /// register allocator doesn't elide the dbg.value intrinsic when
  /// register pressure is high.  There is a trade-off to this: With
  /// shadow copies, we lose the precise lifetime.
  llvm::Value *emitShadowCopy(llvm::Value *Storage,
                              const SILDebugScope *Scope,
                              StringRef Name, unsigned ArgNo,
                              Alignment Align = Alignment(0)) {
    auto Ty = Storage->getType();
    // Never emit shadow copies when optimizing, or if already on the stack.
    if (IGM.IRGen.Opts.Optimize ||
        isa<llvm::AllocaInst>(Storage) ||
        isa<llvm::UndefValue>(Storage) ||
        Ty == IGM.RefCountedPtrTy) // No debug info is emitted for refcounts.
      return Storage;

    // Always emit shadow copies for function arguments.
    if (ArgNo == 0)
      // Otherwise only if debug value range extension is not feasible.
      if (!needsShadowCopy(Storage)) {
        // Mark for debug value range extension unless this is a constant.
        if (auto *Value = dyn_cast<llvm::Instruction>(Storage))
          ValueVariables.insert({Value, getActiveDominancePoint()});
        return Storage;
      }

    if (Align.isZero())
      Align = IGM.getPointerAlignment();

    auto &Alloca = ShadowStackSlots[{ArgNo, {Scope, Name}}];
    if (!Alloca.isValid())
      Alloca = createAlloca(Ty, Align, Name+".addr");

    ArtificialLocation AutoRestore(getDebugScope(), IGM.DebugInfo, Builder);
    Builder.CreateStore(Storage, Alloca.getAddress(), Align);
    return Alloca.getAddress();
  }

  llvm::Value *emitShadowCopy(Address Storage, const SILDebugScope *Scope,
                              StringRef Name, unsigned ArgNo) {
    return emitShadowCopy(Storage.getAddress(), Scope, Name, ArgNo,
                          Storage.getAlignment());
  }

  void emitShadowCopy(ArrayRef<llvm::Value *> vals, const SILDebugScope *Scope,
                      StringRef Name, unsigned ArgNo,
                      llvm::SmallVectorImpl<llvm::Value *> &copy) {
    // Only do this at -O0.
    if (IGM.IRGen.Opts.Optimize) {
      copy.append(vals.begin(), vals.end());
      return;
    }

    // Single or empty values.
    if (vals.size() <= 1) {
      for (auto val : vals)
        copy.push_back(emitShadowCopy(val, Scope, Name, ArgNo));
      return;
    }

    // Create a single aggregate alloca for explosions.
    // TODO: why are we doing this instead of using the TypeInfo?
    llvm::StructType *aggregateType = [&] {
      SmallVector<llvm::Type *, 8> eltTypes;
      for (auto val : vals)
        eltTypes.push_back(val->getType());
      return llvm::StructType::get(IGM.LLVMContext, eltTypes);
    }();

    auto layout = IGM.DataLayout.getStructLayout(aggregateType);
    Alignment align(layout->getAlignment());

    auto alloca = createAlloca(aggregateType, align, Name + ".debug");
    ArtificialLocation AutoRestore(getDebugScope(), IGM.DebugInfo, Builder);
    size_t i = 0;
    for (auto val : vals) {
      auto addr = Builder.CreateStructGEP(alloca, i,
                                          Size(layout->getElementOffset(i)));
      Builder.CreateStore(val, addr);
      i++;
    }
    copy.push_back(alloca.getAddress());
  }

  /// Determine whether a generic variable has been inlined.
  static bool isInlinedGeneric(VarDecl *VarDecl, const SILDebugScope *DS) {
    if (!DS->InlinedCallSite)
      return false;
    if (VarDecl->hasType())
      return VarDecl->getType()->hasArchetype();
    return VarDecl->getInterfaceType()->hasTypeParameter();
  }

  /// Emit debug info for a function argument or a local variable.
  template <typename StorageType>
  void emitDebugVariableDeclaration(StorageType Storage,
                                    DebugTypeInfo Ty,
                                    SILType SILTy,
                                    const SILDebugScope *DS,
                                    VarDecl *VarDecl,
                                    StringRef Name,
                                    unsigned ArgNo = 0,
                                    IndirectionKind Indirection = DirectValue) {
    // Force all archetypes referenced by the type to be bound by this point.
    // TODO: just make sure that we have a path to them that the debug info
    //       can follow.

    // FIXME: The debug info type of all inlined instances of a variable must be
    // the same as the type of the abstract variable.
    if (isInlinedGeneric(VarDecl, DS))
      return;

    auto runtimeTy = getRuntimeReifiedType(IGM,
                                           Ty.getType()->getCanonicalType());
    if (!IGM.IRGen.Opts.Optimize && runtimeTy->hasArchetype())
      runtimeTy.visit([&](Type t) {
        if (auto archetype = dyn_cast<ArchetypeType>(CanType(t)))
          emitTypeMetadataRef(archetype);
       });

    assert(IGM.DebugInfo && "debug info not enabled");
    if (ArgNo) {
      PrologueLocation AutoRestore(IGM.DebugInfo, Builder);
      IGM.DebugInfo->emitVariableDeclaration(Builder, Storage, Ty, DS, VarDecl,
                                             Name, ArgNo, Indirection);
    } else
      IGM.DebugInfo->emitVariableDeclaration(Builder, Storage, Ty, DS, VarDecl,
                                             Name, 0, Indirection);
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

  void emitErrorResultVar(SILResultInfo ErrorInfo, DebugValueInst *DbgValue);
  void emitDebugInfoForAllocStack(AllocStackInst *i, const TypeInfo &type,
                                  llvm::Value *addr);
  void visitAllocStackInst(AllocStackInst *i);
  void visitAllocRefInst(AllocRefInst *i);
  void visitAllocRefDynamicInst(AllocRefDynamicInst *i);
  void visitAllocBoxInst(AllocBoxInst *i);

  void visitProjectBoxInst(ProjectBoxInst *i);

  void visitApplyInst(ApplyInst *i);
  void visitTryApplyInst(TryApplyInst *i);
  void visitFullApplySite(FullApplySite i);
  void visitPartialApplyInst(PartialApplyInst *i);
  void visitBuiltinInst(BuiltinInst *i);

  void visitFunctionRefInst(FunctionRefInst *i);
  void visitAllocGlobalInst(AllocGlobalInst *i);
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
  void visitMarkUninitializedBehaviorInst(MarkUninitializedBehaviorInst *i) {
    llvm_unreachable("mark_uninitialized_behavior is not valid in canonical SIL");
  }
  void visitMarkFunctionEscapeInst(MarkFunctionEscapeInst *i) {
    llvm_unreachable("mark_function_escape is not valid in canonical SIL");
  }
  void visitLoadBorrowInst(LoadBorrowInst *i) {
    llvm_unreachable("unimplemented");
  }
  void visitDebugValueInst(DebugValueInst *i);
  void visitDebugValueAddrInst(DebugValueAddrInst *i);
  void visitLoadWeakInst(LoadWeakInst *i);
  void visitStoreWeakInst(StoreWeakInst *i);
  void visitRetainValueInst(RetainValueInst *i);
  void visitCopyValueInst(CopyValueInst *i);
  void visitCopyUnownedValueInst(CopyUnownedValueInst *i) {
    llvm_unreachable("unimplemented");
  }
  void visitReleaseValueInst(ReleaseValueInst *i);
  void visitDestroyValueInst(DestroyValueInst *i);
  void visitAutoreleaseValueInst(AutoreleaseValueInst *i);
  void visitSetDeallocatingInst(SetDeallocatingInst *i);
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
  void visitRefTailAddrInst(RefTailAddrInst *i);

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
  void visitOpenExistentialOpaqueInst(OpenExistentialOpaqueInst *i);
  void visitInitExistentialAddrInst(InitExistentialAddrInst *i);
  void visitInitExistentialMetatypeInst(InitExistentialMetatypeInst *i);
  void visitInitExistentialRefInst(InitExistentialRefInst *i);
  void visitDeinitExistentialAddrInst(DeinitExistentialAddrInst *i);
  
  void visitAllocExistentialBoxInst(AllocExistentialBoxInst *i);
  void visitOpenExistentialBoxInst(OpenExistentialBoxInst *i);
  void visitProjectExistentialBoxInst(ProjectExistentialBoxInst *i);
  void visitDeallocExistentialBoxInst(DeallocExistentialBoxInst *i);
  
  void visitProjectBlockStorageInst(ProjectBlockStorageInst *i);
  void visitInitBlockStorageHeaderInst(InitBlockStorageHeaderInst *i);
  
  void visitFixLifetimeInst(FixLifetimeInst *i);
  void visitBeginBorrowInst(BeginBorrowInst *i) {
    llvm_unreachable("unimplemented");
  }
  void visitEndBorrowInst(EndBorrowInst *i) {
    llvm_unreachable("unimplemented");
  }
  void visitEndBorrowArgumentInst(EndBorrowArgumentInst *i) {
    llvm_unreachable("unimplemented");
  }
  void visitStoreBorrowInst(StoreBorrowInst *i) {
    llvm_unreachable("unimplemented");
  }
  void visitUnmanagedRetainValueInst(UnmanagedRetainValueInst *i) {
    llvm_unreachable("unimplemented");
  }
  void visitUnmanagedReleaseValueInst(UnmanagedReleaseValueInst *i) {
    llvm_unreachable("unimplemented");
  }
  void visitUnmanagedAutoreleaseValueInst(UnmanagedAutoreleaseValueInst *i) {
    llvm_unreachable("unimplemented");
  }
  void visitMarkDependenceInst(MarkDependenceInst *i);
  void visitCopyBlockInst(CopyBlockInst *i);
  void visitStrongPinInst(StrongPinInst *i);
  void visitStrongUnpinInst(StrongUnpinInst *i);
  void visitStrongRetainInst(StrongRetainInst *i);
  void visitStrongReleaseInst(StrongReleaseInst *i);
  void visitStrongRetainUnownedInst(StrongRetainUnownedInst *i);
  void visitUnownedRetainInst(UnownedRetainInst *i);
  void visitUnownedReleaseInst(UnownedReleaseInst *i);
  void visitLoadUnownedInst(LoadUnownedInst *i);
  void visitStoreUnownedInst(StoreUnownedInst *i);
  void visitIsUniqueInst(IsUniqueInst *i);
  void visitIsUniqueOrPinnedInst(IsUniqueOrPinnedInst *i);
  void visitDeallocStackInst(DeallocStackInst *i);
  void visitDeallocBoxInst(DeallocBoxInst *i);
  void visitDeallocRefInst(DeallocRefInst *i);
  void visitDeallocPartialRefInst(DeallocPartialRefInst *i);

  void visitCopyAddrInst(CopyAddrInst *i);
  void visitDestroyAddrInst(DestroyAddrInst *i);

  void visitBindMemoryInst(BindMemoryInst *i);

  void visitCondFailInst(CondFailInst *i);
  
  void visitConvertFunctionInst(ConvertFunctionInst *i);
  void visitThinFunctionToPointerInst(ThinFunctionToPointerInst *i);
  void visitPointerToThinFunctionInst(PointerToThinFunctionInst *i);
  void visitUpcastInst(UpcastInst *i);
  void visitAddressToPointerInst(AddressToPointerInst *i);
  void visitPointerToAddressInst(PointerToAddressInst *i);
  void visitUncheckedRefCastInst(UncheckedRefCastInst *i);
  void visitUncheckedRefCastAddrInst(UncheckedRefCastAddrInst *i);
  void visitUncheckedAddrCastInst(UncheckedAddrCastInst *i);
  void visitUncheckedTrivialBitCastInst(UncheckedTrivialBitCastInst *i);
  void visitUncheckedBitwiseCastInst(UncheckedBitwiseCastInst *i);
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
  void visitTailAddrInst(TailAddrInst *i);
  void visitIndexRawPointerInst(IndexRawPointerInst *i);
  
  void visitUnreachableInst(UnreachableInst *i);
  void visitBranchInst(BranchInst *i);
  void visitCondBranchInst(CondBranchInst *i);
  void visitReturnInst(ReturnInst *i);
  void visitThrowInst(ThrowInst *i);
  void visitSwitchValueInst(SwitchValueInst *i);
  void visitSwitchEnumInst(SwitchEnumInst *i);
  void visitSwitchEnumAddrInst(SwitchEnumAddrInst *i);
  void visitDynamicMethodBranchInst(DynamicMethodBranchInst *i);
  void visitCheckedCastBranchInst(CheckedCastBranchInst *i);
  void visitCheckedCastAddrBranchInst(CheckedCastAddrBranchInst *i);
};

} // end anonymous namespace

llvm::Value *StaticFunction::getExplosionValue(IRGenFunction &IGF) const {
  return IGF.Builder.CreateBitCast(Function, IGF.IGM.Int8PtrTy);
}

void LoweredValue::getExplosion(IRGenFunction &IGF, Explosion &ex) const {
  switch (kind) {
  case Kind::Address:
  case Kind::ContainedAddress:
    llvm_unreachable("not a value");
      
  case Kind::Explosion:
    for (auto *value : explosion.values)
      ex.add(value);
    break;

  case Kind::BoxWithAddress:
    ex.add(boxWithAddress.getOwner());
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
  case Kind::ContainedAddress:
    llvm_unreachable("not a value");

  case Kind::Explosion:
    assert(explosion.values.size() == 1);
    return explosion.values[0];

  case Kind::BoxWithAddress:
    return boxWithAddress.getOwner();
      
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
    CurSILFn(f) {
  // Apply sanitizer attributes to the function.
  // TODO: Check if the function is ASan black listed either in the external
  // file or via annotations.
  if (IGM.IRGen.Opts.Sanitize == SanitizerKind::Address)
    CurFn->addFnAttr(llvm::Attribute::SanitizeAddress);
  if (IGM.IRGen.Opts.Sanitize == SanitizerKind::Thread) {
    if (dyn_cast_or_null<DestructorDecl>(f->getDeclContext()))
      // Do not report races in deinit and anything called from it
      // because TSan does not observe synchronization between retain
      // count dropping to '0' and the object deinitialization.
      CurFn->addFnAttr("sanitize_thread_no_checking_at_run_time");
    else
      CurFn->addFnAttr(llvm::Attribute::SanitizeThread);
  }
}

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

static PHINodeVector
emitPHINodesForBBArgs(IRGenSILFunction &IGF,
                      SILBasicBlock *silBB,
                      llvm::BasicBlock *llBB) {
  PHINodeVector phis;
  unsigned predecessors = std::distance(silBB->pred_begin(), silBB->pred_end());
  
  IGF.Builder.SetInsertPoint(llBB);
  if (IGF.IGM.DebugInfo) {
    // Use the location of the first instruction in the basic block
    // for the φ-nodes.
    if (!silBB->empty()) {
      SILInstruction &I = *silBB->begin();
      auto DS = I.getDebugScope();
      assert(DS);
      IGF.IGM.DebugInfo->setCurrentLoc(IGF.Builder, DS, I.getLoc());
    }
  }

  for (SILArgument *arg : make_range(silBB->args_begin(), silBB->args_end())) {
    size_t first = phis.size();
    
    const TypeInfo &ti = IGF.getTypeInfo(arg->getType());
    
    emitPHINodesForType(IGF, arg->getType(), ti, predecessors, phis);
    if (arg->getType().isAddress()) {
      IGF.setLoweredAddress(arg,
                            ti.getAddressForPointer(phis.back()));
    } else {
      Explosion argValue;
      for (llvm::PHINode *phi :
               swift::make_range(phis.begin()+first, phis.end()))
        argValue.add(phi);
      IGF.setLoweredExplosion(arg, argValue);
    }
  }

  // Since we return to the entry of the function, reset the location.
  if (IGF.IGM.DebugInfo)
    IGF.IGM.DebugInfo->clearLoc(IGF.Builder);

  return phis;
}

static void addIncomingExplosionToPHINodes(IRGenSILFunction &IGF,
                                           LoweredBB &lbb,
                                           unsigned &phiIndex,
                                           Explosion &argValue);

// TODO: Handle this during SIL AddressLowering.
static ArrayRef<SILArgument*> emitEntryPointIndirectReturn(
                                 IRGenSILFunction &IGF,
                                 SILBasicBlock *entry,
                                 Explosion &params,
                                 CanSILFunctionType funcTy,
                    llvm::function_ref<bool(SILType)> requiresIndirectResult) {
  // Map an indirect return for a type SIL considers loadable but still
  // requires an indirect return at the IR level.
  SILFunctionConventions fnConv(funcTy, IGF.getSILModule());
  SILType directResultType =
      IGF.CurSILFn->mapTypeIntoContext(fnConv.getSILResultType());
  if (requiresIndirectResult(directResultType)) {
    auto &retTI = IGF.IGM.getTypeInfo(directResultType);
    IGF.IndirectReturn = retTI.getAddressForPointer(params.claimNext());
  }

  auto bbargs = entry->getArguments();

  // Map the indirect returns if present.
  unsigned numIndirectResults = fnConv.getNumIndirectSILResults();
  for (unsigned i = 0; i != numIndirectResults; ++i) {
    SILArgument *ret = bbargs[i];
    auto &retTI = IGF.IGM.getTypeInfo(ret->getType());    
    IGF.setLoweredAddress(ret, retTI.getAddressForPointer(params.claimNext()));
  }  

  return bbargs.slice(numIndirectResults);
}

static void bindParameter(IRGenSILFunction &IGF,
                          SILArgument *param,
                          Explosion &allParamValues) {
  // Pull out the parameter value and its formal type.
  auto &paramTI = IGF.getTypeInfo(param->getType());

  // If the SIL parameter isn't passed indirectly, we need to map it
  // to an explosion.
  if (param->getType().isObject()) {
    Explosion paramValues;
    auto &loadableTI = cast<LoadableTypeInfo>(paramTI);
    // If the explosion must be passed indirectly, load the value from the
    // indirect address.
    if (loadableTI.getSchema().requiresIndirectParameter(IGF.IGM)) {
      Address paramAddr
        = loadableTI.getAddressForPointer(allParamValues.claimNext());
      loadableTI.loadAsTake(IGF, paramAddr, paramValues);
    } else {
      // Otherwise, we can just take the exploded arguments.
      // FIXME: It doesn't necessarily make sense to pass all types using their
      // explosion schema.
      loadableTI.reexplode(IGF, allParamValues, paramValues);
    }
    IGF.setLoweredExplosion(param, paramValues);
    return;
  }

  // Okay, the type is passed indirectly in SIL, so we need to map
  // it to an address.
  // FIXME: that doesn't mean we should physically pass it
  // indirectly at this resilience expansion. An @in or @in_guaranteed parameter
  // could be passed by value in the right resilience domain.
  Address paramAddr
    = paramTI.getAddressForPointer(allParamValues.claimNext());
  IGF.setLoweredAddress(param, paramAddr);
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
      [&](SILType retType) -> bool {
        return IGF.IGM.requiresIndirectResult(retType);
      });

  // The witness method CC passes Self as a final argument.
  WitnessMetadata witnessMetadata;
  if (funcTy->getRepresentation() == SILFunctionTypeRepresentation::WitnessMethod) {
    collectTrailingWitnessMetadata(IGF, *IGF.CurSILFn, allParamValues,
                                   witnessMetadata);
  }

  // Bind the error result by popping it off the parameter list.
  if (funcTy->hasErrorResult()) {
    IGF.setErrorResultSlot(allParamValues.takeLast());
  }

  // The 'self' argument might be in the context position, which is
  // now the end of the parameter list.  Bind it now.
  if (funcTy->hasSelfParam() &&
      isSelfContextParameter(funcTy->getSelfParameter())) {
    SILArgument *selfParam = params.back();
    params = params.drop_back();

    Explosion selfTemp;
    selfTemp.add(allParamValues.takeLast());
    bindParameter(IGF, selfParam, selfTemp);

  // Even if we don't have a 'self', if we have an error result, we
  // should have a placeholder argument here.
  } else if (funcTy->hasErrorResult() ||
           funcTy->getRepresentation() == SILFunctionTypeRepresentation::Thick)
  {
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
        SILValue parameter =
          IGF.CurSILFn->getArgumentsWithoutIndirectResults()[paramIndex];
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
  // First, lower the method type.
  ForeignFunctionInfo foreignInfo = IGF.IGM.getForeignFunctionInfo(funcTy);
  assert(foreignInfo.ClangInfo);
  auto &FI = *foreignInfo.ClangInfo;

  // Okay, start processing the parameters explosion.

  // First, claim all the indirect results.
  ArrayRef<SILArgument*> args
    = emitEntryPointIndirectReturn(IGF, entry, params, funcTy,
      [&](SILType directResultType) -> bool {
        return FI.getReturnInfo().isIndirect();
      });

  unsigned nextArgTyIdx = 0;

  // Handle the arguments of an ObjC method.
  if (IGF.CurSILFn->getRepresentation() ==
        SILFunctionTypeRepresentation::ObjCMethod) {
    // Claim the self argument from the end of the formal arguments.
    SILArgument *selfArg = args.back();
    args = args.slice(0, args.size() - 1);

    // Set the lowered explosion for the self argument.
    auto &selfTI = cast<LoadableTypeInfo>(IGF.getTypeInfo(selfArg->getType()));
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

  assert(args.size() == (FI.arg_size() - nextArgTyIdx) &&
         "Number of arguments not equal to number of argument types!");

  // Generate lowered explosions for each explicit argument.
  for (auto i : indices(args)) {
    SILArgument *arg = args[i];
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
    emitForeignParameter(IGF, params, foreignInfo, argTyIdx,
                         arg->getType(), loadableArgTI, argExplosion);
    IGF.setLoweredExplosion(arg, argExplosion);
  }

  assert(params.empty() && "didn't claim all parameters!");

  // emitPolymorphicParameters() may create function calls, so we need
  // to initialize the debug location here.
  ArtificialLocation Loc(IGF.getDebugScope(), IGF.IGM.DebugInfo, IGF.Builder);
  
  // Bind polymorphic arguments. This can only be done after binding
  // all the value parameters, and must be done even for non-polymorphic
  // functions because of imported Objective-C generics.
  emitPolymorphicParameters(
      IGF, *IGF.CurSILFn, params, nullptr,
      [&](unsigned paramIndex) -> llvm::Value * {
        SILValue parameter = entry->getArguments()[paramIndex];
        return IGF.getLoweredSingletonExplosion(parameter);
      });
}

/// Get metadata for the dynamic Self type if we have it.
static void emitLocalSelfMetadata(IRGenSILFunction &IGF) {
  if (!IGF.CurSILFn->hasSelfMetadataParam())
    return;
  
  const SILArgument *selfArg = IGF.CurSILFn->getSelfMetadataArgument();
  CanMetatypeType metaTy =
    dyn_cast<MetatypeType>(selfArg->getType().getSwiftRValueType());
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

  // Configure the dominance resolver.
  // TODO: consider re-using a dom analysis from the PassManager
  // TODO: consider using a cheaper analysis at -O0
  setDominanceResolver([](IRGenFunction &IGF_,
                          DominancePoint activePoint,
                          DominancePoint dominatingPoint) -> bool {
    IRGenSILFunction &IGF = static_cast<IRGenSILFunction&>(IGF_);
    if (!IGF.Dominance) {
      IGF.Dominance.reset(new DominanceInfo(IGF.CurSILFn));
    }
    return IGF.Dominance->dominates(dominatingPoint.as<SILBasicBlock>(),
                                    activePoint.as<SILBasicBlock>());
  });
  
  if (IGM.DebugInfo)
    IGM.DebugInfo->emitFunction(*CurSILFn, CurFn);

  // Map the entry bb.
  LoweredBBs[&*CurSILFn->begin()] = LoweredBB(&*CurFn->begin(), {});
  // Create LLVM basic blocks for the other bbs.
  for (auto bi = std::next(CurSILFn->begin()), be = CurSILFn->end(); bi != be;
       ++bi) {
    // FIXME: Use the SIL basic block's name.
    llvm::BasicBlock *llBB = llvm::BasicBlock::Create(IGM.getLLVMContext());
    auto phis = emitPHINodesForBBArgs(*this, &*bi, llBB);
    CurFn->getBasicBlockList().push_back(llBB);
    LoweredBBs[&*bi] = LoweredBB(llBB, std::move(phis));
  }

  auto entry = LoweredBBs.begin();
  Builder.SetInsertPoint(entry->second.bb);

  // Map the LLVM arguments to arguments on the entry point BB.
  Explosion params = collectParameters();
  auto funcTy = CurSILFn->getLoweredFunctionType();

  switch (funcTy->getLanguage()) {
  case SILFunctionLanguage::Swift:
    emitEntryPointArgumentsNativeCC(*this, entry->first, params);
    break;
  case SILFunctionLanguage::C:
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
  visitedBlocks.insert(&*CurSILFn->begin());
  workQueue.push_back(&*CurSILFn->begin());

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
        assert(&*std::next(curBB->getIterator()) == nextBB &&
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
    for (auto *succBB : bb->getSuccessorBlocks()) {
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

void IRGenSILFunction::estimateStackSize() {
  if (EstimatedStackSize >= 0)
    return;

  // TODO: as soon as we generate alloca instructions with accurate lifetimes
  // we should also do a better stack size calculation here. Currently we
  // add all stack sizes even if life ranges do not overlap.
  for (SILBasicBlock &BB : *CurSILFn) {
    for (SILInstruction &I : BB) {
      if (auto *ASI = dyn_cast<AllocStackInst>(&I)) {
        const TypeInfo &type = getTypeInfo(ASI->getElementType());
        if (llvm::Constant *SizeConst = type.getStaticSize(IGM)) {
          auto *SizeInt = cast<llvm::ConstantInt>(SizeConst);
          EstimatedStackSize += (int)SizeInt->getSExtValue();
        }
      }
    }
  }
}

void IRGenSILFunction::visitSILBasicBlock(SILBasicBlock *BB) {
  // Insert into the lowered basic block.
  llvm::BasicBlock *llBB = getLoweredBB(BB).bb;
  Builder.SetInsertPoint(llBB);

  bool InEntryBlock = BB->pred_empty();

  // Set this block as the dominance point.  This implicitly communicates
  // with the dominance resolver configured in emitSILFunction.
  DominanceScope dominance(*this, InEntryBlock ? DominancePoint::universal()
                                               : DominancePoint(BB));

  // The basic blocks are visited in a random order. Reset the debug location.
  std::unique_ptr<AutoRestoreLocation> ScopedLoc;
  if (InEntryBlock)
    ScopedLoc = llvm::make_unique<PrologueLocation>(IGM.DebugInfo, Builder);
  else
    ScopedLoc = llvm::make_unique<ArtificialLocation>(
        CurSILFn->getDebugScope(), IGM.DebugInfo, Builder);

  // Generate the body.
  bool InCleanupBlock = false;
  bool KeepCurrentLocation = false;

  for (auto InsnIter = BB->begin(); InsnIter != BB->end(); ++InsnIter) {
    auto &I = *InsnIter;
#ifndef NDEBUG
    IGM.EligibleConfs.collect(&I);
    IGM.CurrentInst = &I;
#endif
    if (IGM.DebugInfo) {
      // Set the debug info location for I, if applicable.
      SILLocation ILoc = I.getLoc();
      auto DS = I.getDebugScope();
      // Handle cleanup locations.
      if (ILoc.is<CleanupLocation>()) {
        // Cleanup locations point to the decl of the value that is
        // being destroyed (for diagnostic generation). As far as
        // the linetable is concerned, cleanups at the end of a
        // lexical scope should point to the cleanup location, which
        // is the location of the last instruction in the basic block.
        if (!InCleanupBlock) {
          InCleanupBlock = true;
          // Scan ahead to see if this is the final cleanup block in
          // this basic block.
          auto It = InsnIter;
          do ++It; while (It != BB->end() &&
                          It->getLoc().is<CleanupLocation>());
          // We are still in the middle of a basic block?
          if (It != BB->end() && !isa<TermInst>(It))
            KeepCurrentLocation = true;
        }

        // Assign the cleanup location to this instruction.
        if (!KeepCurrentLocation) {
          assert(BB->getTerminator());
          ILoc = BB->getTerminator()->getLoc();
          DS = BB->getTerminator()->getDebugScope();
        }
      } else if (InCleanupBlock) {
        KeepCurrentLocation = false;
        InCleanupBlock = false;
      }

      // Until SILDebugScopes are properly serialized, bare functions
      // are allowed to not have a scope.
      if (!DS) {
        if (CurSILFn->isBare())
          DS = CurSILFn->getDebugScope();
        assert(maybeScopeless(I) && "instruction has location, but no scope");
      }

      // Set the builder's debug location.
      if (DS && !KeepCurrentLocation)
        IGM.DebugInfo->setCurrentLoc(Builder, DS, ILoc);
      else
        // Use an artificial (line 0) location.
        IGM.DebugInfo->setCurrentLoc(Builder, DS);

      if (isa<TermInst>(&I))
        emitDebugVariableRangeExtension(BB);
    }
    visit(&I);

#ifndef NDEBUG
    IGM.EligibleConfs.clear();
    IGM.CurrentInst = nullptr;
#endif
  }
  
  assert(Builder.hasPostTerminatorIP() && "SIL bb did not terminate block?!");
}

void IRGenSILFunction::visitFunctionRefInst(FunctionRefInst *i) {
  auto fn = i->getReferencedFunction();

  llvm::Function *fnptr = IGM.getAddrOfSILFunction(fn, NotForDefinition);
  auto foreignInfo = IGM.getForeignFunctionInfo(fn->getLoweredFunctionType());
  
  // Store the function constant and calling
  // convention as a StaticFunction so we can avoid bitcasting or thunking if
  // we don't need to.
  setLoweredStaticFunction(i, fnptr, fn->getRepresentation(), foreignInfo);
}

void IRGenSILFunction::visitAllocGlobalInst(AllocGlobalInst *i) {
  SILGlobalVariable *var = i->getReferencedGlobal();
  SILType loweredTy = var->getLoweredType();
  auto &ti = getTypeInfo(loweredTy);

  auto expansion = IGM.getResilienceExpansionForLayout(var);

  // If the global is fixed-size in all resilience domains that can see it,
  // we allocated storage for it statically, and there's nothing to do.
  if (ti.isFixedSize(expansion))
    return;

  // Otherwise, the static storage for the global consists of a fixed-size
  // buffer.
  Address addr = IGM.getAddrOfSILGlobalVariable(var, ti,
                                                NotForDefinition);
  (void) ti.allocateBuffer(*this, addr, loweredTy);
}

void IRGenSILFunction::visitGlobalAddrInst(GlobalAddrInst *i) {
  SILGlobalVariable *var = i->getReferencedGlobal();
  SILType loweredTy = var->getLoweredType();
  assert(loweredTy == i->getType().getObjectType());
  auto &ti = getTypeInfo(loweredTy);
  
  auto expansion = IGM.getResilienceExpansionForLayout(var);

  // If the variable is empty in all resilience domains that can see it,
  // don't actually emit a symbol for the global at all, just return undef.
  if (ti.isKnownEmpty(expansion)) {
    setLoweredAddress(i, ti.getUndefAddress());
    return;
  }

  Address addr = IGM.getAddrOfSILGlobalVariable(var, ti,
                                                NotForDefinition);

  // If the global is fixed-size in all resilience domains that can see it,
  // we allocated storage for it statically, and there's nothing to do.
  if (ti.isFixedSize(expansion)) {
    setLoweredAddress(i, addr);
    return;
  }

  // Otherwise, the static storage for the global consists of a fixed-size
  // buffer; project it.
  addr = ti.projectBuffer(*this, addr, loweredTy);
  
  setLoweredAddress(i, addr);
}

void IRGenSILFunction::visitMetatypeInst(swift::MetatypeInst *i) {
  auto metaTy = i->getType().castTo<MetatypeType>();
  Explosion e;
  emitMetatypeRef(*this, metaTy, e);
  setLoweredExplosion(i, e);
}

static llvm::Value *getClassBaseValue(IRGenSILFunction &IGF,
                                      SILValue v) {
  if (v->getType().isAddress()) {
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

  llvm_unreachable("Not a valid MetatypeRepresentation.");
}

void IRGenSILFunction::visitValueMetatypeInst(swift::ValueMetatypeInst *i) {
  SILType instanceTy = i->getOperand()->getType();
  auto metaTy = i->getType().castTo<MetatypeType>();
  
  if (metaTy->getRepresentation() == MetatypeRepresentation::Thin) {
    Explosion empty;
    setLoweredExplosion(i, empty);
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
                                             i->getOperand()->getType()));
      // FIXME: We need to convert this back to an ObjC class for an
      // ObjC metatype representation.
      if (metaTy->getRepresentation() == MetatypeRepresentation::ObjC)
        unimplemented(i->getLoc().getSourceLoc(),
                      "objc metatype of non-class-bounded archetype");
    }
  } else {
    emitMetatypeRef(*this, metaTy, e);
  }
  
  setLoweredExplosion(i, e);
}

void IRGenSILFunction::visitExistentialMetatypeInst(
                                            swift::ExistentialMetatypeInst *i) {
  Explosion result;
  SILValue op = i->getOperand();
  SILType opType = op->getType();

  switch (opType.getPreferredExistentialRepresentation(IGM.getSILModule())) {
  case ExistentialRepresentation::Metatype: {
    Explosion existential = getLoweredExplosion(op);
    emitMetatypeOfMetatype(*this, existential, opType, result);
    break;
  }
  case ExistentialRepresentation::Class: {
    Explosion existential = getLoweredExplosion(op);
    emitMetatypeOfClassExistential(*this, existential, i->getType(),
                                   opType, result);
    break;
  }
  case ExistentialRepresentation::Boxed: {
    Explosion existential = getLoweredExplosion(op);
    emitMetatypeOfBoxedExistential(*this, existential, opType, result);
    break;
  }
  case ExistentialRepresentation::Opaque: {
    Address existential = getLoweredAddress(op);
    emitMetatypeOfOpaqueExistential(*this, existential, opType, result);
    break;
  }
  case ExistentialRepresentation::None:
    llvm_unreachable("Bad existential representation");
  }

  setLoweredExplosion(i, result);
}

static void emitApplyArgument(IRGenSILFunction &IGF,
                              SILValue arg,
                              SILType paramType,
                              Explosion &out) {
  bool isSubstituted = (arg->getType() != paramType);

  // For indirect arguments, we just need to pass a pointer.
  if (paramType.isAddress()) {
    // This address is of the substituted type.
    auto addr = IGF.getLoweredAddress(arg);

    // If a substitution is in play, just bitcast the address.
    if (isSubstituted) {
      auto origType = IGF.IGM.getStoragePointerType(paramType);
      addr = IGF.Builder.CreateBitCast(addr, origType);
    }
      
    out.add(addr.getAddress());
    return;
  }

  // Otherwise, it's an explosion, which we may need to translate,
  // both in terms of explosion level and substitution levels.
  assert(arg->getType().isObject());

  // Fast path: avoid an unnecessary temporary explosion.
  if (!isSubstituted) {
    IGF.getLoweredExplosion(arg, out);
    return;
  }

  Explosion temp = IGF.getLoweredExplosion(arg);
  reemitAsUnsubstituted(IGF, paramType, arg->getType(),
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

static llvm::Value *emitWitnessTableForLoweredCallee(IRGenSILFunction &IGF,
                                              CanSILFunctionType origCalleeType,
                                              SubstitutionList subs) {
  auto &M = *IGF.getSwiftModule();
  llvm::Value *wtable;

  if (auto *proto = origCalleeType->getDefaultWitnessMethodProtocol(M)) {
    // The generic signature for a witness method with abstract Self must
    // have exactly one protocol requirement.
    //
    // We recover the witness table from the substitution that was used to
    // produce the substituted callee type.
    //
    // There can be multiple substitutions, but the first one is the Self type.
    assert(subs.size() >= 1);
    assert(subs[0].getConformances().size() == 1);

    auto conformance = subs[0].getConformances()[0];
    assert(conformance.getRequirement() == proto); (void) proto;
    auto substSelfType = subs[0].getReplacement()->getCanonicalType();

    llvm::Value *argMetadata = IGF.emitTypeMetadataRef(substSelfType);
    wtable = emitWitnessTableRef(IGF, substSelfType, &argMetadata,
                                 conformance);
  } else {
    // Otherwise, we have no way of knowing the original protocol or
    // conformance, since the witness has a concrete self type.
    //
    // Protocol witnesses for concrete types are thus not allowed to touch
    // the witness table; they already know all the witnesses, and we can't
    // say who they are.
    wtable = llvm::ConstantPointerNull::get(IGF.IGM.WitnessTablePtrTy);
  }

  assert(wtable->getType() == IGF.IGM.WitnessTablePtrTy);
  return wtable;

}

static CallEmission getCallEmissionForLoweredValue(IRGenSILFunction &IGF,
                                         CanSILFunctionType origCalleeType,
                                         CanSILFunctionType substCalleeType,
                                         const LoweredValue &lv,
                                         llvm::Value *selfValue,
                                         SubstitutionList substitutions,
                                         WitnessMetadata *witnessMetadata,
                                         Explosion &args) {
  llvm::Value *calleeFn, *calleeData;
  ForeignFunctionInfo foreignInfo;
  
  switch (lv.kind) {
  case LoweredValue::Kind::StaticFunction:
    calleeFn = lv.getStaticFunction().getFunction();
    calleeData = selfValue;
    foreignInfo = lv.getStaticFunction().getForeignInfo();

    if (origCalleeType->getRepresentation()
          == SILFunctionType::Representation::WitnessMethod) {
      llvm::Value *wtable = emitWitnessTableForLoweredCallee(
          IGF, origCalleeType, substitutions);
      witnessMetadata->SelfWitnessTable = wtable;
    }
    break;
      
  case LoweredValue::Kind::ObjCMethod: {
    assert(selfValue);
    auto &objcMethod = lv.getObjCMethod();
    ObjCMessageKind kind = objcMethod.getMessageKind();

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
    case SILFunctionType::Representation::Block: {
      assert(!selfValue && "block function with self?");

      // Grab the block pointer and make it the first physical argument.
      llvm::Value *blockPtr = lv.getSingletonExplosion(IGF);
      blockPtr = IGF.Builder.CreateBitCast(blockPtr, IGF.IGM.ObjCBlockPtrTy);
      args.add(blockPtr);

      // Extract the invocation pointer for blocks.
      llvm::Value *invokeAddr = IGF.Builder.CreateStructGEP(
          /*Ty=*/nullptr, blockPtr, 3);
      calleeFn = IGF.Builder.CreateLoad(invokeAddr, IGF.IGM.getPointerAlignment());
      calleeData = nullptr;
      break;
    }
    
    case SILFunctionType::Representation::Thin:
    case SILFunctionType::Representation::CFunctionPointer:
    case SILFunctionType::Representation::Method:
    case SILFunctionType::Representation::Closure:
    case SILFunctionType::Representation::ObjCMethod:
    case SILFunctionType::Representation::WitnessMethod:
    case SILFunctionType::Representation::Thick: {
      Explosion calleeValues = lv.getExplosion(IGF);
      calleeFn = calleeValues.claimNext();

      if (origCalleeType->getRepresentation()
            == SILFunctionType::Representation::WitnessMethod) {
        // @convention(witness_method) callees are exploded as a
        // triple consisting of the function, Self metadata, and
        // the Self witness table.
        witnessMetadata->SelfWitnessTable = calleeValues.claimNext();
        assert(witnessMetadata->SelfWitnessTable->getType() ==
               IGF.IGM.WitnessTablePtrTy);
      }

      if (origCalleeType->getRepresentation()
            == SILFunctionType::Representation::Thick) {
        // @convention(thick) callees are exploded as a pair
        // consisting of the function and the self value.
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
    llvm::FunctionType *fnTy =
      IGF.IGM.getFunctionType(origCalleeType, attrs, &foreignInfo);
    calleeFn = IGF.Builder.CreateBitCast(calleeFn, fnTy->getPointerTo());
    break;
  }
      
  case LoweredValue::Kind::BoxWithAddress:
    llvm_unreachable("@box isn't a valid callee");
  case LoweredValue::Kind::ContainedAddress:
  case LoweredValue::Kind::Address:
    llvm_unreachable("sil address isn't a valid callee");
  }
  
  Callee callee = Callee::forKnownFunction(origCalleeType, substCalleeType,
                                           substitutions, calleeFn, calleeData,
                                           foreignInfo);
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
    emitApplyArgument(*this, argValue, argValue->getType(), args);
  }
  
  Explosion result;
  emitBuiltinCall(*this, i->getName(), i->getType(),
                  args, result, i->getSubstitutions());
  
  setLoweredExplosion(i, result);
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
  
  auto args = site.getArguments();
  SILFunctionConventions origConv(origCalleeType, getSILModule());
  assert(origConv.getNumSILArguments() == args.size());

  // Extract 'self' if it needs to be passed as the context parameter.
  llvm::Value *selfValue = nullptr;
  if (origCalleeType->hasSelfParam() &&
      isSelfContextParameter(origCalleeType->getSelfParameter())) {
    SILValue selfArg = args.back();
    args = args.drop_back();

    if (selfArg->getType().isObject()) {
      selfValue = getLoweredSingletonExplosion(selfArg);
    } else {
      selfValue = getLoweredAddress(selfArg).getAddress();
    }
  }

  Explosion llArgs;    
  WitnessMetadata witnessMetadata;
  CallEmission emission =
    getCallEmissionForLoweredValue(*this, origCalleeType, substCalleeType,
                                   calleeLV, selfValue, site.getSubstitutions(),
                                   &witnessMetadata, llArgs);

  // Lower the arguments and return value in the callee's generic context.
  GenericContextScope scope(IGM, origCalleeType->getGenericSignature());

  // Lower the SIL arguments to IR arguments.
  
  // Turn the formal SIL parameters into IR-gen things.
  for (auto index : indices(args)) {
    emitApplyArgument(*this, args[index], origConv.getSILArgumentType(index),
                      llArgs);
  }

  // Pass the generic arguments.
  if (hasPolymorphicParameters(origCalleeType)) {
    SubstitutionMap subMap;
    if (auto genericSig = origCalleeType->getGenericSignature())
      subMap = genericSig->getSubstitutionMap(site.getSubstitutions());
    emitPolymorphicArguments(*this, origCalleeType, substCalleeType,
                             subMap, &witnessMetadata, llArgs);
  }

  // Add all those arguments.
  emission.setArgs(llArgs, &witnessMetadata);

  SILInstruction *i = site.getInstruction();
  
  Explosion result;
  emission.emitToExplosion(result);

  if (isa<ApplyInst>(i)) {
    setLoweredExplosion(i, result);
  } else {
    auto tryApplyInst = cast<TryApplyInst>(i);

    // Load the error value.
    SILFunctionConventions substConv(substCalleeType, getSILModule());
    SILType errorType = substConv.getSILErrorType();
    Address errorSlot = getErrorResultSlot(errorType);
    auto errorValue = Builder.CreateLoad(errorSlot);

    auto &normalDest = getLoweredBB(tryApplyInst->getNormalBB());
    auto &errorDest = getLoweredBB(tryApplyInst->getErrorBB());

    // Zero the error slot to maintain the invariant that it always
    // contains null.  This will frequently become a dead store.
    auto nullError = llvm::Constant::getNullValue(errorValue->getType());
    if (!tryApplyInst->getErrorBB()->getSinglePredecessorBlock()) {
      // Only do that here if we can't move the store to the error block.
      // See below.
      Builder.CreateStore(nullError, errorSlot);
    }

    // If the error value is non-null, branch to the error destination.
    auto hasError = Builder.CreateICmpNE(errorValue, nullError);
    Builder.CreateCondBr(hasError, errorDest.bb, normalDest.bb);

    // Set up the PHI nodes on the normal edge.
    unsigned firstIndex = 0;
    addIncomingExplosionToPHINodes(*this, normalDest, firstIndex, result);
    assert(firstIndex == normalDest.phis.size());

    // Set up the PHI nodes on the error edge.
    assert(errorDest.phis.size() == 1);
    errorDest.phis[0]->addIncoming(errorValue, Builder.GetInsertBlock());

    if (tryApplyInst->getErrorBB()->getSinglePredecessorBlock()) {
      // Zeroing out the error slot only in the error block increases the chance
      // that it will become a dead store.
      auto origBB = Builder.GetInsertBlock();
      Builder.SetInsertPoint(errorDest.bb);
      Builder.CreateStore(nullError, errorSlot);
      Builder.SetInsertPoint(origBB);
    }
  }
}

/// If the value is a @convention(witness_method) function, the context
/// is the witness table that must be passed to the call.
///
/// \param v A value of possibly-polymorphic SILFunctionType.
/// \param subs This is the set of substitutions that we are going to be
/// applying to 'v'.
static std::tuple<llvm::Value*, llvm::Value*, CanSILFunctionType>
getPartialApplicationFunction(IRGenSILFunction &IGF, SILValue v,
                              SubstitutionList subs) {
  LoweredValue &lv = IGF.getLoweredValue(v);
  auto fnType = v->getType().castTo<SILFunctionType>();

  switch (lv.kind) {
  case LoweredValue::Kind::ContainedAddress:
  case LoweredValue::Kind::Address:
    llvm_unreachable("can't partially apply an address");
  case LoweredValue::Kind::BoxWithAddress:
    llvm_unreachable("can't partially apply a @box");
  case LoweredValue::Kind::ObjCMethod:
    llvm_unreachable("objc method partial application shouldn't get here");

  case LoweredValue::Kind::StaticFunction: {
    llvm::Value *context = nullptr;
    switch (lv.getStaticFunction().getRepresentation()) {
    case SILFunctionTypeRepresentation::CFunctionPointer:
    case SILFunctionTypeRepresentation::Block:
    case SILFunctionTypeRepresentation::ObjCMethod:
      assert(false && "partial_apply of foreign functions not implemented");
      break;
        
    case SILFunctionTypeRepresentation::WitnessMethod:
      context = emitWitnessTableForLoweredCallee(IGF, fnType, subs);
      break;
    case SILFunctionTypeRepresentation::Thick:
    case SILFunctionTypeRepresentation::Thin:
    case SILFunctionTypeRepresentation::Method:
    case SILFunctionTypeRepresentation::Closure:
      break;
    }
    return std::make_tuple(lv.getStaticFunction().getFunction(),
                           context, v->getType().castTo<SILFunctionType>());
  }
  case LoweredValue::Kind::Explosion: {
    Explosion ex = lv.getExplosion(IGF);
    llvm::Value *fn = ex.claimNext();
    llvm::Value *context = nullptr;
    
    switch (fnType->getRepresentation()) {
    case SILFunctionType::Representation::Thin:
    case SILFunctionType::Representation::Method:
    case SILFunctionType::Representation::Closure:
    case SILFunctionType::Representation::ObjCMethod:
      break;
    case SILFunctionType::Representation::WitnessMethod: {
      llvm::Value *wtable = ex.claimNext();
      assert(wtable->getType() == IGF.IGM.WitnessTablePtrTy);
      context = wtable;
      break;
    }
    case SILFunctionType::Representation::CFunctionPointer:
      break;
    case SILFunctionType::Representation::Thick:
      context = ex.claimNext();
      break;
    case SILFunctionType::Representation::Block:
      llvm_unreachable("partial application of block not implemented");
    }
    
    return std::make_tuple(fn, context, fnType);
  }
  }

  llvm_unreachable("Not a valid SILFunctionType.");
}

void IRGenSILFunction::visitPartialApplyInst(swift::PartialApplyInst *i) {
  SILValue v(i);

  // NB: We collect the arguments under the substituted type.
  auto args = i->getArguments();
  auto params = i->getSubstCalleeType()->getParameters();
  params = params.slice(params.size() - args.size(), args.size());
  
  Explosion llArgs;

  {
    // Lower the parameters in the callee's generic context.
    GenericContextScope scope(IGM, i->getOrigCalleeType()->getGenericSignature());
    for (auto index : indices(args)) {
      assert(args[index]->getType() == IGM.silConv.getSILType(params[index]));
      emitApplyArgument(*this, args[index],
                        IGM.silConv.getSILType(params[index]), llArgs);
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
                               objcMethod,
                               i->getOrigCalleeType(),
                               i->getType().castTo<SILFunctionType>(),
                               selfVal,
                               i->getArguments()[0]->getType(),
                               function);
    setLoweredExplosion(i, function);
    return;
  }
  
  // Get the function value.
  llvm::Value *calleeFn = nullptr;
  llvm::Value *innerContext = nullptr;
  CanSILFunctionType origCalleeTy;
  
  std::tie(calleeFn, innerContext, origCalleeTy)
    = getPartialApplicationFunction(*this, i->getCallee(),
                                    i->getSubstitutions());

  // Create the thunk and function value.
  Explosion function;
  emitFunctionPartialApplication(*this, *CurSILFn,
                                 calleeFn, innerContext, llArgs,
                                 params, i->getSubstitutions(),
                                 origCalleeTy, i->getSubstCalleeType(),
                                 i->getType().castTo<SILFunctionType>(),
                                 function);
  setLoweredExplosion(v, function);
}

void IRGenSILFunction::visitIntegerLiteralInst(swift::IntegerLiteralInst *i) {
  llvm::Value *constant = emitConstantInt(IGM, i);

  Explosion e;
  e.add(constant);
  setLoweredExplosion(i, e);
}

void IRGenSILFunction::visitFloatLiteralInst(swift::FloatLiteralInst *i) {
  llvm::Value *constant = emitConstantFP(IGM, i);
  Explosion e;
  e.add(constant);
  setLoweredExplosion(i, e);
}

void IRGenSILFunction::visitStringLiteralInst(swift::StringLiteralInst *i) {
  llvm::Value *addr;

  // Emit a load of a selector.
  if (i->getEncoding() == swift::StringLiteralInst::Encoding::ObjCSelector)
    addr = emitObjCSelectorRefLoad(i->getValue());
  else
    addr = emitAddrOfConstantString(IGM, i);

  Explosion e;
  e.add(addr);
  setLoweredExplosion(i, e);
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

  // Implicitly autorelease the return value if the function's result
  // convention is autoreleased.
  auto fnConv = CurSILFn->getConventions();
  if (fnConv.getNumDirectSILResults() == 1
      && (fnConv.getDirectSILResults().begin()->getConvention()
          == ResultConvention::Autoreleased)) {
    Explosion temp;
    temp.add(emitObjCAutoreleaseReturnValue(*this, result.claimNext()));
    result = std::move(temp);
  }

  emitReturnInst(*this, i->getOperand()->getType(), result);
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
    return dyn_cast<llvm::ConstantInt>(emitConstantInt(IGF.IGM, IL));
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
      if (casePair.first->getType().getSwiftType()->is<SILFunctionType>()) {
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

  emitSwitchValueDispatch(*this, inst->getOperand()->getType(),
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
    if (!casePair.second->args_empty())
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
  auto &EIS = getEnumImplStrategy(IGM, inst->getOperand()->getType());
  EIS.emitValueSwitch(*this, value, dests, defaultDest);
  
  // Bind arguments for cases that want them.
  for (unsigned i = 0, e = inst->getNumCases(); i < e; ++i) {
    auto casePair = inst->getCase(i);

    if (!casePair.second->args_empty()) {
      auto waypointBB = dests[i].second;
      auto &destLBB = getLoweredBB(casePair.second);
      
      Builder.emitBlock(waypointBB);
      
      Explosion inValue = getLoweredExplosion(inst->getOperand());
      Explosion projected;
      emitProjectLoadableEnum(*this, inst->getOperand()->getType(),
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
  emitSwitchAddressOnlyEnumDispatch(*this, inst->getOperand()->getType(),
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
    if (value->getType().isAddress()) {
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
    
    IntegerLiteralInst *intLit = dyn_cast<IntegerLiteralInst>(casePair.second);
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
    (void)enumValue.claimAll();
    return nullptr;
  }

  // Cast to the result type.
  result = IGF.Builder.CreateIntCast(result, resultType, false);
  if (commonOffset != 0) {
    // The offset, if any.
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
  if (trueValue->getType().isAddress()) {
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
  if (falseValue->getType().isAddress()) {
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
  auto &EIS = getEnumImplStrategy(IGM, inst->getEnumOperand()->getType());
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
  setLoweredValue(inst,
                  getLoweredValueForSelect(*this, result, inst));
}

void IRGenSILFunction::visitSelectEnumAddrInst(SelectEnumAddrInst *inst) {
  Address value = getLoweredAddress(inst->getEnumOperand());
  Explosion result;

  if ((inst->getNumCases() == 1 && inst->hasDefault()) ||
      (inst->getNumCases() == 2 && !inst->hasDefault())) {
    auto &EIS = getEnumImplStrategy(IGM, inst->getEnumOperand()->getType());
    // If this is testing for one case, do simpler codegen.  This is
    // particularly common when testing optionals.
    auto isTrue = EIS.emitIndirectCaseTest(*this,
                                           inst->getEnumOperand()->getType(),
                                           value, inst->getCase(0).first);
    emitSingleEnumMemberSelectResult(*this, inst, isTrue, result);
  } else {
      // Map the SIL dest bbs to their LLVM bbs.
    SmallVector<std::pair<EnumElementDecl*, llvm::BasicBlock*>, 4> dests;
    llvm::BasicBlock *defaultDest;
    llvm::BasicBlock *contBB
      = emitBBMapForSelect(*this, result, dests, defaultDest, inst);
    
    // Emit the dispatch.
    emitSwitchAddressOnlyEnumDispatch(*this, inst->getEnumOperand()->getType(),
                                      value, dests, defaultDest);
    
    // emitBBMapForSelectEnum set up a phi node to receive the result.
    Builder.SetInsertPoint(contBB);
  }
  
  setLoweredValue(inst,
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
  emitSwitchValueDispatch(*this, inst->getOperand()->getType(), value, dests,
                          defaultDest);

  // emitBBMapForSelectEnum set up a continuation block and phi nodes to
  // receive the result.
  Builder.SetInsertPoint(contBB);

  setLoweredValue(inst,
                  getLoweredValueForSelect(*this, result, inst));
}

void IRGenSILFunction::visitDynamicMethodBranchInst(DynamicMethodBranchInst *i){
  LoweredBB &hasMethodBB = getLoweredBB(i->getHasMethodBB());
  LoweredBB &noMethodBB = getLoweredBB(i->getNoMethodBB());

  // Emit the respondsToSelector: call.
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
  
  llvm::Value *respondsToSelector
    = emitObjCSelectorRefLoad("respondsToSelector:");
  
  llvm::Constant *messenger = IGM.getObjCMsgSendFn();
  llvm::Type *argTys[] = {
    IGM.ObjCPtrTy,
    IGM.Int8PtrTy,
    IGM.Int8PtrTy,
  };
  auto respondsToSelectorTy = llvm::FunctionType::get(IGM.Int1Ty,
                                                      argTys,
                                                      /*isVarArg*/ false)
  ->getPointerTo();
  messenger = llvm::ConstantExpr::getBitCast(messenger,
                                             respondsToSelectorTy);
  llvm::CallInst *call = Builder.CreateCall(messenger,
                                        {object, respondsToSelector, loadSel});
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
  SILValue methodArg = i->getHasMethodBB()->args_begin()[0];
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
  cast<LoadableTypeInfo>(getTypeInfo(i->getOperand()->getType()))
      .copy(*this, in, out, i->isAtomic() ? irgen::Atomicity::Atomic
                                          : irgen::Atomicity::NonAtomic);
  (void)out.claimAll();
}

void IRGenSILFunction::visitCopyValueInst(swift::CopyValueInst *i) {
  Explosion in = getLoweredExplosion(i->getOperand());
  Explosion out;
  cast<LoadableTypeInfo>(getTypeInfo(i->getOperand()->getType()))
      .copy(*this, in, out, irgen::Atomicity::Atomic);
  setLoweredExplosion(i, out);
}

// TODO: Implement this more generally for arbitrary values. Currently the
// SIL verifier restricts it to single-refcounted-pointer types.
void IRGenSILFunction::visitAutoreleaseValueInst(swift::AutoreleaseValueInst *i)
{
  Explosion in = getLoweredExplosion(i->getOperand());
  auto val = in.claimNext();
  
  emitObjCAutoreleaseCall(val);
}

void IRGenSILFunction::visitSetDeallocatingInst(SetDeallocatingInst *i) {
  auto *ARI = dyn_cast<AllocRefInst>(i->getOperand());
  if (ARI && StackAllocs.count(ARI)) {
    // A small peep-hole optimization: If the operand is allocated on stack and
    // there is no "significant" code between the set_deallocating and the final
    // dealloc_ref, the set_deallocating is not required.
    //   %0 = alloc_ref [stack]
    //     ...
    //   set_deallocating %0 // not needed
    //     // code which does not depend on the RC_DEALLOCATING_FLAG flag.
    //   dealloc_ref %0      // not needed (stems from the inlined deallocator)
    //     ...
    //   dealloc_ref [stack] %0
    SILBasicBlock::iterator Iter(i);
    SILBasicBlock::iterator End = i->getParent()->end();
    for (++Iter; Iter != End; ++Iter) {
      SILInstruction *I = &*Iter;
      if (auto *DRI = dyn_cast<DeallocRefInst>(I)) {
        if (DRI->getOperand() == ARI) {
          // The set_deallocating is followed by a dealloc_ref -> we can ignore
          // it.
          return;
        }
      }
      // Assume that any instruction with side-effects may depend on the
      // RC_DEALLOCATING_FLAG flag.
      if (I->mayHaveSideEffects())
        break;
    }
  }
  Explosion lowered = getLoweredExplosion(i->getOperand());
  emitNativeSetDeallocating(lowered.claimNext());
}

void IRGenSILFunction::visitReleaseValueInst(swift::ReleaseValueInst *i) {
  Explosion in = getLoweredExplosion(i->getOperand());
  cast<LoadableTypeInfo>(getTypeInfo(i->getOperand()->getType()))
      .consume(*this, in, i->isAtomic() ? irgen::Atomicity::Atomic
                                        : irgen::Atomicity::NonAtomic);
}

void IRGenSILFunction::visitDestroyValueInst(swift::DestroyValueInst *i) {
  Explosion in = getLoweredExplosion(i->getOperand());
  cast<LoadableTypeInfo>(getTypeInfo(i->getOperand()->getType()))
      .consume(*this, in, irgen::Atomicity::Atomic);
}

void IRGenSILFunction::visitStructInst(swift::StructInst *i) {
  Explosion out;
  for (SILValue elt : i->getElements())
    out.add(getLoweredExplosion(elt).claimAll());
  setLoweredExplosion(i, out);
}

void IRGenSILFunction::visitTupleInst(swift::TupleInst *i) {
  Explosion out;
  for (SILValue elt : i->getElements())
    out.add(getLoweredExplosion(elt).claimAll());
  setLoweredExplosion(i, out);
}

void IRGenSILFunction::visitEnumInst(swift::EnumInst *i) {
  Explosion data = (i->hasOperand())
    ? getLoweredExplosion(i->getOperand())
    : Explosion();
  Explosion out;
  emitInjectLoadableEnum(*this, i->getType(), i->getElement(), data, out);
  setLoweredExplosion(i, out);
}

void IRGenSILFunction::visitInitEnumDataAddrInst(swift::InitEnumDataAddrInst *i) {
  Address enumAddr = getLoweredAddress(i->getOperand());
  Address dataAddr = emitProjectEnumAddressForStore(*this,
                                                     i->getOperand()->getType(),
                                                     enumAddr,
                                                     i->getElement());
  setLoweredAddress(i, dataAddr);
}

void IRGenSILFunction::visitUncheckedEnumDataInst(swift::UncheckedEnumDataInst *i) {
  Explosion enumVal = getLoweredExplosion(i->getOperand());
  Explosion data;
  emitProjectLoadableEnum(*this, i->getOperand()->getType(),
                          enumVal, i->getElement(), data);
  setLoweredExplosion(i, data);
}

void IRGenSILFunction::visitUncheckedTakeEnumDataAddrInst(swift::UncheckedTakeEnumDataAddrInst *i) {
  Address enumAddr = getLoweredAddress(i->getOperand());
  Address dataAddr = emitDestructiveProjectEnumAddressForLoad(*this,
                                                    i->getOperand()->getType(),
                                                    enumAddr,
                                                    i->getElement());
  setLoweredAddress(i, dataAddr);
}

void IRGenSILFunction::visitInjectEnumAddrInst(swift::InjectEnumAddrInst *i) {
  Address enumAddr = getLoweredAddress(i->getOperand());
  emitStoreEnumTagToAddress(*this, i->getOperand()->getType(),
                             enumAddr, i->getElement());
}

void IRGenSILFunction::visitTupleExtractInst(swift::TupleExtractInst *i) {
  Explosion fullTuple = getLoweredExplosion(i->getOperand());
  Explosion output;
  SILType baseType = i->getOperand()->getType();
  
  projectTupleElementFromExplosion(*this,
                                   baseType,
                                   fullTuple,
                                   i->getFieldNo(),
                                   output);
  (void)fullTuple.claimAll();
  setLoweredExplosion(i, output);
}

void IRGenSILFunction::visitTupleElementAddrInst(swift::TupleElementAddrInst *i)
{
  Address base = getLoweredAddress(i->getOperand());
  SILType baseType = i->getOperand()->getType();

  Address field = projectTupleElementAddress(*this, base, baseType,
                                             i->getFieldNo());
  setLoweredAddress(i, field);
}

void IRGenSILFunction::visitStructExtractInst(swift::StructExtractInst *i) {
  Explosion operand = getLoweredExplosion(i->getOperand());
  Explosion lowered;
  SILType baseType = i->getOperand()->getType();
  
  projectPhysicalStructMemberFromExplosion(*this,
                                           baseType,
                                           operand,
                                           i->getField(),
                                           lowered);

  (void)operand.claimAll();
  setLoweredExplosion(i, lowered);
}

void IRGenSILFunction::visitStructElementAddrInst(
                                              swift::StructElementAddrInst *i) {
  Address base = getLoweredAddress(i->getOperand());
  SILType baseType = i->getOperand()->getType();

  Address field = projectPhysicalStructMemberAddress(*this, base, baseType,
                                                     i->getField());
  setLoweredAddress(i, field);
}

void IRGenSILFunction::visitRefElementAddrInst(swift::RefElementAddrInst *i) {
  Explosion base = getLoweredExplosion(i->getOperand());
  llvm::Value *value = base.claimNext();

  SILType baseTy = i->getOperand()->getType();
  Address field = projectPhysicalClassMemberAddress(*this,
                                                    value,
                                                    baseTy,
                                                    i->getType(),
                                                    i->getField())
    .getAddress();
  setLoweredAddress(i, field);
}

void IRGenSILFunction::visitRefTailAddrInst(RefTailAddrInst *i) {
  SILValue Ref = i->getOperand();
  llvm::Value *RefValue = getLoweredExplosion(Ref).claimNext();

  Address TailAddr = emitTailProjection(*this, RefValue, Ref->getType(),
                                            i->getTailType());
  setLoweredAddress(i, TailAddr);
}

void IRGenSILFunction::visitLoadInst(swift::LoadInst *i) {
  Explosion lowered;
  Address source = getLoweredAddress(i->getOperand());
  SILType objType = i->getType().getObjectType();
  const auto &typeInfo = cast<LoadableTypeInfo>(getTypeInfo(objType));

  switch (i->getOwnershipQualifier()) {
  case LoadOwnershipQualifier::Unqualified:
  case LoadOwnershipQualifier::Trivial:
  case LoadOwnershipQualifier::Take:
    typeInfo.loadAsTake(*this, source, lowered);
    break;
  case LoadOwnershipQualifier::Copy:
    typeInfo.loadAsCopy(*this, source, lowered);
    break;
  }

  setLoweredExplosion(i, lowered);
}

void IRGenSILFunction::visitStoreInst(swift::StoreInst *i) {
  Explosion source = getLoweredExplosion(i->getSrc());
  Address dest = getLoweredAddress(i->getDest());
  SILType objType = i->getSrc()->getType().getObjectType();

  const auto &typeInfo = cast<LoadableTypeInfo>(getTypeInfo(objType));
  switch (i->getOwnershipQualifier()) {
  case StoreOwnershipQualifier::Unqualified:
  case StoreOwnershipQualifier::Init:
  case StoreOwnershipQualifier::Trivial:
    typeInfo.initialize(*this, source, dest);
    break;
  case StoreOwnershipQualifier::Assign:
    typeInfo.assign(*this, source, dest);
    break;
  }
}

/// Emit the artificial error result argument.
void IRGenSILFunction::emitErrorResultVar(SILResultInfo ErrorInfo,
                                          DebugValueInst *DbgValue) {
  auto ErrorResultSlot = getErrorResultSlot(IGM.silConv.getSILType(ErrorInfo));
  SILDebugVariable Var = DbgValue->getVarInfo();
  auto Storage = emitShadowCopy(ErrorResultSlot.getAddress(), getDebugScope(),
                                Var.Name, Var.ArgNo);
  DebugTypeInfo DTI(nullptr, ErrorInfo.getType(), ErrorResultSlot->getType(),
                    IGM.getPointerSize(), IGM.getPointerAlignment());
  IGM.DebugInfo->emitVariableDeclaration(Builder, Storage, DTI, getDebugScope(),
                                         nullptr, Var.Name, Var.ArgNo,
                                         IndirectValue, ArtificialValue);
}

void IRGenSILFunction::visitDebugValueInst(DebugValueInst *i) {
  if (!IGM.DebugInfo)
    return;

  auto SILVal = i->getOperand();
  if (isa<SILUndef>(SILVal)) {
    // We cannot track the location of inlined error arguments because it has no
    // representation in SIL.
    if (!i->getDebugScope()->InlinedCallSite &&
        i->getVarInfo().Name == "$error") {
      auto funcTy = CurSILFn->getLoweredFunctionType();
      emitErrorResultVar(funcTy->getErrorResult(), i);
    }
    return;
  }

  StringRef Name = getVarName(i);
  DebugTypeInfo DbgTy;
  SILType SILTy = SILVal->getType();
  // An inout/lvalue type that is described by a debug value has been
  // promoted by an optimization pass. Unwrap the type.
  bool Unwrap = true;
  auto RealTy = SILVal->getType().getSwiftType();
  if (VarDecl *Decl = i->getDecl()) {
    DbgTy = DebugTypeInfo::getLocalVariable(
        CurSILFn->getDeclContext(), Decl, RealTy,
        getTypeInfo(SILVal->getType()), Unwrap);
  } else if (i->getFunction()->isBare() &&
             !SILTy.getSwiftType()->hasArchetype() && !Name.empty()) {
    // Preliminary support for .sil debug information.
    DbgTy = DebugTypeInfo::getFromTypeInfo(CurSILFn->getDeclContext(), RealTy,
                                           getTypeInfo(SILTy));
    if (Unwrap)
      DbgTy.unwrapLValueOrInOutType();
  } else
    return;

  // Put the value into a stack slot at -Onone.
  llvm::SmallVector<llvm::Value *, 8> Copy; 
  Explosion e = getLoweredExplosion(SILVal);
  unsigned ArgNo = i->getVarInfo().ArgNo;
  emitShadowCopy(e.claimAll(), i->getDebugScope(), Name, ArgNo, Copy);
  emitDebugVariableDeclaration(Copy, DbgTy, SILTy, i->getDebugScope(),
                               i->getDecl(), Name, ArgNo);
}

void IRGenSILFunction::visitDebugValueAddrInst(DebugValueAddrInst *i) {
  if (!IGM.DebugInfo)
    return;
  VarDecl *Decl = i->getDecl();
  if (!Decl)
    return;

  auto SILVal = i->getOperand();
  if (isa<SILUndef>(SILVal))
    return;

  StringRef Name = getVarName(i);
  auto Addr = getLoweredAddress(SILVal).getAddress();
  SILType SILTy = SILVal->getType();
  auto RealType = SILTy.getSwiftType();
  // Unwrap implicitly indirect types and types that are passed by
  // reference only at the SIL level and below.
  //
  // FIXME: Should this check if the lowered SILType is address only
  // instead? Otherwise optionals of archetypes etc will still have
  // 'Unwrap' set to false.
  bool Unwrap =
      i->getVarInfo().Constant ||
      RealType->getLValueOrInOutObjectType()->is<ArchetypeType>();
  auto DbgTy = DebugTypeInfo::getLocalVariable(
      CurSILFn->getDeclContext(), Decl, RealType,
      getTypeInfo(SILVal->getType()), Unwrap);
  // Put the value's address into a stack slot at -Onone and emit a debug
  // intrinsic.
  unsigned ArgNo = i->getVarInfo().ArgNo;
  emitDebugVariableDeclaration(
      emitShadowCopy(Addr, i->getDebugScope(), Name, ArgNo), DbgTy,
      i->getType(), i->getDebugScope(), Decl, Name, ArgNo,
      DbgTy.isImplicitlyIndirect() ? DirectValue : IndirectValue);
}

void IRGenSILFunction::visitLoadWeakInst(swift::LoadWeakInst *i) {
  Address source = getLoweredAddress(i->getOperand());
  auto &weakTI = cast<WeakTypeInfo>(getTypeInfo(i->getOperand()->getType()));

  Explosion result;
  if (i->isTake()) {
    weakTI.weakTakeStrong(*this, source, result);
  } else {
    weakTI.weakLoadStrong(*this, source, result);
  }

  setLoweredExplosion(i, result);
}

void IRGenSILFunction::visitStoreWeakInst(swift::StoreWeakInst *i) {
  Explosion source = getLoweredExplosion(i->getSrc());
  Address dest = getLoweredAddress(i->getDest());

  auto &weakTI = cast<WeakTypeInfo>(getTypeInfo(i->getDest()->getType()));
  if (i->isInitializationOfDest()) {
    weakTI.weakInit(*this, source, dest);
  } else {
    weakTI.weakAssign(*this, source, dest);
  }
}

void IRGenSILFunction::visitFixLifetimeInst(swift::FixLifetimeInst *i) {
  if (i->getOperand()->getType().isAddress()) {
    // Just pass in the address to fix lifetime if we have one. We will not do
    // anything to it so nothing bad should happen.
    emitFixLifetime(getLoweredAddress(i->getOperand()).getAddress());
    return;
  }

  // Handle objects.
  Explosion in = getLoweredExplosion(i->getOperand());
  cast<LoadableTypeInfo>(getTypeInfo(i->getOperand()->getType()))
    .fixLifetime(*this, in);
}

void IRGenSILFunction::visitMarkDependenceInst(swift::MarkDependenceInst *i) {
  // Dependency-marking is purely for SIL.  Just forward the input as
  // the result.

  SILValue value = i->getValue();
  if (value->getType().isAddress()) {
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
  setLoweredExplosion(i, result);
}

void IRGenSILFunction::visitStrongPinInst(swift::StrongPinInst *i) {
  Explosion lowered = getLoweredExplosion(i->getOperand());
  llvm::Value *object = lowered.claimNext();
  llvm::Value *pinHandle =
      emitNativeTryPin(object, i->isAtomic() ? irgen::Atomicity::Atomic
                                             : irgen::Atomicity::NonAtomic);

  Explosion result;
  result.add(pinHandle);
  setLoweredExplosion(i, result);
}

void IRGenSILFunction::visitStrongUnpinInst(swift::StrongUnpinInst *i) {
  Explosion lowered = getLoweredExplosion(i->getOperand());
  llvm::Value *pinHandle = lowered.claimNext();
  emitNativeUnpin(pinHandle, i->isAtomic() ? irgen::Atomicity::Atomic
                                           : irgen::Atomicity::NonAtomic);
}

void IRGenSILFunction::visitStrongRetainInst(swift::StrongRetainInst *i) {
  Explosion lowered = getLoweredExplosion(i->getOperand());
  auto &ti = cast<ReferenceTypeInfo>(getTypeInfo(i->getOperand()->getType()));
  ti.strongRetain(*this, lowered, i->isAtomic() ? irgen::Atomicity::Atomic
                                                : irgen::Atomicity::NonAtomic);
}

void IRGenSILFunction::visitStrongReleaseInst(swift::StrongReleaseInst *i) {
  Explosion lowered = getLoweredExplosion(i->getOperand());
  auto &ti = cast<ReferenceTypeInfo>(getTypeInfo(i->getOperand()->getType()));
  ti.strongRelease(*this, lowered, i->isAtomic() ? irgen::Atomicity::Atomic
                                                 : irgen::Atomicity::NonAtomic);
}

/// Given a SILType which is a ReferenceStorageType, return the type
/// info for the underlying reference type.
static const ReferenceTypeInfo &getReferentTypeInfo(IRGenFunction &IGF,
                                                    SILType silType) {
  auto type = silType.castTo<ReferenceStorageType>().getReferentType();
  return cast<ReferenceTypeInfo>(IGF.getTypeInfoForLowered(type));
}

void IRGenSILFunction::
visitStrongRetainUnownedInst(swift::StrongRetainUnownedInst *i) {
  Explosion lowered = getLoweredExplosion(i->getOperand());
  auto &ti = getReferentTypeInfo(*this, i->getOperand()->getType());
  ti.strongRetainUnowned(*this, lowered);
}

void IRGenSILFunction::visitUnownedRetainInst(swift::UnownedRetainInst *i) {
  Explosion lowered = getLoweredExplosion(i->getOperand());
  auto &ti = getReferentTypeInfo(*this, i->getOperand()->getType());
  ti.unownedRetain(*this, lowered);
}

void IRGenSILFunction::visitUnownedReleaseInst(swift::UnownedReleaseInst *i) {
  Explosion lowered = getLoweredExplosion(i->getOperand());
  auto &ti = getReferentTypeInfo(*this, i->getOperand()->getType());
  ti.unownedRelease(*this, lowered);
}

void IRGenSILFunction::visitLoadUnownedInst(swift::LoadUnownedInst *i) {
  Address source = getLoweredAddress(i->getOperand());
  auto &ti = getReferentTypeInfo(*this, i->getOperand()->getType());

  Explosion result;
  if (i->isTake()) {
    ti.unownedTakeStrong(*this, source, result);
  } else {
    ti.unownedLoadStrong(*this, source, result);
  }

  setLoweredExplosion(i, result);
}

void IRGenSILFunction::visitStoreUnownedInst(swift::StoreUnownedInst *i) {
  Explosion source = getLoweredExplosion(i->getSrc());
  Address dest = getLoweredAddress(i->getDest());

  auto &ti = getReferentTypeInfo(*this, i->getDest()->getType());
  if (i->isInitializationOfDest()) {
    ti.unownedInit(*this, source, dest);
  } else {
    ti.unownedAssign(*this, source, dest);
  }
}

static bool hasReferenceSemantics(IRGenSILFunction &IGF,
                                  SILType silType) {
  auto operType = silType.getSwiftRValueType();
  auto valueType = operType->getAnyOptionalObjectType();
  auto objType = valueType ? valueType : operType;
  return (objType->mayHaveSuperclass()
          || objType->isClassExistentialType()
          || objType->is<BuiltinNativeObjectType>()
          || objType->is<BuiltinBridgeObjectType>()
          || objType->is<BuiltinUnknownObjectType>());
}

static llvm::Value *emitIsUnique(IRGenSILFunction &IGF, SILValue operand,
                                 SourceLoc loc, bool checkPinned) {
  if (!hasReferenceSemantics(IGF, operand->getType())) {
    llvm::Function *trapIntrinsic = llvm::Intrinsic::getDeclaration(
      &IGF.IGM.Module, llvm::Intrinsic::ID::trap);
    IGF.Builder.CreateCall(trapIntrinsic, {});
    return llvm::UndefValue::get(IGF.IGM.Int1Ty);
  }

  auto &operTI = cast<LoadableTypeInfo>(IGF.getTypeInfo(operand->getType()));
  LoadedRef ref =
    operTI.loadRefcountedPtr(IGF, loc, IGF.getLoweredAddress(operand));

  return
    IGF.emitIsUniqueCall(ref.getValue(), loc, ref.isNonNull(), checkPinned);
}

void IRGenSILFunction::visitIsUniqueInst(swift::IsUniqueInst *i) {
  llvm::Value *result = emitIsUnique(*this, i->getOperand(),
                                     i->getLoc().getSourceLoc(), false);
  Explosion out;
  out.add(result);
  setLoweredExplosion(i, out);
}

void IRGenSILFunction::
visitIsUniqueOrPinnedInst(swift::IsUniqueOrPinnedInst *i) {
  llvm::Value *result = emitIsUnique(*this, i->getOperand(),
                                     i->getLoc().getSourceLoc(), true);
  Explosion out;
  out.add(result);
  setLoweredExplosion(i, out);
}

static bool tryDeferFixedSizeBufferInitialization(IRGenSILFunction &IGF,
                                                  SILInstruction *allocInst,
                                                  const TypeInfo &ti,
                                                  Address fixedSizeBuffer,
                                                  const llvm::Twine &name) {
  // There's no point in doing this for fixed-sized types, since we'll allocate
  // an appropriately-sized buffer for them statically.
  if (ti.isFixedSize())
    return false;

  // TODO: More interesting dominance analysis could be done here to see
  // if the alloc_stack is dominated by copy_addrs into it on all paths.
  // For now, check only that the copy_addr is the first use within the same
  // block.
  for (auto ii = std::next(allocInst->getIterator()),
            ie = std::prev(allocInst->getParent()->end());
       ii != ie; ++ii) {
    auto *inst = &*ii;

    // Does this instruction use the allocation? If not, continue.
    auto Ops = inst->getAllOperands();
    if (std::none_of(Ops.begin(), Ops.end(),
                     [allocInst](const Operand &Op) {
                       return Op.get() == allocInst;
                     }))
      continue;

    // Is this a copy?
    auto *copy = dyn_cast<swift::CopyAddrInst>(inst);
    if (!copy)
      return false;

    // Destination must be the allocation.
    if (copy->getDest() != SILValue(allocInst))
      return false;

    // Copy must be an initialization.
    if (!copy->isInitializationOfDest())
      return false;
    
    // We can defer to this initialization. Allocate the fixed-size buffer
    // now, but don't allocate the value inside it.
    if (!fixedSizeBuffer.getAddress()) {
      fixedSizeBuffer = IGF.createFixedSizeBufferAlloca(name);
      IGF.Builder.CreateLifetimeStart(fixedSizeBuffer,
                                      getFixedBufferSize(IGF.IGM));
    }
    IGF.setContainerOfUnallocatedAddress(allocInst, fixedSizeBuffer);
    return true;
  }
  
  return false;
}

void IRGenSILFunction::emitDebugInfoForAllocStack(AllocStackInst *i,
                                                  const TypeInfo &type,
                                                  llvm::Value *addr) {
  VarDecl *Decl = i->getDecl();
  if (IGM.DebugInfo && Decl) {
    // Ignore compiler-generated patterns but not optional bindings.
    if (auto *Pattern = Decl->getParentPattern())
      if (Pattern->isImplicit() &&
          Pattern->getKind() != PatternKind::OptionalSome)
        return;

    // Discard any inout or lvalue qualifiers. Since the object itself
    // is stored in the alloca, emitting it as a reference type would
    // be wrong.
    bool Unwrap = true;
    SILType SILTy = i->getType();
    auto RealType = SILTy.getSwiftType().getLValueOrInOutObjectType();
    auto DbgTy = DebugTypeInfo::getLocalVariable(CurSILFn->getDeclContext(),
                                                 Decl, RealType, type, Unwrap);
    StringRef Name = getVarName(i);
    if (auto DS = i->getDebugScope())
      emitDebugVariableDeclaration(addr, DbgTy, SILTy, DS, Decl, Name,
                                   i->getVarInfo().ArgNo);
  }
}

void IRGenSILFunction::visitAllocStackInst(swift::AllocStackInst *i) {
  const TypeInfo &type = getTypeInfo(i->getElementType());

  // Derive name from SIL location.
  VarDecl *Decl = i->getDecl();
  StringRef dbgname;
# ifndef NDEBUG
  // If this is a DEBUG build, use pretty names for the LLVM IR.
  dbgname = getVarName(i);
# endif

  (void) Decl;

  bool isEntryBlock =
      i->getParentBlock() == i->getFunction()->getEntryBlock();
  auto addr =
      type.allocateStack(*this, i->getElementType(), isEntryBlock, dbgname);

  emitDebugInfoForAllocStack(i, type, addr.getAddress().getAddress());
  
  setLoweredStackAddress(i, addr);
}

static void
buildTailArrays(IRGenSILFunction &IGF,
                SmallVectorImpl<std::pair<SILType, llvm::Value *>> &TailArrays,
                AllocRefInstBase *ARI) {
  auto Types = ARI->getTailAllocatedTypes();
  auto Counts = ARI->getTailAllocatedCounts();
  for (unsigned Idx = 0, NumTypes = Types.size(); Idx < NumTypes; ++Idx) {
    Explosion ElemCount = IGF.getLoweredExplosion(Counts[Idx].get());
    TailArrays.push_back({Types[Idx], ElemCount.claimNext()});
  }
}

void IRGenSILFunction::visitAllocRefInst(swift::AllocRefInst *i) {
  int StackAllocSize = -1;
  if (i->canAllocOnStack()) {
    estimateStackSize();
    // Is there enough space for stack allocation?
    StackAllocSize = IGM.IRGen.Opts.StackPromotionSizeLimit - EstimatedStackSize;
  }
  SmallVector<std::pair<SILType, llvm::Value *>, 4> TailArrays;
  buildTailArrays(*this, TailArrays, i);

  llvm::Value *alloced = emitClassAllocation(*this, i->getType(), i->isObjC(),
                                             StackAllocSize, TailArrays);
  if (StackAllocSize >= 0) {
    // Remember that this alloc_ref allocates the object on the stack.

    StackAllocs.insert(i);
    EstimatedStackSize += StackAllocSize;
  }
  Explosion e;
  e.add(alloced);
  setLoweredExplosion(i, e);
}

void IRGenSILFunction::visitAllocRefDynamicInst(swift::AllocRefDynamicInst *i) {
  SmallVector<std::pair<SILType, llvm::Value *>, 4> TailArrays;
  buildTailArrays(*this, TailArrays, i);

  Explosion metadata = getLoweredExplosion(i->getMetatypeOperand());
  auto metadataValue = metadata.claimNext();
  llvm::Value *alloced = emitClassAllocationDynamic(*this, metadataValue,
                                                    i->getType(), i->isObjC(),
                                                    TailArrays);
  Explosion e;
  e.add(alloced);
  setLoweredExplosion(i, e);
}

void IRGenSILFunction::visitDeallocStackInst(swift::DeallocStackInst *i) {
  auto allocatedType = i->getOperand()->getType();
  const TypeInfo &allocatedTI = getTypeInfo(allocatedType);
  StackAddress stackAddr = getLoweredStackAddress(i->getOperand());

  allocatedTI.deallocateStack(*this, stackAddr, allocatedType);
}

void IRGenSILFunction::visitDeallocRefInst(swift::DeallocRefInst *i) {
  // Lower the operand.
  Explosion self = getLoweredExplosion(i->getOperand());
  auto selfValue = self.claimNext();
  auto *ARI = dyn_cast<AllocRefInst>(i->getOperand());
  if (!i->canAllocOnStack()) {
    if (ARI && StackAllocs.count(ARI)) {
      // We can ignore dealloc_refs (without [stack]) for stack allocated
      // objects.
      //
      //   %0 = alloc_ref [stack]
      //     ...
      //   dealloc_ref %0     // not needed (stems from the inlined deallocator)
      //     ...
      //   dealloc_ref [stack] %0
      return;
    }

    auto classType = i->getOperand()->getType();
    emitClassDeallocation(*this, classType, selfValue);
    return;
  }
  // It's a dealloc_ref [stack]. Even if the alloc_ref did not allocate the
  // object on the stack, we don't have to deallocate it, because it is
  // deallocated in the final release.
  assert(ARI->canAllocOnStack());
  if (StackAllocs.count(ARI)) {
    if (IGM.IRGen.Opts.EmitStackPromotionChecks) {
      selfValue = Builder.CreateBitCast(selfValue, IGM.RefCountedPtrTy);
      emitVerifyEndOfLifetimeCall(selfValue);
    } else {
      // This has two purposes:
      // 1. Tell LLVM the lifetime of the allocated stack memory.
      // 2. Avoid tail-call optimization which may convert the call to the final
      //    release to a jump, which is done after the stack frame is
      //    destructed.
      Builder.CreateLifetimeEnd(selfValue);
    }
  }
}

void IRGenSILFunction::visitDeallocPartialRefInst(swift::DeallocPartialRefInst *i) {
  Explosion self = getLoweredExplosion(i->getInstance());
  auto selfValue = self.claimNext();
  Explosion metadata = getLoweredExplosion(i->getMetatype());
  auto metadataValue = metadata.claimNext();
  auto classType = i->getInstance()->getType();

  emitPartialClassDeallocation(*this, classType, selfValue, metadataValue);
}

void IRGenSILFunction::visitDeallocBoxInst(swift::DeallocBoxInst *i) {
  Explosion owner = getLoweredExplosion(i->getOperand());
  llvm::Value *ownerPtr = owner.claimNext();

  auto boxTy = i->getOperand()->getType().castTo<SILBoxType>();
  emitDeallocateBox(*this, ownerPtr, boxTy);
}

void IRGenSILFunction::visitAllocBoxInst(swift::AllocBoxInst *i) {
  assert(i->getBoxType()->getLayout()->getFields().size() == 1
         && "multi field boxes not implemented yet");
  const TypeInfo &type = getTypeInfo(i->getBoxType()
                                      ->getFieldType(IGM.getSILModule(), 0));

  // Derive name from SIL location.
  VarDecl *Decl = i->getDecl();
  StringRef Name = getVarName(i);
  StringRef DbgName =
# ifndef NDEBUG
    // If this is a DEBUG build, use pretty names for the LLVM IR.
    Name;
# else
    "";
# endif

  auto boxTy = i->getType().castTo<SILBoxType>();
  OwnedAddress boxWithAddr = emitAllocateBox(*this, boxTy,
                                             CurSILFn->getGenericEnvironment(),
                                             DbgName);
  setLoweredBox(i, boxWithAddr);

  if (IGM.DebugInfo && Decl) {
    // FIXME: This is a workaround to not produce local variables for
    // capture list arguments like "[weak self]". The better solution
    // would be to require all variables to be described with a
    // SILDebugValue(Addr) and then not describe capture list
    // arguments.
    if (Name == IGM.Context.Id_self.str())
      return;

    assert(i->getBoxType()->getLayout()->getFields().size() == 1
           && "box for a local variable should only have one field");
    auto DbgTy = DebugTypeInfo::getLocalVariable(
        CurSILFn->getDeclContext(), Decl,
        i->getBoxType()->getFieldType(IGM.getSILModule(), 0).getSwiftType(),
          type, /*Unwrap=*/false);

    if (isInlinedGeneric(Decl, i->getDebugScope()))
      return;

    IGM.DebugInfo->emitVariableDeclaration(
        Builder,
        emitShadowCopy(boxWithAddr.getAddress(), i->getDebugScope(), Name, 0),
        DbgTy, i->getDebugScope(), Decl, Name, 0,
        DbgTy.isImplicitlyIndirect() ? DirectValue : IndirectValue);
  }
}

void IRGenSILFunction::visitProjectBoxInst(swift::ProjectBoxInst *i) {
  auto boxTy = i->getOperand()->getType().castTo<SILBoxType>();

  const LoweredValue &val = getLoweredValue(i->getOperand());
  if (val.isBoxWithAddress()) {
    // The operand is an alloc_box. We can directly reuse the address.
    setLoweredAddress(i, val.getAddressOfBox());
  } else {
    // The slow-path: we have to emit code to get from the box to it's
    // value address.
    Explosion box = val.getExplosion(*this);
    auto addr = emitProjectBox(*this, box.claimNext(), boxTy);
    setLoweredAddress(i, addr);
  }
}

void IRGenSILFunction::visitConvertFunctionInst(swift::ConvertFunctionInst *i) {
  // This instruction is specified to be a no-op.
  Explosion temp = getLoweredExplosion(i->getOperand());
  setLoweredExplosion(i, temp);
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
  setLoweredExplosion(i, to);
}

// Ignores the isStrict flag because Swift TBAA is not lowered into LLVM IR.
void IRGenSILFunction::visitPointerToAddressInst(swift::PointerToAddressInst *i)
{
  Explosion from = getLoweredExplosion(i->getOperand());
  llvm::Value *ptrValue = from.claimNext();

  auto &ti = getTypeInfo(i->getType());
  
  llvm::Type *destType = ti.getStorageType()->getPointerTo();
  ptrValue = Builder.CreateBitCast(ptrValue, destType);
  
  setLoweredAddress(i,
                    ti.getAddressForPointer(ptrValue));
}

static void emitPointerCastInst(IRGenSILFunction &IGF,
                                SILValue src,
                                SILValue dest,
                                const TypeInfo &ti) {
  Explosion from = IGF.getLoweredExplosion(src);
  llvm::Value *ptrValue = from.claimNext();
  // The input may have witness tables or other additional data, but the class
  // reference is always first.
  (void)from.claimAll();

  auto schema = ti.getSchema();
  assert(schema.size() == 1
         && schema[0].isScalar()
         && "pointer schema is not a single scalar?!");
  auto castToType = schema[0].getScalarType();

  // A retainable pointer representation may be wrapped in an optional, so we
  // need to provide inttoptr/ptrtoint in addition to bitcast.
  ptrValue = IGF.Builder.CreateBitOrPointerCast(ptrValue, castToType);
  
  Explosion to;
  to.add(ptrValue);
  IGF.setLoweredExplosion(dest, to);
}

void IRGenSILFunction::visitUncheckedRefCastInst(
                                             swift::UncheckedRefCastInst *i) {
  auto &ti = getTypeInfo(i->getType());
  emitPointerCastInst(*this, i->getOperand(), i, ti);
}

// TODO: Although runtime checks are not required, we get them anyway when
// asking the runtime to perform this cast. If this is a performance impact, we
// can add a CheckedCastMode::Unchecked.
void IRGenSILFunction::
visitUncheckedRefCastAddrInst(swift::UncheckedRefCastAddrInst *i) {
  Address dest = getLoweredAddress(i->getDest());
  Address src = getLoweredAddress(i->getSrc());
  emitCheckedCast(*this, src, i->getSourceType(), dest, i->getTargetType(),
                  i->getConsumptionKind(), CheckedCastMode::Unconditional);
}

void IRGenSILFunction::visitUncheckedAddrCastInst(
                                             swift::UncheckedAddrCastInst *i) {
  auto addr = getLoweredAddress(i->getOperand());
  auto &ti = getTypeInfo(i->getType());
  auto result = Builder.CreateBitCast(addr,ti.getStorageType()->getPointerTo());
  setLoweredAddress(i, result);
}

static bool isStructurallySame(const llvm::Type *T1, const llvm::Type *T2) {
  if (T1 == T2) return true;
  
  if (auto *S1 = dyn_cast<llvm::StructType>(T1))
    if (auto *S2 = dyn_cast<llvm::StructType>(T2))
      return S1->isLayoutIdentical(const_cast<llvm::StructType*>(S2));
  return false;
}

// Emit a trap in the event a type does not match expected layout constraints.
// 
// We can hit this case in specialized functions even for correct user code.
// If the user dynamically checks for correct type sizes in the generic
// function, a specialized function can contain the (not executed) bitcast
// with mismatching fixed sizes.
// Usually llvm can eliminate this code again because the user's safety
// check should be constant foldable on llvm level.
static void emitTrapAndUndefValue(IRGenSILFunction &IGF,
                                  Explosion &in,
                                  Explosion &out,
                                  const LoadableTypeInfo &outTI) {
  llvm::BasicBlock *failBB =
    llvm::BasicBlock::Create(IGF.IGM.getLLVMContext());
  IGF.Builder.CreateBr(failBB);
  IGF.FailBBs.push_back(failBB);
  
  IGF.Builder.emitBlock(failBB);
  llvm::Function *trapIntrinsic = llvm::Intrinsic::getDeclaration(
    &IGF.IGM.Module, llvm::Intrinsic::ID::trap);
  IGF.Builder.CreateCall(trapIntrinsic, {});
  IGF.Builder.CreateUnreachable();

  llvm::BasicBlock *contBB = llvm::BasicBlock::Create(IGF.IGM.getLLVMContext());
  IGF.Builder.emitBlock(contBB);
  (void)in.claimAll();
  for (auto schema : outTI.getSchema())
    out.add(llvm::UndefValue::get(schema.getScalarType()));
}

static void emitUncheckedValueBitCast(IRGenSILFunction &IGF,
                                      SourceLoc loc,
                                      Explosion &in,
                                      const LoadableTypeInfo &inTI,
                                      Explosion &out,
                                      const LoadableTypeInfo &outTI) {
  // If the transfer is doable bitwise, and if the elements of the explosion are
  // the same type, then just transfer the elements.
  if (inTI.isBitwiseTakable(ResilienceExpansion::Maximal) &&
      outTI.isBitwiseTakable(ResilienceExpansion::Maximal) &&
      isStructurallySame(inTI.getStorageType(), outTI.getStorageType())) {
    in.transferInto(out, in.size());
    return;
  }
  
  // TODO: We could do bitcasts entirely in the value domain in some cases, but
  // for simplicity, let's just always go through the stack for now.
  
  // Create the allocation.
  auto inStorage = IGF.createAlloca(inTI.getStorageType(),
                                  std::max(inTI.getFixedAlignment(),
                                           outTI.getFixedAlignment()),
                                  "bitcast");
  
  auto maxSize = std::max(inTI.getFixedSize(), outTI.getFixedSize());
  IGF.Builder.CreateLifetimeStart(inStorage, maxSize);
  
  // Store the 'in' value.
  inTI.initialize(IGF, in, inStorage);
  // Load the 'out' value as the destination type.
  auto outStorage = IGF.Builder.CreateBitCast(inStorage,
                                        outTI.getStorageType()->getPointerTo());
  outTI.loadAsTake(IGF, outStorage, out);
  
  IGF.Builder.CreateLifetimeEnd(inStorage, maxSize);
  return;
}

static void emitValueBitwiseCast(IRGenSILFunction &IGF,
                                 SourceLoc loc,
                                 Explosion &in,
                                 const LoadableTypeInfo &inTI,
                                 Explosion &out,
                                 const LoadableTypeInfo &outTI) {
  // Unfortunately, we can't check this invariant until we get to IRGen, since
  // the AST and SIL don't know anything about type layout.
  if (inTI.getFixedSize() < outTI.getFixedSize()) {
    emitTrapAndUndefValue(IGF, in, out, outTI);
    return;
  }
  emitUncheckedValueBitCast(IGF, loc, in, inTI, out, outTI);
}

void IRGenSILFunction::visitUncheckedTrivialBitCastInst(
                                      swift::UncheckedTrivialBitCastInst *i) {
  Explosion in = getLoweredExplosion(i->getOperand());
  Explosion out;
  
  emitValueBitwiseCast(*this, i->getLoc().getSourceLoc(),
            in,  cast<LoadableTypeInfo>(getTypeInfo(i->getOperand()->getType())),
            out, cast<LoadableTypeInfo>(getTypeInfo(i->getType())));
  
  setLoweredExplosion(i, out);
}

void IRGenSILFunction::
visitUncheckedBitwiseCastInst(swift::UncheckedBitwiseCastInst *i) {
  Explosion in = getLoweredExplosion(i->getOperand());
  Explosion out;

  emitValueBitwiseCast(*this, i->getLoc().getSourceLoc(),
            in,  cast<LoadableTypeInfo>(getTypeInfo(i->getOperand()->getType())),
            out, cast<LoadableTypeInfo>(getTypeInfo(i->getType())));
  
  setLoweredExplosion(i, out);
}

void IRGenSILFunction::visitRefToRawPointerInst(
                                             swift::RefToRawPointerInst *i) {
  auto &ti = getTypeInfo(i->getType());
  emitPointerCastInst(*this, i->getOperand(), i, ti);
}

void IRGenSILFunction::visitRawPointerToRefInst(swift::RawPointerToRefInst *i) {
  auto &ti = getTypeInfo(i->getType());
  emitPointerCastInst(*this, i->getOperand(), i, ti);
}

// SIL scalar conversions which never change the IR type.
// FIXME: Except for optionals, which get bit-packed into an integer.
static void trivialRefConversion(IRGenSILFunction &IGF,
                                 SILValue input,
                                 SILValue result) {
  Explosion temp = IGF.getLoweredExplosion(input);
  auto &inputTI = IGF.getTypeInfo(input->getType());
  auto &resultTI = IGF.getTypeInfo(result->getType());
  
  // If the types are the same, forward the existing value.
  if (inputTI.getStorageType() == resultTI.getStorageType()) {
    IGF.setLoweredExplosion(result, temp);
    return;
  }

  auto schema = resultTI.getSchema();
  Explosion out;

  for (auto schemaElt : schema) {
    auto resultTy = schemaElt.getScalarType();

    llvm::Value *value = temp.claimNext();
    if (value->getType() == resultTy) {
      // Nothing to do.  This happens with the unowned conversions.
    } else if (resultTy->isPointerTy()) {
      value = IGF.Builder.CreateIntToPtr(value, resultTy);
    } else {
      value = IGF.Builder.CreatePtrToInt(value, resultTy);
    }
    out.add(value);
  }
  
  IGF.setLoweredExplosion(result, out);
}

// SIL scalar conversions which never change the IR type.
// FIXME: Except for optionals, which get bit-packed into an integer.
#define NOOP_CONVERSION(KIND)                                     \
void IRGenSILFunction::visit##KIND##Inst(swift::KIND##Inst *i) {  \
  ::trivialRefConversion(*this, i->getOperand(), i); \
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
  setLoweredExplosion(i, to);
}

void IRGenSILFunction::visitThickToObjCMetatypeInst(ThickToObjCMetatypeInst *i){
  Explosion from = getLoweredExplosion(i->getOperand());
  llvm::Value *swiftMeta = from.claimNext();
  CanType instanceType(i->getType().castTo<AnyMetatypeType>().getInstanceType());
  Explosion to;
  llvm::Value *classPtr =
    emitClassHeapMetadataRefForMetatype(*this, swiftMeta, instanceType);
  to.add(Builder.CreateBitCast(classPtr, IGM.ObjCClassPtrTy));
  setLoweredExplosion(i, to);
}

void IRGenSILFunction::visitObjCToThickMetatypeInst(
                         ObjCToThickMetatypeInst *i) {
  Explosion from = getLoweredExplosion(i->getOperand());
  llvm::Value *classPtr = from.claimNext();
  
  // Fetch the metadata for that class.
  Explosion to;
  auto metadata = emitObjCMetadataRefForMetadata(*this, classPtr);
  to.add(metadata);
  setLoweredExplosion(i, to);  
}

void IRGenSILFunction::visitUnconditionalCheckedCastInst(
                                       swift::UnconditionalCheckedCastInst *i) {
  Explosion value = getLoweredExplosion(i->getOperand());
  Explosion ex;
  emitScalarCheckedCast(*this, value, i->getOperand()->getType(), i->getType(),
                        CheckedCastMode::Unconditional, ex);
  setLoweredExplosion(i, ex);
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
  setLoweredExplosion(i, to);
}

void IRGenSILFunction::visitObjCExistentialMetatypeToObjectInst(
                                       ObjCExistentialMetatypeToObjectInst *i){
  // Bitcast the @objc metatype reference, which is already an ObjC object, to
  // the destination type. The metatype may carry additional witness tables we
  // can drop.
  Explosion from = getLoweredExplosion(i->getOperand());
  llvm::Value *value = from.claimNext();
  (void)from.claimAll();
  value = Builder.CreateBitCast(value, IGM.UnknownRefCountedPtrTy);
  Explosion to;
  to.add(value);
  setLoweredExplosion(i, to);
}
void IRGenSILFunction::visitObjCProtocolInst(ObjCProtocolInst *i) {
  // Get the protocol reference.
  llvm::Value *protoRef = emitReferenceToObjCProtocol(*this, i->getProtocol());
  // Bitcast it to the class reference type.
  protoRef = Builder.CreateBitCast(protoRef,
                                   getTypeInfo(i->getType()).getStorageType());
  Explosion ex;
  ex.add(protoRef);
  setLoweredExplosion(i, ex);
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
  
  setLoweredExplosion(i, resultEx);
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
  
  ClassDecl *Cl = i->getType().getClassOrBoundGenericClass();
  if (IGM.TargetInfo.hasObjCTaggedPointers() &&
      (!Cl || !isKnownNotTaggedPointer(IGM, Cl))) {
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
  setLoweredExplosion(i, resultEx);
}

void IRGenSILFunction::visitBridgeObjectToWordInst(
                                             swift::BridgeObjectToWordInst *i) {
  Explosion boEx = getLoweredExplosion(i->getConverted());
  llvm::Value *val = boEx.claimNext();
  val = Builder.CreatePtrToInt(val, IGM.SizeTy);
  Explosion wordEx;
  wordEx.add(val);
  setLoweredExplosion(i, wordEx);
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
  SILType destTy = i->getCastType();
  FailableCastResult castResult;
  Explosion ex;
  if (i->isExact()) {
    auto operand = i->getOperand();
    Explosion source = getLoweredExplosion(operand);
    castResult = emitClassIdenticalCast(*this, source.claimNext(),
                                        operand->getType(), destTy);
  } else {
    Explosion value = getLoweredExplosion(i->getOperand());
    emitScalarCheckedCast(*this, value, i->getOperand()->getType(),
                          i->getCastType(), CheckedCastMode::Conditional, ex);
    auto val = ex.claimNext();
    castResult.casted = val;
    llvm::Value *nil =
      llvm::ConstantPointerNull::get(cast<llvm::PointerType>(val->getType()));
    castResult.succeeded = Builder.CreateICmpNE(val, nil);
  }
  
  // Branch on the success of the cast.
  // All cast operations currently return null on failure.


  auto &successBB = getLoweredBB(i->getSuccessBB());
  llvm::Type *toTy = IGM.getTypeInfo(destTy).getStorageType();
  if (toTy->isPointerTy())
    castResult.casted = Builder.CreateBitCast(castResult.casted, toTy);

  Builder.CreateCondBr(castResult.succeeded,
                       successBB.bb,
                       getLoweredBB(i->getFailureBB()).bb);
  
  // Feed the cast result into the nonnull branch.
  unsigned phiIndex = 0;
  Explosion ex2;
  ex2.add(castResult.casted);
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
  
  if (i->getOperand()->getType().getSwiftType()->is<SILFunctionType>()) {
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
  setLoweredExplosion(i, out);
}

void IRGenSILFunction::visitUpcastInst(swift::UpcastInst *i) {
  auto toTy = getTypeInfo(i->getType()).getSchema()[0].getScalarType();

  // If we have an address, just bitcast, don't explode.
  if (i->getOperand()->getType().isAddress()) {
    Address fromAddr = getLoweredAddress(i->getOperand());
    llvm::Value *toValue = Builder.CreateBitCast(
      fromAddr.getAddress(), toTy->getPointerTo());
    Address Addr(toValue, fromAddr.getAlignment());
    setLoweredAddress(i, Addr);
    return;
  }

  Explosion from = getLoweredExplosion(i->getOperand());
  Explosion to;
  assert(from.size() == 1 && "class should explode to single value");
  llvm::Value *fromValue = from.claimNext();
  to.add(Builder.CreateBitCast(fromValue, toTy));
  setLoweredExplosion(i, to);
}

void IRGenSILFunction::visitIndexAddrInst(swift::IndexAddrInst *i) {
  Address base = getLoweredAddress(i->getBase());
  Explosion indexValues = getLoweredExplosion(i->getIndex());
  llvm::Value *index = indexValues.claimNext();
  
  auto baseTy = i->getBase()->getType();
  auto &ti = getTypeInfo(baseTy);
  
  Address dest = ti.indexArray(*this, base, index, baseTy);
  setLoweredAddress(i, dest);
}

void IRGenSILFunction::visitTailAddrInst(swift::TailAddrInst *i) {
  Address base = getLoweredAddress(i->getBase());
  Explosion indexValues = getLoweredExplosion(i->getIndex());
  llvm::Value *index = indexValues.claimNext();

  SILType baseTy = i->getBase()->getType();
  const TypeInfo &baseTI = getTypeInfo(baseTy);

  Address dest = baseTI.indexArray(*this, base, index, baseTy);
  const TypeInfo &TailTI = getTypeInfo(i->getTailType());
  dest = TailTI.roundUpToTypeAlignment(*this, dest, i->getTailType());
  llvm::Type *destType = TailTI.getStorageType()->getPointerTo();
  dest = Builder.CreateBitCast(dest, destType);
  setLoweredAddress(i, dest);
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
  setLoweredExplosion(i, result);
}

void IRGenSILFunction::visitAllocValueBufferInst(
                                          swift::AllocValueBufferInst *i) {
  Address buffer = getLoweredAddress(i->getOperand());
  auto valueType = i->getValueType();
  Address value =
    getTypeInfo(valueType).allocateBuffer(*this, buffer, valueType);
  setLoweredAddress(i, value);
}

void IRGenSILFunction::visitProjectValueBufferInst(
                                          swift::ProjectValueBufferInst *i) {
  Address buffer = getLoweredAddress(i->getOperand());
  auto valueType = i->getValueType();
  Address value =
    getTypeInfo(valueType).projectBuffer(*this, buffer, valueType);
  setLoweredAddress(i, value);
}

void IRGenSILFunction::visitDeallocValueBufferInst(
                                          swift::DeallocValueBufferInst *i) {
  Address buffer = getLoweredAddress(i->getOperand());
  auto valueType = i->getValueType();
  getTypeInfo(valueType).deallocateBuffer(*this, buffer, valueType);
}

void IRGenSILFunction::visitInitExistentialAddrInst(swift::InitExistentialAddrInst *i) {
  Address container = getLoweredAddress(i->getOperand());
  SILType destType = i->getOperand()->getType();
  Address buffer = emitOpaqueExistentialContainerInit(*this,
                                                container,
                                                destType,
                                                i->getFormalConcreteType(),
                                                i->getLoweredConcreteType(),
                                                i->getConformances());

  auto &srcTI = getTypeInfo(i->getLoweredConcreteType());

  // See if we can defer initialization of the buffer to a copy_addr into it.
  if (tryDeferFixedSizeBufferInitialization(*this, i, srcTI, buffer, ""))
    return;
  
  // Allocate in the destination fixed-size buffer.
  Address address =
    srcTI.allocateBuffer(*this, buffer, i->getLoweredConcreteType());  
  setLoweredAddress(i, address);
}

void IRGenSILFunction::visitInitExistentialMetatypeInst(
                                              InitExistentialMetatypeInst *i) {
  Explosion metatype = getLoweredExplosion(i->getOperand());
  Explosion result;
  emitExistentialMetatypeContainer(*this,
                                   result, i->getType(),
                                   metatype.claimNext(),
                                   i->getOperand()->getType(),
                                   i->getConformances());
  setLoweredExplosion(i, result);
}

void IRGenSILFunction::visitInitExistentialRefInst(InitExistentialRefInst *i) {
  Explosion instance = getLoweredExplosion(i->getOperand());
  Explosion result;
  emitClassExistentialContainer(*this,
                               result, i->getType(),
                               instance.claimNext(),
                               i->getFormalConcreteType(),
                               i->getOperand()->getType(),
                               i->getConformances());
  setLoweredExplosion(i, result);
}

void IRGenSILFunction::visitDeinitExistentialAddrInst(
                                              swift::DeinitExistentialAddrInst *i) {
  Address container = getLoweredAddress(i->getOperand());
  emitOpaqueExistentialContainerDeinit(*this, container,
                                       i->getOperand()->getType());
}

void IRGenSILFunction::visitOpenExistentialAddrInst(OpenExistentialAddrInst *i) {
  SILType baseTy = i->getOperand()->getType();
  Address base = getLoweredAddress(i->getOperand());

  auto openedArchetype = cast<ArchetypeType>(
                           i->getType().getSwiftRValueType());
  Address object = emitOpaqueExistentialProjection(*this, base, baseTy,
                                                   openedArchetype);

  setLoweredAddress(i, object);
}

void IRGenSILFunction::visitOpenExistentialRefInst(OpenExistentialRefInst *i) {

  SILType baseTy = i->getOperand()->getType();
  Explosion base = getLoweredExplosion(i->getOperand());
  auto openedArchetype = cast<ArchetypeType>(
                           i->getType().getSwiftRValueType());

  Explosion result;
  llvm::Value *instance
    = emitClassExistentialProjection(*this, base, baseTy,
                                     openedArchetype);
  result.add(instance);
  setLoweredExplosion(i, result);
}

void IRGenSILFunction::visitOpenExistentialMetatypeInst(
                                              OpenExistentialMetatypeInst *i) {
  SILType baseTy = i->getOperand()->getType();
  Explosion base = getLoweredExplosion(i->getOperand());
  auto openedTy = i->getType().getSwiftRValueType();

  llvm::Value *metatype =
    emitExistentialMetatypeProjection(*this, base, baseTy, openedTy);
  Explosion result;
  result.add(metatype);
  setLoweredExplosion(i, result);
}

void IRGenSILFunction::visitOpenExistentialOpaqueInst(
    OpenExistentialOpaqueInst *i) {
  llvm_unreachable("unsupported instruction during IRGen");
}

void IRGenSILFunction::visitProjectBlockStorageInst(ProjectBlockStorageInst *i){
  // TODO
  Address block = getLoweredAddress(i->getOperand());
  Address capture = projectBlockStorageCapture(*this, block,
                       i->getOperand()->getType().castTo<SILBlockStorageType>());
  
  setLoweredAddress(i, capture);
}

void IRGenSILFunction::visitInitBlockStorageHeaderInst(
                                               InitBlockStorageHeaderInst *i) {
  auto addr = getLoweredAddress(i->getBlockStorage());
  
  // We currently only support static invoke functions.
  auto &invokeVal = getLoweredValue(i->getInvokeFunction());
  llvm::Function *invokeFn = nullptr;
  ForeignFunctionInfo foreignInfo;
  if (invokeVal.kind != LoweredValue::Kind::StaticFunction) {
    IGM.unimplemented(i->getLoc().getSourceLoc(),
                      "non-static block invoke function");
  } else {
    invokeFn = invokeVal.getStaticFunction().getFunction();
    foreignInfo = invokeVal.getStaticFunction().getForeignInfo();
  }

  assert(foreignInfo.ClangInfo && "no clang info for block function?");
  
  // Initialize the header.
  emitBlockHeader(*this, addr,
          i->getBlockStorage()->getType().castTo<SILBlockStorageType>(),
          invokeFn, i->getInvokeFunction()->getType().castTo<SILFunctionType>(),
          foreignInfo);
  
  // Cast the storage to the block type to produce the result value.
  llvm::Value *asBlock = Builder.CreateBitCast(addr.getAddress(),
                                               IGM.ObjCBlockPtrTy);
  Explosion e;
  e.add(asBlock);
  setLoweredExplosion(i, e);
}

void IRGenSILFunction::visitAllocExistentialBoxInst(AllocExistentialBoxInst *i){
  OwnedAddress boxWithAddr =
    emitBoxedExistentialContainerAllocation(*this, i->getExistentialType(),
                                            i->getFormalConcreteType(),
                                            i->getConformances());
  setLoweredBox(i, boxWithAddr);
}

void IRGenSILFunction::visitDeallocExistentialBoxInst(
                                                DeallocExistentialBoxInst *i) {
  Explosion box = getLoweredExplosion(i->getOperand());
  emitBoxedExistentialContainerDeallocation(*this, box,
                                            i->getOperand()->getType(),
                                            i->getConcreteType());
}

void IRGenSILFunction::visitOpenExistentialBoxInst(OpenExistentialBoxInst *i) {
  Explosion box = getLoweredExplosion(i->getOperand());
  auto openedArchetype = cast<ArchetypeType>(i->getType().getSwiftRValueType());

  auto addr = emitOpenExistentialBox(*this, box, i->getOperand()->getType(),
                                     openedArchetype);
  setLoweredAddress(i, addr);
}

void
IRGenSILFunction::visitProjectExistentialBoxInst(ProjectExistentialBoxInst *i) {
  const LoweredValue &val = getLoweredValue(i->getOperand());
  if (val.isBoxWithAddress()) {
    // The operand is an alloc_existential_box.
    // We can directly reuse the address.
    setLoweredAddress(i, val.getAddressOfBox());
  } else {
    Explosion box = getLoweredExplosion(i->getOperand());
    auto caddr = emitBoxedExistentialProjection(*this, box,
                                                i->getOperand()->getType(),
                                                i->getType().getSwiftRValueType());
    setLoweredAddress(i, caddr.getAddress());
  }
}

void IRGenSILFunction::visitDynamicMethodInst(DynamicMethodInst *i) {
  assert(i->getMember().isForeign && "dynamic_method requires [objc] method");
  setLoweredObjCMethod(i, i->getMember());
  return;
}

void IRGenSILFunction::visitWitnessMethodInst(swift::WitnessMethodInst *i) {
  // For Objective-C classes we need to arrange for a msgSend
  // to happen when the method is called.
  if (i->getMember().isForeign) {
    setLoweredObjCMethod(i, i->getMember());
    return;
  }

  CanType baseTy = i->getLookupType();
  ProtocolConformanceRef conformance = i->getConformance();
  SILDeclRef member = i->getMember();

  // It would be nice if this weren't discarded.
  llvm::Value *baseMetadataCache = nullptr;

  Explosion lowered;
  emitWitnessMethodValue(*this, baseTy, &baseMetadataCache,
                         member, conformance, lowered);
  
  setLoweredExplosion(i, lowered);
}

void IRGenSILFunction::setAllocatedAddressForBuffer(SILValue v,
                                                const Address &allocedAddress) {
  overwriteAllocatedAddress(v, allocedAddress);

  // Emit the debug info for the variable if any.
  if (auto allocStack = dyn_cast<AllocStackInst>(v)) {
    emitDebugInfoForAllocStack(allocStack, getTypeInfo(v->getType()),
                               allocedAddress.getAddress());
  }
}

void IRGenSILFunction::visitCopyAddrInst(swift::CopyAddrInst *i) {
  SILType addrTy = i->getSrc()->getType();
  const TypeInfo &addrTI = getTypeInfo(addrTy);
  Address src = getLoweredAddress(i->getSrc());
  // See whether we have a deferred fixed-size buffer initialization.
  auto &loweredDest = getLoweredValue(i->getDest());
  if (loweredDest.isUnallocatedAddressInBuffer()) {
    assert(i->isInitializationOfDest()
           && "need to initialize an unallocated buffer");
    Address cont = loweredDest.getContainerOfAddress();
    if (i->isTakeOfSrc()) {
      Address addr = addrTI.initializeBufferWithTake(*this, cont, src, addrTy);
      setAllocatedAddressForBuffer(i->getDest(), addr);
    } else {
      Address addr = addrTI.initializeBufferWithCopy(*this, cont, src, addrTy);
      setAllocatedAddressForBuffer(i->getDest(), addr);
    }
  } else {
    Address dest = loweredDest.getAddress();
    
    if (i->isInitializationOfDest()) {
      if (i->isTakeOfSrc()) {
        addrTI.initializeWithTake(*this, dest, src, addrTy);
      } else {
        addrTI.initializeWithCopy(*this, dest, src, addrTy);
      }
    } else {
      if (i->isTakeOfSrc()) {
        addrTI.assignWithTake(*this, dest, src, addrTy);
      } else {
        addrTI.assignWithCopy(*this, dest, src, addrTy);
      }
    }
  }
}

// This is a no-op because we do not lower Swift TBAA info to LLVM IR, and it
// does not produce any values.
void IRGenSILFunction::visitBindMemoryInst(swift::BindMemoryInst *) {}

void IRGenSILFunction::visitDestroyAddrInst(swift::DestroyAddrInst *i) {
  SILType addrTy = i->getOperand()->getType();
  const TypeInfo &addrTI = getTypeInfo(addrTy);

  // Otherwise, do the normal thing.
  Address base = getLoweredAddress(i->getOperand());
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

  if (IGM.IRGen.Opts.Optimize) {
    // Emit unique side-effecting inline asm calls in order to eliminate
    // the possibility that an LLVM optimization or code generation pass
    // will merge these blocks back together again. We emit an empty asm
    // string with the side-effect flag set, and with a unique integer
    // argument for each cond_fail we see in the function.
    llvm::IntegerType *asmArgTy = IGM.Int32Ty;
    llvm::Type *argTys = { asmArgTy };
    llvm::FunctionType *asmFnTy =
      llvm::FunctionType::get(IGM.VoidTy, argTys, false /* = isVarArg */);
    llvm::InlineAsm *inlineAsm =
      llvm::InlineAsm::get(asmFnTy, "", "n", true /* = SideEffects */);
    Builder.CreateCall(inlineAsm,
                       llvm::ConstantInt::get(asmArgTy, NumCondFails++));
  }

  // Emit the trap instruction.
  llvm::Function *trapIntrinsic =
      llvm::Intrinsic::getDeclaration(&IGM.Module, llvm::Intrinsic::ID::trap);
  Builder.CreateCall(trapIntrinsic, {});
  Builder.CreateUnreachable();
  Builder.emitBlock(contBB);
  FailBBs.push_back(failBB);
}

void IRGenSILFunction::visitSuperMethodInst(swift::SuperMethodInst *i) {
  if (i->getMember().isForeign) {
    setLoweredObjCMethodBounded(i, i->getMember(),
                                i->getOperand()->getType(),
                                /*startAtSuper=*/true);
    return;
  }

  auto base = getLoweredExplosion(i->getOperand());
  auto baseType = i->getOperand()->getType();
  llvm::Value *baseValue = base.claimNext();

  auto method = i->getMember();
  auto methodType = i->getType().castTo<SILFunctionType>();

  llvm::Value *fnValue = emitVirtualMethodValue(*this, baseValue,
                                                baseType,
                                                method, methodType,
                                                /*useSuperVTable*/ true);
  fnValue = Builder.CreateBitCast(fnValue, IGM.Int8PtrTy);
  Explosion e;
  e.add(fnValue);
  setLoweredExplosion(i, e);
}

void IRGenSILFunction::visitClassMethodInst(swift::ClassMethodInst *i) {
  // For Objective-C classes we need to arrange for a msgSend
  // to happen when the method is called.
  if (i->getMember().isForeign) {
    setLoweredObjCMethod(i, i->getMember());
    return;
  }
  
  Explosion base = getLoweredExplosion(i->getOperand());
  llvm::Value *baseValue = base.claimNext();
  
  SILDeclRef method = i->getMember();
  auto methodType = i->getType().castTo<SILFunctionType>();
 
  // For Swift classes, get the method implementation from the vtable.
  // FIXME: better explosion kind, map as static.
  llvm::Value *fnValue = emitVirtualMethodValue(*this, baseValue,
                                                i->getOperand()->getType(),
                                                method, methodType,
                                                /*useSuperVTable*/ false);
  fnValue = Builder.CreateBitCast(fnValue, IGM.Int8PtrTy);
  Explosion e;
  e.add(fnValue);
  setLoweredExplosion(i, e);
}

void IRGenModule::emitSILStaticInitializers() {
  SmallVector<SILFunction *, 8> StaticInitializers;
  for (SILGlobalVariable &Global : getSILModule().getSILGlobals()) {
    if (!Global.getInitializer())
      continue;

    auto *IRGlobal =
        Module.getGlobalVariable(Global.getName(), true /* = AllowLocal */);

    // A check for multi-threaded compilation: Is this the llvm module where the
    // global is defined and not only referenced (or not referenced at all).
    if (!IRGlobal || !IRGlobal->hasInitializer())
      continue;

    auto *InitValue = Global.getValueOfStaticInitializer();

    // Set the IR global's initializer to the constant for this SIL
    // struct.
    if (auto *SI = dyn_cast<StructInst>(InitValue)) {
      IRGlobal->setInitializer(emitConstantStruct(*this, SI));
      continue;
    }

    // Set the IR global's initializer to the constant for this SIL
    // tuple.
    auto *TI = cast<TupleInst>(InitValue);
    IRGlobal->setInitializer(emitConstantTuple(*this, TI));
  }
}
