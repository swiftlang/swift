//===--- SILGenDecl.cpp - Implements Lowering of ASTs -> SIL for Decls ----===//
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

#include "SILGen.h"
#include "Initialization.h"
#include "RValue.h"
#include "Scope.h"
#include "SILGenDynamicCast.h"
#include "swift/SIL/FormalLinkage.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILDebuggerClient.h"
#include "swift/SIL/SILType.h"
#include "swift/SIL/TypeLowering.h"
#include "swift/AST/AST.h"
#include "swift/AST/Mangle.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/Basic/Fallthrough.h"
#include "llvm/ADT/SmallString.h"
#include <iterator>
using namespace swift;
using namespace Mangle;
using namespace Lowering;

void Initialization::_anchor() {}
void SILDebuggerClient::anchor() {}

namespace {
  /// A "null" initialization that indicates that any value being initialized
  /// into this initialization should be discarded. This represents AnyPatterns
  /// (that is, 'var (_)') that bind to values without storing them.
  class BlackHoleInitialization : public Initialization {
  public:
    BlackHoleInitialization() {}

    SILValue getAddressOrNull() const override { return SILValue(); }

    bool canSplitIntoSubelementAddresses() const override {
      return true;
    }
    
    ArrayRef<InitializationPtr>
    getSubInitializationsForTuple(SILGenFunction &gen, CanType type,
                                  SmallVectorImpl<InitializationPtr> &buf,
                                  SILLocation Loc) override {
      // "Destructure" an ignored binding into multiple ignored bindings.
      for (auto fieldType : cast<TupleType>(type)->getElementTypes()) {
        (void) fieldType;
        buf.push_back(InitializationPtr(new BlackHoleInitialization()));
      }
      return buf;
    }

    void copyOrInitValueInto(ManagedValue explodedElement, bool isInit,
                             SILLocation loc, SILGenFunction &gen) override {
      /// This just ignores the provided value.
    }
  };
} // end anonymous namespace

namespace {
  /// An Initialization of a tuple pattern, such as "var (a,b)".
  class TupleInitialization : public Initialization {
  public:
    /// The sub-Initializations aggregated by this tuple initialization.
    /// The TupleInitialization object takes ownership of Initializations pushed
    /// here.
    SmallVector<InitializationPtr, 4> subInitializations;
    
    TupleInitialization() {}
    
    SILValue getAddressOrNull() const override {
      if (subInitializations.size() == 1)
        return subInitializations[0]->getAddressOrNull();
      else
        return SILValue();
    }
    
    bool canSplitIntoSubelementAddresses() const override {
      return true;
    }
    
    ArrayRef<InitializationPtr>
    getSubInitializationsForTuple(SILGenFunction &gen, CanType type,
                                  SmallVectorImpl<InitializationPtr> &buf,
                                  SILLocation Loc) override {
      return subInitializations;
    }
    
    void finishInitialization(SILGenFunction &gen) override {
      for (auto &sub : subInitializations)
        sub->finishInitialization(gen);
    }
    
    void copyOrInitValueInto(ManagedValue valueMV, bool isInit, SILLocation loc,
                             SILGenFunction &SGF) override {
      // A scalar value is being copied into the tuple, break it into elements
      // and assign/init each element in turn.
      SILValue value = valueMV.forward(SGF);
      auto sourceType = cast<TupleType>(valueMV.getSwiftType());
      auto sourceSILType = value.getType();
      for (unsigned i = 0, e = sourceType->getNumElements(); i != e; ++i) {
        SILType fieldTy = sourceSILType.getTupleElementType(i);
        auto &fieldTL = SGF.getTypeLowering(fieldTy);
        
        SILValue member;
        if (value.getType().isAddress()) {
          member = SGF.B.createTupleElementAddr(loc, value, i, fieldTy);
          if (!fieldTL.isAddressOnly())
            member = SGF.B.createLoad(loc, member);
        } else {
          member = SGF.B.createTupleExtract(loc, value, i, fieldTy);
        }
        
        auto elt = SGF.emitManagedRValueWithCleanup(member, fieldTL);
        
        subInitializations[i]->copyOrInitValueInto(elt, isInit, loc, SGF);
      }
    }
  };
} // end anonymous namespace


namespace {
  class CleanupClosureConstant : public Cleanup {
    SILValue closure;
  public:
    CleanupClosureConstant(SILValue closure) : closure(closure) {}
    void emit(SILGenFunction &gen, CleanupLocation l) override {
      gen.B.emitStrongReleaseAndFold(l, closure);
    }
  };
}

ArrayRef<Substitution> SILGenFunction::getForwardingSubstitutions() {
  return F.getForwardingSubstitutions();
}

void SILGenFunction::visitFuncDecl(FuncDecl *fd) {
  // Generate the local function body.
  SGM.emitFunction(fd);

  // If there are captures or we are in a generic context, build the local
  // closure value for the function and store it as a local constant.
  if (fd->getCaptureInfo().hasLocalCaptures()
      || F.getContextGenericParams()) {
    SILValue closure =
        emitClosureValue(fd, SILDeclRef(fd), F.getForwardingSubstitutions(), fd)
            .forward(*this);
    Cleanups.pushCleanup<CleanupClosureConstant>(closure);
    LocalFunctions[SILDeclRef(fd)] = closure;
  }
}

ArrayRef<InitializationPtr> SingleBufferInitialization::
getSubInitializationsForTuple(SILGenFunction &gen, CanType type,
                              SmallVectorImpl<InitializationPtr> &buf,
                              SILLocation Loc) {
  // Destructure the buffer into per-element buffers.
  auto tupleTy = cast<TupleType>(type);
  SILValue baseAddr = getAddress();
  for (unsigned i = 0, size = tupleTy->getNumElements(); i < size; ++i) {
    auto fieldType = tupleTy.getElementType(i);
    SILType fieldTy = gen.getLoweredType(fieldType).getAddressType();
    SILValue fieldAddr = gen.B.createTupleElementAddr(Loc,
                                                      baseAddr, i,
                                                      fieldTy);
    
    buf.push_back(InitializationPtr(new
                                    KnownAddressInitialization(fieldAddr)));
  }
  finishInitialization(gen);
  return buf;
}

void SingleBufferInitialization::
copyOrInitValueIntoSingleBuffer(ManagedValue explodedElement, bool isInit,
                                SILValue BufferAddress,
                                SILLocation loc, SILGenFunction &gen) {
  if (!isInit) {
    assert(explodedElement.getValue() != BufferAddress && "copying in place?!");
    explodedElement.copyInto(gen, BufferAddress, loc);
    return;
  }
  
  // If we didn't evaluate into the initialization buffer, do so now.
  if (explodedElement.getValue() != BufferAddress) {
    explodedElement.forwardInto(gen, loc, BufferAddress);
  } else {
    // If we did evaluate into the initialization buffer, disable the
    // cleanup.
    explodedElement.forwardCleanup(gen);
  }
}

void KnownAddressInitialization::anchor() const {
}

void TemporaryInitialization::finishInitialization(SILGenFunction &gen) {
  if (Cleanup.isValid())
    gen.Cleanups.setCleanupState(Cleanup, CleanupState::Active);
};

namespace {
class ReleaseValueCleanup : public Cleanup {
  SILValue v;
public:
  ReleaseValueCleanup(SILValue v) : v(v) {}

  void emit(SILGenFunction &gen, CleanupLocation l) override {
    if (v.getType().isAddress())
      gen.B.emitDestroyAddrAndFold(l, v);
    else
      gen.B.emitReleaseValueOperation(l, v);
  }
};
} // end anonymous namespace

namespace {
/// Cleanup to destroy an initialized variable.
class DeallocStackCleanup : public Cleanup {
  SILValue Addr;
public:
  DeallocStackCleanup(SILValue addr) : Addr(addr) {}

  void emit(SILGenFunction &gen, CleanupLocation l) override {
    gen.B.createDeallocStack(l, Addr);
  }
};
} // end anonymous namespace

namespace {
/// Cleanup to destroy an initialized 'var' variable.
class DestroyLocalVariable : public Cleanup {
  VarDecl *Var;
public:
  DestroyLocalVariable(VarDecl *var) : Var(var) {}

  void emit(SILGenFunction &gen, CleanupLocation l) override {
    gen.destroyLocalVariable(l, Var);
  }
};
} // end anonymous namespace

namespace {
/// Cleanup to destroy an uninitialized local variable.
class DeallocateUninitializedLocalVariable : public Cleanup {
  VarDecl *Var;
public:
  DeallocateUninitializedLocalVariable(VarDecl *var) : Var(var) {}

  void emit(SILGenFunction &gen, CleanupLocation l) override {
    gen.deallocateUninitializedLocalVariable(l, Var);
  }
};
} // end anonymous namespace

namespace {
/// An initialization of a local 'var'.
class LocalVariableInitialization : public SingleBufferInitialization {
  /// The local variable decl being initialized.
  VarDecl *decl;
  SILGenFunction &SGF;

  /// The cleanup we pushed to deallocate the local variable before it
  /// gets initialized.
  CleanupHandle DeallocCleanup;

  /// The cleanup we pushed to destroy and deallocate the local variable.
  CleanupHandle ReleaseCleanup;

  bool DidFinish = false;
public:
  /// Sets up an initialization for the allocated box. This pushes a
  /// CleanupUninitializedBox cleanup that will be replaced when
  /// initialization is completed.
  LocalVariableInitialization(VarDecl *decl, bool NeedsMarkUninit,
                              SILGenFunction &SGF)
    : decl(decl), SGF(SGF) {
    assert(decl->getDeclContext()->isLocalContext() &&
           "can't emit a local var for a non-local var decl");
    assert(decl->hasStorage() && "can't emit storage for a computed variable");
    assert(!SGF.VarLocs.count(decl) && "Already have an entry for this decl?");

    SILType lType = SGF.getLoweredType(decl->getType()->getRValueType());

    // The variable may have its lifetime extended by a closure, heap-allocate
    // it using a box.
    AllocBoxInst *allocBox = SGF.B.createAllocBox(decl, lType);
    auto box = SILValue(allocBox, 0);
    auto addr = SILValue(allocBox, 1);

    // Mark the memory as uninitialized, so DI will track it for us.
    if (NeedsMarkUninit)
      addr = SGF.B.createMarkUninitializedVar(decl, addr);

    /// Remember that this is the memory location that we're emitting the
    /// decl to.
    SGF.VarLocs[decl] = SILGenFunction::VarLoc::get(addr, box);

    // Push a cleanup to destroy the local variable.  This has to be
    // inactive until the variable is initialized.
    SGF.Cleanups.pushCleanupInState<DestroyLocalVariable>(CleanupState::Dormant,
                                                          decl);
    ReleaseCleanup = SGF.Cleanups.getTopCleanup();

    // Push a cleanup to deallocate the local variable.
    SGF.Cleanups.pushCleanup<DeallocateUninitializedLocalVariable>(decl);
    DeallocCleanup = SGF.Cleanups.getTopCleanup();
  }

  ~LocalVariableInitialization() override {
    assert(DidFinish && "did not call VarInit::finishInitialization!");
  }

  SILValue getAddressOrNull() const override {
    assert(SGF.VarLocs.count(decl) && "did not emit var?!");
    return SGF.VarLocs[decl].value;
  }

  void finishInitialization(SILGenFunction &SGF) override {
    assert(!DidFinish &&
           "called LocalVariableInitialization::finishInitialization twice!");
    SGF.Cleanups.setCleanupState(DeallocCleanup, CleanupState::Dead);
    SGF.Cleanups.setCleanupState(ReleaseCleanup, CleanupState::Active);
    DidFinish = true;
  }
};
} // end anonymous namespace

namespace {
/// Initialize a writeback buffer that receives the value of a 'let'
/// declaration.
class LetValueInitialization : public Initialization {
  /// The VarDecl for the let decl.
  VarDecl *vd;

  /// The address of the buffer used for the binding, if this is an address-only
  /// let.
  SILValue address;

  /// The cleanup we pushed to destroy the local variable.
  CleanupHandle DestroyCleanup;

  bool DidFinish = false;

public:
  LetValueInitialization(VarDecl *vd, SILGenFunction &gen) : vd(vd)
  {
    auto &lowering = gen.getTypeLowering(vd->getType());
    
    // Decide whether we need a temporary stack buffer to evaluate this 'let'.
    // There are three cases we need to handle here: parameters, initialized (or
    // bound) decls, and uninitialized ones.
    bool needsTemporaryBuffer;
    bool isUninitialized = false;

    assert(!isa<ParamDecl>(vd)
           && "should not bind function params on this path");
    if (vd->getParentPatternBinding() && !vd->getParentInitializer()) {
      // This value is uninitialized (and unbound) if it has a pattern binding
      // decl, with no initializer value.
      assert(!vd->hasNonPatternBindingInit() && "Bound values aren't uninit!");
      
      // If this is a let-value without an initializer, then we need a temporary
      // buffer.  DI will make sure it is only assigned to once.
      needsTemporaryBuffer = true;
      isUninitialized = true;
    } else {
      // If this is a let with an initializer or bound value, we only need a
      // buffer if the type is address only.
      needsTemporaryBuffer = lowering.isAddressOnly();
    }
   
    if (needsTemporaryBuffer) {
      address = gen.emitTemporaryAllocation(vd, lowering.getLoweredType());
      if (isUninitialized)
        address = gen.B.createMarkUninitializedVar(vd, address);
      DestroyCleanup = gen.enterDormantTemporaryCleanup(address, lowering);
      gen.VarLocs[vd] = SILGenFunction::VarLoc::get(address);
    } else if (!lowering.isTrivial()) {
      // Push a cleanup to destroy the let declaration.  This has to be
      // inactive until the variable is initialized: if control flow exits the
      // before the value is bound, we don't want to destroy the value.
      gen.Cleanups.pushCleanupInState<DestroyLocalVariable>(
                                                    CleanupState::Dormant, vd);
      DestroyCleanup = gen.Cleanups.getTopCleanup();
    } else {
      DestroyCleanup = CleanupHandle::invalid();
    }
  }

  ~LetValueInitialization() override {
    assert(DidFinish && "did not call LetValueInit::finishInitialization!");
  }

  bool hasAddress() const { return address.isValid(); }
  
  // SingleBufferInitializations always have an address.
  SILValue getAddressForInPlaceInitialization() const override {
    // Emit into the buffer that 'let's produce for address-only values if
    // we have it.
    if (hasAddress()) return address;
    return SILValue();
  }

  /// Return true if we can get the addresses of elements with the
  /// 'getSubInitializationsForTuple' method.
  ///
  /// Let-value initializations cannot be broken into constituent pieces if a
  /// scalar value needs to be bound.  If there is an address in play, then we
  /// can initialize the address elements of the tuple though.
  bool canSplitIntoSubelementAddresses() const override {
    return hasAddress();
  }
  
  ArrayRef<InitializationPtr>
  getSubInitializationsForTuple(SILGenFunction &gen, CanType type,
                                SmallVectorImpl<InitializationPtr> &buf,
                                SILLocation Loc) override {
    // Destructure the buffer into per-element buffers.
    auto tupleTy = cast<TupleType>(type);
    SILValue baseAddr = getAddress();
    for (unsigned i = 0, size = tupleTy->getNumElements(); i < size; ++i) {
      auto fieldType = tupleTy.getElementType(i);
      SILType fieldTy = gen.getLoweredType(fieldType).getAddressType();
      SILValue fieldAddr = gen.B.createTupleElementAddr(Loc,
                                                        baseAddr, i,
                                                        fieldTy);
      
      buf.push_back(InitializationPtr(new
                                      KnownAddressInitialization(fieldAddr)));
    }
    finishInitialization(gen);
    return buf;
  }

  SILValue getAddressOrNull() const override {
    return address;
  }

  void bindValue(SILValue value, SILGenFunction &gen) {
    assert(!gen.VarLocs.count(vd) && "Already emitted this vardecl?");
    // If we're binding an address to this let value, then we can use it as an
    // address later.  This happens when binding an address only parameter to
    // an argument, for example.
    if (value.getType().isAddress())
      address = value;
    gen.VarLocs[vd] = SILGenFunction::VarLoc::get(value);

    // Emit a debug_value[_addr] instruction to record the start of this value's
    // lifetime.
    SILLocation PrologueLoc(vd);
    PrologueLoc.markAsPrologue();
    if (address.isValid())
      gen.B.createDebugValueAddr(PrologueLoc, value);
    else
      gen.B.createDebugValue(PrologueLoc, value);
  }
  
  void copyOrInitValueInto(ManagedValue explodedElement, bool isInit,
                           SILLocation loc, SILGenFunction &gen) override {
    // If this let value has an address, we can handle it just like a single
    // buffer value.
    if (hasAddress())
      return SingleBufferInitialization::
           copyOrInitValueIntoSingleBuffer(explodedElement, isInit,
                                           getAddress(), loc, gen);
    
    // Otherwise, we bind the value.
    if (isInit) {
      // Disable the rvalue expression cleanup, since the let value
      // initialization has a cleanup that lives for the entire scope of the
      // let declaration.
      bindValue(explodedElement.forward(gen), gen);
    } else {
      // Disable the expression cleanup of the copy, since the let value
      // initialization has a cleanup that lives for the entire scope of the
      // let declaration.
      bindValue(explodedElement.copyUnmanaged(gen, loc).forward(gen), gen);
    }
  }

  void finishInitialization(SILGenFunction &gen) override {
    assert(!DidFinish &&
           "called LetValueInit::finishInitialization twice!");
    assert(gen.VarLocs.count(vd) && "Didn't bind a value to this let!");
    if (DestroyCleanup != CleanupHandle::invalid())
      gen.Cleanups.setCleanupState(DestroyCleanup, CleanupState::Active);
    DidFinish = true;
  }
};
} // end anonymous namespace


namespace {
/// Initialize a variable of reference-storage type.
class ReferenceStorageInitialization : public Initialization {
  InitializationPtr VarInit;
public:
  ReferenceStorageInitialization(InitializationPtr &&subInit)
    : VarInit(std::move(subInit)) {}

  SILValue getAddressOrNull() const override { return SILValue(); }


  void copyOrInitValueInto(ManagedValue explodedElement, bool isInit,
                           SILLocation loc, SILGenFunction &gen) override {
    // If this is not an initialization, copy the value before we translateIt,
    // translation expects a +1 value.
    if (isInit)
      explodedElement.forwardInto(gen, loc, VarInit->getAddress());
    else
      explodedElement.copyInto(gen, VarInit->getAddress(), loc);
  }
  
  void finishInitialization(SILGenFunction &gen) override {
    VarInit->finishInitialization(gen);
  }
};
} // end anonymous namespace

namespace {
/// Abstract base class for refutable pattern initializations.
class RefutablePatternInitialization : public Initialization {
  /// This is the label to jump to if the pattern fails to match.
  JumpDest failureDest;
public:
  RefutablePatternInitialization(JumpDest failureDest)
    : failureDest(failureDest) {
    assert(failureDest.isValid() &&
           "Refutable patterns can only exist in failable conditions");
  }

  JumpDest getFailureDest() const { return failureDest; }

  SILValue getAddressOrNull() const override { return SILValue(); }

  void copyOrInitValueInto(ManagedValue explodedElement, bool isInit,
                           SILLocation loc, SILGenFunction &SGF) override = 0;


  void bindVariable(SILLocation loc, VarDecl *var, ManagedValue value,
                    CanType formalValueType, SILGenFunction &SGF) {
    // Initialize the variable value.
    InitializationPtr init = SGF.emitInitializationForVarDecl(var);
    RValue(SGF, loc, formalValueType, value).forwardInto(SGF, init.get(), loc);
  }

};
} // end anonymous namespace

namespace {
class ExprPatternInitialization : public RefutablePatternInitialization {
  ExprPattern *P;
public:
  ExprPatternInitialization(ExprPattern *P, JumpDest patternFailDest)
    : RefutablePatternInitialization(patternFailDest), P(P) {}

  void copyOrInitValueInto(ManagedValue explodedElement, bool isInit,
                           SILLocation loc, SILGenFunction &SGF) override;
};
} // end anonymous namespace

void ExprPatternInitialization::
copyOrInitValueInto(ManagedValue value, bool isInit,
                    SILLocation loc, SILGenFunction &SGF) {
  assert(isInit && "Only initialization is supported for refutable patterns");

  FullExpr scope(SGF.Cleanups, CleanupLocation(P));
  bindVariable(P, P->getMatchVar(), value,
               P->getType()->getCanonicalType(), SGF);

  // Emit the match test.
  SILValue testBool;
  {
    FullExpr scope(SGF.Cleanups, CleanupLocation(P->getMatchExpr()));
    testBool = SGF.emitRValueAsSingleValue(P->getMatchExpr()).
       getUnmanagedValue();
  }

  SILBasicBlock *contBB = SGF.B.splitBlockForFallthrough();
  auto falseBB = SGF.Cleanups.emitBlockForCleanups(getFailureDest(), loc);
  SGF.B.createCondBranch(loc, testBool, contBB, falseBB);

  SGF.B.setInsertionPoint(contBB);
}

namespace {
class EnumElementPatternInitialization : public RefutablePatternInitialization {
  EnumElementDecl *ElementDecl;
  InitializationPtr subInitialization;
public:
  EnumElementPatternInitialization(EnumElementDecl *ElementDecl,
                                   InitializationPtr &&subInitialization,
                                   JumpDest patternFailDest)
    : RefutablePatternInitialization(patternFailDest), ElementDecl(ElementDecl),
      subInitialization(std::move(subInitialization)) {}
    
  void copyOrInitValueInto(ManagedValue value, bool isInit,
                           SILLocation loc, SILGenFunction &SGF) override {
    assert(isInit && "Only initialization is supported for refutable patterns");
    emitEnumMatch(value, ElementDecl, subInitialization.get(), getFailureDest(),
                  loc, SGF);
  }

  static void emitEnumMatch(ManagedValue value, EnumElementDecl *ElementDecl,
                            Initialization *subInit, JumpDest FailureDest,
                            SILLocation loc, SILGenFunction &SGF);
  
  void finishInitialization(SILGenFunction &SGF) override {
    if (subInitialization.get())
      subInitialization.get()->finishInitialization(SGF);
  }
};
} // end anonymous namespace

static bool shouldDisableCleanupOnFailurePath(ManagedValue value,
                                              EnumElementDecl *elementDecl,
                                              SILGenFunction &SGF) {
  // If the enum is trivial, then there is no cleanup to disabled.
  if (value.isPlusZeroRValueOrTrivial()) return false;
  
  // Check all of the members of the enum.  If any have a non-trivial payload,
  // then we can't disable the cleanup.
  for (auto elt : elementDecl->getParentEnum()->getAllElements()) {
    // Ignore the element that will be handled.
    if (elt == elementDecl) continue;
    
    // Elements without payloads are trivial.
    if (!elt->hasArgumentType()) continue;
    
    if (!SGF.getTypeLowering(elt->getArgumentType()).isTrivial())
      return false;
  }
  return true;
}

void EnumElementPatternInitialization::
emitEnumMatch(ManagedValue value, EnumElementDecl *ElementDecl,
              Initialization *subInit, JumpDest failureDest,
              SILLocation loc, SILGenFunction &SGF) {
  
  SILBasicBlock *contBB = SGF.B.splitBlockForFallthrough();
  auto destination = std::make_pair(ElementDecl, contBB);
  
  
  // Get a destination that runs all of the cleanups needed when existing on the
  // failure path.  If the enum we're testing is non-trivial, there will be a
  // cleanup in this stack that will release its value.
  //
  // However, if the tested case is the only non-trivial case in the enum, then
  // the destruction on the failure path will be a no-op, so we can disable the
  // cleanup on that path.  This is an important micro-optimization for
  // Optional, since the .None case doesn't need to be cleaned up.
  bool ShouldDisableCleanupOnFailure =
    shouldDisableCleanupOnFailurePath(value, ElementDecl, SGF);
  
  if (ShouldDisableCleanupOnFailure)
    SGF.Cleanups.setCleanupState(value.getCleanup(), CleanupState::Dormant);
  
  auto defaultBB = SGF.Cleanups.emitBlockForCleanups(failureDest, loc);

  // Restore it if we disabled it.
  if (ShouldDisableCleanupOnFailure)
    SGF.Cleanups.setCleanupState(value.getCleanup(), CleanupState::Active);
  
  if (value.getType().isAddress())
    SGF.B.createSwitchEnumAddr(loc, value.getValue(), defaultBB, destination);
  else
    SGF.B.createSwitchEnum(loc, value.getValue(), defaultBB, destination);
  
  SGF.B.setInsertionPoint(contBB);
  
  // If the enum case has no bound value, we're done.
  if (!ElementDecl->hasArgumentType()) {
    assert(subInit == nullptr &&
           "Cannot have a subinit when there is no value to match against");
    return;
  }
  
  // Otherwise, the bound value for the enum case is available.
  SILType eltTy = value.getType().getEnumElementType(ElementDecl, SGF.SGM.M);
  auto &eltTL = SGF.getTypeLowering(eltTy);
  
  // If the case value is provided to us as a BB argument as long as the enum
  // is not address-only.
  SILValue eltValue;
  if (!value.getType().isAddress())
    eltValue = new (SGF.F.getModule()) SILArgument(contBB, eltTy);

  if (subInit == nullptr) {
    // If there is no subinitialization, then we are done matching.  Don't
    // bother projecting out the address-only element value only to ignore it.
    return;
  }
  
  if (value.getType().isAddress()) {
    // If the enum is address-only, take from the enum we have and load it if
    // the element value is loadable.
    eltValue = SGF.B.createUncheckedTakeEnumDataAddr(loc, value.forward(SGF),
                                                     ElementDecl, eltTy);
    // Load a loadable data value.
    if (eltTL.isLoadable())
      eltValue = SGF.B.createLoad(loc, eltValue);
  } else {
    // Otherwise, we're consuming this as a +1 value.
    value.forward(SGF);
  }
  
  // Now we have a +1 value.
  auto eltMV = SGF.emitManagedRValueWithCleanup(eltValue, eltTL);
  
  // Reabstract to the substituted type, if needed.
  CanType substEltTy =
    value.getSwiftType()->getTypeOfMember(SGF.SGM.M.getSwiftModule(),
                                          ElementDecl, nullptr,
                                      ElementDecl->getArgumentInterfaceType())
      ->getCanonicalType();
  
  eltMV = SGF.emitOrigToSubstValue(loc, eltMV,
                             AbstractionPattern(ElementDecl->getArgumentType()),
                                   substEltTy);
  
  // Pass the +1 value down into the sub initialization.
  subInit->copyOrInitValueInto(eltMV, /*is an init*/true, loc, SGF);
}

namespace {
class IsPatternInitialization : public RefutablePatternInitialization {
  IsPattern *pattern;
  InitializationPtr subInitialization;
public:
  IsPatternInitialization(IsPattern *pattern,
                          InitializationPtr &&subInitialization,
                          JumpDest patternFailDest)
  : RefutablePatternInitialization(patternFailDest), pattern(pattern),
    subInitialization(std::move(subInitialization)) {}
    
  void copyOrInitValueInto(ManagedValue explodedElement, bool isInit,
                           SILLocation loc, SILGenFunction &SGF) override;
  
  void finishInitialization(SILGenFunction &SGF) override {
    if (subInitialization.get())
      subInitialization.get()->finishInitialization(SGF);
  }
};
} // end anonymous namespace

void IsPatternInitialization::
copyOrInitValueInto(ManagedValue value, bool isInit,
                    SILLocation loc, SILGenFunction &SGF) {
  assert(isInit && "Only initialization is supported for refutable patterns");
  
  // Try to perform the cast to the destination type, producing an optional that
  // indicates whether we succeeded.
  auto destType = OptionalType::get(pattern->getCastTypeLoc().getType());
  
  value = emitConditionalCheckedCast(SGF, loc, value, pattern->getType(),
                                     destType, pattern->getCastKind(),
                                     SGFContext())
            .getAsSingleValue(SGF, loc);
  
  // Now that we have our result as an optional, we can use an enum projection
  // to do all the work.
  EnumElementPatternInitialization::
  emitEnumMatch(value, SGF.getASTContext().getOptionalSomeDecl(),
                subInitialization.get(), getFailureDest(), loc, SGF);
}

namespace {

/// InitializationForPattern - A visitor for traversing a pattern, generating
/// SIL code to allocate the declared variables, and generating an
/// Initialization representing the needed initializations.
struct InitializationForPattern
  : public PatternVisitor<InitializationForPattern, InitializationPtr>
{
  SILGenFunction &SGF;

  /// This is the place that should be jumped to if the pattern fails to match.
  /// This is invalid for irrefutable pattern initializations.
  JumpDest patternFailDest;

  InitializationForPattern(SILGenFunction &SGF, JumpDest patternFailDest)
    : SGF(SGF), patternFailDest(patternFailDest) {}

  // Paren, Typed, and Var patterns are noops, just look through them.
  InitializationPtr visitParenPattern(ParenPattern *P) {
    return visit(P->getSubPattern());
  }
  InitializationPtr visitTypedPattern(TypedPattern *P) {
    return visit(P->getSubPattern());
  }
  InitializationPtr visitVarPattern(VarPattern *P) {
    return visit(P->getSubPattern());
  }

  // AnyPatterns (i.e, _) don't require any storage. Any value bound here will
  // just be dropped.
  InitializationPtr visitAnyPattern(AnyPattern *P) {
    return InitializationPtr(new BlackHoleInitialization());
  }

  // Bind to a named pattern by creating a memory location and initializing it
  // with the initial value.
  InitializationPtr visitNamedPattern(NamedPattern *P) {
    if (!P->getDecl()->hasName()) {
      // Unnamed parameters don't require any storage. Any value bound here will
      // just be dropped.
      return InitializationPtr(new BlackHoleInitialization());
    }

    return SGF.emitInitializationForVarDecl(P->getDecl());
  }

  // Bind a tuple pattern by aggregating the component variables into a
  // TupleInitialization.
  InitializationPtr visitTuplePattern(TuplePattern *P) {
    TupleInitialization *init = new TupleInitialization();
    for (auto &elt : P->getElements())
      init->subInitializations.push_back(visit(elt.getPattern()));
    return InitializationPtr(init);
  }

  InitializationPtr visitEnumElementPattern(EnumElementPattern *P) {
    InitializationPtr subInit;
    if (auto *subP = P->getSubPattern())
      subInit = visit(subP);
    auto *res = new EnumElementPatternInitialization(P->getElementDecl(),
                                                     std::move(subInit),
                                                     patternFailDest);
    return InitializationPtr(res);
  }
  InitializationPtr visitOptionalSomePattern(OptionalSomePattern *P) {
    InitializationPtr subInit = visit(P->getSubPattern());
    auto *res = new EnumElementPatternInitialization(P->getElementDecl(),
                                                     std::move(subInit),
                                                     patternFailDest);
    return InitializationPtr(res);
  }
  InitializationPtr visitIsPattern(IsPattern *P) {
    InitializationPtr subInit;
    if (auto *subP = P->getSubPattern())
      subInit = visit(subP);
    return InitializationPtr(new IsPatternInitialization(P, std::move(subInit),
                                                         patternFailDest));
  }
  InitializationPtr visitBoolPattern(BoolPattern *P) {
    llvm_unreachable("bools not supported in let/else yet");
  }
  InitializationPtr visitExprPattern(ExprPattern *P) {
    return InitializationPtr(new ExprPatternInitialization(P, patternFailDest));
  }
  InitializationPtr visitNominalTypePattern(NominalTypePattern *P) {
    P->dump();
    llvm_unreachable("pattern not supported in let/else yet");
  }
};

} // end anonymous namespace

InitializationPtr SILGenFunction::emitInitializationForVarDecl(VarDecl *vd) {
  // If this is a computed variable, we don't need to do anything here.
  // We'll generate the getter and setter when we see their FuncDecls.
  if (!vd->hasStorage())
    return InitializationPtr(new BlackHoleInitialization());

  if (vd->isDebuggerVar()) {
    DebuggerClient *DebugClient = SGM.SwiftModule->getDebugClient();
    assert(DebugClient && "Debugger variables with no debugger client");
    SILDebuggerClient *SILDebugClient = DebugClient->getAsSILDebuggerClient();
    assert(SILDebugClient && "Debugger client doesn't support SIL");
    SILValue SV = SILDebugClient->emitLValueForVariable(vd, B);

    VarLocs[vd] = SILGenFunction::VarLoc::get(SV);
    return InitializationPtr(new KnownAddressInitialization(SV));
  }

  CanType varType = vd->getType()->getCanonicalType();

  assert(!isa<InOutType>(varType) && "local variables should never be inout");

  // If this is a 'let' initialization for a non-global, set up a
  // let binding, which stores the initialization value into VarLocs directly.
  if (vd->isLet() && vd->getDeclContext()->isLocalContext() &&
      !isa<ReferenceStorageType>(varType))
    return InitializationPtr(new LetValueInitialization(vd, *this));

  // If the variable has no initial value, emit a mark_uninitialized instruction
  // so that DI tracks and enforces validity of it.
  bool isUninitialized =
    vd->getParentPatternBinding() && !vd->getParentInitializer();
  
  // If this is a global variable, initialize it without allocations or
  // cleanups.
  InitializationPtr Result;
  if (!vd->getDeclContext()->isLocalContext()) {
    auto *silG = SGM.getSILGlobalVariable(vd, NotForDefinition);
    SILValue addr = B.createGlobalAddr(vd, silG);
    if (isUninitialized)
      addr = B.createMarkUninitializedVar(vd, addr);

    VarLocs[vd] = SILGenFunction::VarLoc::get(addr);
    Result = InitializationPtr(new KnownAddressInitialization(addr));
  } else {
    Result = emitLocalVariableWithCleanup(vd, isUninitialized);
  }

  // If we're initializing a weak or unowned variable, this requires a change in
  // type.
  if (isa<ReferenceStorageType>(varType))
    Result = InitializationPtr(new
                           ReferenceStorageInitialization(std::move(Result)));
  return Result;
}

void SILGenFunction::visitPatternBindingDecl(PatternBindingDecl *PBD) {
  // If this is a conditional PBD, it will have an 'else' block.  Create a block
  // and emit code into it before processing any of the patterns, because none
  // of the bound variables will be in scope in the 'else' context.
  JumpDest elseBB = JumpDest::invalid();
  if (PBD->isRefutable()) {
    assert(PBD->getElse().isExplicit() &&
           "Refutable PatternBinding must have an else block");

    // Create a basic block for the else block we'll jump to.
    elseBB = JumpDest(createBasicBlock(), getCleanupsDepth(),
                      CleanupLocation(PBD));
    
    // Move the insertion point to the 'else' block temporarily and emit the
    // body of the 'else'.
    SavedInsertionPoint savedIP(*this, elseBB.getBlock());
    emitStmt(PBD->getElse().getExplicitBody());

    // The else block must end in a noreturn call, return, break etc.  It
    // isn't valid to fall off into the normal flow.  To model this, we emit
    // an unreachable instruction and then have SIL diagnostic check this.
    if (B.hasValidInsertionPoint())
      B.createUnreachable(PBD);
  }

  // Allocate the variables and build up an Initialization over their
  // allocated storage.
  for (auto entry : PBD->getPatternList()) {
    InitializationPtr initialization =
      InitializationForPattern(*this, elseBB).visit(entry.ThePattern);

    // If an initial value expression was specified by the decl, emit it into
    // the initialization. Otherwise, mark it uninitialized for DI to resolve.
    if (auto *Init = entry.Init) {
      FullExpr Scope(Cleanups, CleanupLocation(Init));
      emitExprInto(Init, initialization.get());
    } else {
      initialization->finishInitialization(*this);
    }
  }
}


/// Emit the boolean test and/or pattern bindings indicated by the specified
/// stmt condition.  If the condition fails, control flow is transfered to the
/// specified JumpDest.  The insertion point is left in the block where the
/// condition has matched and any bound variables are in scope.
///
void SILGenFunction::emitStmtCondition(StmtCondition Cond,
                                       JumpDest FailDest, SILLocation loc) {

  assert(B.hasValidInsertionPoint() &&
         "emitting condition at unreachable point");
  
  for (const auto &elt : Cond) {
    // Handle boolean conditions.
    if (auto *expr = elt.getCondition()) {
      // Evaluate the condition as an i1 value (guaranteed by Sema).
      SILValue V;
      {
        FullExpr Scope(Cleanups, CleanupLocation(expr));
        V = emitRValue(expr).forwardAsSingleValue(*this, expr);
      }
      assert(V.getType().castTo<BuiltinIntegerType>()->isFixedWidth(1) &&
             "Sema forces conditions to have Builtin.i1 type");
      
      // Just branch on the condition.  On failure, we unwind any active cleanups,
      // on success we fall through to a new block.
      SILBasicBlock *ContBB = createBasicBlock();
      auto FailBB = Cleanups.emitBlockForCleanups(FailDest, loc);
      B.createCondBranch(expr, V, ContBB, FailBB);
      
      // Finally, emit the continue block and keep emitting the rest of the
      // condition.
      B.emitBlock(ContBB);
      continue;
    }
    
    auto *PBD = elt.getBinding();
    for (auto entry : PBD->getPatternList()) {
      InitializationPtr initialization =
        InitializationForPattern(*this, FailDest).visit(entry.ThePattern);
      
      // If an initial value expression was specified by the decl, emit it into
      // the initialization. Otherwise, mark it uninitialized for DI to resolve.
      if (auto *Init = entry.Init) {
        FullExpr Scope(Cleanups, CleanupLocation(Init));
        emitExprInto(Init, initialization.get());
      } else {
        initialization->finishInitialization(*this);
      }
    }
    
    if (auto where = PBD->getWhereExpr()) {
      // Evaluate the condition as an i1 value (guaranteed by Sema).
      SILValue V;
      {
        FullExpr Scope(Cleanups, CleanupLocation(where));
        V = emitRValue(where).forwardAsSingleValue(*this, where);
      }
      assert(V.getType().castTo<BuiltinIntegerType>()->isFixedWidth(1) &&
             "Sema forces conditions to have Builtin.i1 type");
      
      // Just branch on the condition.  On failure, we unwind any active cleanups,
      // on success we fall through to a new block.
      SILBasicBlock *ContBB = createBasicBlock();
      auto FailBB = Cleanups.emitBlockForCleanups(FailDest, loc);
      B.createCondBranch(where, V, ContBB, FailBB);
      
      // Finally, emit the continue block and keep emitting the rest of the
      // condition.
      B.emitBlock(ContBB);
    }
  }
}

InitializationPtr
SILGenFunction::emitPatternBindingInitialization(Pattern *P) {
  return InitializationForPattern(*this, JumpDest::invalid()).visit(P);
}

/// Enter a cleanup to deallocate the given location.
CleanupHandle SILGenFunction::enterDeallocStackCleanup(SILValue temp) {
  assert(temp.getType().isLocalStorage() &&
         "must deallocate container operand, not address operand!");
  Cleanups.pushCleanup<DeallocStackCleanup>(temp);
  return Cleanups.getTopCleanup();
}

CleanupHandle SILGenFunction::enterDestroyCleanup(SILValue valueOrAddr) {
  Cleanups.pushCleanup<ReleaseValueCleanup>(valueOrAddr);
  return Cleanups.getTopCleanup();
}

void SILGenModule::emitExternalDefinition(Decl *d) {
  switch (d->getKind()) {
  case DeclKind::Func: {
    // We'll emit all the members of an enum when we visit the enum.
    if (isa<EnumDecl>(d->getDeclContext()))
      break;
    emitFunction(cast<FuncDecl>(d));
    break;
  }
  case DeclKind::Constructor: {
    auto C = cast<ConstructorDecl>(d);
    // We'll emit all the members of an enum when we visit the enum.
    if (isa<EnumDecl>(d->getDeclContext()))
      break;
    // For factories, we don't need to emit a special thunk; the normal
    // foreign-to-native thunk is sufficient.
    if (C->isFactoryInit())
      break;

    emitConstructor(C);
    break;
  }
  case DeclKind::Enum: {
    auto ed = cast<EnumDecl>(d);
    // Emit the enum cases and derived conformance methods for the type.
    for (auto member : ed->getMembers()) {
      if (auto elt = dyn_cast<EnumElementDecl>(member))
        emitEnumConstructor(elt);
      else if (auto func = dyn_cast<FuncDecl>(member))
        emitFunction(func);
      else if (auto ctor = dyn_cast<ConstructorDecl>(member))
        emitConstructor(ctor);
    }
    // Emit derived global decls.
    for (auto derived : ed->getDerivedGlobalDecls()) {
      emitFunction(cast<FuncDecl>(derived));
    }
    SWIFT_FALLTHROUGH;
  }
  case DeclKind::Struct:
  case DeclKind::Class: {
    // Emit witness tables.
    for (auto c : cast<NominalTypeDecl>(d)->getLocalConformances()) {
      if (Types.protocolRequiresWitnessTable(c->getProtocol()) &&
          c->isComplete() && isa<NormalProtocolConformance>(c))
        getWitnessTable(c);
    }
    break;
  }

  case DeclKind::Protocol:
    // Nothing to do in SILGen for other external types.
    break;

  case DeclKind::Var:
    // Imported static vars are handled solely in IRGen.
    break;

  case DeclKind::IfConfig:
  case DeclKind::Extension:
  case DeclKind::PatternBinding:
  case DeclKind::EnumCase:
  case DeclKind::EnumElement:
  case DeclKind::TopLevelCode:
  case DeclKind::TypeAlias:
  case DeclKind::AssociatedType:
  case DeclKind::GenericTypeParam:
  case DeclKind::Param:
  case DeclKind::Import:
  case DeclKind::Subscript:
  case DeclKind::Destructor:
  case DeclKind::InfixOperator:
  case DeclKind::PrefixOperator:
  case DeclKind::PostfixOperator:
    llvm_unreachable("Not a valid external definition for SILGen");
  }
}

/// Create a LocalVariableInitialization for the uninitialized var.
InitializationPtr SILGenFunction::
emitLocalVariableWithCleanup(VarDecl *vd, bool NeedsMarkUninit) {
  return InitializationPtr(new LocalVariableInitialization(vd, NeedsMarkUninit,
                                                           *this));
}

/// Create an Initialization for an uninitialized temporary.
std::unique_ptr<TemporaryInitialization>
SILGenFunction::emitTemporary(SILLocation loc, const TypeLowering &tempTL) {
  SILValue addr = emitTemporaryAllocation(loc, tempTL.getLoweredType());
  return useBufferAsTemporary(loc, addr, tempTL);
}

/// Create an Initialization for an uninitialized buffer.
std::unique_ptr<TemporaryInitialization>
SILGenFunction::useBufferAsTemporary(SILLocation loc,
                                     SILValue addr,
                                     const TypeLowering &tempTL) {
  CleanupHandle cleanup = enterDormantTemporaryCleanup(addr, tempTL);
  return std::unique_ptr<TemporaryInitialization>(
                                    new TemporaryInitialization(addr, cleanup));
}

CleanupHandle
SILGenFunction::enterDormantTemporaryCleanup(SILValue addr,
                                             const TypeLowering &tempTL) {
  if (tempTL.isTrivial())
    return CleanupHandle::invalid();

  Cleanups.pushCleanupInState<ReleaseValueCleanup>(CleanupState::Dormant, addr);
  return Cleanups.getCleanupsDepth();
}

void SILGenFunction::destroyLocalVariable(SILLocation silLoc, VarDecl *vd) {
  assert(vd->getDeclContext()->isLocalContext() &&
         "can't emit a local var for a non-local var decl");
  assert(vd->hasStorage() && "can't emit storage for a computed variable");

  assert(VarLocs.count(vd) && "var decl wasn't emitted?!");

  auto loc = VarLocs[vd];

  // For a heap variable, the box is responsible for the value. We just need
  // to give up our retain count on it.
  if (loc.box) {
    B.emitStrongReleaseAndFold(silLoc, loc.box);
    return;
  }

  // For 'let' bindings, we emit a release_value or destroy_addr, depending on
  // whether we have an address or not.
  SILValue Val = loc.value;
  if (!Val.getType().isAddress())
    B.emitReleaseValueOperation(silLoc, Val);
  else
    B.emitDestroyAddrAndFold(silLoc, Val);
}

void SILGenFunction::deallocateUninitializedLocalVariable(SILLocation silLoc,
                                                          VarDecl *vd) {
  assert(vd->getDeclContext()->isLocalContext() &&
         "can't emit a local var for a non-local var decl");
  assert(vd->hasStorage() && "can't emit storage for a computed variable");

  assert(VarLocs.count(vd) && "var decl wasn't emitted?!");

  auto loc = VarLocs[vd];

  // Ignore let values captured without a memory location.
  if (!loc.value.getType().isAddress()) return;

  assert(loc.box && "captured var should have been given a box");
  B.createDeallocBox(silLoc, loc.value.getType().getObjectType(),
                     loc.box);
}

namespace {

// Is this a free function witness satisfying a static method requirement?
static IsFreeFunctionWitness_t isFreeFunctionWitness(ValueDecl *requirement,
                                                     ValueDecl *witness) {
  if (!witness->getDeclContext()->isTypeContext()) {
    assert(!requirement->isInstanceMember()
           && "free function satisfying instance method requirement?!");
    return IsFreeFunctionWitness;
  }

  return IsNotFreeFunctionWitness;
}

/// Emit a witness table for a protocol conformance.
class SILGenConformance : public Lowering::ASTVisitor<SILGenConformance> {
public:
  SILGenModule &SGM;
  NormalProtocolConformance *Conformance;
  std::vector<SILWitnessTable::Entry> Entries;
  SILLinkage Linkage;

  SILGenConformance(SILGenModule &SGM, NormalProtocolConformance *C)
    // We only need to emit witness tables for base NormalProtocolConformances.
    : SGM(SGM), Conformance(C->getRootNormalConformance()),
      Linkage(SGM.Types.getLinkageForProtocolConformance(Conformance,
                                                         ForDefinition))
  {
    // Not all protocols use witness tables.
    if (!SGM.Types.protocolRequiresWitnessTable(Conformance->getProtocol()))
      Conformance = nullptr;
  }

  SILWitnessTable *emit() {
    // Nothing to do if this wasn't a normal conformance.
    if (!Conformance)
      return nullptr;

    // Reference conformances for refined protocols.
    auto protocol = Conformance->getProtocol();
    for (auto base : protocol->getInheritedProtocols(nullptr))
      emitBaseProtocolWitness(base);

    // Emit witnesses in protocol declaration order.
    for (auto reqt : protocol->getMembers())
      visit(reqt);

    // Check if we already have a declaration or definition for this witness
    // table.
    if (auto *wt = SGM.M.lookUpWitnessTable(Conformance, false).first) {
      // If we have a definition already, just return it.
      //
      // FIXME: I am not sure if this is possible, if it is not change this to an
      // assert.
      if (wt->isDefinition())
        return wt;

      // If we have a declaration, convert the witness table to a definition.
      if (wt->isDeclaration()) {
        wt->convertToDefinition(Entries, SGM.makeModuleFragile);

        // Since we had a declaration before, its linkage should be external,
        // ensure that we have a compatible linkage for sanity. *NOTE* we are ok
        // with both being shared since we do not have a shared_external
        // linkage.
        assert(stripExternalFromLinkage(wt->getLinkage()) == Linkage &&
               "Witness table declaration has inconsistent linkage with"
               " silgen definition.");

        // And then override the linkage with the new linkage.
        wt->setLinkage(Linkage);
        return wt;
      }
    }

    // Otherwise if we have no witness table yet, create it.
    return SILWitnessTable::create(SGM.M, Linkage, SGM.makeModuleFragile,
                                 Conformance, Entries);
  }

  void emitBaseProtocolWitness(ProtocolDecl *baseProtocol) {
    // Only include the witness if the base protocol requires it.
    if (!SGM.Types.protocolRequiresWitnessTable(baseProtocol))
      return;

    auto foundBaseConformance
      = Conformance->getInheritedConformances().find(baseProtocol);
    assert(foundBaseConformance != Conformance->getInheritedConformances().end()
           && "no inherited conformance for base protocol");

    auto conformance = foundBaseConformance->second;

    Entries.push_back(SILWitnessTable::BaseProtocolWitness{
      baseProtocol,
      conformance,
    });

    // Emit the witness table for the base conformance if it belongs to this
    // module or is shared.
    if (conformance->getDeclContext()->getParentModule()
          == SGM.SwiftModule
        || SGM.Types.getLinkageForProtocolConformance(
                                        conformance->getRootNormalConformance(),
                                        NotForDefinition)
          == SILLinkage::Shared)
      SGM.getWitnessTable(conformance->getRootNormalConformance());
  }

  /// Fallback for unexpected protocol requirements.
  void visitDecl(Decl *d) {
    d->print(llvm::errs());
    llvm_unreachable("unhandled protocol requirement");
  }

  void visitFuncDecl(FuncDecl *fd) {
    // FIXME: Emit getter and setter (if settable) witnesses.
    // For now we ignore them, like the IRGen witness table builder did.
    if (fd->isAccessor())
      return;

    // Find the witness in the conformance.
    ConcreteDeclRef witness = Conformance->getWitness(fd, nullptr);
    emitFuncEntry(fd, witness.getDecl(), witness.getSubstitutions());
  }

  void emitFuncEntry(FuncDecl *fd, ValueDecl *witnessDecl,
                     ArrayRef<Substitution> WitnessSubstitutions) {
    // Emit the witness thunk and add it to the table.

    // If this is a non-present optional requirement, emit a MissingOptional.
    if (!witnessDecl) {
      assert(fd->getAttrs().hasAttribute<OptionalAttr>() &&
             "Non-optional protocol requirement lacks a witness?");
      Entries.push_back(SILWitnessTable::MissingOptionalWitness{ fd });
      return;
    }


    // TODO: multiple resilience expansions?
    // TODO: multiple uncurry levels?
    SILDeclRef requirementRef(fd, SILDeclRef::Kind::Func,
                              ResilienceExpansion::Minimal);
    // Free function witnesses have an implicit uncurry layer imposed on them by
    // the inserted metatype argument.
    auto isFree = isFreeFunctionWitness(fd, witnessDecl);
    unsigned witnessUncurryLevel = isFree ? requirementRef.uncurryLevel - 1
                                          : requirementRef.uncurryLevel;

    SILDeclRef witnessRef(witnessDecl, SILDeclRef::Kind::Func,
                          SILDeclRef::ConstructAtBestResilienceExpansion,
                          witnessUncurryLevel);

    SILFunction *witnessFn =
      SGM.emitProtocolWitness(Conformance, Linkage, requirementRef, witnessRef,
                              isFree, WitnessSubstitutions);
    Entries.push_back(
                    SILWitnessTable::MethodWitness{requirementRef, witnessFn});
  }

  void visitConstructorDecl(ConstructorDecl *cd) {
    SILDeclRef requirementRef(cd, SILDeclRef::Kind::Allocator,
                              ResilienceExpansion::Minimal);

    ConcreteDeclRef witness = Conformance->getWitness(cd, nullptr);
    SILDeclRef witnessRef(witness.getDecl(), SILDeclRef::Kind::Allocator,
                          SILDeclRef::ConstructAtBestResilienceExpansion,
                          requirementRef.uncurryLevel);
    SILFunction *witnessFn =
      SGM.emitProtocolWitness(Conformance, Linkage, requirementRef, witnessRef,
                              IsNotFreeFunctionWitness,
                              witness.getSubstitutions());
    Entries.push_back(
      SILWitnessTable::MethodWitness{requirementRef, witnessFn});
  }

  void visitAbstractStorageDecl(AbstractStorageDecl *d) {
    // Find the witness in the conformance.
    ConcreteDeclRef witness = Conformance->getWitness(d, nullptr);
    auto *witnessSD = cast<AbstractStorageDecl>(witness.getDecl());
    emitFuncEntry(d->getGetter(), witnessSD->getGetter(),
                  witness.getSubstitutions());
    if (d->isSettable(d->getDeclContext()))
      emitFuncEntry(d->getSetter(), witnessSD->getSetter(),
                    witness.getSubstitutions());
    if (auto materializeForSet = d->getMaterializeForSetFunc())
      emitFuncEntry(materializeForSet, witnessSD->getMaterializeForSetFunc(),
                    witness.getSubstitutions());
  }

  void visitAssociatedTypeDecl(AssociatedTypeDecl *td) {
    // Find the substitution info for the witness type.
    const auto &witness = Conformance->getTypeWitness(td, /*resolver=*/nullptr);

    // Emit the record for the type itself.
    Entries.push_back(SILWitnessTable::AssociatedTypeWitness{td,
                                witness.getReplacement()->getCanonicalType()});

    // Emit records for the protocol requirements on the type.
    assert(td->getConformingProtocols(nullptr).size()
             == witness.getConformances().size()
           && "number of conformances in assoc type substitution do not match "
              "number of requirements on assoc type");
    // The conformances should be all null or all nonnull.
    assert(witness.getConformances().empty()
           || (witness.getConformances()[0]
                 ? std::all_of(witness.getConformances().begin(),
                               witness.getConformances().end(),
                               [&](const ProtocolConformance *C) -> bool {
                                 return C;
                               })
                 : std::all_of(witness.getConformances().begin(),
                               witness.getConformances().end(),
                               [&](const ProtocolConformance *C) -> bool {
                                 return !C;
                               })));

    for (unsigned i = 0, e = td->getConformingProtocols(nullptr).size(); i < e;
         ++i) {
      auto protocol = td->getConformingProtocols(nullptr)[i];

      // Only reference the witness if the protocol requires it.
      if (!SGM.Types.protocolRequiresWitnessTable(protocol))
        continue;

      ProtocolConformance *conformance = nullptr;
      // If the associated type requirement is satisfied by an associated type,
      // these will all be null.
      if (witness.getConformances()[0]) {
        auto foundConformance = std::find_if(witness.getConformances().begin(),
                                        witness.getConformances().end(),
                                        [&](ProtocolConformance *c) {
                                          return c->getProtocol() == protocol;
                                        });
        assert(foundConformance != witness.getConformances().end());
        conformance = *foundConformance;
      }

      Entries.push_back(SILWitnessTable::AssociatedTypeProtocolWitness{
        td, protocol, conformance
      });
    }
  }

  void visitPatternBindingDecl(PatternBindingDecl *pbd) {
    // We only care about the contained VarDecls.
  }

  void visitIfConfigDecl(IfConfigDecl *icd) {
    // We only care about the active members, which were already subsumed by the
    // enclosing type.
  }
};

} // end anonymous namespace

SILWitnessTable *
SILGenModule::getWitnessTable(ProtocolConformance *conformance) {
  auto normal = conformance->getRootNormalConformance();

  // If we've already emitted this witness table, return it.
  auto found = emittedWitnessTables.find(normal);
  if (found != emittedWitnessTables.end())
    return found->second;

  SILWitnessTable *table = SILGenConformance(*this, normal).emit();
  emittedWitnessTables.insert({normal, table});
  return table;
}

/// FIXME: This should just be a call down to Types.getLoweredType(), but I
/// really don't want to thread an old-type/interface-type pair through all
/// of TypeLowering.
static SILType
getWitnessFunctionType(SILModule &M,
                       AbstractionPattern origRequirementTy,
                       CanAnyFunctionType witnessSubstTy,
                       CanAnyFunctionType witnessSubstIfaceTy,
                       unsigned uncurryLevel) {
  // Lower the types to uncurry and get ExtInfo.
  AbstractionPattern origLoweredTy = origRequirementTy;
  if (auto origFTy = origRequirementTy.getAs<AnyFunctionType>())
    origLoweredTy =
      AbstractionPattern(M.Types.getLoweredASTFunctionType(origFTy,
                                                           uncurryLevel,
                                                           None));
  auto witnessLoweredTy
    = M.Types.getLoweredASTFunctionType(witnessSubstTy, uncurryLevel, None);
  auto witnessLoweredIfaceTy
    = M.Types.getLoweredASTFunctionType(witnessSubstIfaceTy, uncurryLevel, None);

  // Convert to SILFunctionType.
  auto fnTy = getNativeSILFunctionType(M, origLoweredTy,
                                       witnessLoweredTy,
                                       witnessLoweredIfaceTy);
  return SILType::getPrimitiveObjectType(fnTy);
}

SILFunction *
SILGenModule::emitProtocolWitness(ProtocolConformance *conformance,
                                  SILLinkage linkage,
                                  SILDeclRef requirement,
                                  SILDeclRef witness,
                                  IsFreeFunctionWitness_t isFree,
                                  ArrayRef<Substitution> witnessSubs) {
  // Get the type of the protocol requirement and the original type of the
  // witness.
  // FIXME: Rework for interface types.
  auto requirementInfo = Types.getConstantInfo(requirement);
  auto requirementTy
    = cast<PolymorphicFunctionType>(requirementInfo.FormalType);
  unsigned witnessUncurryLevel = witness.uncurryLevel;

  // Substitute the 'self' type into the requirement to get the concrete
  // witness type.
  auto witnessSubstTy = cast<AnyFunctionType>(
    requirementTy
      ->substGenericArgs(conformance->getDeclContext()->getParentModule(),
                         conformance->getType())
      ->getCanonicalType());

  GenericParamList *conformanceParams = conformance->getGenericParams();

  // If the requirement is generic, reparent its generic parameter list to
  // the generic parameters of the conformance.
  CanType methodTy = witnessSubstTy.getResult();
  if (auto pft = dyn_cast<PolymorphicFunctionType>(methodTy)) {
    auto &reqtParams = pft->getGenericParams();
    // Preserve the depth of generic arguments by adding an empty outer generic
    // param list if the conformance is concrete.
    GenericParamList *outerParams = conformanceParams;
    if (!outerParams)
      outerParams = GenericParamList::getEmpty(getASTContext());
    auto methodParams
      = reqtParams.cloneWithOuterParameters(getASTContext(), outerParams);
    methodTy = CanPolymorphicFunctionType::get(pft.getInput(), pft.getResult(),
                                               methodParams,
                                               pft->getExtInfo());
  }

  // If the conformance is generic, its generic parameters apply to
  // the witness as its outer generic param list.
  if (conformanceParams) {
    witnessSubstTy = CanPolymorphicFunctionType::get(witnessSubstTy.getInput(),
                                                   methodTy,
                                                   conformanceParams,
                                                   witnessSubstTy->getExtInfo());
  } else {
    witnessSubstTy = CanFunctionType::get(witnessSubstTy.getInput(),
                                          methodTy,
                                          witnessSubstTy->getExtInfo());
  }

  // If the witness is a free function, consider the self argument
  // uncurry level.
  if (isFree)
    ++witnessUncurryLevel;

  // The witness SIL function has the type of the AST-level witness, at the
  // abstraction level of the original protocol requirement.
  assert(requirement.uncurryLevel == witnessUncurryLevel &&
         "uncurry level of requirement and witness do not match");

  // Work out the interface type for the witness.
  auto reqtIfaceTy
    = cast<GenericFunctionType>(requirementInfo.FormalInterfaceType);
  // Substitute the 'self' type into the requirement to get the concrete witness
  // type, leaving the other generic parameters open.
  CanAnyFunctionType witnessSubstIfaceTy = cast<AnyFunctionType>(
    reqtIfaceTy->partialSubstGenericArgs(conformance->getDeclContext()->getParentModule(),
                                         conformance->getInterfaceType())
               ->getCanonicalType());

  // If the conformance is generic, its generic parameters apply to the witness.
  GenericSignature *sig
    = conformance->getGenericSignature();
  if (sig) {
    if (auto gft = dyn_cast<GenericFunctionType>(witnessSubstIfaceTy)) {
      SmallVector<GenericTypeParamType*, 4> allParams(sig->getGenericParams().begin(),
                                                      sig->getGenericParams().end());
      allParams.append(gft->getGenericParams().begin(),
                       gft->getGenericParams().end());
      SmallVector<Requirement, 4> allReqts(sig->getRequirements().begin(),
                                           sig->getRequirements().end());
      allReqts.append(gft->getRequirements().begin(),
                      gft->getRequirements().end());
      GenericSignature *witnessSig = GenericSignature::get(allParams, allReqts);

      witnessSubstIfaceTy = cast<GenericFunctionType>(
        GenericFunctionType::get(witnessSig,
                                 gft.getInput(), gft.getResult(),
                                 gft->getExtInfo())
          ->getCanonicalType());
    } else {
      assert(isa<FunctionType>(witnessSubstIfaceTy));
      witnessSubstIfaceTy = cast<GenericFunctionType>(
        GenericFunctionType::get(sig,
                                 witnessSubstIfaceTy.getInput(),
                                 witnessSubstIfaceTy.getResult(),
                                 witnessSubstIfaceTy->getExtInfo())
          ->getCanonicalType());
    }
  }
  // Lower the witness type with the requirement's abstraction level.
  // FIXME: We should go through TypeConverter::getLoweredType once we settle
  // on interface types.
  /*
  SILType witnessSILType = Types.getLoweredType(
                                              AbstractionPattern(requirementTy),
                                              witnessSubstTy,
                                              requirement.uncurryLevel);
   */
  SILType witnessSILType = getWitnessFunctionType(M,
                                              AbstractionPattern(requirementTy),
                                              witnessSubstTy,
                                              witnessSubstIfaceTy,
                                              requirement.uncurryLevel);

  // Mangle the name of the witness thunk.
  llvm::SmallString<128> nameBuffer;
  {
    llvm::raw_svector_ostream nameStream(nameBuffer);
    nameStream << "_TTW";
    Mangler mangler(nameStream);
    mangler.mangleProtocolConformance(conformance);

    if (auto ctor = dyn_cast<ConstructorDecl>(requirement.getDecl())) {
      mangler.mangleConstructorEntity(ctor, /*isAllocating=*/true,
                                      ResilienceExpansion::Minimal,
                                      requirement.uncurryLevel);
    } else {
      assert(isa<FuncDecl>(requirement.getDecl())
             && "need to handle mangling of non-Func SILDeclRefs here");
      auto requiredDecl = cast<FuncDecl>(requirement.getDecl());
      auto accessorKind = requiredDecl->getAccessorKind();
      if (accessorKind != AccessorKind::NotAccessor) {
        mangler.mangleAccessorEntity(accessorKind,
                                     requiredDecl->getAddressorKind(),
                                     requiredDecl->getAccessorStorageDecl(),
                                     ResilienceExpansion::Minimal);
      } else {
        mangler.mangleEntity(requiredDecl, ResilienceExpansion::Minimal,
                             requirement.uncurryLevel);
      }
    }
  }

  // Collect the context generic parameters for the witness.
  GenericParamList *witnessContextParams = conformanceParams;
  // If the requirement is generic, reparent its parameters to the conformance
  // parameters.
  if (auto reqtParams = requirementInfo.InnerGenericParams) {
    // Preserve the depth of generic arguments by adding an empty outer generic
    // param list if the conformance is concrete.
    GenericParamList *outerParams = conformanceParams;
    if (!outerParams)
      outerParams = GenericParamList::getEmpty(getASTContext());

    witnessContextParams
      = reqtParams->cloneWithOuterParameters(getASTContext(), outerParams);
  }

  // If the thunked-to function is set to be always inlined, do the
  // same with the witness, on the theory that the user wants all
  // calls removed if possible, e.g. when we're able to devirtualize
  // the witness method call. Otherwise, use the default inlining
  // setting on the theory that forcing inlining off should only
  // effect the user's function, not otherwise invisible thunks.
  Inline_t InlineStrategy = InlineDefault;
  if (witness.isAlwaysInline())
    InlineStrategy = AlwaysInline;

  auto *f = SILFunction::create(M, linkage, nameBuffer,
                                witnessSILType.castTo<SILFunctionType>(),
                                witnessContextParams,
                                SILLocation(witness.getDecl()),
                                IsNotBare,
                                IsTransparent,
                                makeModuleFragile ? IsFragile : IsNotFragile,
                                IsThunk,
                                SILFunction::NotRelevant,
                                InlineStrategy);

  f->setDebugScope(new (M)
                   SILDebugScope(RegularLocation(witness.getDecl()), *f));

  // Create the witness.
  SILGenFunction(*this, *f)
    .emitProtocolWitness(conformance, requirement, witness, witnessSubs,isFree);

  f->verify();

  return f;
}

SILFunction * SILGenModule::
getOrCreateReabstractionThunk(GenericParamList *thunkContextParams,
                              CanSILFunctionType thunkType,
                              CanSILFunctionType fromType,
                              CanSILFunctionType toType,
                              IsFragile_t Fragile) {
  // Mangle the reabstraction thunk.
  llvm::SmallString<256> buffer;
  {
    llvm::raw_svector_ostream stream(buffer);
    Mangler mangler(stream);

    // This is actually the SIL helper function.  For now, IR-gen
    // makes the actual thunk.
    stream << "_TTR";
    if (auto generics = thunkType->getGenericSignature()) {
      stream << 'G';
      mangler.mangleGenericSignature(generics,
                                     ResilienceExpansion::Minimal);
    }

    // Substitute context parameters out of the "from" and "to" types.
    auto fromInterfaceType
      = Types.getInterfaceTypeOutOfContext(fromType, thunkContextParams);
    auto toInterfaceType
      = Types.getInterfaceTypeOutOfContext(toType, thunkContextParams);

    mangler.mangleType(fromInterfaceType,
                       ResilienceExpansion::Minimal, /*uncurry*/ 0);
    mangler.mangleType(toInterfaceType,
                       ResilienceExpansion::Minimal, /*uncurry*/ 0);
  }

  auto loc = RegularLocation::getAutoGeneratedLocation();
  return M.getOrCreateSharedFunction(loc,
                                     buffer.str(),
                                     thunkType,
                                     IsBare, IsTransparent,
                                     Fragile, IsThunk);
}
