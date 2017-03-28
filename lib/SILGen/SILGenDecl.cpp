//===--- SILGenDecl.cpp - Implements Lowering of ASTs -> SIL for Decls ----===//
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

#include "SILGen.h"
#include "Initialization.h"
#include "RValue.h"
#include "Scope.h"
#include "SILGenDynamicCast.h"
#include "swift/SIL/FormalLinkage.h"
#include "swift/SIL/PrettyStackTrace.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILDebuggerClient.h"
#include "swift/SIL/SILType.h"
#include "swift/SIL/TypeLowering.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ProtocolConformance.h"
#include "llvm/ADT/SmallString.h"
#include <iterator>

using namespace swift;
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

    bool canSplitIntoTupleElements() const override {
      return true;
    }
    
    MutableArrayRef<InitializationPtr>
    splitIntoTupleElements(SILGenFunction &gen, SILLocation loc,
                           CanType type,
                           SmallVectorImpl<InitializationPtr> &buf) override {
      // "Destructure" an ignored binding into multiple ignored bindings.
      for (auto fieldType : cast<TupleType>(type)->getElementTypes()) {
        (void) fieldType;
        buf.push_back(InitializationPtr(new BlackHoleInitialization()));
      }
      return buf;
    }

    void copyOrInitValueInto(SILGenFunction &gen, SILLocation loc,
                             ManagedValue value, bool isInit) override {
      /// This just ignores the provided value.
    }

    void finishUninitialized(SILGenFunction &gen) override {
      // do nothing
    }
  };
} // end anonymous namespace

static void copyOrInitValueIntoHelper(
    SILGenFunction &SGF, SILLocation loc, ManagedValue value, bool isInit,
    ArrayRef<InitializationPtr> subInitializations,
    llvm::function_ref<ManagedValue(ManagedValue, unsigned, SILType)> func) {
  auto sourceType = value.getType().castTo<TupleType>();
  auto sourceSILType = value.getType();
  for (unsigned i = 0, e = sourceType->getNumElements(); i != e; ++i) {
    SILType fieldTy = sourceSILType.getTupleElementType(i);
    ManagedValue elt = func(value, i, fieldTy);
    subInitializations[i]->copyOrInitValueInto(SGF, loc, elt, isInit);
    subInitializations[i]->finishInitialization(SGF);
  }
}

void TupleInitialization::copyOrInitValueInto(SILGenFunction &SGF,
                                              SILLocation loc,
                                              ManagedValue value, bool isInit) {
  // In the object case, we perform a borrow + extract + copy sequence. This is
  // because we do not have a destructure operation.
  if (value.getType().isObject()) {
    value = value.borrow(SGF, loc);
    return copyOrInitValueIntoHelper(
        SGF, loc, value, isInit, SubInitializations,
        [&](ManagedValue aggregate, unsigned i,
            SILType fieldType) -> ManagedValue {
          auto elt = SGF.B.createTupleExtract(loc, aggregate, i, fieldType);
          return SGF.B.createCopyValue(loc, elt);
        });
  }

  // In the address case, we can support takes directly, so forward the cleanup
  // of the aggregate and create takes of the underlying addresses.
  value = ManagedValue::forUnmanaged(value.forward(SGF));
  return copyOrInitValueIntoHelper(
      SGF, loc, value, isInit, SubInitializations,
      [&](ManagedValue aggregate, unsigned i,
          SILType fieldType) -> ManagedValue {
        ManagedValue elt =
            SGF.B.createTupleElementAddr(loc, value, i, fieldType);
        if (!fieldType.isAddressOnly(SGF.F.getModule())) {
          return SGF.B.createLoadTake(loc, elt);
        }

        return SGF.emitManagedRValueWithCleanup(elt.getValue());
      });
}

void TupleInitialization::finishUninitialized(SILGenFunction &gen) {
  for (auto &subInit : SubInitializations) {
    subInit->finishUninitialized(gen);
  }
}

namespace {
  class CleanupClosureConstant : public Cleanup {
    SILValue closure;
  public:
    CleanupClosureConstant(SILValue closure) : closure(closure) {}
    void emit(SILGenFunction &gen, CleanupLocation l) override {
      gen.B.emitDestroyValueOperation(l, closure);
    }
    void dump(SILGenFunction &) const override {
#ifndef NDEBUG
      llvm::errs() << "CleanupClosureConstant\n"
                   << "State:" << getState() << "\n"
                   << "closure:" << closure << "\n";
#endif
    }
  };
} // end anonymous namespace

SubstitutionList SILGenFunction::getForwardingSubstitutions() {
  return F.getForwardingSubstitutions();
}

void SILGenFunction::visitFuncDecl(FuncDecl *fd) {
  // Generate the local function body.
  SGM.emitFunction(fd);
}

MutableArrayRef<InitializationPtr>
SingleBufferInitialization::
splitIntoTupleElements(SILGenFunction &gen, SILLocation loc, CanType type,
                       SmallVectorImpl<InitializationPtr> &buf) {
  assert(SplitCleanups.empty() && "getting sub-initializations twice?");
  return splitSingleBufferIntoTupleElements(gen, loc, type, getAddress(), buf,
                                            SplitCleanups);
}

MutableArrayRef<InitializationPtr>
SingleBufferInitialization::
splitSingleBufferIntoTupleElements(SILGenFunction &gen, SILLocation loc,
                                   CanType type, SILValue baseAddr,
                                   SmallVectorImpl<InitializationPtr> &buf,
                     TinyPtrVector<CleanupHandle::AsPointer> &splitCleanups) {
  // Destructure the buffer into per-element buffers.
  for (auto i : indices(cast<TupleType>(type)->getElementTypes())) {
    // Project the element.
    SILValue eltAddr = gen.B.createTupleElementAddr(loc, baseAddr, i);

    // Create an initialization to initialize the element.
    auto &eltTL = gen.getTypeLowering(eltAddr->getType());
    auto eltInit = gen.useBufferAsTemporary(eltAddr, eltTL);

    // Remember the element cleanup.
    auto eltCleanup = eltInit->getInitializedCleanup();
    if (eltCleanup.isValid())
      splitCleanups.push_back(eltCleanup);

    buf.emplace_back(eltInit.release());
  }

  return buf;
}

void SingleBufferInitialization::
copyOrInitValueIntoSingleBuffer(SILGenFunction &gen, SILLocation loc,
                                ManagedValue value, bool isInit,
                                SILValue destAddr) {
  if (!isInit) {
    assert(value.getValue() != destAddr && "copying in place?!");
    value.copyInto(gen, destAddr, loc);
    return;
  }
  
  // If we didn't evaluate into the initialization buffer, do so now.
  if (value.getValue() != destAddr) {
    value.forwardInto(gen, loc, destAddr);
  } else {
    // If we did evaluate into the initialization buffer, disable the
    // cleanup.
    value.forwardCleanup(gen);
  }
}

void SingleBufferInitialization::finishInitialization(SILGenFunction &gen) {
  // Forward all of the split element cleanups, assuming we made any.
  for (CleanupHandle eltCleanup : SplitCleanups)
    gen.Cleanups.forwardCleanup(eltCleanup);
}

void KnownAddressInitialization::anchor() const {
}

void TemporaryInitialization::finishInitialization(SILGenFunction &gen) {
  SingleBufferInitialization::finishInitialization(gen);
  if (Cleanup.isValid())
    gen.Cleanups.setCleanupState(Cleanup, CleanupState::Active);
}

namespace {
class EndBorrowCleanup : public Cleanup {
  SILValue original;
  SILValue borrowed;

public:
  EndBorrowCleanup(SILValue original, SILValue borrowed)
      : original(original), borrowed(borrowed) {}

  void emit(SILGenFunction &gen, CleanupLocation l) override {
    gen.B.createEndBorrow(l, borrowed, original);
  }

  void dump(SILGenFunction &) const override {
#ifndef NDEBUG
    llvm::errs() << "EndBorrowCleanup "
                 << "State:" << getState() << "\n"
                 << "original:" << original << "\n"
                 << "borrowed:" << borrowed << "\n";
#endif
  }
};
} // end anonymous namespace

namespace {
class ReleaseValueCleanup : public Cleanup {
  SILValue v;
public:
  ReleaseValueCleanup(SILValue v) : v(v) {}

  void emit(SILGenFunction &gen, CleanupLocation l) override {
    if (v->getType().isAddress())
      gen.B.createDestroyAddr(l, v);
    else
      gen.B.emitDestroyValueOperation(l, v);
  }

  void dump(SILGenFunction &) const override {
#ifndef NDEBUG
    llvm::errs() << "ReleaseValueCleanup\n"
                 << "State:" << getState() << "\n"
                 << "Value:" << v << "\n";
#endif
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

  void dump(SILGenFunction &) const override {
#ifndef NDEBUG
    llvm::errs() << "DeallocStackCleanup\n"
                 << "State:" << getState() << "\n"
                 << "Addr:" << Addr << "\n";
#endif
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

  void dump(SILGenFunction &SGF) const override {
#ifndef NDEBUG
    llvm::errs() << "DestroyLocalVariable\n"
                 << "State:" << getState() << "\n"
                 << "Decl: ";
    Var->print(llvm::errs());
    llvm::errs() << "\n";
    if (isActive()) {
      auto loc = SGF.VarLocs[Var];
      assert((loc.box || loc.value) && "One of box or value should be set");
      if (loc.box) {
        llvm::errs() << "Box: " << loc.box << "\n";
      } else {
        llvm::errs() << "Value: " << loc.value << "\n";
      }
    }
    llvm::errs() << "\n";
#endif
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

  void dump(SILGenFunction &) const override {
#ifndef NDEBUG
    llvm::errs() << "DeallocateUninitializedLocalVariable\n"
                 << "State:" << getState() << "\n";
    // TODO: Make sure we dump var.
    llvm::errs() << "\n";
#endif
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
                              unsigned ArgNo, SILGenFunction &SGF)
      : decl(decl), SGF(SGF) {
    assert(decl->getDeclContext()->isLocalContext() &&
           "can't emit a local var for a non-local var decl");
    assert(decl->hasStorage() && "can't emit storage for a computed variable");
    assert(!SGF.VarLocs.count(decl) && "Already have an entry for this decl?");

    auto boxType = SGF.SGM.Types
      .getContextBoxTypeForCapture(decl,
                     SGF.getLoweredType(decl->getType()).getSwiftRValueType(),
                     SGF.F.getGenericEnvironment(),
                     /*mutable*/ true);

    // The variable may have its lifetime extended by a closure, heap-allocate
    // it using a box.
    AllocBoxInst *allocBox =
        SGF.B.createAllocBox(decl, boxType, {decl->isLet(), ArgNo});
    SILValue addr = SGF.B.createProjectBox(decl, allocBox, 0);

    // Mark the memory as uninitialized, so DI will track it for us.
    if (NeedsMarkUninit)
      addr = SGF.B.createMarkUninitializedVar(decl, addr);

    /// Remember that this is the memory location that we're emitting the
    /// decl to.
    SGF.VarLocs[decl] = SILGenFunction::VarLoc::get(addr, allocBox);

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

  void finishUninitialized(SILGenFunction &gen) override {
    LocalVariableInitialization::finishInitialization(gen);
  }

  void finishInitialization(SILGenFunction &SGF) override {
    SingleBufferInitialization::finishInitialization(SGF);
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

  /// Cleanups we introduced when splitting.
  TinyPtrVector<CleanupHandle::AsPointer> SplitCleanups;

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
      needsTemporaryBuffer =
          lowering.isAddressOnly() && gen.silConv.useLoweredAddresses();
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

  bool hasAddress() const { return (bool)address; }
  
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
  bool canSplitIntoTupleElements() const override {
    return hasAddress();
  }
  
  MutableArrayRef<InitializationPtr>
  splitIntoTupleElements(SILGenFunction &gen, SILLocation loc, CanType type,
                         SmallVectorImpl<InitializationPtr> &buf) override {
    assert(SplitCleanups.empty());
    return SingleBufferInitialization
       ::splitSingleBufferIntoTupleElements(gen, loc, type, getAddress(), buf,
                                            SplitCleanups);
  }

  SILValue getAddressOrNull() const override {
    return address;
  }

  void bindValue(SILValue value, SILGenFunction &gen) {
    assert(!gen.VarLocs.count(vd) && "Already emitted this vardecl?");
    // If we're binding an address to this let value, then we can use it as an
    // address later.  This happens when binding an address only parameter to
    // an argument, for example.
    if (value->getType().isAddress())
      address = value;
    gen.VarLocs[vd] = SILGenFunction::VarLoc::get(value);

    // Emit a debug_value[_addr] instruction to record the start of this value's
    // lifetime.
    SILLocation PrologueLoc(vd);
    PrologueLoc.markAsPrologue();
    if (address)
      gen.B.createDebugValueAddr(PrologueLoc, value);
    else
      gen.B.createDebugValue(PrologueLoc, value);
  }
  
  void copyOrInitValueInto(SILGenFunction &gen, SILLocation loc,
                           ManagedValue value, bool isInit) override {
    // If this let value has an address, we can handle it just like a single
    // buffer value.
    if (hasAddress())
      return SingleBufferInitialization::
        copyOrInitValueIntoSingleBuffer(gen, loc, value, isInit, getAddress());
    
    // Otherwise, we bind the value.
    if (isInit) {
      // Disable the rvalue expression cleanup, since the let value
      // initialization has a cleanup that lives for the entire scope of the
      // let declaration.
      bindValue(value.forward(gen), gen);
    } else {
      // Disable the expression cleanup of the copy, since the let value
      // initialization has a cleanup that lives for the entire scope of the
      // let declaration.
      bindValue(value.copyUnmanaged(gen, loc).forward(gen), gen);
    }
  }

  void finishUninitialized(SILGenFunction &gen) override {
    LetValueInitialization::finishInitialization(gen);
  }

  void finishInitialization(SILGenFunction &gen) override {
    assert(!DidFinish &&
           "called LetValueInit::finishInitialization twice!");
    assert(gen.VarLocs.count(vd) && "Didn't bind a value to this let!");

    // Deactivate any cleanups we made when splitting the tuple.
    for (auto cleanup : SplitCleanups)
      gen.Cleanups.forwardCleanup(cleanup);

    // Activate the destroy cleanup.
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


  void copyOrInitValueInto(SILGenFunction &gen, SILLocation loc,
                           ManagedValue value, bool isInit) override {
    // If this is not an initialization, copy the value before we translateIt,
    // translation expects a +1 value.
    if (isInit)
      value.forwardInto(gen, loc, VarInit->getAddress());
    else
      value.copyInto(gen, VarInit->getAddress(), loc);
  }

  void finishUninitialized(SILGenFunction &gen) override {
    ReferenceStorageInitialization::finishInitialization(gen);
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

  void copyOrInitValueInto(SILGenFunction &gen, SILLocation loc,
                           ManagedValue value, bool isInit) override = 0;

  void bindVariable(SILLocation loc, VarDecl *var, ManagedValue value,
                    CanType formalValueType, SILGenFunction &SGF) {
    // Initialize the variable value.
    InitializationPtr init = SGF.emitInitializationForVarDecl(var);
    RValue(SGF, loc, formalValueType, value).forwardInto(SGF, loc, init.get());
  }

};
} // end anonymous namespace

namespace {
class ExprPatternInitialization : public RefutablePatternInitialization {
  ExprPattern *P;
public:
  ExprPatternInitialization(ExprPattern *P, JumpDest patternFailDest)
    : RefutablePatternInitialization(patternFailDest), P(P) {}

  void copyOrInitValueInto(SILGenFunction &gen, SILLocation loc,
                           ManagedValue value, bool isInit) override;
};
} // end anonymous namespace

void ExprPatternInitialization::
copyOrInitValueInto(SILGenFunction &SGF, SILLocation loc,
                    ManagedValue value, bool isInit) {
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
    
  void copyOrInitValueInto(SILGenFunction &SGF, SILLocation loc,
                           ManagedValue value, bool isInit) override {
    assert(isInit && "Only initialization is supported for refutable patterns");
    emitEnumMatch(value, ElementDecl, subInitialization.get(), getFailureDest(),
                  loc, SGF);
  }

  static void emitEnumMatch(ManagedValue value, EnumElementDecl *ElementDecl,
                            Initialization *subInit, JumpDest FailureDest,
                            SILLocation loc, SILGenFunction &SGF);
  
  void finishInitialization(SILGenFunction &SGF) override {
    if (subInitialization.get())
      subInitialization->finishInitialization(SGF);
  }
};
} // end anonymous namespace

static bool shouldDisableCleanupOnFailurePath(ManagedValue value,
                                              EnumElementDecl *elementDecl,
                                              SILGenFunction &SGF) {
  // If the enum is trivial, then there is no cleanup to disable.
  if (value.isPlusZeroRValueOrTrivial()) return false;
  
  // Check all of the members of the enum.  If any have a non-trivial payload,
  // then we can't disable the cleanup.
  for (auto elt : elementDecl->getParentEnum()->getAllElements()) {
    // Ignore the element that will be handled.
    if (elt == elementDecl) continue;
    
    // Elements without payloads are trivial.
    if (!elt->getArgumentInterfaceType()) continue;

    auto eltTy = value.getType().getEnumElementType(elt, SGF.SGM.M);
    if (!eltTy.isTrivial(SGF.SGM.M))
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
  if (!ElementDecl->getArgumentInterfaceType()) {
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
    eltValue = contBB->createPHIArgument(eltTy, ValueOwnershipKind::Owned);

  if (subInit == nullptr) {
    // If there is no subinitialization, then we are done matching.  Don't
    // bother projecting out the address-only element value only to ignore it.
    return;
  }
  
  if (value.getType().isAddress()) {
    // If the enum is address-only, take from the enum we have and load it if
    // the element value is loadable.
    assert((eltTL.isTrivial() || value.hasCleanup())
           && "must be able to consume value");
    eltValue = SGF.B.createUncheckedTakeEnumDataAddr(loc, value.forward(SGF),
                                                     ElementDecl, eltTy);
    // Load a loadable data value.
    if (eltTL.isLoadable())
      eltValue =
          eltTL.emitLoad(SGF.B, loc, eltValue, LoadOwnershipQualifier::Take);
  } else {
    // Otherwise, we're consuming this as a +1 value.
    value.forward(SGF);
  }
  
  // Now we have a +1 value.
  auto eltMV = SGF.emitManagedRValueWithCleanup(eltValue, eltTL);

  // If the payload is indirect, project it out of the box.
  if (ElementDecl->isIndirect() || ElementDecl->getParentEnum()->isIndirect()) {
    SILValue boxedValue = SGF.B.createProjectBox(loc, eltMV.getValue(), 0);
    auto &boxedTL = SGF.getTypeLowering(boxedValue->getType());
    // SEMANTIC ARC TODO: Revisit this when the verifier is enabled.
    if (boxedTL.isLoadable() || !SGF.silConv.useLoweredAddresses())
      boxedValue = boxedTL.emitLoad(SGF.B, loc, boxedValue,
                                    LoadOwnershipQualifier::Take);

    // We must treat the boxed value as +0 since it may be shared. Copy it if
    // nontrivial.
    // TODO: Should be able to hand it off at +0 in some cases.
    eltMV = ManagedValue::forUnmanaged(boxedValue);
    eltMV = eltMV.copyUnmanaged(SGF, loc);
  }
  
  // Reabstract to the substituted type, if needed.
  CanType substEltTy =
    value.getType().getSwiftRValueType()
      ->getTypeOfMember(SGF.SGM.M.getSwiftModule(),
                        ElementDecl,
                        ElementDecl->getArgumentInterfaceType())
      ->getCanonicalType();

  AbstractionPattern origEltTy =
    (ElementDecl == SGF.getASTContext().getOptionalSomeDecl()
       ? AbstractionPattern(substEltTy)
       : SGF.SGM.M.Types.getAbstractionPattern(ElementDecl));
  
  eltMV = SGF.emitOrigToSubstValue(loc, eltMV, origEltTy, substEltTy);

  // Pass the +1 value down into the sub initialization.
  subInit->copyOrInitValueInto(SGF, loc, eltMV, /*is an init*/true);
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
    
  void copyOrInitValueInto(SILGenFunction &SGF, SILLocation loc,
                           ManagedValue value, bool isInit) override;
  
  void finishInitialization(SILGenFunction &SGF) override {
    if (subInitialization.get())
      subInitialization->finishInitialization(SGF);
  }
};
} // end anonymous namespace

void IsPatternInitialization::
copyOrInitValueInto(SILGenFunction &SGF, SILLocation loc,
                    ManagedValue value, bool isInit) {
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
class BoolPatternInitialization : public RefutablePatternInitialization {
  BoolPattern *pattern;
public:
  BoolPatternInitialization(BoolPattern *pattern,
                            JumpDest patternFailDest)
    : RefutablePatternInitialization(patternFailDest), pattern(pattern) {}

  void copyOrInitValueInto(SILGenFunction &SGF, SILLocation loc,
                           ManagedValue value, bool isInit) override;
};
} // end anonymous namespace

void BoolPatternInitialization::
copyOrInitValueInto(SILGenFunction &SGF, SILLocation loc,
                    ManagedValue value, bool isInit) {
  assert(isInit && "Only initialization is supported for refutable patterns");

  // Extract the i1 from the Bool struct.
  StructDecl *BoolStruct = cast<StructDecl>(SGF.getASTContext().getBoolDecl());
  auto Members = BoolStruct->lookupDirect(SGF.getASTContext().Id_value_);
  assert(Members.size() == 1 &&
         "Bool should have only one property with name '_value'");
  auto Member = dyn_cast<VarDecl>(Members[0]);
  assert(Member &&"Bool should have a property with name '_value' of type Int1");
  auto *i1Val = SGF.B.createStructExtract(loc, value.forward(SGF), Member);

  // Branch on the boolean based on whether we're testing for true or false.
  SILBasicBlock *trueBB = SGF.B.splitBlockForFallthrough();
  auto contBB = trueBB;
  auto falseBB = SGF.Cleanups.emitBlockForCleanups(getFailureDest(), loc);

  if (!pattern->getValue())
    std::swap(trueBB, falseBB);
  SGF.B.createCondBranch(loc, i1Val, trueBB, falseBB);
  SGF.B.setInsertionPoint(contBB);
}


namespace {

/// InitializationForPattern - A visitor for traversing a pattern, generating
/// SIL code to allocate the declared variables, and generating an
/// Initialization representing the needed initializations.
///
/// It is important that any Initialization created for a pattern that might
/// not have an immediate initializer implement finishUninitialized.  Note
/// that this only applies to irrefutable patterns.
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
      init->SubInitializations.push_back(visit(elt.getPattern()));
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
    return InitializationPtr(new BoolPatternInitialization(P, patternFailDest));
  }
  InitializationPtr visitExprPattern(ExprPattern *P) {
    return InitializationPtr(new ExprPatternInitialization(P, patternFailDest));
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
    B.createAllocGlobal(vd, silG);
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

void SILGenFunction::emitPatternBinding(PatternBindingDecl *PBD,
                                        unsigned pbdEntry) {
  auto &entry = PBD->getPatternList()[pbdEntry];
  auto initialization = emitPatternBindingInitialization(entry.getPattern(),
                                                         JumpDest::invalid());

  // If an initial value expression was specified by the decl, emit it into
  // the initialization. Otherwise, mark it uninitialized for DI to resolve.
  if (auto *Init = entry.getInit()) {
    FullExpr Scope(Cleanups, CleanupLocation(Init));
    emitExprInto(Init, initialization.get());
  } else {
    initialization->finishUninitialized(*this);
  }
}

void SILGenFunction::visitPatternBindingDecl(PatternBindingDecl *PBD) {

  // Allocate the variables and build up an Initialization over their
  // allocated storage.
  for (unsigned i : indices(PBD->getPatternList())) {
    emitPatternBinding(PBD, i);
  }
}

void SILGenFunction::visitVarDecl(VarDecl *D) {
  // We handle emitting the variable storage when we see the pattern binding.
  // Here we just emit the behavior witness table, if any.
  
  if (D->hasBehavior())
    SGM.emitPropertyBehavior(D);
}

/// Emit a check that returns 1 if the running OS version is in
/// the specified version range and 0 otherwise. The returned SILValue
/// (which has type Builtin.Int1) represents the result of this check.
SILValue SILGenFunction::emitOSVersionRangeCheck(SILLocation loc,
                                                 const VersionRange &range) {
  // Emit constants for the checked version range.
  clang::VersionTuple Vers = range.getLowerEndpoint();
  unsigned major = Vers.getMajor();
  unsigned minor =
      (Vers.getMinor().hasValue() ? Vers.getMinor().getValue() : 0);
  unsigned subminor =
      (Vers.getSubminor().hasValue() ? Vers.getSubminor().getValue() : 0);

  SILType wordType = SILType::getBuiltinWordType(getASTContext());

  SILValue majorValue = B.createIntegerLiteral(loc, wordType, major);
  SILValue minorValue = B.createIntegerLiteral(loc, wordType, minor);
  SILValue subminorValue = B.createIntegerLiteral(loc, wordType, subminor);

  // Emit call to _stdlib_isOSVersionAtLeast(major, minor, patch)
  FuncDecl *versionQueryDecl =
      getASTContext().getIsOSVersionAtLeastDecl(nullptr);
  assert(versionQueryDecl);

  auto silDeclRef = SILDeclRef(versionQueryDecl);
  SILValue availabilityGTEFn = emitGlobalFunctionRef(
      loc, silDeclRef, getConstantInfo(silDeclRef));

  SILValue args[] = {majorValue, minorValue, subminorValue};
  return B.createApply(loc, availabilityGTEFn, args, false);
}


/// Emit the boolean test and/or pattern bindings indicated by the specified
/// stmt condition.  If the condition fails, control flow is transferred to the
/// specified JumpDest.  The insertion point is left in the block where the
/// condition has matched and any bound variables are in scope.
///
void SILGenFunction::emitStmtCondition(StmtCondition Cond,
                                       JumpDest FailDest, SILLocation loc) {

  assert(B.hasValidInsertionPoint() &&
         "emitting condition at unreachable point");
  
  for (const auto &elt : Cond) {
    SILLocation booleanTestLoc = loc;
    SILValue booleanTestValue;

    switch (elt.getKind()) {
    case StmtConditionElement::CK_PatternBinding: {
      InitializationPtr initialization =
      InitializationForPattern(*this, FailDest).visit(elt.getPattern());

      // Emit the initial value into the initialization.
      FullExpr Scope(Cleanups, CleanupLocation(elt.getInitializer()));
      emitExprInto(elt.getInitializer(), initialization.get());
      // Pattern bindings handle their own tests, we don't need a boolean test.
      continue;
    }

    case StmtConditionElement::CK_Boolean: { // Handle boolean conditions.
      auto *expr = elt.getBoolean();
      // Evaluate the condition as an i1 value (guaranteed by Sema).
      FullExpr Scope(Cleanups, CleanupLocation(expr));
      booleanTestValue = emitRValue(expr).forwardAsSingleValue(*this, expr);
      booleanTestLoc = expr;
      break;
    }
    case StmtConditionElement::CK_Availability:
      // Check the running OS version to determine whether it is in the range
      // specified by elt.
      VersionRange OSVersion = elt.getAvailability()->getAvailableRange();
      assert(!OSVersion.isEmpty());

      if (OSVersion.isAll()) {
        // If there's no check for the current platform, this condition is
        // trivially true.
        SILType i1 = SILType::getBuiltinIntegerType(1, getASTContext());
        booleanTestValue = B.createIntegerLiteral(loc, i1, true);
      } else {
        booleanTestValue = emitOSVersionRangeCheck(loc, OSVersion);
      }
      break;
    }

    // Now that we have a boolean test as a Builtin.i1, emit the branch.
    assert(booleanTestValue->getType().
           castTo<BuiltinIntegerType>()->isFixedWidth(1) &&
           "Sema forces conditions to have Builtin.i1 type");
    
    // Just branch on the condition.  On failure, we unwind any active cleanups,
    // on success we fall through to a new block.
    SILBasicBlock *ContBB = createBasicBlock();
    auto FailBB = Cleanups.emitBlockForCleanups(FailDest, loc);
    B.createCondBranch(booleanTestLoc, booleanTestValue, ContBB, FailBB);
    
    // Finally, emit the continue block and keep emitting the rest of the
    // condition.
    B.emitBlock(ContBB);
  }
}

InitializationPtr
SILGenFunction::emitPatternBindingInitialization(Pattern *P,
                                                 JumpDest failureDest) {
  return InitializationForPattern(*this, failureDest).visit(P);
}

/// Enter a cleanup to deallocate the given location.
CleanupHandle SILGenFunction::enterDeallocStackCleanup(SILValue temp) {
  assert(temp->getType().isAddress() &&  "dealloc must have an address type");
  Cleanups.pushCleanup<DeallocStackCleanup>(temp);
  return Cleanups.getTopCleanup();
}

CleanupHandle SILGenFunction::enterDestroyCleanup(SILValue valueOrAddr) {
  Cleanups.pushCleanup<ReleaseValueCleanup>(valueOrAddr);
  return Cleanups.getTopCleanup();
}

namespace {
  /// A cleanup that deinitializes an opaque existential container
  /// before a value has been stored into it, or after its value was taken.
  class DeinitExistentialCleanup: public Cleanup {
    SILValue existentialAddr;
    CanType concreteFormalType;
    ExistentialRepresentation repr;
  public:
    DeinitExistentialCleanup(SILValue existentialAddr,
                             CanType concreteFormalType,
                             ExistentialRepresentation repr)
      : existentialAddr(existentialAddr),
        concreteFormalType(concreteFormalType),
        repr(repr) {}
    
    void emit(SILGenFunction &gen, CleanupLocation l) override {
      switch (repr) {
      case ExistentialRepresentation::None:
      case ExistentialRepresentation::Class:
      case ExistentialRepresentation::Metatype:
        llvm_unreachable("cannot cleanup existential");
      case ExistentialRepresentation::Opaque:
        if (gen.silConv.useLoweredAddresses()) {
          gen.B.createDeinitExistentialAddr(l, existentialAddr);
        } else {
          gen.B.createDeinitExistentialOpaque(l, existentialAddr);
        }
        break;
      case ExistentialRepresentation::Boxed:
        gen.B.createDeallocExistentialBox(l, concreteFormalType,
                                          existentialAddr);
        break;
      }
    }

    void dump(SILGenFunction &) const override {
#ifndef NDEBUG
      llvm::errs() << "DeinitExistentialCleanup\n"
                   << "State:" << getState() << "\n"
                   << "Value:" << existentialAddr << "\n";
#endif
    }
  };
} // end anonymous namespace

/// Enter a cleanup to emit a DeinitExistentialAddr or DeinitExistentialBox
/// of the specified value.
CleanupHandle SILGenFunction::enterDeinitExistentialCleanup(
                                               SILValue valueOrAddr,
                                               CanType concreteFormalType,
                                               ExistentialRepresentation repr) {
  Cleanups.pushCleanup<DeinitExistentialCleanup>(valueOrAddr,
                                                 concreteFormalType,
                                                 repr);
  return Cleanups.getTopCleanup();
}

void SILGenModule::emitExternalWitnessTable(ProtocolConformance *c) {
  auto root = c->getRootNormalConformance();
  // Emit the witness table right now if we used it.
  if (usedConformances.count(root)) {
    getWitnessTable(c);
    return;
  }
  // Otherwise, remember it for later.
  delayedConformances.insert({root, {lastEmittedConformance}});
  lastEmittedConformance = root;
}

void SILGenModule::emitExternalDefinition(Decl *d) {
  switch (d->getKind()) {
  case DeclKind::Func: {
    emitFunction(cast<FuncDecl>(d));
    break;
  }
  case DeclKind::Constructor: {
    auto C = cast<ConstructorDecl>(d);
    // For factories, we don't need to emit a special thunk; the normal
    // foreign-to-native thunk is sufficient.
    if (C->isFactoryInit())
      break;

    emitConstructor(C);
    break;
  }
  case DeclKind::Enum:
  case DeclKind::Struct:
  case DeclKind::Class: {
    // Emit witness tables.
    auto nom = cast<NominalTypeDecl>(d);
    for (auto c : nom->getLocalConformances(ConformanceLookupKind::All,
                                            nullptr, /*sorted=*/true)) {
      auto *proto = c->getProtocol();
      if (Lowering::TypeConverter::protocolRequiresWitnessTable(proto) &&
          isa<NormalProtocolConformance>(c) &&
          c->isComplete())
        emitExternalWitnessTable(c);
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
  case DeclKind::PrecedenceGroup:
  case DeclKind::Module:
    llvm_unreachable("Not a valid external definition for SILGen");
  }
}

/// Create a LocalVariableInitialization for the uninitialized var.
InitializationPtr
SILGenFunction::emitLocalVariableWithCleanup(VarDecl *vd, bool NeedsMarkUninit,
                                             unsigned ArgNo) {
  return InitializationPtr(
      new LocalVariableInitialization(vd, NeedsMarkUninit, ArgNo, *this));
}

/// Create an Initialization for an uninitialized temporary.
std::unique_ptr<TemporaryInitialization>
SILGenFunction::emitTemporary(SILLocation loc, const TypeLowering &tempTL) {
  SILValue addr = emitTemporaryAllocation(loc, tempTL.getLoweredType());
  return useBufferAsTemporary(addr, tempTL);
}

std::unique_ptr<TemporaryInitialization>
SILGenFunction::emitFormalAccessTemporary(SILLocation loc,
                                          const TypeLowering &tempTL) {
  SILValue addr = emitTemporaryAllocation(loc, tempTL.getLoweredType());
  CleanupHandle cleanup =
      enterDormantFormalAccessTemporaryCleanup(addr, loc, tempTL);
  return std::unique_ptr<TemporaryInitialization>(
      new TemporaryInitialization(addr, cleanup));
}

/// Create an Initialization for an uninitialized buffer.
std::unique_ptr<TemporaryInitialization>
SILGenFunction::useBufferAsTemporary(SILValue addr,
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

namespace {

struct FormalAccessReleaseValueCleanup : Cleanup {
  FormalEvaluationContext::stable_iterator Depth;

  FormalAccessReleaseValueCleanup() : Depth() {}

  void setState(SILGenFunction &gen, CleanupState newState) override {
    if (newState == CleanupState::Dead) {
      getEvaluation(gen).setFinished();
    }

    state = newState;
  }

  void emit(SILGenFunction &gen, CleanupLocation l) override {
    getEvaluation(gen).finish(gen);
  }

  void dump(SILGenFunction &gen) const override {
#ifndef NDEBUG
    llvm::errs() << "FormalAccessReleaseValueCleanup "
                 << "State:" << getState() << "\n"
                 << "Value:" << getValue(gen) << "\n";
#endif
  }

  OwnedFormalAccess &getEvaluation(SILGenFunction &gen) const {
    auto &evaluation = *gen.FormalEvalContext.find(Depth);
    assert(evaluation.getKind() == FormalAccess::Owned);
    return static_cast<OwnedFormalAccess &>(evaluation);
  }

  SILValue getValue(SILGenFunction &gen) const {
    return getEvaluation(gen).getValue();
  }
};

} // end anonymous namespace

ManagedValue
SILGenFunction::emitFormalAccessManagedBufferWithCleanup(SILLocation loc,
                                                         SILValue addr) {
  assert(InWritebackScope && "Must be in formal evaluation scope");
  auto &lowering = F.getTypeLowering(addr->getType());
  if (lowering.isTrivial())
    return ManagedValue::forUnmanaged(addr);

  auto &cleanup = Cleanups.pushCleanup<FormalAccessReleaseValueCleanup>();
  CleanupHandle handle = Cleanups.getTopCleanup();
  FormalEvalContext.push<OwnedFormalAccess>(loc, handle, addr);
  cleanup.Depth = FormalEvalContext.stable_begin();
  return ManagedValue(addr, handle);
}

ManagedValue
SILGenFunction::emitFormalAccessManagedRValueWithCleanup(SILLocation loc,
                                                         SILValue value) {
  assert(InWritebackScope && "Must be in formal evaluation scope");
  auto &lowering = F.getTypeLowering(value->getType());
  if (lowering.isTrivial())
    return ManagedValue::forUnmanaged(value);

  auto &cleanup = Cleanups.pushCleanup<FormalAccessReleaseValueCleanup>();
  CleanupHandle handle = Cleanups.getTopCleanup();
  FormalEvalContext.push<OwnedFormalAccess>(loc, handle, value);
  cleanup.Depth = FormalEvalContext.stable_begin();
  return ManagedValue(value, handle);
}

CleanupHandle SILGenFunction::enterDormantFormalAccessTemporaryCleanup(
    SILValue addr, SILLocation loc, const TypeLowering &tempTL) {
  assert(InWritebackScope && "Must be in formal evaluation scope");
  if (tempTL.isTrivial())
    return CleanupHandle::invalid();

  auto &cleanup = Cleanups.pushCleanup<FormalAccessReleaseValueCleanup>();
  CleanupHandle handle = Cleanups.getTopCleanup();
  Cleanups.setCleanupState(handle, CleanupState::Dormant);
  FormalEvalContext.push<OwnedFormalAccess>(loc, handle, addr);
  cleanup.Depth = FormalEvalContext.stable_begin();
  return handle;
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
    B.emitDestroyValueOperation(silLoc, loc.box);
    return;
  }

  // For 'let' bindings, we emit a release_value or destroy_addr, depending on
  // whether we have an address or not.
  SILValue Val = loc.value;
  if (!Val->getType().isAddress())
    B.emitDestroyValueOperation(silLoc, Val);
  else
    B.createDestroyAddr(silLoc, Val);
}

void SILGenFunction::deallocateUninitializedLocalVariable(SILLocation silLoc,
                                                          VarDecl *vd) {
  assert(vd->getDeclContext()->isLocalContext() &&
         "can't emit a local var for a non-local var decl");
  assert(vd->hasStorage() && "can't emit storage for a computed variable");

  assert(VarLocs.count(vd) && "var decl wasn't emitted?!");

  auto loc = VarLocs[vd];

  // Ignore let values captured without a memory location.
  if (!loc.value->getType().isAddress()) return;

  assert(loc.box && "captured var should have been given a box");
  B.createDeallocBox(silLoc, loc.box);
}
