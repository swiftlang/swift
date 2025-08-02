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

#include "Initialization.h"
#include "LValue.h"
#include "RValue.h"
#include "SILGen.h"
#include "SILGenDynamicCast.h"
#include "Scope.h"
#include "SwitchEnumBuilder.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/PropertyWrappers.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/Basic/Platform.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/ProfileCounter.h"
#include "swift/SIL/FormalLinkage.h"
#include "swift/SIL/PrettyStackTrace.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILDebuggerClient.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILSymbolVisitor.h"
#include "swift/SIL/SILType.h"
#include "swift/SIL/TypeLowering.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/ErrorHandling.h"
#include <iterator>

using namespace swift;
using namespace Lowering;

// Utility for emitting diagnostics.
template <typename... T, typename... U>
static void diagnose(ASTContext &Context, SourceLoc loc, Diag<T...> diag,
                     U &&...args) {
  Context.Diags.diagnose(loc, diag, std::forward<U>(args)...);
}

void Initialization::_anchor() {}
void SILDebuggerClient::anchor() {}

static void copyOrInitPackExpansionInto(SILGenFunction &SGF,
                                        SILLocation loc,
                                        SILValue tupleAddr,
                                        CanPackType formalPackType,
                                        unsigned componentIndex,
                                        CleanupHandle componentCleanup,
                                        Initialization *expansionInit,
                                        bool isInit) {
  auto expansionTy = tupleAddr->getType().getTupleElementType(componentIndex);
  assert(expansionTy.is<PackExpansionType>());

  auto opening = SGF.createOpenedElementValueEnvironment(expansionTy);
  auto openedEnv = opening.first;
  auto eltTy = opening.second;

  assert(expansionInit);
  assert(expansionInit->canPerformPackExpansionInitialization());

  // Exit the component-wide cleanup for the expansion component.
  if (componentCleanup.isValid())
    SGF.Cleanups.forwardCleanup(componentCleanup);

  SGF.emitDynamicPackLoop(loc, formalPackType, componentIndex, openedEnv,
                          [&](SILValue indexWithinComponent,
                              SILValue packExpansionIndex,
                              SILValue packIndex) {
    expansionInit->performPackExpansionInitialization(SGF, loc,
                                                      indexWithinComponent,
                                                [&](Initialization *eltInit) {
      // Project the current tuple element.
      auto eltAddr =
        SGF.B.createTuplePackElementAddr(loc, packIndex, tupleAddr, eltTy);

      SILValue elt = eltAddr;
      if (!eltTy.isAddressOnly(SGF.F)) {
        elt = SGF.B.emitLoadValueOperation(loc, elt,
                                           LoadOwnershipQualifier::Take);
      }

      // Enter a cleanup for the current element, which we need to consume
      // on this iteration of the loop, and the remaining elements in the
      // expansion component, which we need to destroy if we throw from
      // the initialization.
      CleanupHandle eltCleanup = CleanupHandle::invalid();
      CleanupHandle tailCleanup = CleanupHandle::invalid();
      if (componentCleanup.isValid()) {
        eltCleanup = SGF.enterDestroyCleanup(elt);
        tailCleanup = SGF.enterPartialDestroyRemainingTupleCleanup(tupleAddr,
                        formalPackType, componentIndex, indexWithinComponent);
      }

      ManagedValue eltMV;
      if (eltCleanup == CleanupHandle::invalid()) {
        eltMV = ManagedValue::forRValueWithoutOwnership(elt);
      } else {
        eltMV = ManagedValue::forOwnedRValue(elt, eltCleanup);
      }

      // Perform the initialization.  If this doesn't consume the
      // element value, that's fine, we'll just destroy it as part of
      // leaving the iteration.
      eltInit->copyOrInitValueInto(SGF, loc, eltMV, isInit);
      eltInit->finishInitialization(SGF);

      // Deactivate the tail cleanup before continuing the loop.
      if (tailCleanup.isValid())
        SGF.Cleanups.forwardCleanup(tailCleanup);
    });
  });

  expansionInit->finishInitialization(SGF);
}

void TupleInitialization::copyOrInitValueInto(SILGenFunction &SGF,
                                              SILLocation loc,
                                              ManagedValue value, bool isInit) {
  auto sourceType = value.getType().castTo<TupleType>();
  assert(sourceType->getNumElements() == SubInitializations.size());

  // We have to emit a different pattern when there are pack expansions.
  // Fortunately, we can assume this doesn't happen with objects because
  // tuples contain pack expansions are address-only.
  auto containsPackExpansion = sourceType.containsPackExpansionType();

  CanPackType formalPackType;
  if (containsPackExpansion)
    formalPackType = FormalTupleType.getInducedPackType();

  // Process all values before initialization all at once to ensure
  // all cleanups are setup on all tuple elements before a potential
  // early exit.
  SmallVector<ManagedValue, 8> destructuredValues;

  // In the object case, destructure the tuple.
  if (value.getType().isObject()) {
    assert(!containsPackExpansion);
    SGF.B.emitDestructureValueOperation(loc, value, destructuredValues);
  } else {
    // In the address case, we forward the underlying value and store it
    // into memory and then create a +1 cleanup. since we assume here
    // that we have a +1 value since we are forwarding into memory.
    assert(value.isPlusOneOrTrivial(SGF) &&
           "Can not store a +0 value into memory?!");
    CleanupCloner cloner(SGF, value);
    SILValue v = value.forward(SGF);

    auto sourceSILType = value.getType();
    for (auto i : range(sourceType->getNumElements())) {
      SILType fieldTy = sourceSILType.getTupleElementType(i);
      if (containsPackExpansion && fieldTy.is<PackExpansionType>()) {
        destructuredValues.push_back(
          cloner.cloneForTuplePackExpansionComponent(v, formalPackType, i));
        continue;
      }

      SILValue elt;
      if (containsPackExpansion) {
        auto packIndex = SGF.B.createScalarPackIndex(loc, i, formalPackType);
        elt = SGF.B.createTuplePackElementAddr(loc, packIndex, v, fieldTy);
      } else {
        elt = SGF.B.createTupleElementAddr(loc, v, i, fieldTy);
      }

      if (!fieldTy.isAddressOnly(SGF.F)) {
        elt = SGF.B.emitLoadValueOperation(loc, elt,
                                           LoadOwnershipQualifier::Take);
      }
      destructuredValues.push_back(cloner.clone(elt));
    }
  }

  assert(destructuredValues.size() == SubInitializations.size());

  for (auto i : indices(destructuredValues)) {
    if (containsPackExpansion) {
      bool isPackExpansion =
        (destructuredValues[i].getValue() == value.getValue());
      assert(isPackExpansion ==
               isa<PackExpansionType>(sourceType.getElementType(i)));
      if (isPackExpansion) {
        auto packAddr = destructuredValues[i].getValue();
        auto componentCleanup = destructuredValues[i].getCleanup();
        copyOrInitPackExpansionInto(SGF, loc, packAddr, formalPackType,
                                    i, componentCleanup,
                                    SubInitializations[i].get(), isInit);
        continue;
      }
    }

    SubInitializations[i]->copyOrInitValueInto(SGF, loc, destructuredValues[i],
                                               isInit);
    SubInitializations[i]->finishInitialization(SGF);
  }
}

void TupleInitialization::finishUninitialized(SILGenFunction &SGF) {
  for (auto &subInit : SubInitializations) {
    subInit->finishUninitialized(SGF);
  }
}

namespace {
  class CleanupClosureConstant : public Cleanup {
    SILValue closure;
  public:
    CleanupClosureConstant(SILValue closure) : closure(closure) {}
    void emit(SILGenFunction &SGF, CleanupLocation l,
              ForUnwind_t forUnwind) override {
      SGF.B.emitDestroyValueOperation(l, closure);
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

SubstitutionMap SILGenFunction::getForwardingSubstitutionMap() {
  return F.getForwardingSubstitutionMap();
}

void SILGenFunction::visitFuncDecl(FuncDecl *fd) {
  // Generate the local function body.
  SGM.emitFunction(fd);
}

MutableArrayRef<InitializationPtr>
SingleBufferInitialization::
splitIntoTupleElements(SILGenFunction &SGF, SILLocation loc, CanType type,
                       SmallVectorImpl<InitializationPtr> &buf) {
  assert(SplitCleanups.empty() && "getting sub-initializations twice?");
  auto address = getAddressForInPlaceInitialization(SGF, loc);
  return splitSingleBufferIntoTupleElements(SGF, loc, type, address,
                                            buf, SplitCleanups);
}

MutableArrayRef<InitializationPtr>
SingleBufferInitialization::
splitSingleBufferIntoTupleElements(SILGenFunction &SGF, SILLocation loc,
                                   CanType type, SILValue baseAddr,
                                   SmallVectorImpl<InitializationPtr> &buf,
                     TinyPtrVector<CleanupHandle::AsPointer> &splitCleanups) {
  auto tupleType = cast<TupleType>(type);

  // We can still split the initialization of a tuple with a pack
  // expansion component (as long as the initializer is cooperative),
  // but we have to emit a different code pattern.
  bool hasExpansion = tupleType.containsPackExpansionType();

  // If there's an expansion in the tuple, we'll need the induced pack
  // type for the tuple elements below.
  CanPackType inducedPackType;
  if (hasExpansion) {
    inducedPackType = tupleType.getInducedPackType();
  }

  // Destructure the buffer into per-element buffers.
  for (auto i : indices(tupleType->getElementTypes())) {
    // Project the element.
    SILValue eltAddr;

    // If this element is a pack expansion, we have to produce an
    // Initialization that will drill appropriately to the right tuple
    // element within a dynamic pack loop.
    if (hasExpansion && isa<PackExpansionType>(tupleType.getElementType(i))) {
      auto expansionInit =
        TuplePackExpansionInitialization::create(SGF, baseAddr,
                                                 inducedPackType, i);
      auto expansionCleanup = expansionInit->getExpansionCleanup();
      if (expansionCleanup.isValid())
        splitCleanups.push_back(expansionCleanup);
      buf.emplace_back(expansionInit.release());
      continue;

    // If this element is scalar, but it's into a tuple with pack
    // expansions, produce a structural pack index into the induced
    // pack type and use that to project the right element.
    } else if (hasExpansion) {
      auto packIndex = SGF.B.createScalarPackIndex(loc, i, inducedPackType);
      auto eltTy = baseAddr->getType().getTupleElementType(i);
      eltAddr = SGF.B.createTuplePackElementAddr(loc, packIndex, baseAddr,
                                                 eltTy);

    // Otherwise, we can just use simple projection.
    } else {
      eltAddr = SGF.B.createTupleElementAddr(loc, baseAddr, i);
    }

    // Create an initialization to initialize the element.
    auto &eltTL = SGF.getTypeLowering(eltAddr->getType());
    auto eltInit = SGF.useBufferAsTemporary(eltAddr, eltTL);

    // Remember the element cleanup.
    auto eltCleanup = eltInit->getInitializedCleanup();
    if (eltCleanup.isValid())
      splitCleanups.push_back(eltCleanup);

    buf.emplace_back(std::move(eltInit));
  }

  return buf;
}

void SingleBufferInitialization::
copyOrInitValueIntoSingleBuffer(SILGenFunction &SGF, SILLocation loc,
                                ManagedValue value, bool isInit,
                                SILValue destAddr) {
  // Emit an unchecked access around initialization of the local buffer to
  // silence access marker verification.
  //
  // FIXME: This is not a good place for FormalEvaluationScope +
  // UnenforcedFormalAccess.  However, there's no way to identify the buffer
  // initialization sequence after SILGen, and no easy way to wrap the
  // Initialization in an access during top-level expression evaluation.
  FormalEvaluationScope scope(SGF);
  if (!isInit) {
    assert(value.getValue() != destAddr && "copying in place?!");
    SILValue accessAddr =
      UnenforcedFormalAccess::enter(SGF, loc, destAddr, SILAccessKind::Modify);
    value.copyInto(SGF, loc, accessAddr);
    return;
  }
  
  // If we didn't evaluate into the initialization buffer, do so now.
  if (value.getValue() != destAddr) {
    SILValue accessAddr =
      UnenforcedFormalAccess::enter(SGF, loc, destAddr, SILAccessKind::Modify);
    value.forwardInto(SGF, loc, accessAddr);
  } else {
    // If we did evaluate into the initialization buffer, disable the
    // cleanup.
    value.forwardCleanup(SGF);
  }
}

void SingleBufferInitialization::finishInitialization(SILGenFunction &SGF) {
  // Forward all of the split element cleanups, assuming we made any.
  for (CleanupHandle eltCleanup : SplitCleanups)
    SGF.Cleanups.forwardCleanup(eltCleanup);
}

bool KnownAddressInitialization::isInPlaceInitializationOfGlobal() const {
  return isa<GlobalAddrInst>(address);
}

bool TemporaryInitialization::isInPlaceInitializationOfGlobal() const {
  return isa<GlobalAddrInst>(Addr);
}

void TemporaryInitialization::finishInitialization(SILGenFunction &SGF) {
  SingleBufferInitialization::finishInitialization(SGF);
  if (Cleanup.isValid())
    SGF.Cleanups.setCleanupState(Cleanup, CleanupState::Active);
}

StoreBorrowInitialization::StoreBorrowInitialization(SILValue address)
    : address(address) {
  assert(isa<AllocStackInst>(address) ||
         isa<MarkUnresolvedNonCopyableValueInst>(address) &&
             "invalid destination for store_borrow initialization!?");
}

void StoreBorrowInitialization::copyOrInitValueInto(SILGenFunction &SGF,
                                                    SILLocation loc,
                                                    ManagedValue mv,
                                                    bool isInit) {
  auto value = mv.getValue();
  auto &lowering = SGF.getTypeLowering(value->getType());
  if (lowering.isAddressOnly() && SGF.silConv.useLoweredAddresses()) {
    llvm::report_fatal_error(
        "Attempting to store_borrow an address-only value!?");
  }
  if (value->getType().isAddress()) {
    value = SGF.emitManagedLoadBorrow(loc, value).getValue();
  }
  if (!isInit) {
    value = lowering.emitCopyValue(SGF.B, loc, value);
  }
  storeBorrow = SGF.emitManagedStoreBorrow(loc, value, address);
}

SILValue StoreBorrowInitialization::getAddress() const {
  if (storeBorrow) {
    return storeBorrow.getValue();
  }
  return address;
}

ManagedValue StoreBorrowInitialization::getManagedAddress() const {
  return storeBorrow;
}

namespace {
class ReleaseValueCleanup : public Cleanup {
  SILValue v;
public:
  ReleaseValueCleanup(SILValue v) : v(v) {}

  void emit(SILGenFunction &SGF, CleanupLocation l,
            ForUnwind_t forUnwind) override {
    if (v->getType().isAddress())
      SGF.B.createDestroyAddr(l, v);
    else
      SGF.B.emitDestroyValueOperation(l, v);
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
/// Cleanup to deallocate a now-uninitialized variable.
class DeallocStackCleanup : public Cleanup {
  SILValue Addr;
public:
  DeallocStackCleanup(SILValue addr) : Addr(addr) {}

  void emit(SILGenFunction &SGF, CleanupLocation l,
            ForUnwind_t forUnwind) override {
    SGF.B.createDeallocStack(l, Addr);
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

  void emit(SILGenFunction &SGF, CleanupLocation l,
            ForUnwind_t forUnwind) override {
    SGF.destroyLocalVariable(l, Var);
  }

  void dump(SILGenFunction &SGF) const override {
#ifndef NDEBUG
    llvm::errs() << "DestroyLocalVariable\n"
                 << "State:" << getState() << "\n"
                 << "Decl: ";
    Var->print(llvm::errs());
    llvm::errs() << "\n";
    if (isActive()) {
      auto &loc = SGF.VarLocs[Var];
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
  SILValue Box;
public:
  DeallocateUninitializedLocalVariable(SILValue box) : Box(box) {}

  void emit(SILGenFunction &SGF, CleanupLocation l,
            ForUnwind_t forUnwind) override {
    auto box = Box;
    if (SGF.getASTContext().SILOpts.supportsLexicalLifetimes(SGF.getModule())) {
      if (auto *bbi = dyn_cast<BeginBorrowInst>(box)) {
        SGF.B.createEndBorrow(l, bbi);
        box = bbi->getOperand();
      }
    }
    SGF.B.createDeallocBox(l, box);
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

  /// The alloc_box instruction.
  SILValue Box;

  /// The projected address.
  SILValue Addr;

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
  LocalVariableInitialization(VarDecl *decl,
                              std::optional<MarkUninitializedInst::Kind> kind,
                              uint16_t ArgNo, bool generateDebugInfo,
                              SILGenFunction &SGF)
      : decl(decl) {
    assert(decl->getDeclContext()->isLocalContext() &&
           "can't emit a local var for a non-local var decl");
    assert(decl->hasStorage() && "can't emit storage for a computed variable");
    assert(!SGF.VarLocs.count(decl) && "Already have an entry for this decl?");

    // The box type's context is lowered in the minimal resilience domain.
    auto instanceType = SGF.SGM.Types.getLoweredRValueType(
        TypeExpansionContext::minimal(), decl->getTypeInContext());


    bool isNoImplicitCopy = instanceType->is<SILMoveOnlyWrappedType>();

    // If our instance type is not already @moveOnly wrapped, and it's a
    // no-implicit-copy parameter, wrap it.
    if (!isNoImplicitCopy && !instanceType->isNoncopyable()) {
      if (auto *pd = dyn_cast<ParamDecl>(decl)) {
        isNoImplicitCopy = pd->isNoImplicitCopy();
        isNoImplicitCopy |= pd->getSpecifier() == ParamSpecifier::Consuming;
        if (pd->isSelfParameter()) {
          auto *dc = pd->getDeclContext();
          if (auto *fn = dyn_cast<FuncDecl>(dc)) {
            auto accessKind = fn->getSelfAccessKind();
            isNoImplicitCopy |= accessKind == SelfAccessKind::Consuming;
          }
        }
        if (isNoImplicitCopy)
          instanceType = SILMoveOnlyWrappedType::get(instanceType);
      }
    }

    const bool isCopyable = isNoImplicitCopy || !instanceType->isNoncopyable();

    auto boxType = SGF.SGM.Types.getContextBoxTypeForCapture(
        decl, instanceType, SGF.F.getGenericEnvironment(),
        /*mutable=*/ isCopyable || !decl->isLet());

    // The variable may have its lifetime extended by a closure, heap-allocate
    // it using a box.

    std::optional<SILDebugVariable> DbgVar;
    if (generateDebugInfo)
      DbgVar = SILDebugVariable(decl->isLet(), ArgNo);
    Box = SGF.B.createAllocBox(
        decl, boxType, DbgVar, DoesNotHaveDynamicLifetime,
        /*reflection*/ false, DoesNotUseMoveableValueDebugInfo,
        !generateDebugInfo);

    // Mark the memory as uninitialized, so DI will track it for us.
    if (kind)
      Box = SGF.B.createMarkUninitialized(decl, Box, kind.value());

    // If we have a reference binding, mark it.
    if (decl->getIntroducer() == VarDecl::Introducer::InOut)
      Box = SGF.B.createMarkUnresolvedReferenceBindingInst(
          decl, Box, MarkUnresolvedReferenceBindingInst::Kind::InOut);

    if (SGF.getASTContext().SILOpts.supportsLexicalLifetimes(SGF.getModule())) {
      auto loweredType = SGF.getTypeLowering(decl->getTypeInContext()).getLoweredType();
      auto lifetime = SGF.F.getLifetime(decl, loweredType);
      // The box itself isn't lexical--neither a weak reference nor an unsafe
      // pointer to a box can be formed; and the box doesn't synchronize on
      // deinit.
      //
      // Only add a lexical lifetime to the box if the variable it stores
      // requires one.
      Box =
          SGF.B.createBeginBorrow(decl, Box, IsLexical_t(lifetime.isLexical()),
                                  DoesNotHavePointerEscape, IsFromVarDecl);
    }

    Addr = SGF.B.createProjectBox(decl, Box, 0);

    // Push a cleanup to destroy the local variable.  This has to be
    // inactive until the variable is initialized.
    SGF.Cleanups.pushCleanupInState<DestroyLocalVariable>(CleanupState::Dormant,
                                                          decl);
    ReleaseCleanup = SGF.Cleanups.getTopCleanup();

    // Push a cleanup to deallocate the local variable. This references the
    // box directly since it might be activated before we update
    // SGF.VarLocs.
    SGF.Cleanups.pushCleanup<DeallocateUninitializedLocalVariable>(Box);
    DeallocCleanup = SGF.Cleanups.getTopCleanup();
  }

  ~LocalVariableInitialization() override {
    assert(DidFinish && "did not call VarInit::finishInitialization!");
  }

  SILValue getAddress() const {
    assert(Addr);
    return Addr;
  }

  /// If we have an address, returns the address. Otherwise, if we only have a
  /// box, lazily projects it out and returns it.
  SILValue getAddressForInPlaceInitialization(SILGenFunction &SGF,
                                              SILLocation loc) override {
    if (!Addr && Box) {
      auto pbi = SGF.B.createProjectBox(loc, Box, 0);
      return pbi;
    }

    return getAddress();
  }

  bool isInPlaceInitializationOfGlobal() const override {
    return dyn_cast_or_null<GlobalAddrInst>(Addr);
  }

  void finishUninitialized(SILGenFunction &SGF) override {
    LocalVariableInitialization::finishInitialization(SGF);
  }

  void finishInitialization(SILGenFunction &SGF) override {
    /// Remember that this is the memory location that we've emitted the
    /// decl to.
    assert(SGF.VarLocs.count(decl) == 0 && "Already emitted the local?");

    SGF.VarLocs[decl] = SILGenFunction::VarLoc(Addr,
      SILAccessEnforcement::Dynamic, Box);

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

static void deallocateAddressable(SILGenFunction &SGF,
                SILLocation l,
                const SILGenFunction::VarLoc::AddressableBuffer::State &state) {
  SGF.B.createEndBorrow(l, state.storeBorrow);
  SGF.B.createDeallocStack(l, state.allocStack);
  if (state.reabstraction
      && !state.reabstraction->getType().isTrivial(SGF.F)) {
    SGF.B.createDestroyValue(l, state.reabstraction);
  }
}

/// Cleanup to deallocate the addressable buffer for a parameter or let
/// binding.
class DeallocateLocalVariableAddressableBuffer : public Cleanup {
  ValueDecl *vd;
public:
  DeallocateLocalVariableAddressableBuffer(ValueDecl *vd) : vd(vd) {}

  void emit(SILGenFunction &SGF, CleanupLocation l,
            ForUnwind_t forUnwind) override {
    auto addressableBuffer = SGF.getAddressableBufferInfo(vd);
    if (!addressableBuffer) {
      return;
    }
    auto found = SGF.VarLocs.find(vd);
    if (found == SGF.VarLocs.end()) {
      return;
    }
    
    if (auto *state = addressableBuffer->getState()) {
      // The addressable buffer was forced, so clean it up now.
      deallocateAddressable(SGF, l, *state);
    } else {
      // Remember this insert location in case we need to force the addressable
      // buffer later.
      SILInstruction *marker = SGF.B.createTuple(l, {});
      addressableBuffer->cleanupPoints.emplace_back(marker);
    }
  }

  void dump(SILGenFunction &SGF) const override {
#ifndef NDEBUG
    llvm::errs() << "DeallocateLocalVariableAddressableBuffer\n"
                 << "State:" << getState() << "\n"
                 << "Decl: ";
    vd->print(llvm::errs());
    llvm::errs() << "\n";
#endif
  }
};

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
  LetValueInitialization(VarDecl *vd, SILGenFunction &SGF) : vd(vd) {
    const TypeLowering *lowering = nullptr;
    if (vd->isNoImplicitCopy()) {
      lowering = &SGF.getTypeLowering(
          SILMoveOnlyWrappedType::get(vd->getTypeInContext()->getCanonicalType()));
    } else {
      lowering = &SGF.getTypeLowering(vd->getTypeInContext());
    }

    // Decide whether we need a temporary stack buffer to evaluate this 'let'.
    // There are four cases we need to handle here: parameters, initialized (or
    // bound) decls, uninitialized ones, and async let declarations.
    bool needsTemporaryBuffer;
    bool isUninitialized = false;

    assert(!isa<ParamDecl>(vd)
           && "should not bind function params on this path");
    if (vd->getParentPatternBinding() &&
        !vd->getParentExecutableInitializer()) {
      // If this is a let-value without an initializer, then we need a temporary
      // buffer.  DI will make sure it is only assigned to once.
      needsTemporaryBuffer = true;
      isUninitialized = true;
    } else if (vd->isAsyncLet()) {
      // If this is an async let, treat it like a let-value without an
      // initializer. The initializer runs concurrently in a child task,
      // and value will be initialized at the point the variable in the
      // async let is used.
      needsTemporaryBuffer = true;
      isUninitialized = true;
    } else {
      // If this is a let with an initializer or bound value, we only need a
      // buffer if the type is address only or is noncopyable.
      //
      // For noncopyable types, we always need to box them.
      needsTemporaryBuffer =
          (lowering->isAddressOnly() && SGF.silConv.useLoweredAddresses()) ||
              lowering->getLoweredType().isMoveOnly(/*orWrapped=*/false);
    }

    // Make sure that we have a non-address only type when binding a
    // @_noImplicitCopy let.
    if (lowering->isAddressOnly() && vd->isNoImplicitCopy()) {
      auto d = diag::noimplicitcopy_used_on_generic_or_existential;
      diagnose(SGF.getASTContext(), vd->getLoc(), d);
    }

    if (needsTemporaryBuffer) {
      bool lexicalLifetimesEnabled =
          SGF.getASTContext().SILOpts.supportsLexicalLifetimes(SGF.getModule());
      auto lifetime = SGF.F.getLifetime(vd, lowering->getLoweredType());
      auto isLexical =
          IsLexical_t(lexicalLifetimesEnabled && lifetime.isLexical());
      address = SGF.emitTemporaryAllocation(vd, lowering->getLoweredType(),
                                            DoesNotHaveDynamicLifetime,
                                            isLexical, IsFromVarDecl);
      if (isUninitialized)
        address = SGF.B.createMarkUninitializedVar(vd, address);
      DestroyCleanup = SGF.enterDormantTemporaryCleanup(address, *lowering);
      SGF.VarLocs[vd] = SILGenFunction::VarLoc(address,
                                               SILAccessEnforcement::Unknown);
    }
    // Push a cleanup to destroy the let declaration.  This has to be
    // inactive until the variable is initialized: if control flow exits the
    // before the value is bound, we don't want to destroy the value.
    //
    // Cleanups are required for all lexically scoped variables to delimite
    // the variable scope, even if the cleanup does nothing.
    SGF.Cleanups.pushCleanupInState<DestroyLocalVariable>(
      CleanupState::Dormant, vd);
    DestroyCleanup = SGF.Cleanups.getTopCleanup();

    // If the binding has an addressable buffer forced, it should be cleaned
    // up at this scope.
    SGF.enterLocalVariableAddressableBufferScope(vd);
  }

  ~LetValueInitialization() override {
    assert(DidFinish && "did not call LetValueInit::finishInitialization!");
  }

  bool hasAddress() const { return (bool)address; }

  bool canPerformInPlaceInitialization() const override {
    return hasAddress();
  }

  bool isInPlaceInitializationOfGlobal() const override {
    return isa<GlobalAddrInst>(address);
  }
  
  SILValue getAddressForInPlaceInitialization(SILGenFunction &SGF,
                                              SILLocation loc) override {
    // Emit into the buffer that 'let's produce for address-only values if
    // we have it.
    assert(hasAddress());
    return address;
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
  splitIntoTupleElements(SILGenFunction &SGF, SILLocation loc, CanType type,
                         SmallVectorImpl<InitializationPtr> &buf) override {
    assert(SplitCleanups.empty());
    auto address = getAddressForInPlaceInitialization(SGF, loc);
    return SingleBufferInitialization
       ::splitSingleBufferIntoTupleElements(SGF, loc, type, address, buf,
                                            SplitCleanups);
  }

  /// This is a helper method for bindValue that creates a scopes operation for
  /// the lexical variable lifetime and handles any changes to the value needed
  /// for move-only values.
  SILValue getValueForLexicalLifetimeBinding(SILGenFunction &SGF,
                                             SILLocation PrologueLoc,
                                             SILValue value, bool wasPlusOne) {
    // TODO: emitPatternBindingInitialization creates fake local variables for
    // metatypes within function_conversion expressions that operate on static
    // functions. Creating SIL local variables for all these is impractical and
    // undesirable. We need a better way of representing these "capture_list"
    // locals in a way that doesn't produce SIL locals. For now, bypassing
    // metatypes mostly avoids the issue, but it's not robust and doesn't allow
    // SIL-level analysis of real metatype variables.
    if (isa<MetatypeType>(value->getType().getASTType())) {
      return value;
    }

    // Preprocess an owned moveonly value that had a cleanup. Even if we are
    // only performing a borrow for our lexical lifetime, this ensures that
    // defs see this initialization as consuming this value.
    if (value->getOwnershipKind() == OwnershipKind::Owned &&
        value->getType().isMoveOnlyWrapped()) {
      assert(wasPlusOne);
      // NOTE: If our type is trivial when not wrapped in a
      // SILMoveOnlyWrappedType, this will return a trivial value. We rely
      // on the checker to determine if this is an acceptable use of the
      // value.
      value =
          SGF.B.createOwnedMoveOnlyWrapperToCopyableValue(PrologueLoc, value);
    }
    auto isLexical =
        IsLexical_t(SGF.F.getLifetime(vd, value->getType()).isLexical());
    switch (value->getOwnershipKind()) {
    case OwnershipKind::None:
    case OwnershipKind::Owned:
      value = SGF.B.createMoveValue(PrologueLoc, value, isLexical,
                                    DoesNotHavePointerEscape, IsFromVarDecl);
      break;
    case OwnershipKind::Guaranteed:
      value = SGF.B.createBeginBorrow(PrologueLoc, value, isLexical,
                                      DoesNotHavePointerEscape, IsFromVarDecl);
      break;
    case OwnershipKind::Unowned:
    case OwnershipKind::Any:
      llvm_unreachable("unexpected ownership");
    }
    if (vd->isNoImplicitCopy()) {
      value =
          SGF.B.createOwnedCopyableToMoveOnlyWrapperValue(PrologueLoc, value);
      // fall-through to the owned move-only case...
    }
    if (value->getType().isMoveOnly(/*orWrapped=*/true)
        && value->getOwnershipKind() == OwnershipKind::Owned) {
      value = SGF.B.createMarkUnresolvedNonCopyableValueInst(
        PrologueLoc, value,
        MarkUnresolvedNonCopyableValueInst::CheckKind::
        ConsumableAndAssignable);
    }
    return value;
  }

  void bindValue(SILValue value, SILGenFunction &SGF, bool wasPlusOne,
                 SILLocation loc) {
    assert(!SGF.VarLocs.count(vd) && "Already emitted this vardecl?");
    // If we're binding an address to this let value, then we can use it as an
    // address later.  This happens when binding an address only parameter to
    // an argument, for example.
    if (value->getType().isAddress())
      address = value;

    if (SGF.getASTContext().SILOpts.supportsLexicalLifetimes(SGF.getModule()))
      value = getValueForLexicalLifetimeBinding(SGF, loc, value, wasPlusOne);

    SGF.VarLocs[vd] = SILGenFunction::VarLoc(value,
                                             SILAccessEnforcement::Unknown);

    // Emit a debug_value[_addr] instruction to record the start of this value's
    // lifetime, if permitted to do so.
    if (!EmitDebugValueOnInit)
      return;

    // Use the scope from loc and diagnostic location from vd.
    RegularLocation PrologueLoc(vd);
    PrologueLoc.markAsPrologue();
    SILDebugVariable DbgVar(vd->getName().str(), vd->isLet(), /*ArgNo=*/0);
    SGF.B.emitDebugDescription(PrologueLoc, value, DbgVar);
  }

  void copyOrInitValueInto(SILGenFunction &SGF, SILLocation loc,
                           ManagedValue value, bool isInit) override {
    // If this let value has an address, we can handle it just like a single
    // buffer value.
    if (hasAddress()) {
      return SingleBufferInitialization::
        copyOrInitValueIntoSingleBuffer(SGF, loc, value, isInit, address);
    }

    // Otherwise, we bind the value.
    if (isInit) {
      // Disable the rvalue expression cleanup, since the let value
      // initialization has a cleanup that lives for the entire scope of the
      // let declaration.
      bool isPlusOne = value.isPlusOne(SGF);
      bindValue(value.forward(SGF), SGF, isPlusOne, loc);
    } else {
      // Disable the expression cleanup of the copy, since the let value
      // initialization has a cleanup that lives for the entire scope of the
      // let declaration.
      bindValue(value.copyUnmanaged(SGF, loc).forward(SGF), SGF, true, loc);
    }
  }

  void finishUninitialized(SILGenFunction &SGF) override {
    LetValueInitialization::finishInitialization(SGF);
  }

  void finishInitialization(SILGenFunction &SGF) override {
    assert(!DidFinish &&
           "called LetValueInit::finishInitialization twice!");
    assert(SGF.VarLocs.count(vd) && "Didn't bind a value to this let!");

    // Deactivate any cleanups we made when splitting the tuple.
    for (auto cleanup : SplitCleanups)
      SGF.Cleanups.forwardCleanup(cleanup);

    // Activate the destroy cleanup.
    if (DestroyCleanup != CleanupHandle::invalid()) {
      SGF.Cleanups.setCleanupState(DestroyCleanup, CleanupState::Active);
    }

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
    : VarInit(std::move(subInit)) {
    assert(VarInit->canPerformInPlaceInitialization());
  }

  void copyOrInitValueInto(SILGenFunction &SGF, SILLocation loc,
                           ManagedValue value, bool isInit) override {
    auto address = VarInit->getAddressForInPlaceInitialization(SGF, loc);
    // If this is not an initialization, copy the value before we translateIt,
    // translation expects a +1 value.
    if (isInit)
      value.forwardInto(SGF, loc, address);
    else
      value.copyInto(SGF, loc, address);
  }

  void finishUninitialized(SILGenFunction &SGF) override {
    ReferenceStorageInitialization::finishInitialization(SGF);
  }
  
  void finishInitialization(SILGenFunction &SGF) override {
    VarInit->finishInitialization(SGF);
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

  void copyOrInitValueInto(SILGenFunction &SGF, SILLocation loc,
                           ManagedValue value, bool isInit) override = 0;

  void bindVariable(SILLocation loc, VarDecl *var, ManagedValue value,
                    CanType formalValueType, SILGenFunction &SGF) {
    // Initialize the variable value.
    InitializationPtr init = SGF.emitInitializationForVarDecl(var, var->isLet());
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

  void copyOrInitValueInto(SILGenFunction &SGF, SILLocation loc,
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

  assert(testBool->getType().getASTType()->isBool());
  auto i1Value = SGF.emitUnwrapIntegerResult(loc, testBool);

  SILBasicBlock *contBB = SGF.B.splitBlockForFallthrough();
  auto falseBB = SGF.Cleanups.emitBlockForCleanups(getFailureDest(), loc);
  SGF.B.createCondBranch(loc, i1Value, contBB, falseBB);

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

/// If \p elt belongs to an enum that has exactly two cases and that can be
/// exhaustively switched, return the other case. Otherwise, return nullptr.
static EnumElementDecl *getOppositeBinaryDecl(const SILGenFunction &SGF,
                                              const EnumElementDecl *elt) {
  const EnumDecl *enumDecl = elt->getParentEnum();
  if (!enumDecl->isEffectivelyExhaustive(SGF.SGM.SwiftModule,
                                         SGF.F.getResilienceExpansion())) {
    return nullptr;
  }

  EnumDecl::ElementRange range = enumDecl->getAllElements();
  auto iter = range.begin();
  if (iter == range.end())
    return nullptr;
  bool seenDecl = false;
  EnumElementDecl *result = nullptr;
  if (*iter == elt) {
    seenDecl = true;
  } else {
    result = *iter;
  }

  ++iter;
  if (iter == range.end())
    return nullptr;
  if (seenDecl) {
    assert(!result);
    result = *iter;
  } else {
    if (elt != *iter)
      return nullptr;
    seenDecl = true;
  }
  ++iter;

  // If we reach this point, we saw the decl we were looking for and one other
  // case. If we have any additional cases, then we do not have a binary enum.
  if (iter != range.end())
    return nullptr;

  // This is always true since we have already returned earlier nullptr if we
  // did not see the decl at all.
  assert(seenDecl);
  return result;
}

void EnumElementPatternInitialization::emitEnumMatch(
    ManagedValue value, EnumElementDecl *eltDecl, Initialization *subInit,
    JumpDest failureDest, SILLocation loc, SILGenFunction &SGF) {

  // Create all of the blocks early so we can maintain a consistent ordering
  // (and update less tests). Break this at your fingers parallel.
  //
  // *NOTE* This needs to be in reverse order to preserve the textual SIL.
  auto *contBlock = SGF.createBasicBlock();
  auto *someBlock = SGF.createBasicBlock();
  auto *defaultBlock = SGF.createBasicBlock();
  auto *originalBlock = SGF.B.getInsertionBB();

  SwitchEnumBuilder switchBuilder(SGF.B, loc, value);

  // Handle the none case.
  //
  // *NOTE*: Since we are performing an initialization here, it is *VERY*
  // important that we emit the negative case first. The reason why is that
  // currently the initialization has a dormant cleanup in a scope that may be
  // after the failureDest depth. Once we run the positive case, this
  // initialization will be enabled. Thus if we run the negative case /after/
  // the positive case, a cleanup will be emitted for the initialization on the
  // negative path... but the actual initialization happened on the positive
  // path, causing a use (the destroy on the negative path) to be created that
  // does not dominate its definition (in the positive path).
  auto handler = [&SGF, &loc, &failureDest](ManagedValue mv,
                                            SwitchCaseFullExpr &&expr) {
    expr.exit();
    SGF.Cleanups.emitBranchAndCleanups(failureDest, loc);
  };

  // If we have a binary enum, do not emit a true default case. This ensures
  // that we do not emit a destroy_value on a .None.
  bool inferredBinaryEnum = false;
  if (auto *otherDecl = getOppositeBinaryDecl(SGF, eltDecl)) {
    inferredBinaryEnum = true;
    switchBuilder.addCase(otherDecl, defaultBlock, nullptr, handler);
  } else {
    switchBuilder.addDefaultCase(
        defaultBlock, nullptr, handler,
        SwitchEnumBuilder::DefaultDispatchTime::BeforeNormalCases);
  }

  // Always insert the some case at the front of the list. In the default case,
  // this will not matter, but in the case where we have a binary enum, we want
  // to preserve the old ordering of .some/.none. to make it easier to update
  // tests.
  switchBuilder.addCase(
      eltDecl, someBlock, contBlock,
      [&SGF, &loc, &eltDecl, &subInit, &value](ManagedValue mv,
                                               SwitchCaseFullExpr &&expr) {
        // If the enum case has no bound value, we're done.
        if (!eltDecl->hasAssociatedValues()) {
          assert(
              subInit == nullptr &&
              "Cannot have a subinit when there is no value to match against");
          expr.exitAndBranch(loc);
          return;
        }

        if (subInit == nullptr) {
          // If there is no subinitialization, then we are done matching.  Don't
          // bother projecting out the any elements value only to ignore it.
          expr.exitAndBranch(loc);
          return;
        }

        // Otherwise, the bound value for the enum case is available.
        SILType eltTy = value.getType().getEnumElementType(
            eltDecl, SGF.SGM.M, SGF.getTypeExpansionContext());
        auto &eltTL = SGF.getTypeLowering(eltTy);

        if (mv.getType().isAddress()) {
          // If the enum is address-only, take from the enum we have and load it
          // if
          // the element value is loadable.
          assert((eltTL.isTrivial() || mv.hasCleanup()) &&
                 "must be able to consume value");
          mv = SGF.B.createUncheckedTakeEnumDataAddr(loc, mv, eltDecl, eltTy);
          // Load a loadable data value.
          if (eltTL.isLoadable())
            mv = SGF.B.createLoadTake(loc, mv);
        }

        // If the payload is indirect, project it out of the box.
        if (eltDecl->isIndirect() || eltDecl->getParentEnum()->isIndirect()) {
          ManagedValue boxedValue = SGF.B.createProjectBox(loc, mv, 0);
          auto &boxedTL = SGF.getTypeLowering(boxedValue.getType());

          // We must treat the boxed value as +0 since it may be shared. Copy it
          // if nontrivial.
          //
          // NOTE: The APIs that we are using here will ensure that if we have
          // a trivial value, the load_borrow will become a load [trivial] and
          // the copies will be "automagically" elided.
          if (boxedTL.isLoadable() || !SGF.silConv.useLoweredAddresses()) {
            UnenforcedAccess access;
            SILValue accessAddress = access.beginAccess(
                SGF, loc, boxedValue.getValue(), SILAccessKind::Read);
            auto mvAccessAddress =
                ManagedValue::forBorrowedAddressRValue(accessAddress);
            {
              Scope loadScope(SGF, loc);
              ManagedValue borrowedVal =
                  SGF.B.createLoadBorrow(loc, mvAccessAddress);
              mv = loadScope.popPreservingValue(
                  borrowedVal.copyUnmanaged(SGF, loc));
            }
            access.endAccess(SGF);
          } else {
            // If we do not have a loadable value, just do a copy of the
            // boxedValue.
            mv = boxedValue.copyUnmanaged(SGF, loc);
          }
        }

        // Reabstract to the substituted type, if needed.
        CanType substEltTy =
            value.getType()
                .getASTType()
                ->getTypeOfMember(eltDecl,
                                  eltDecl->getPayloadInterfaceType())
                ->getCanonicalType();

        AbstractionPattern origEltTy =
            (eltDecl == SGF.getASTContext().getOptionalSomeDecl()
                 ? AbstractionPattern(substEltTy)
                 : SGF.SGM.M.Types.getAbstractionPattern(eltDecl));

        mv = SGF.emitOrigToSubstValue(loc, mv, origEltTy, substEltTy);

        // Pass the +1 value down into the sub initialization.
        subInit->copyOrInitValueInto(SGF, loc, mv, /*is an init*/ true);
        expr.exitAndBranch(loc);
      });

  std::move(switchBuilder).emit();

  // If we inferred a binary enum, put the asked for case first so we preserve
  // the current code structure. This just ensures that less test updates are
  // needed.
  if (inferredBinaryEnum) {
    if (auto *switchEnum =
            dyn_cast<SwitchEnumInst>(originalBlock->getTerminator())) {
      switchEnum->swapCase(0, 1);
    } else {
      auto *switchEnumAddr =
          cast<SwitchEnumAddrInst>(originalBlock->getTerminator());
      switchEnumAddr->swapCase(0, 1);
    }
  }

  // Reset the insertion point to the end of contBlock.
  SGF.B.setInsertionPoint(contBlock);
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
  auto destType = OptionalType::get(pattern->getCastType());

  value =
      emitConditionalCheckedCast(SGF, loc, value, pattern->getType(), destType,
                                 pattern->getCastKind(), SGFContext(),
                                 ProfileCounter(), ProfileCounter())
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
  auto i1Value = SGF.emitUnwrapIntegerResult(loc, value.forward(SGF));

  // Branch on the boolean based on whether we're testing for true or false.
  SILBasicBlock *trueBB = SGF.B.splitBlockForFallthrough();
  auto contBB = trueBB;
  auto falseBB = SGF.Cleanups.emitBlockForCleanups(getFailureDest(), loc);

  if (!pattern->getValue())
    std::swap(trueBB, falseBB);
  SGF.B.createCondBranch(loc, i1Value, trueBB, falseBB);
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

  bool generateDebugInfo = true;

  InitializationForPattern(SILGenFunction &SGF, JumpDest patternFailDest,
                           bool generateDebugInfo)
      : SGF(SGF), patternFailDest(patternFailDest),
        generateDebugInfo(generateDebugInfo) {}

  // Paren, Typed, and Var patterns are noops, just look through them.
  InitializationPtr visitParenPattern(ParenPattern *P) {
    return visit(P->getSubPattern());
  }
  InitializationPtr visitTypedPattern(TypedPattern *P) {
    return visit(P->getSubPattern());
  }
  InitializationPtr visitBindingPattern(BindingPattern *P) {
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

    return SGF.emitInitializationForVarDecl(P->getDecl(), P->getDecl()->isLet(),
                                            generateDebugInfo);
  }

  // Bind a tuple pattern by aggregating the component variables into a
  // TupleInitialization.
  InitializationPtr visitTuplePattern(TuplePattern *P) {
    TupleInitialization *init = new TupleInitialization(
      cast<TupleType>(P->getType()->getCanonicalType()));
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

InitializationPtr
SILGenFunction::emitInitializationForVarDecl(VarDecl *vd, bool forceImmutable,
                                             bool generateDebugInfo) {
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

    VarLocs[vd] = VarLoc(SV, SILAccessEnforcement::Dynamic);
    return InitializationPtr(new KnownAddressInitialization(SV));
  }

  CanType varType = vd->getTypeInContext()->getCanonicalType();

  assert(!isa<InOutType>(varType) && "local variables should never be inout");

  // If this is a 'let' initialization for a copyable non-global, set up a let
  // binding, which stores the initialization value into VarLocs directly.
  if (forceImmutable && vd->getDeclContext()->isLocalContext() &&
      !isa<ReferenceStorageType>(varType) && !varType->isNoncopyable())
    return InitializationPtr(new LetValueInitialization(vd, *this));

  // If the variable has no initial value, emit a mark_uninitialized instruction
  // so that DI tracks and enforces validity of it.
  bool isUninitialized =
    vd->getParentPatternBinding() && !vd->getParentExecutableInitializer();

  // If this is a global variable, initialize it without allocations or
  // cleanups.
  InitializationPtr Result;
  if (!vd->getDeclContext()->isLocalContext()) {
    auto *silG = SGM.getSILGlobalVariable(vd, NotForDefinition);
    RegularLocation loc(vd);
    loc.markAutoGenerated();
    B.createAllocGlobal(loc, silG);
    SILValue addr = B.createGlobalAddr(loc, silG, /*dependencyToken=*/ SILValue());
    if (isUninitialized)
      addr = B.createMarkUninitializedVar(loc, addr);

    VarLocs[vd] = VarLoc(addr, SILAccessEnforcement::Dynamic);
    Result = InitializationPtr(new KnownAddressInitialization(addr));
  } else {
    std::optional<MarkUninitializedInst::Kind> uninitKind;
    if (isUninitialized) {
      uninitKind = MarkUninitializedInst::Kind::Var;
    }
    Result = emitLocalVariableWithCleanup(vd, uninitKind, /*argno*/ 0,
                                          generateDebugInfo);
  }

  // If we're initializing a weak or unowned variable, this requires a change in
  // type.
  if (isa<ReferenceStorageType>(varType))
    Result = InitializationPtr(new
                           ReferenceStorageInitialization(std::move(Result)));
  return Result;
}

void SILGenFunction::emitPatternBinding(PatternBindingDecl *PBD, unsigned idx,
                                        bool generateDebugInfo) {
  auto &C = PBD->getASTContext();

  // If this is an async let, create a child task to compute the initializer
  // value.
  if (PBD->isAsyncLet()) {
    // Look through the implicit await (if present), try (if present), and
    // call to reach the autoclosure that computes the value.
    auto *init = PBD->getExecutableInit(idx);
    if (auto awaitExpr = dyn_cast<AwaitExpr>(init))
      init = awaitExpr->getSubExpr();
    if (auto tryExpr = dyn_cast<TryExpr>(init))
      init = tryExpr->getSubExpr();
    init = cast<CallExpr>(init)->getFn();
    auto initClosure = cast<AutoClosureExpr>(init);
    bool isThrowing = init->getType()->castTo<AnyFunctionType>()->isThrowing();

    // Allocate space to receive the child task's result.
    auto initLoweredTy = getLoweredType(AbstractionPattern::getOpaque(),
                                        PBD->getPattern(idx)->getType());
    SILLocation loc(PBD);
    SILValue resultBuf = emitTemporaryAllocation(loc, initLoweredTy);
    SILValue resultBufPtr = B.createAddressToPointer(loc, resultBuf,
                          SILType::getPrimitiveObjectType(C.TheRawPointerType),
                          /*needsStackProtection=*/ false);
    
    // Emit the closure for the child task.
    // Prepare the opaque `AsyncLet` representation.
    SILValue alet;
    {

      // Currently we don't pass any task options here, so just grab a 'nil'.

      // If we can statically detect some option needs to be passed, e.g.
      // an executor preference, we'd construct the appropriate option here and
      // pass it to the async let start.
      auto options = B.createManagedOptionalNone(
          loc, SILType::getOptionalType(SILType::getRawPointerType(C)));

      alet = emitAsyncLetStart(
          loc,
          options.forward(*this), // options is B.createManagedOptionalNone
          initClosure,
          resultBufPtr
        ).forward(*this);
    }
    
    // Push a cleanup to destroy the AsyncLet along with the task and child record.
    enterAsyncLetCleanup(alet, resultBufPtr);

    // Save the child task so we can await it as needed.
    AsyncLetChildTasks[{PBD, idx}] = {alet, resultBufPtr, isThrowing};
    return;
  }

  auto initialization = emitPatternBindingInitialization(
      PBD->getPattern(idx), JumpDest::invalid(), generateDebugInfo);

  auto getWrappedValueExpr = [&](VarDecl *var) -> Expr * {
    if (auto *orig = var->getOriginalWrappedProperty()) {
      auto initInfo = orig->getPropertyWrapperInitializerInfo();
      if (auto *placeholder = initInfo.getWrappedValuePlaceholder()) {
        return placeholder->getOriginalWrappedValue();
      }
    }
    return nullptr;
  };

  auto *initExpr = PBD->getExecutableInit(idx);

  // If we do not have an explicit initialization expression, just mark the
  // initialization as unfinished for DI to resolve.
  if (!initExpr) {
    return initialization->finishUninitialized(*this);
  }

  // Otherwise, an initial value expression was specified by the decl... emit it
  // into the initialization.
  FullExpr Scope(Cleanups, CleanupLocation(initExpr));

  auto *singleVar = PBD->getSingleVar();
  bool isLocalSingleVar =
      singleVar && singleVar->getDeclContext()->isLocalContext();
  if (isLocalSingleVar) {
    if (auto *orig = singleVar->getOriginalWrappedProperty()) {
      if (auto *initExpr = getWrappedValueExpr(singleVar)) {
        auto value = emitRValue(initExpr);
        emitApplyOfPropertyWrapperBackingInitializer(
            PBD, orig, getForwardingSubstitutionMap(), std::move(value))
            .forwardInto(*this, SILLocation(PBD), initialization.get());
        return;
      }
    }
  }

  emitExprInto(initExpr, initialization.get());
}

void SILGenFunction::visitPatternBindingDecl(PatternBindingDecl *PBD,
                                             bool generateDebugInfo) {

  // Allocate the variables and build up an Initialization over their
  // allocated storage.
  for (unsigned i : range(PBD->getNumPatternEntries())) {
    emitPatternBinding(PBD, i, generateDebugInfo);
  }
}

void SILGenFunction::visitVarDecl(VarDecl *D) {
  // We handle emitting the variable storage when we see the pattern binding.

  // Avoid request evaluator overhead in the common case where there's
  // no wrapper.
  if (D->getAttrs().hasAttribute<CustomAttr>()) {
    // Emit the property wrapper backing initializer if necessary.
    auto initInfo = D->getPropertyWrapperInitializerInfo();
    if (initInfo.hasInitFromWrappedValue())
      SGM.emitPropertyWrapperBackingInitializer(D);
  }

  // Emit lazy and property wrapper backing storage.
  D->visitAuxiliaryDecls([&](VarDecl *var) {
    if (auto *patternBinding = var->getParentPatternBinding())
      visitPatternBindingDecl(patternBinding);

    visit(var);
  });

  // Emit the variable's accessors.
  SGM.visitEmittedAccessors(D, [&](AccessorDecl *accessor) {
    SGM.emitFunction(accessor);
  });
}

void SILGenFunction::visitMacroExpansionDecl(MacroExpansionDecl *D) {
  D->forEachExpandedNode([&](ASTNode node) {
    if (auto *expr = node.dyn_cast<Expr *>())
      emitIgnoredExpr(expr);
    else if (auto *stmt = node.dyn_cast<Stmt *>())
      emitStmt(stmt);
    else
      visit(cast<Decl *>(node));
  });
}

/// Emit the boolean test and/or pattern bindings indicated by the specified
/// stmt condition.  If the condition fails, control flow is transferred to the
/// specified JumpDest.  The insertion point is left in the block where the
/// condition has matched and any bound variables are in scope.
///
void SILGenFunction::emitStmtCondition(StmtCondition Cond, JumpDest FalseDest,
                                       SILLocation loc,
                                       ProfileCounter NumTrueTaken,
                                       ProfileCounter NumFalseTaken) {

  assert(B.hasValidInsertionPoint() &&
         "emitting condition at unreachable point");
  
  for (const auto &elt : Cond) {
    SILLocation booleanTestLoc = loc;
    SILValue booleanTestValue;

    switch (elt.getKind()) {
    case StmtConditionElement::CK_PatternBinding: {
          // Begin a new binding scope, which is popped when the next innermost debug
          // scope ends. The cleanup location loc isn't the perfect source location
          // but it's close enough.
          B.getSILGenFunction().enterDebugScope(loc, /*isBindingScope=*/true);
        InitializationPtr initialization =
          emitPatternBindingInitialization(elt.getPattern(), FalseDest);

      // Emit the initial value into the initialization.
      FullExpr Scope(Cleanups, CleanupLocation(elt.getInitializer()));
      emitExprInto(elt.getInitializer(), initialization.get(), loc);
      // Pattern bindings handle their own tests, we don't need a boolean test.
      continue;
    }

    case StmtConditionElement::CK_Boolean: { // Handle boolean conditions.
      auto *expr = elt.getBoolean();
      // Evaluate the condition as an i1 value (guaranteed by Sema).
      FullExpr Scope(Cleanups, CleanupLocation(expr));
      booleanTestValue = emitRValue(expr).forwardAsSingleValue(*this, expr);
      booleanTestValue = emitUnwrapIntegerResult(expr, booleanTestValue);
      booleanTestLoc = expr;
      break;
    }

    case StmtConditionElement::CK_Availability: {
      booleanTestValue = emitIfAvailableQuery(loc, elt.getAvailability());
      break;
    }

    case StmtConditionElement::CK_HasSymbol: {
      auto info = elt.getHasSymbolInfo();
      if (info->isInvalid()) {
        // This condition may have referenced a decl that isn't valid in some
        // way but for developer convenience wasn't treated as an error. Just
        // emit a 'true' condition value.
        SILType i1 = SILType::getBuiltinIntegerType(1, getASTContext());
        booleanTestValue = B.createIntegerLiteral(loc, i1, 1);
        break;
      }

      auto expr = info->getSymbolExpr();
      auto declRef = info->getReferencedDecl();
      assert(declRef);

      auto decl = declRef.getDecl();
      booleanTestValue = B.createHasSymbol(expr, decl);
      booleanTestValue = emitUnwrapIntegerResult(expr, booleanTestValue);
      booleanTestLoc = expr;

      // Ensure that function declarations for each function associated with
      // the decl are emitted so that they can be referenced during IRGen.
      enumerateFunctionsForHasSymbol(
          getModule(), decl, [this](SILDeclRef declRef) {
            (void)SGM.getFunction(declRef, NotForDefinition);
          });

      break;
    }
    }

    // Now that we have a boolean test as a Builtin.i1, emit the branch.
    assert(booleanTestValue->getType().
           castTo<BuiltinIntegerType>()->isFixedWidth(1) &&
           "Sema forces conditions to have Builtin.i1 type");
    
    // Just branch on the condition.  On failure, we unwind any active cleanups,
    // on success we fall through to a new block.
    auto FailBB = Cleanups.emitBlockForCleanups(FalseDest, loc);
    SILBasicBlock *ContBB = createBasicBlock();
    B.createCondBranch(booleanTestLoc, booleanTestValue, ContBB, FailBB,
                       NumTrueTaken, NumFalseTaken);

    // Finally, emit the continue block and keep emitting the rest of the
    // condition.
    B.emitBlock(ContBB);
  }
}

InitializationPtr SILGenFunction::emitPatternBindingInitialization(
    Pattern *P, JumpDest failureDest, bool generateDebugInfo) {
  auto init =
      InitializationForPattern(*this, failureDest, generateDebugInfo).visit(P);
  init->setEmitDebugValueOnInit(generateDebugInfo);
  return init;
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
class EndLifetimeCleanup : public Cleanup {
  SILValue v;
public:
  EndLifetimeCleanup(SILValue v) : v(v) {}

  void emit(SILGenFunction &SGF, CleanupLocation l,
            ForUnwind_t forUnwind) override {
    SGF.B.createEndLifetime(l, v);
  }

  void dump(SILGenFunction &) const override {
#ifndef NDEBUG
    llvm::errs() << "EndLifetimeCleanup\n"
                 << "State:" << getState() << "\n"
                 << "Value:" << v << "\n";
#endif
  }
};
} // end anonymous namespace

ManagedValue SILGenFunction::emitManagedRValueWithEndLifetimeCleanup(
    SILValue value) {
  Cleanups.pushCleanup<EndLifetimeCleanup>(value);
  return ManagedValue::forUnmanagedOwnedValue(value);
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
    
    void emit(SILGenFunction &SGF, CleanupLocation l,
              ForUnwind_t forUnwind) override {
      switch (repr) {
      case ExistentialRepresentation::None:
      case ExistentialRepresentation::Class:
      case ExistentialRepresentation::Metatype:
        llvm_unreachable("cannot cleanup existential");
      case ExistentialRepresentation::Opaque:
        if (SGF.silConv.useLoweredAddresses()) {
          SGF.B.createDeinitExistentialAddr(l, existentialAddr);
        } else {
          SGF.B.createDeinitExistentialValue(l, existentialAddr);
        }
        break;
      case ExistentialRepresentation::Boxed:
        auto box = SGF.B.createLoad(l, existentialAddr,
                                    LoadOwnershipQualifier::Take);
        SGF.B.createDeallocExistentialBox(l, concreteFormalType, box);
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
                                               CleanupState state,
                                               SILValue addr,
                                               CanType concreteFormalType,
                                               ExistentialRepresentation repr) {
  assert(addr->getType().isAddress());
  Cleanups.pushCleanupInState<DeinitExistentialCleanup>(state, addr,
                                                      concreteFormalType, repr);
  return Cleanups.getTopCleanup();
}

namespace {
  /// A cleanup that cancels an asynchronous task.
  class CancelAsyncTaskCleanup: public Cleanup {
    SILValue task;
  public:
    CancelAsyncTaskCleanup(SILValue task) : task(task) { }

    void emit(SILGenFunction &SGF, CleanupLocation l,
              ForUnwind_t forUnwind) override {
      SILValue borrowedTask = SGF.B.createBeginBorrow(l, task);
      SGF.emitCancelAsyncTask(l, borrowedTask);
      SGF.B.createEndBorrow(l, borrowedTask);
    }

    void dump(SILGenFunction &) const override {
#ifndef NDEBUG
      llvm::errs() << "CancelAsyncTaskCleanup\n"
                   << "Task:" << task << "\n";
#endif
    }
  };
} // end anonymous namespace

CleanupHandle SILGenFunction::enterCancelAsyncTaskCleanup(SILValue task) {
  Cleanups.pushCleanupInState<CancelAsyncTaskCleanup>(
      CleanupState::Active, task);
  return Cleanups.getTopCleanup();
}

namespace {
/// A cleanup that destroys the AsyncLet along with the child task and record.
class AsyncLetCleanup: public Cleanup {
  SILValue alet;
  SILValue resultBuf;
public:
  AsyncLetCleanup(SILValue alet, SILValue resultBuf)
    : alet(alet), resultBuf(resultBuf) { }

  void emit(SILGenFunction &SGF, CleanupLocation l,
            ForUnwind_t forUnwind) override {
    SGF.emitFinishAsyncLet(l, alet, resultBuf);
  }

  void dump(SILGenFunction &) const override {
#ifndef NDEBUG
    llvm::errs() << "AsyncLetCleanup\n"
                 << "AsyncLet:" << alet << "\n"
                 << "result buffer:" << resultBuf << "\n";
#endif
  }
};
} // end anonymous namespace

CleanupHandle SILGenFunction::enterAsyncLetCleanup(SILValue alet,
                                                   SILValue resultBuf) {
  Cleanups.pushCleanupInState<AsyncLetCleanup>(
      CleanupState::Active, alet, resultBuf);
  return Cleanups.getTopCleanup();
}

/// Create a LocalVariableInitialization for the uninitialized var.
InitializationPtr SILGenFunction::emitLocalVariableWithCleanup(
    VarDecl *vd, std::optional<MarkUninitializedInst::Kind> kind,
    unsigned ArgNo, bool generateDebugInfo) {
  return InitializationPtr(new LocalVariableInitialization(
      vd, kind, ArgNo, generateDebugInfo, *this));
}

/// Create an Initialization for an uninitialized temporary.
TemporaryInitializationPtr
SILGenFunction::emitTemporary(SILLocation loc, const TypeLowering &tempTL) {
  SILValue addr = emitTemporaryAllocation(loc, tempTL.getLoweredType());
  if (addr->getType().isMoveOnly())
    addr = B.createMarkUnresolvedNonCopyableValueInst(
        loc, addr,
        MarkUnresolvedNonCopyableValueInst::CheckKind::ConsumableAndAssignable);
  return useBufferAsTemporary(addr, tempTL);
}

TemporaryInitializationPtr
SILGenFunction::emitFormalAccessTemporary(SILLocation loc,
                                          const TypeLowering &tempTL) {
  SILValue addr = emitTemporaryAllocation(loc, tempTL.getLoweredType());
  if (addr->getType().isMoveOnly())
    addr = B.createMarkUnresolvedNonCopyableValueInst(
        loc, addr,
        MarkUnresolvedNonCopyableValueInst::CheckKind::ConsumableAndAssignable);
  CleanupHandle cleanup =
      enterDormantFormalAccessTemporaryCleanup(addr, loc, tempTL);
  return TemporaryInitializationPtr(
      new TemporaryInitialization(addr, cleanup));
}

/// Create an Initialization for an uninitialized buffer.
TemporaryInitializationPtr
SILGenFunction::useBufferAsTemporary(SILValue addr,
                                     const TypeLowering &tempTL) {
  CleanupHandle cleanup = enterDormantTemporaryCleanup(addr, tempTL);
  return TemporaryInitializationPtr(
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

struct FormalAccessReleaseValueCleanup final : Cleanup {
  FormalEvaluationContext::stable_iterator Depth;

  FormalAccessReleaseValueCleanup() : Cleanup(), Depth() {
    setIsFormalAccess();
  }

  void setState(SILGenFunction &SGF, CleanupState newState) override {
    if (newState == CleanupState::Dead) {
      getEvaluation(SGF).setFinished();
    }

    Cleanup::setState(SGF, newState);
  }

  void emit(SILGenFunction &SGF, CleanupLocation l,
            ForUnwind_t forUnwind) override {
    getEvaluation(SGF).finish(SGF);
  }

  void dump(SILGenFunction &SGF) const override {
#ifndef NDEBUG
    llvm::errs() << "FormalAccessReleaseValueCleanup "
                 << "State:" << getState() << "\n"
                 << "Value:" << getValue(SGF) << "\n";
#endif
  }

  OwnedFormalAccess &getEvaluation(SILGenFunction &SGF) const {
    auto &evaluation = *SGF.FormalEvalContext.find(Depth);
    assert(evaluation.getKind() == FormalAccess::Owned);
    return static_cast<OwnedFormalAccess &>(evaluation);
  }

  SILValue getValue(SILGenFunction &SGF) const {
    return getEvaluation(SGF).getValue();
  }
};

} // end anonymous namespace

ManagedValue
SILGenFunction::emitFormalAccessManagedBufferWithCleanup(SILLocation loc,
                                                         SILValue addr) {
  assert(isInFormalEvaluationScope() && "Must be in formal evaluation scope");
  auto &lowering = getTypeLowering(addr->getType());
  if (lowering.isTrivial())
    return ManagedValue::forTrivialAddressRValue(addr);

  auto &cleanup = Cleanups.pushCleanup<FormalAccessReleaseValueCleanup>();
  CleanupHandle handle = Cleanups.getTopCleanup();
  FormalEvalContext.push<OwnedFormalAccess>(loc, handle, addr);
  cleanup.Depth = FormalEvalContext.stable_begin();
  return ManagedValue::forOwnedAddressRValue(addr, handle);
}

ManagedValue
SILGenFunction::emitFormalAccessManagedRValueWithCleanup(SILLocation loc,
                                                         SILValue value) {
  assert(isInFormalEvaluationScope() && "Must be in formal evaluation scope");
  auto &lowering = getTypeLowering(value->getType());
  if (lowering.isTrivial())
    return ManagedValue::forRValueWithoutOwnership(value);

  auto &cleanup = Cleanups.pushCleanup<FormalAccessReleaseValueCleanup>();
  CleanupHandle handle = Cleanups.getTopCleanup();
  FormalEvalContext.push<OwnedFormalAccess>(loc, handle, value);
  cleanup.Depth = FormalEvalContext.stable_begin();
  return ManagedValue::forOwnedRValue(value, handle);
}

CleanupHandle SILGenFunction::enterDormantFormalAccessTemporaryCleanup(
    SILValue addr, SILLocation loc, const TypeLowering &tempTL) {
  assert(isInFormalEvaluationScope() && "Must be in formal evaluation scope");
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

  auto emitEndBorrow = [this, silLoc](SILValue value){
    if (value->getOwnershipKind() == OwnershipKind::None) {
      B.createExtendLifetime(silLoc, value);
    } else {
      B.createEndBorrow(silLoc, value);
    }
  };

  auto emitDestroy = [this, silLoc](SILValue value){
    if (value->getOwnershipKind() == OwnershipKind::None) {
      B.createExtendLifetime(silLoc, value);
    } else {
      B.emitDestroyValueOperation(silLoc, value);
    }
  };

  // For a heap variable, the box is responsible for the value. We just need
  // to give up our retain count on it.
  if (auto boxValue = VarLocs[vd].box) {
    if (!getASTContext().SILOpts.supportsLexicalLifetimes(getModule())) {
      emitDestroy(boxValue);
      return;
    }

    if (auto *bbi = dyn_cast<BeginBorrowInst>(boxValue)) {
      emitEndBorrow(bbi);
      boxValue = bbi->getOperand();
    }
    emitDestroy(boxValue);
    return;
  }

  // For 'let' bindings, we emit a release_value or destroy_addr, depending on
  // whether we have an address or not.
  SILValue Val = VarLocs[vd].value;

  if (Val->getType().isAddress()) {
    B.createDestroyAddr(silLoc, Val);
    return;
  }

  if (!getASTContext().SILOpts.supportsLexicalLifetimes(getModule())) {
    emitDestroy(Val);
    return;
  }

  // If no variable scope was emitted, then we might not have a defining
  // instruction.
  if (!Val.getDefiningInstruction()) {
    emitDestroy(Val);
    return;
  }

  // This handles any case where we copy + begin_borrow or copyable_to_moveonly
  // + begin_borrow. In either case we just need to end the lifetime of the
  // begin_borrow's operand.
  if (auto *bbi = dyn_cast<BeginBorrowInst>(Val.getDefiningInstruction())) {
    emitEndBorrow(bbi);
    emitDestroy(bbi->getOperand());
    return;
  }

  if (auto *mvi = dyn_cast<MoveValueInst>(Val.getDefiningInstruction())) {
    emitDestroy(mvi);
    return;
  }

  // Handle BeginBorrow and MoveValue before bailing-out on trivial values.
  if (Val->getOwnershipKind() == OwnershipKind::None) {
    return;
  }

  if (auto *mark = dyn_cast<MarkUnresolvedNonCopyableValueInst>(
          Val.getDefiningInstruction())) {
    if (mark->hasMoveCheckerKind()) {
      if (auto *cvi = dyn_cast<CopyValueInst>(mark->getOperand())) {
        if (auto *bbi = dyn_cast<BeginBorrowInst>(cvi->getOperand())) {
          emitDestroy(mark);
          emitEndBorrow(bbi);
          emitDestroy(bbi->getOperand());
          return;
        }
      }

      if (auto *copyToMove = dyn_cast<CopyableToMoveOnlyWrapperValueInst>(
              mark->getOperand())) {
        if (copyToMove->getOperand()->isFromVarDecl()) {
          emitDestroy(mark);
          return;
        }
      }

      if (auto *cvi = dyn_cast<ExplicitCopyValueInst>(mark->getOperand())) {
        if (auto *bbi = dyn_cast<BeginBorrowInst>(cvi->getOperand())) {
          emitDestroy(mark);
          emitEndBorrow(bbi);
          emitDestroy(bbi->getOperand());
          return;
        }
      }

      // Handle trivial arguments.
      if (isa<MoveValueInst>(mark->getOperand())) {
        emitDestroy(mark);
        return;
      }
    }
  }  
  llvm_unreachable("unhandled case");
}

void
SILGenFunction::enterLocalVariableAddressableBufferScope(VarDecl *decl) {
  auto marker = B.createTuple(decl, {});
  AddressableBuffers[decl] = marker;
  Cleanups.pushCleanup<DeallocateLocalVariableAddressableBuffer>(decl);
}

SILValue
SILGenFunction::getLocalVariableAddressableBuffer(VarDecl *decl,
                                                  SILLocation curLoc,
                                                  ValueOwnership ownership) {
  auto foundVarLoc = VarLocs.find(decl);
  if (foundVarLoc == VarLocs.end()) {
    return SILValue();
  }
  
  auto value = foundVarLoc->second.value;
  auto access = foundVarLoc->second.access;
  auto *state = getAddressableBufferInfo(decl)->getState();
  
  SILType fullyAbstractedTy = getLoweredType(AbstractionPattern::getOpaque(),
                                     decl->getTypeInContext()->getRValueType());
  
  // Check whether the bound value is inherently suitable for addressability.
  // It must already be in memory and fully abstracted.
  if (value->getType().isAddress()
      && fullyAbstractedTy.getASTType() == value->getType().getASTType()) {
    SILValue address = value;
    // Begin an access if the address is mutable.
    if (access != SILAccessEnforcement::Unknown) {
      address = B.emitBeginAccess(curLoc, address,
        ownership == ValueOwnership::InOut ? SILAccessKind::Modify
                                           : SILAccessKind::Read,
        access);
    }
    return address;
  }
  
  // We can't retroactively introduce a reabstracted representation for a
  // mutable binding (since we would now have two mutable memory locations
  // representing the same value).
  if (access != SILAccessEnforcement::Unknown) {
    return SILValue();
  }
  
  assert(ownership == ValueOwnership::Shared);
  
  // Check whether the in-memory representation has already been forced.
  if (state) {
    return state->storeBorrow;
  }
  
  // Otherwise, force the addressable representation.
  SILValue reabstraction, allocStack, storeBorrow;
  {
    SavedInsertionPointRAII save(B);
    SILInstruction *insertPoint = nullptr;
    // Look through bindings that might alias the original addressable buffer
    // (such as case block variables, which use an alias variable to represent the
    // incoming value from all of the case label patterns).
    VarDecl *origDecl = decl;
    do {
      auto bufferIter = AddressableBuffers.find(origDecl);
      ASSERT(bufferIter != AddressableBuffers.end()
             && "local variable didn't have an addressability scope set");

      insertPoint = bufferIter->second.getInsertPoint();
      if (insertPoint) {
        break;
      }

      origDecl = bufferIter->second.getOriginalForAlias();
      ASSERT(origDecl && "no insert point or alias for addressable declaration!");
    } while (true);
      
    assert(insertPoint && "didn't find an insertion point for the addressable buffer");
    B.setInsertionPoint(insertPoint);
    auto allocStackTy = fullyAbstractedTy;
    if (value->getType().isMoveOnlyWrapped()) {
      allocStackTy = allocStackTy.addingMoveOnlyWrapper();
    }
    allocStack = B.createAllocStack(decl,
                                allocStackTy,
                                std::nullopt,
                                DoesNotHaveDynamicLifetime,
                                IsNotLexical,
                                IsNotFromVarDecl,
                                DoesNotUseMoveableValueDebugInfo,
                                /*skipVarDeclAssert*/ true);
  }
  {
    SavedInsertionPointRAII save(B);
    if (isa<ParamDecl>(decl)) {
      B.setInsertionPoint(allocStack->getNextInstruction());
    } else if (auto inst = value->getDefiningInstruction()) {
      B.setInsertionPoint(inst->getParent(), std::next(inst->getIterator()));
    } else if (auto arg = dyn_cast<SILArgument>(value)) {
      if (auto farg = dyn_cast<SILFunctionArgument>(value);
          farg && farg->isClosureCapture()) {
        B.setInsertionPoint(allocStack->getNextInstruction());
      } else {
        B.setInsertionPoint(arg->getParent()->begin());
      }
    } else {
      llvm_unreachable("unexpected value source!");
    }
    auto declarationLoc = value->getDefiningInsertionPoint()->getLoc();
    
    // Reabstract if necessary.
    auto newValue = value;
    reabstraction = SILValue();
    if (newValue->getType().getASTType() != fullyAbstractedTy.getASTType()){
      auto reabstracted = emitSubstToOrigValue(curLoc,
        ManagedValue::forBorrowedRValue(value),
        AbstractionPattern::getOpaque(),
        decl->getTypeInContext()->getCanonicalType(),
        SGFContext());
      reabstraction = reabstracted.forward(*this);
      newValue = reabstraction;
    }
    storeBorrow = B.createStoreBorrow(declarationLoc, newValue, allocStack);
  }
  
  // Record the addressable representation.
  auto *addressableBuffer = getAddressableBufferInfo(decl);
  auto *newState
    = new VarLoc::AddressableBuffer::State(reabstraction, allocStack, storeBorrow);
  addressableBuffer->stateOrAlias = newState;

  // Emit cleanups on any paths where we previously would have cleaned up
  // the addressable representation if it had been forced earlier.
  decltype(addressableBuffer->cleanupPoints) cleanupPoints;
  cleanupPoints.swap(addressableBuffer->cleanupPoints);
  
  for (SILInstruction *cleanupPoint : cleanupPoints) {
    SavedInsertionPointRAII insertCleanup(B, cleanupPoint);
    deallocateAddressable(*this, cleanupPoint->getLoc(), *newState);
    cleanupPoint->eraseFromParent();
  }
  
  return storeBorrow;
}

void BlackHoleInitialization::performPackExpansionInitialization(
                                        SILGenFunction &SGF,
                                        SILLocation loc,
                                        SILValue indexWithinComponent,
                          llvm::function_ref<void(Initialization *into)> fn) {
  BlackHoleInitialization subInit;
  fn(&subInit);
}

void BlackHoleInitialization::copyOrInitValueInto(SILGenFunction &SGF, SILLocation loc,
                                                  ManagedValue value, bool isInit) {
  // If we do not have a noncopyable type, just insert an ignored use.
  if (!value.getType().isMoveOnly()) {
    SGF.B.createIgnoredUse(loc, value.getValue());
    return;
  }

  // If we have a noncopyable type, we need to do a little more work to satisfy
  // the move checkers. If we have an address, then this will create a new
  // temporary allocation which will trigger the move checker...
  value = value.ensurePlusOne(SGF, loc);
  if (value.getType().isAddress()) {
    SGF.B.createIgnoredUse(loc, value.getValue());
    return;
  }

  // If we have an object though, we need to insert an extra move_value to make
  // sure the object checker behaves correctly.
  value = SGF.B.createMoveValue(loc, value);
  SGF.B.createIgnoredUse(loc, value.getValue());
}

SILGenFunction::VarLoc::AddressableBuffer::~AddressableBuffer() {
  for (auto cleanupPoint : cleanupPoints) {
    cleanupPoint->eraseFromParent();
  }
  if (auto state = stateOrAlias.dyn_cast<State*>()) {
    delete state;
  }
}
