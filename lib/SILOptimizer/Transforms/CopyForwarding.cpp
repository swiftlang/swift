//===--- CopyForwarding.cpp - Forward local copies from caller to callee --===//
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
// Eliminate local copies of either address-only or reference types.
//
// This opportunity frequently results from a calling convention that transfers
// object ownership from caller to callee. In this convention, the caller
// creates a local copy before passing it to the callee. If the original object
// is immediately destroyed after passing off the copy, then the copy was
// unnecessary. Removing the useless copy can be thought of as forwarding the
// original object directly to the call argument in place of the copy. Hence
// "copy forwarding".
//
// There are two classifications of types that copy forwarding applies to:
// address-only types and references.
//
// Useless copies of address-only types look like this:
//
// %copy = alloc_stack $T
// copy_addr %arg to [initialization] %copy : $*T
// %ret = apply %callee<T>(%copy) : $@convention(thin) <τ_0_0> (@in τ_0_0) -> ()
// dealloc_stack %copy : $*T
// destroy_addr %arg : $*T
//
// Eliminating the address-only copies eliminates a very expensive call to
// getGenericMetadata.
//
// Useless copies of references look like this:
//
// strong_retain %arg : $A
// %ret = apply %callee(%arg) : $@convention(thin) (@owned A) -> ()
// strong_release %arg : $A
//
// Eliminating the reference copies, avoids artificially bumping the refcount
// which could save a copy of all elements in a COW container.
//
// The actual analysis and optimization do not depend on the copy being linked
// to call arguments. Any obviously useless copy will be eliminated.
//
// TODO: Currently we only handle the address-only case, not the retain/release
// case.
//
// TODO: We should run this at -Onone even though it's not diagnostic.
//
// TODO: Currently we only handle cases in which one side of the copy is block
// local. Either:
// (1) Forward propagate: copy src -> dest; deinit(dest)
// (2) Backward propagate: init(src); copy src -> dest
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "copy-forwarding"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILOptimizer/Analysis/AliasAnalysis.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/Analysis/PostOrderAnalysis.h"
#include "swift/SILOptimizer/Analysis/RCIdentityAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CFG.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"

STATISTIC(NumCopyNRVO, "Number of copies removed via named return value opt.");
STATISTIC(NumCopyForward, "Number of copies removed via forward propagation");
STATISTIC(NumCopyBackward,
          "Number of copies removed via backward propagation");
STATISTIC(NumDeadTemp, "Number of copies removed from unused temporaries");

using namespace swift;

// Temporary debugging flag until this pass is better tested.
static llvm::cl::opt<bool> EnableCopyForwarding("enable-copyforwarding",
                                                llvm::cl::init(true));
static llvm::cl::opt<bool> EnableDestroyHoisting("enable-destroyhoisting",
                                                llvm::cl::init(true));

/// \return true if the given copy source value can only be accessed via the
/// given def (this def uniquely identifies the object).
///
/// (1) An "in" argument.
///     (inouts are also nonaliased, but won't be destroyed in scope)
///
/// (2) A local alloc_stack variable.
static bool isIdentifiedSourceValue(SILValue Def) {
  if (auto *Arg = dyn_cast<SILFunctionArgument>(Def)) {
    // Check that the argument is passed as an in type. This means there are
    // no aliases accessible within this function scope.
    SILArgumentConvention Conv =  Arg->getArgumentConvention();
    switch (Conv) {
    case SILArgumentConvention::Indirect_In:
    case SILArgumentConvention::Indirect_In_Guaranteed:
      return true;
    default:
      LLVM_DEBUG(llvm::dbgs() << "  Skipping Def: Not an @in argument!\n");
      return false;
    }
  }

  if (isa<AllocStackInst>(Def))
    return true;

  return false;
}

/// \return true if the given copy dest value can only be accessed via the given
/// def (this def uniquely identifies the object).
///
/// (1) An "out" or inout argument.
///
/// (2) A local alloc_stack variable.
static bool isIdentifiedDestValue(SILValue Def) {
  if (auto *Arg = dyn_cast<SILFunctionArgument>(Def)) {
    // Check that the argument is passed as an out type. This means there are
    // no aliases accessible within this function scope.
    SILArgumentConvention Conv =  Arg->getArgumentConvention();
    switch (Conv) {
    case SILArgumentConvention::Indirect_Inout:
    case SILArgumentConvention::Indirect_Out:
      return true;
    default:
      LLVM_DEBUG(llvm::dbgs() << "  Skipping Def: Not an @in argument!\n");
      return false;
    }
  }

  if (isa<AllocStackInst>(Def))
    return true;

  return false;
}

/// Return the parameter convention used by Apply to pass an argument
/// indirectly via Address.
///
/// Set Oper to the Apply operand that passes Address.
static SILArgumentConvention getAddressArgConvention(ApplyInst *Apply,
                                                     SILValue Address,
                                                     Operand *&Oper) {
  Oper = nullptr;
  auto Args = Apply->getArgumentOperands();
  for (auto ArgIdx : indices(Args)) {
    if (Args[ArgIdx].get() != Address)
      continue;

    assert(!Oper && "Address can only be passed once as an indirection.");
    Oper = &Args[ArgIdx];
#ifdef NDEBUG
    break;
#endif
  }
  assert(Oper && "Address value not passed as an argument to this call.");
  return ApplySite(Apply).getArgumentConvention(*Oper);
}

/// If the given instruction is a store, return the stored value.
static SILValue getStoredValue(SILInstruction *I) {
  switch (I->getKind()) {
#define NEVER_OR_SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  case SILInstructionKind::Store##Name##Inst:
#include "swift/AST/ReferenceStorage.def"
  case SILInstructionKind::StoreInst:
  case SILInstructionKind::StoreBorrowInst:
    return I->getOperand(0);
  default:
    return SILValue();
  }
}

//===----------------------------------------------------------------------===//
//                 Forward and backward copy propagation
//===----------------------------------------------------------------------===//

// Visitor for visitAddressUsers.
namespace {
class AddressUserVisitor {
public:
  virtual ~AddressUserVisitor() {}

  virtual bool visitNormalUse(SILInstruction *user) = 0;
  virtual bool visitTake(CopyAddrInst *copy) = 0;
  virtual bool visitDestroy(DestroyAddrInst *destroy) = 0;
  virtual bool visitDebugValue(DebugValueAddrInst *debugValue) = 0;
};
} // namespace

/// Gather all instructions that use the given `address`
///
/// "Normal" uses are a whitelisted set of uses that guarantees the address is
/// only used as if it refers to a single value and all uses are accounted for
/// (no address projections).
///
/// Takes are "copy_addr [take]"
///
/// Destroys are "destroy_addr"
/// -
///
/// If we are unable to find all uses, for example, because we don't look
/// through struct_element_addr, then return false.
///
/// The collected use points will be consulted during forward and backward
/// copy propagation.
///
/// \param ignoredUser will be ignored if it is is non-null.
static bool visitAddressUsers(SILValue address, SILInstruction *ignoredUser,
                              AddressUserVisitor &visitor) {
  for (Operand *use : address->getUses()) {
    SILInstruction *UserInst = use->getUser();
    if (UserInst == ignoredUser)
      continue;

    if (auto *Apply = dyn_cast<ApplyInst>(UserInst)) {
      /// A call to materializeForSet exposes an address within the parent
      /// object. However, we can rely on a subsequent mark_dependent
      /// instruction to take that object as an operand, causing it to escape
      /// for the purpose of this analysis.
      assert(Apply->getSubstCalleeConv()
                 .getSILArgumentConvention(use->getOperandNumber()
                                           - Apply->getArgumentOperandNumber())
                 .isIndirectConvention()
             && "copy_addr location should be passed indirect");
      if (!visitor.visitNormalUse(UserInst))
        return false;

      continue;
    }
    if (auto *CopyInst = dyn_cast<CopyAddrInst>(UserInst)) {
      if (CopyInst->getSrc() == use->get() && CopyInst->isTakeOfSrc()) {
        if (!visitor.visitTake(CopyInst))
          return false;
      } else {
        if (!visitor.visitNormalUse(CopyInst))
          return false;
      }
      continue;
    }
    if (auto *Destroy = dyn_cast<DestroyAddrInst>(UserInst)) {
      if (!visitor.visitDestroy(Destroy))
        return false;

      continue;
    }
    switch (UserInst->getKind()) {
    case SILInstructionKind::LoadInst:
      if (!visitor.visitNormalUse(UserInst))
        return false;

      break;
    case SILInstructionKind::ExistentialMetatypeInst:
    case SILInstructionKind::InjectEnumAddrInst:
    case SILInstructionKind::StoreInst:
      if (!visitor.visitNormalUse(UserInst))
        return false;

      break;
    case SILInstructionKind::DebugValueAddrInst:
      if (!visitor.visitDebugValue(cast<DebugValueAddrInst>(UserInst)))
        return false;

      break;
    case SILInstructionKind::DeallocStackInst:
      break;
    default:
      // Most likely one of:
      //   init_enum_data_addr
      //   open_existential_addr
      //   partial_apply
      //   struct_element_addr
      //   unchecked_take_enum_data_addr
      //
      // TODO: Peek through struct element users like COWArrayOpts.
      //
      // TODO: Attempt to analyze partial applies or run closure propagation
      // first.
      //
      // TODO: assert that this list is consistent with
      // isTransitiveEscapeInst().
      LLVM_DEBUG(llvm::dbgs() << "  Skipping copy: use exposes def"
                              << *UserInst);
      return false;
    }
  }
  return true;
}

namespace {
/// Analyze an instruction that operates on the Address of a forward propagated
/// value.
///
/// Set Oper to the operand that may be safely replaced by an address
/// pointing to an equivalent value. If UserInst cannot be analyzed, Oper is set
/// to nullptr.
///
/// Return true if the instruction destroys the value at Address.
///
/// This checks for the following cases of deinit:
/// - 'in' argument
/// - copy_addr [take] src
/// - copy_addr [!init] dest
/// - destroy_addr
/// - unchecked_take_enum_data_addr
///
/// The copy_addr [!init] case is special because the operand cannot simply be
/// replaced with a new address without causing that location to be
/// reinitialized (after being deinitialized). The caller must check for and
/// handle this case.
///
/// This returns false and sets Oper to a valid operand if the instruction is a
/// projection of the value at the given address. The assumption is that we
/// cannot deinitialize memory via projections.
class AnalyzeForwardUse
    : public SILInstructionVisitor<AnalyzeForwardUse, bool> {
public:
  SILValue Address;
  Operand *Oper;

  AnalyzeForwardUse(SILValue Address): Address(Address), Oper(nullptr) {}

  bool visitApplyInst(ApplyInst *Apply) {
    switch (getAddressArgConvention(Apply, Address, Oper)) {
    case SILArgumentConvention::Indirect_In:
      return true;
    case SILArgumentConvention::Indirect_In_Guaranteed:
    case SILArgumentConvention::Indirect_Inout:
    case SILArgumentConvention::Indirect_InoutAliasable:
      return false;
    default:
      llvm_unreachable("unexpected calling convention for copy_addr user");
    }
  }
  bool visitCopyAddrInst(CopyAddrInst *CopyInst) {
    if (CopyInst->getSrc() == Address) {
      Oper = &CopyInst->getAllOperands()[CopyAddrInst::Src];
      return CopyInst->isTakeOfSrc();
    }
    assert(!CopyInst->isInitializationOfDest() && "illegal reinitialization");
    Oper = &CopyInst->getAllOperands()[CopyAddrInst::Dest];
    return true;
  }
  bool visitStoreInst(StoreInst *Store) {
    llvm_unreachable("illegal reinitialization or store of an address");
  }
  bool visitDestroyAddrInst(DestroyAddrInst *UserInst) {
    Oper = &UserInst->getOperandRef();
    return true;
  }
  bool visitUncheckedTakeEnumDataAddrInst(
    UncheckedTakeEnumDataAddrInst *UserInst) {
    Oper = &UserInst->getOperandRef();
    return true;
  }
  bool visitExistentialMetatypeInst(ExistentialMetatypeInst *UserInst) {
    Oper = &UserInst->getOperandRef();
    return false;
  }
  bool visitLoadInst(LoadInst *UserInst) {
    Oper = &UserInst->getOperandRef();
    return false;
  }
  bool visitOpenExistentialAddrInst(OpenExistentialAddrInst *UserInst) {
    Oper = &UserInst->getOperandRef();
    return false;
  }
  bool visitStructElementAddrInst(StructElementAddrInst *UserInst) {
    Oper = &UserInst->getOperandRef();
    return false;
  }
  bool visitDebugValueAddrInst(DebugValueAddrInst *UserInst) {
    Oper = &UserInst->getOperandRef();
    return false;
  }
  bool visitInitEnumDataAddrInst(InitEnumDataAddrInst *UserInst) {
    llvm_unreachable("illegal reinitialization");
  }
  bool visitInjectEnumAddrInst(InjectEnumAddrInst *UserInst) {
    llvm_unreachable("illegal reinitialization");
  }
  bool visitSILInstruction(SILInstruction *UserInst) {
    return false;
  }
};

/// Analyze an instruction that operates on the Address of a backward propagated
/// value.
///
/// Set Oper to the operand that my be safely replaced by an address
/// pointing to an equivalent value. If UserInst cannot be analyzed, Oper is set
/// to nullptr.
///
/// Return true if the instruction initializes the value at Address.
///
/// We currently check for the following cases of init:
/// - 'out' argument
/// - copy_addr [init] dest
/// - copy_addr [!init] dest
/// - store
///
/// The copy_addr [!init] case is special because the operand cannot simply be
/// replaced with a new address without causing that location to be
/// deinitialized (before being initialized). The caller must check for and
/// handle this case.
///
/// This returns false and sets Oper to nullptr for projections of the value at
/// the given address. For example, init_enum_data_addr and struct_element_addr
/// may be part of a decoupled initialization sequence.
class AnalyzeBackwardUse
    : public SILInstructionVisitor<AnalyzeBackwardUse, bool> {
public:
  SILValue Address;
  Operand *Oper;

  AnalyzeBackwardUse(SILValue Address): Address(Address), Oper(nullptr) {}

  bool visitApplyInst(ApplyInst *Apply) {
    switch (getAddressArgConvention(Apply, Address, Oper)) {
    case SILArgumentConvention::Indirect_Out:
      return true;
    case SILArgumentConvention::Indirect_Inout:
    case SILArgumentConvention::Indirect_InoutAliasable:
    case SILArgumentConvention::Indirect_In_Guaranteed:
      return false;
    case SILArgumentConvention::Indirect_In:
      llvm_unreachable("copy_addr src destroyed without reinitialization");
    default:
      llvm_unreachable("unexpected calling convention for copy_addr user");
    }
  }
  bool visitCopyAddrInst(CopyAddrInst *CopyInst) {
    if (CopyInst->getDest() == Address) {
      Oper = &CopyInst->getAllOperands()[CopyAddrInst::Dest];
      return true;
    }
    Oper = &CopyInst->getAllOperands()[CopyAddrInst::Src];
    assert(!CopyInst->isTakeOfSrc() && "illegal deinitialization");
    return false;
  }
  bool visitStoreInst(StoreInst *Store) {
    Oper = &Store->getAllOperands()[StoreInst::Dest];
    assert(Oper->get() == Address && "illegal store of an address");
    return true;
  }
  bool visitExistentialMetatypeInst(ExistentialMetatypeInst *UserInst) {
    Oper = &UserInst->getOperandRef();
    return false;
  }
  bool visitInjectEnumAddrInst(InjectEnumAddrInst *UserInst) {
    Oper = &UserInst->getOperandRef();
    return false;
  }
  bool visitLoadInst(LoadInst *UserInst) {
    Oper = &UserInst->getOperandRef();
    return false;
  }
  bool visitOpenExistentialAddrInst(OpenExistentialAddrInst *UserInst) {
    Oper = &UserInst->getOperandRef();
    return false;
  }
  bool visitDestroyAddrInst(DestroyAddrInst *UserInst) {
    llvm_unreachable("illegal deinitialization");
  }
  bool visitUncheckedTakeEnumDataAddrInst(
    UncheckedTakeEnumDataAddrInst *UserInst) {
    llvm_unreachable("illegal deinitialization");
  }
  bool visitUncheckedRefCastAddrInst(
    UncheckedRefCastAddrInst *UserInst) {
    if (UserInst->getDest() == Address) {
      Oper = &UserInst->getAllOperands()[UncheckedRefCastAddrInst::Dest];
    }
    return true;
  }
  bool visitDebugValueAddrInst(DebugValueAddrInst *UserInst) {
    Oper = &UserInst->getOperandRef();
    return false;
  }
  bool visitSILInstruction(SILInstruction *UserInst) {
    return false;
  }
};

class CopyForwarding {
  // Per-function state.
  PostOrderAnalysis *PostOrder;
  DominanceAnalysis *DomAnalysis;
  RCIdentityAnalysis *RCIAnalysis;
  bool DoGlobalHoisting;
  bool HasChanged;
  bool HasChangedCFG;

  // --- Per copied-def state ---

  // Transient state for the current Def valid during forwardCopiesOf.
  SILValue CurrentDef;

  // Is the addressed defined by CurrentDef ever loaded from?
  // This indicates that lifetime of any transitively referenced objects lives
  // beyond the value's immediate uses.
  bool IsSrcLoadedFrom;

  // Does the address defined by CurrentDef have unrecognized uses of a
  // nontrivial value stored at its address?
  bool HasUnknownStoredValue;

  bool HasForwardedToCopy;
  SmallPtrSet<SILInstruction*, 16> SrcUserInsts;
  SmallPtrSet<DebugValueAddrInst*, 4> SrcDebugValueInsts;
  SmallVector<CopyAddrInst*, 4> TakePoints;
  SmallPtrSet<SILInstruction *, 16> StoredValueUserInsts;
  SmallVector<DestroyAddrInst*, 4> DestroyPoints;
  SmallPtrSet<SILBasicBlock*, 32> DeadInBlocks;

  // --- Per copy_addr state ---
  CopyAddrInst *CurrentCopy = nullptr;

  class CopySrcUserVisitor : public AddressUserVisitor {
    CopyForwarding &CPF;
  public:
    CopySrcUserVisitor(CopyForwarding &CPF) : CPF(CPF) {}

    virtual bool visitNormalUse(SILInstruction *user) {
      if (isa<LoadInst>(user))
        CPF.IsSrcLoadedFrom = true;

      if (SILValue storedValue = getStoredValue(user)) {
        if (!CPF.markStoredValueUsers(storedValue))
          CPF.HasUnknownStoredValue = true;
      }

      // Bail on multiple uses in the same instruction to avoid complexity.
      return CPF.SrcUserInsts.insert(user).second;
    }
    virtual bool visitTake(CopyAddrInst *take) {
      if (take->getSrc() == take->getDest())
        return false;

      CPF.TakePoints.push_back(take);
      return true;
    }
    virtual bool visitDestroy(DestroyAddrInst *destroy) {
      CPF.DestroyPoints.push_back(destroy);
      return true;
    }
    virtual bool visitDebugValue(DebugValueAddrInst *debugValue) {
      return CPF.SrcDebugValueInsts.insert(debugValue).second;
    }
  };

public:
  CopyForwarding(PostOrderAnalysis *PO, DominanceAnalysis *DA,
                 RCIdentityAnalysis *RCIAnalysis)
    : PostOrder(PO), DomAnalysis(DA), RCIAnalysis(RCIAnalysis),
      DoGlobalHoisting(false), HasChanged(false), HasChangedCFG(false),
      IsSrcLoadedFrom(false), HasUnknownStoredValue(false),
      HasForwardedToCopy(false), CurrentCopy(nullptr) {}

  void reset(SILFunction *F) {
    // Don't hoist destroy_addr globally in transparent functions. Avoid cloning
    // destroy_addr instructions and splitting critical edges before mandatory
    // diagnostic passes. For example, PredictableMemOps can no longer remove
    // some alloc_stack cases after global destroy hoisting. CopyForwarding will
    // be reapplied after the transparent function is inlined at which point
    // global hoisting will be done.
    DoGlobalHoisting = !F->isTransparent();
    if (HasChangedCFG) {
      // We are only invalidating the analysis that we use internally.
      // We'll invalidate the analysis that are used by other passes at the end.
      DomAnalysis->invalidate(F, SILAnalysis::InvalidationKind::Everything);
      PostOrder->invalidate(F, SILAnalysis::InvalidationKind::Everything);
      RCIAnalysis->invalidate(F, SILAnalysis::InvalidationKind::Everything);
    }
    CurrentDef = SILValue();
    IsSrcLoadedFrom = false;
    HasUnknownStoredValue = false;
    HasForwardedToCopy = false;
    SrcUserInsts.clear();
    SrcDebugValueInsts.clear();
    TakePoints.clear();
    StoredValueUserInsts.clear();
    DestroyPoints.clear();
    DeadInBlocks.clear();
    CurrentCopy = nullptr;
  }

  bool hasChanged() const { return HasChanged; }
  bool hasChangedCFG() const { return HasChangedCFG; }

  /// Return true if CurrentDef has been forwarded through one copy into
  /// another. This means we should iterate.
  bool hasForwardedToCopy() const { return HasForwardedToCopy; }

  void forwardCopiesOf(SILValue Def, SILFunction *F);

protected:
  bool propagateCopy(CopyAddrInst *CopyInst, bool hoistingDestroy);
  CopyAddrInst *findCopyIntoDeadTemp(CopyAddrInst *destCopy);
  bool forwardDeadTempCopy(CopyAddrInst *srcCopy, CopyAddrInst *destCopy);
  bool forwardPropagateCopy();
  bool backwardPropagateCopy();
  bool hoistDestroy(SILInstruction *DestroyPoint, SILLocation DestroyLoc);

  bool isSourceDeadAtCopy();

  typedef llvm::SmallSetVector<SILInstruction *, 16> UserVector;
  bool doesCopyDominateDestUsers(const UserVector &DirectDestUses);

  bool markStoredValueUsers(SILValue storedValue);
};

class CopyDestUserVisitor : public AddressUserVisitor {
  SmallPtrSetImpl<SILInstruction *> &DestUsers;

public:
  CopyDestUserVisitor(SmallPtrSetImpl<SILInstruction *> &DestUsers)
      : DestUsers(DestUsers) {}

  virtual bool visitNormalUse(SILInstruction *user) {
    // Bail on multiple uses in the same instruction to avoid complexity.
    return DestUsers.insert(user).second;
  }
  virtual bool visitTake(CopyAddrInst *take) {
    return DestUsers.insert(take).second;
  }
  virtual bool visitDestroy(DestroyAddrInst *destroy) {
    return DestUsers.insert(destroy).second;
  }
  virtual bool visitDebugValue(DebugValueAddrInst *debugValue) {
    return DestUsers.insert(debugValue).second;
  }
};
} // end anonymous namespace

/// Attempt to forward, then backward propagate this copy.
///
/// The caller has already proven that lifetime of the value being copied ends
/// at the copy. (Either it is a [take] or is immediately destroyed).
/// 
///
/// If the forwarded copy is not an [init], then insert a destroy of the copy's
/// dest.
bool CopyForwarding::
propagateCopy(CopyAddrInst *CopyInst, bool hoistingDestroy) {
  if (!EnableCopyForwarding)
    return false;

  // CopyForwarding should be split into per-def-state vs. per-copy-state, but
  // this hack is good enough for a pass that's going away "soon".
  struct RAIISetCurrentCopy {
    CopyAddrInst *&CurrentCopy;

    RAIISetCurrentCopy(CopyAddrInst *&CurrentCopy, CopyAddrInst *CopyInst)
      : CurrentCopy(CurrentCopy) {
      assert(!CurrentCopy);
      CurrentCopy = CopyInst;
    }
    ~RAIISetCurrentCopy() {
      CurrentCopy = nullptr;
    }
  };
  RAIISetCurrentCopy setCurrentCopy(CurrentCopy, CopyInst);

  // Handle copy-of-copy without analyzing uses.
  // Assumes that CurrentCopy->getSrc() is dead after CurrentCopy.
  assert(CurrentCopy->isTakeOfSrc() || hoistingDestroy);
  if (auto *srcCopy = findCopyIntoDeadTemp(CurrentCopy)) {
    if (forwardDeadTempCopy(srcCopy, CurrentCopy)) {
      HasChanged = true;
      ++NumDeadTemp;
      return true;
    }
  }

  if (forwardPropagateCopy()) {
    LLVM_DEBUG(llvm::dbgs() << "  Forwarding Copy:" << *CurrentCopy);
    if (!CurrentCopy->isInitializationOfDest()) {
      // Replace the original copy with a destroy. We may be able to hoist it
      // more in another pass but don't currently iterate.
      SILBuilderWithScope(CurrentCopy)
          .createDestroyAddr(CurrentCopy->getLoc(), CurrentCopy->getDest());
    }
    CurrentCopy->eraseFromParent();
    HasChanged = true;
    ++NumCopyForward;
    return true;
  }
  // Forward propagation failed. Attempt to backward propagate.
  if (CurrentCopy->isInitializationOfDest() && backwardPropagateCopy()) {
    LLVM_DEBUG(llvm::dbgs() << "  Reversing Copy:" << *CurrentCopy);
    CurrentCopy->eraseFromParent();
    HasChanged = true;
    ++NumCopyBackward;
    return true;
  }
  return false;
}

/// Find a copy into an otherwise dead temporary:
///
/// The given copy is copying out of the temporary
/// copy_addr %temp, %dest
///
/// Precondition: The lifetime of %temp ends at `destCopy`
/// (%temp is CurrentDef).
///
/// Find a previous copy:
/// copy_addr %src, %temp
///
/// Such that it is safe to forward its source into the source of
/// `destCopy`. i.e. `destCopy` can be safely rewritten as:
/// copy_addr %src, %dest
///
/// Otherwise return nullptr. No instructions are harmed in this analysis.
///
/// This can be checked with a simple instruction walk that ends at:
/// - an intervening instruction that may write to memory
/// - a use of the temporary, %temp
///
/// Unlike the forward and backward propagation that finds all use points, this
/// handles copies of address projections. By conservatively checking all
/// intervening instructions, it avoids the need to analyze projection paths.
CopyAddrInst *CopyForwarding::findCopyIntoDeadTemp(CopyAddrInst *destCopy) {
  auto tmpVal = destCopy->getSrc();
  assert(tmpVal == CurrentDef);
  assert(isIdentifiedSourceValue(tmpVal));

  for (auto II = destCopy->getIterator(), IB = destCopy->getParent()->begin();
       II != IB;) {
    --II;
    SILInstruction *UserInst = &*II;
    if (auto *srcCopy = dyn_cast<CopyAddrInst>(UserInst)) {
      if (srcCopy->getDest() == tmpVal)
        return srcCopy;
    }
    if (SrcUserInsts.count(UserInst))
      return nullptr;
    if (UserInst->mayWriteToMemory())
      return nullptr;
  }
  return nullptr;
}

/// Forward a copy into a dead temporary as identified by
/// `findCopyIntoDeadTemp`.
///
/// Returns true if the copy was successfully forwarded.
///
/// Old SIL: 
/// copy_addr %src, %temp
/// copy_addr %temp, %dest
///
/// New SIL: 
/// copy_addr %src, %dest
///
/// Precondition: `srcCopy->getDest()` == `destCopy->getSrc()`
/// Precondition: %src is unused between srcCopy and destCopy.
/// Precondition: The lifetime of %temp ends immediate after `destCopy`.
/// 
/// Postcondition:
/// - `srcCopy` is erased.
/// - Any initial value in %temp is destroyed at `srcCopy` position.
/// - %temp is uninitialized following `srcCopy` and subsequent instruction
///   attempts to destroy this uninitialized value.
bool CopyForwarding::
forwardDeadTempCopy(CopyAddrInst *srcCopy, CopyAddrInst *destCopy) {
  LLVM_DEBUG(llvm::dbgs() << "  Temp Copy:" << *srcCopy
                          << "         to " << *destCopy);

  assert(srcCopy->getDest() == destCopy->getSrc());
  
  // This pattern can be trivially folded without affecting %temp destroys:
  // copy_addr [...] %src, [init] %temp
  // copy_addr [take] %temp, [...] %dest

  // If copy into temp is not initializing, add a destroy:
  // - copy_addr %src, %temp
  // + destroy %temp
  if (!srcCopy->isInitializationOfDest()) {
    SILBuilderWithScope(srcCopy)
      .createDestroyAddr(srcCopy->getLoc(), srcCopy->getDest());
  }

  // Either `destCopy` is a take, or the caller is hoisting a destroy:
  // copy_addr %temp, %dest
  // ...
  // destroy %temp
  //
  // If the caller is hoisting a destroy, and we return `true` then it will
  // erase the destroy for us. Either way, it's safe to simply rewrite destCopy.
  // For now, don't bother finding the subsequent destroy, because this isn't
  // the common case.

  destCopy->setSrc(srcCopy->getSrc());
  destCopy->setIsTakeOfSrc(srcCopy->isTakeOfSrc());
  srcCopy->eraseFromParent();
  return true;
}

/// Check that the lifetime of %src ends at the copy and is not reinitialized
/// thereafter with a new value.
bool CopyForwarding::isSourceDeadAtCopy() {
  // A single copy_addr [take] %Src.
  if (TakePoints.size() == 1 && DestroyPoints.empty() && SrcUserInsts.empty())
    return true;

  if (TakePoints.empty() && DestroyPoints.size() == 1 &&
      SrcUserInsts.size() == 1) {
    assert(*SrcUserInsts.begin() == CurrentCopy);
    return true;
  }
  // For now just check for a single copy_addr that destroys its source.
  return false;
}

/// Check that all immediate users of the destination address of the copy are
/// dominated by the copy. There is no path around copy that could initialize
/// %dest with a different value.
bool CopyForwarding::doesCopyDominateDestUsers(
    const UserVector &DirectDestUsers) {
  DominanceInfo *DT = DomAnalysis->get(CurrentCopy->getFunction());
  for (auto *user : DirectDestUsers) {
    // Check dominance of the parent blocks.
    if (!DT->properlyDominates(CurrentCopy, user))
      return false;
  }
  return true;
}

// Add all recognized users of storedValue to StoredValueUserInsts. Return true
// if all users were recgonized.
//
// To find all SSA users of storedValue, we first find the RC root, then search
// past any instructions that may propagate the reference.
bool CopyForwarding::markStoredValueUsers(SILValue storedValue) {
  auto *F = storedValue->getFunction();

  if (storedValue->getType().isTrivial(*F))
    return true;

  // Find the RC root, peeking past things like struct_extract.
  RCIdentityFunctionInfo *RCI = RCIAnalysis->get(F);
  SILValue root = RCI->getRCIdentityRoot(storedValue);

  SmallVector<SILInstruction *, 8> users;
  RCI->getRCUsers(root, users);

  for (SILInstruction *user : users) {
    // Recognize any uses that have no results as normal uses. They cannot
    // transitively propagate a reference.
    if (user->getResults().empty()) {
      StoredValueUserInsts.insert(user);
      continue;
    }
    // Recognize full applies as normal uses. They may transitively retain, but
    // the caller cannot rely on that.
    if (FullApplySite::isa(user)) {
      StoredValueUserInsts.insert(user);
      continue;
    }
    // A single-valued use is nontransitive if its result is trivial.
    if (auto *SVI = dyn_cast<SingleValueInstruction>(user)) {
      if (SVI->getType().isTrivial(*F)) {
        StoredValueUserInsts.insert(user);
        continue;
      }
    }
    // Conservatively treat everything else as potentially transitively
    // retaining the stored value.
    LLVM_DEBUG(llvm::dbgs() << "  Cannot reduce lifetime. May retain "
                            << storedValue
                            << " at: " << *user << "\n");
    return false;
  }
  return true;
}

/// Returns the associated dealloc_stack if \p ASI has a single dealloc_stack.
/// Usually this is the case, but the optimizations may generate something like:
/// %1 = alloc_stack
/// if (...) {
///   dealloc_stack %1
/// } else {
///   dealloc_stack %1
/// }
static DeallocStackInst *getSingleDealloc(AllocStackInst *ASI) {
  return ASI->getSingleDeallocStack();
}

/// Perform forward copy-propagation. Find a set of uses that the given copy can
/// forward to and replace them with the copy's source.
///
/// We must only replace uses of this copy's value. To do this, we search
/// forward in the current block from the copy that initializes the value to the
/// point of deinitialization. Typically, this will be a point at which the
/// value is passed as an 'in' argument:
/// \code
/// %copy = alloc_stack $T
/// ...
/// CurrentBlock:
/// copy_addr %arg to [initialization] %copy : $*T
/// ...
/// %ret = apply %callee<T>(%copy) : $@convention(thin) <τ_0_0> (@in τ_0_0) -> ()
/// \endcode
///
/// If the last use (deinit) is a copy, replace it with a destroy+copy[init].
///
/// The caller has already guaranteed that the lifetime of the copy's source
/// ends at this copy. Either the copy is a [take] or a destroy can be hoisted
/// to the copy.
bool CopyForwarding::forwardPropagateCopy() {

  SILValue CopyDest = CurrentCopy->getDest();
  // Require the copy dest to be a simple alloc_stack. This ensures that all
  // instructions that may read from the destination address depend on CopyDest.
  if (!isa<AllocStackInst>(CopyDest))
    return false;

  // Record all direct dest uses. Forward propagation doesn't care if they are
  // projections or propagate the address in any way--their operand only needs
  // to be substituted with the copy's source.
  UserVector DirectDestUsers;
  for (auto *Use : CopyDest->getUses()) {
    auto *UserInst = Use->getUser();
    if (UserInst == CurrentCopy)
      continue;

    if (isa<DeallocStackInst>(UserInst))
      continue;

    // Bail on multiple uses in the same instruction so that AnalyzeForwardUse
    // does not need to deal with it.
    if (!DirectDestUsers.insert(UserInst))
      return false;
  }
  // Looking at
  //
  //    copy_addr %Src, [init] %Dst
  //
  // We can reuse %Src if it is dead after the copy and not reinitialized. To
  // know that we can safely replace all uses of %Dst with source we must know
  // that it is uniquely named and cannot be accessed outside of the function
  // (an alloc_stack instruction qualifies for this, an inout parameter does
  // not).  Additionally, we must know that all accesses to %Dst further on must
  // have had this copy on their path (there might be reinitialization of %Dst
  // later, but there must not be a path around this copy that reads from %Dst).
  if (isSourceDeadAtCopy() && doesCopyDominateDestUsers(DirectDestUsers)) {
    SILValue CopySrc = CurrentCopy->getSrc();
    // Replace all uses of Dest with a use of Src.
    for (SILInstruction *user : DirectDestUsers) {
      for (Operand &oper : user->getAllOperands()) {
        if (oper.get() != CopyDest)
          continue;

        // Rewrite both read and writes of CopyDest as CopySrc.
        oper.set(CopySrc);
      }
      if (isa<CopyAddrInst>(user))
        HasForwardedToCopy = true;
    }
    // The caller will Remove the destroy_addr of %src.
    assert((DestroyPoints.empty() ||
            (!CurrentCopy->isTakeOfSrc() && DestroyPoints.size() == 1)) &&
           "Must only have one destroy");

    // The caller will remove the copy_addr.
    return true;
  }

  SILInstruction *DefDealloc = nullptr;
  if (auto *ASI = dyn_cast<AllocStackInst>(CurrentDef)) {
    DefDealloc = getSingleDealloc(ASI);
    if (!DefDealloc) {
      LLVM_DEBUG(llvm::dbgs() << "  Skipping copy" << *CurrentCopy
                              << "  stack address has multiple uses.\n");
      return false;
    }
  }

  // Scan forward recording all operands that use CopyDest until we see the
  // next deinit of CopyDest.
  SmallVector<Operand*, 16> ValueUses;
  auto SI = CurrentCopy->getIterator(), SE = CurrentCopy->getParent()->end();
  for (++SI; SI != SE; ++SI) {
    SILInstruction *UserInst = &*SI;
    // If we see another use of Src, then the source location is reinitialized
    // before the Dest location is deinitialized. So we really need the copy.
    if (SrcUserInsts.count(UserInst)) {
      LLVM_DEBUG(llvm::dbgs() << "  Skipping copy" << *CurrentCopy
                              << "  source used by" << *UserInst);
      return false;
    }
    if (UserInst == DefDealloc) {
      LLVM_DEBUG(llvm::dbgs() << "  Skipping copy" << *CurrentCopy
                              << "    dealloc_stack before dest use.\n");
      return false;
    }
    // Early check to avoid scanning unrelated instructions.
    if (!DirectDestUsers.count(UserInst))
      continue;

    AnalyzeForwardUse AnalyzeUse(CopyDest);
    bool seenDeinit = AnalyzeUse.visit(UserInst);
    // If this use cannot be analyzed, then abort.
    if (!AnalyzeUse.Oper)
      return false;
    // Otherwise record the operand.
    ValueUses.push_back(AnalyzeUse.Oper);
    // If this is a deinit, we're done searching.
    if (seenDeinit)
      break;
  }
  if (SI == SE)
    return false;

  // Convert a reinitialization of this address into a destroy, followed by an
  // initialization. Replacing a copy with a destroy+init is not by itself
  // profitable. However, it does allow eliminating the earlier copy, and we may
  // later be able to eliminate this initialization copy.
  if (auto Copy = dyn_cast<CopyAddrInst>(&*SI)) {
    if (Copy->getDest() == CopyDest) {
      assert(!Copy->isInitializationOfDest() && "expected a deinit");

      DestroyAddrInst *Destroy =
          SILBuilderWithScope(Copy).createDestroyAddr(Copy->getLoc(), CopyDest);
      Copy->setIsInitializationOfDest(IsInitialization);

      assert(ValueUses.back()->getUser() == Copy && "bad value use");
      ValueUses.back() = &Destroy->getOperandRef();
    }
  }
  // Now that a deinit was found, it is safe to substitute all recorded uses
  // with the copy's source.
  for (auto *Oper : ValueUses) {
    Oper->set(CurrentCopy->getSrc());
    if (isa<CopyAddrInst>(Oper->getUser()))
      HasForwardedToCopy = true;
  }
  return true;
}

/// Given an address defined by 'Def', find the object root and all direct uses,
/// not including:
/// - 'Def' itself
/// - Transitive uses of 'Def' (listed elsewhere in DestUserInsts)
///
/// i.e. If Def is returned directly, RootUserInsts will be empty.
///
/// Return nullptr when the root != Def, and root has unrecognized uses.
/// 
/// If the returned root is not 'Def' itself, then 'Def' must be an address
/// projection that can be trivially rematerialized with the root as its
/// operand.
static ValueBase *
findAddressRootAndUsers(ValueBase *Def,
                        SmallPtrSetImpl<SILInstruction*> &RootUserInsts) {
  switch (Def->getKind()) {
  default:
    return Def;
  case ValueKind::InitEnumDataAddrInst:
  case ValueKind::InitExistentialAddrInst:
    auto InitInst = cast<SingleValueInstruction>(Def);
    SILValue InitRoot = InitInst->getOperand(0);

    CopyDestUserVisitor visitor(RootUserInsts);
    if (!visitAddressUsers(InitRoot, InitInst, visitor))
      return nullptr;
    return InitRoot;
  }
}

/// Perform backward copy-propagation. Find the initialization point of the
/// copy's source and replace the initializer's address with the copy's dest.
bool CopyForwarding::backwardPropagateCopy() {

  SILValue CopySrc = CurrentCopy->getSrc();
  ValueBase *CopyDestDef = CurrentCopy->getDest();

  SmallPtrSet<SILInstruction *, 16> DestUserInsts;
  CopyDestUserVisitor visitor(DestUserInsts);
  if (!visitAddressUsers(CopyDestDef, CurrentCopy, visitor))
    return false;

  // RootUserInsts will contain any users of the same object not covered by
  // DestUserInsts.
  SmallPtrSet<SILInstruction*, 8> RootUserInsts;
  ValueBase *CopyDestRoot = findAddressRootAndUsers(CopyDestDef, RootUserInsts);
  if (!CopyDestRoot)
    return false;

  // Require the copy dest value to be identified by this address. This ensures
  // that all instructions that may write to destination address depend on
  // CopyDestRoot.
  if (!isIdentifiedDestValue(CopyDestRoot))
    return false;

  // Scan backward recording all operands that use CopySrc until we see the
  // most recent init of CopySrc.
  bool seenInit = false;
  bool seenCopyDestDef = false;
  // ValueUses records the uses of CopySrc in reverse order.
  SmallVector<Operand*, 16> ValueUses;
  SmallVector<DebugValueAddrInst*, 4> DebugValueInstsToDelete;
  auto SI = CurrentCopy->getIterator(), SE = CurrentCopy->getParent()->begin();
  while (SI != SE) {
    --SI;
    SILInstruction *UserInst = &*SI;
    if (UserInst == CopyDestDef->getDefiningInstruction())
      seenCopyDestDef = true;

    // If we see another use of Dest, then Dest is live after the Src location
    // is initialized, so we really need the copy.
    if (UserInst == CopyDestRoot->getDefiningInstruction()
        || DestUserInsts.count(UserInst)
        || RootUserInsts.count(UserInst)) {
      if (auto *DVAI = dyn_cast<DebugValueAddrInst>(UserInst)) {
        DebugValueInstsToDelete.push_back(DVAI);
        continue;
      }
      LLVM_DEBUG(llvm::dbgs() << "  Skipping copy" << *CurrentCopy
                              << "    dest used by " << *UserInst);
      return false;
    }
    // Early check to avoid scanning unrelated instructions.
    if (!SrcUserInsts.count(UserInst)
        && !(isa<DebugValueAddrInst>(UserInst)
             && SrcDebugValueInsts.count(cast<DebugValueAddrInst>(UserInst))))
      continue;

    AnalyzeBackwardUse AnalyzeUse(CopySrc);
    seenInit = AnalyzeUse.visit(UserInst);
    // If this use cannot be analyzed, then abort.
    if (!AnalyzeUse.Oper)
      return false;
    // Otherwise record the operand with the earliest use last in the list.
    ValueUses.push_back(AnalyzeUse.Oper);
    // If this is an init, we're done searching.
    if (seenInit)
      break;
  }
  if (!seenInit)
    return false;

  for (auto *DVAI : DebugValueInstsToDelete)
    DVAI->eraseFromParent();
  
  // Convert a reinitialization of this address into a destroy, followed by an
  // initialization. Replacing a copy with a destroy+init is not by itself
  // profitable. However, it does allow us to eliminate the later copy, and the
  // init copy may be eliminated later.
  if (auto Copy = dyn_cast<CopyAddrInst>(&*SI)) {
    if (Copy->getDest() == CopySrc && !Copy->isInitializationOfDest()) {
      SILBuilderWithScope(Copy).createDestroyAddr(Copy->getLoc(), CopySrc);
      Copy->setIsInitializationOfDest(IsInitialization);
    }
  }
  // Rematerialize the projection if needed by simply moving it.
  if (seenCopyDestDef) {
    CopyDestDef->getDefiningInstruction()->moveBefore(&*SI);
  }
  // Now that an init was found, it is safe to substitute all recorded uses
  // with the copy's dest.
  for (auto *Oper : ValueUses) {
    Oper->set(CurrentCopy->getDest());
    if (isa<CopyAddrInst>(Oper->getUser()))
      HasForwardedToCopy = true;
  }
  return true;
}

/// Attempt to hoist a destroy point up to the last use. If the last use is a
/// copy, eliminate both the copy and the destroy.
///
/// The copy will be eliminated if the original is not accessed between the
/// point of copy and the original's destruction.
///
/// CurrentDef = <uniquely identified> // no aliases
/// ...
/// Copy = copy_addr [init] Def
/// ...                    // no access to CurrentDef
/// destroy_addr Def
///
/// Return true if a destroy was inserted, forwarded from a copy, or the
/// block was marked dead-in.
bool CopyForwarding::hoistDestroy(SILInstruction *DestroyPoint,
                                  SILLocation DestroyLoc) {
  if (!EnableDestroyHoisting)
    return false;

  assert(!SrcUserInsts.count(DestroyPoint) && "caller should check terminator");
  SILBasicBlock *BB = DestroyPoint->getParent();

  // If DestroyPoint is a block terminator, we must hoist.
  bool MustHoist = (DestroyPoint == BB->getTerminator());
  // If we haven't seen anything significant, avoid useless hoisting.
  bool ShouldHoist = MustHoist;

  auto tryToInsertHoistedDestroyAfter = [&](SILInstruction *afterInst) {
    if (!ShouldHoist)
      return false;
    LLVM_DEBUG(llvm::dbgs() << "  Hoisting to Use:" << *afterInst);
    SILBuilderWithScope(std::next(afterInst->getIterator()), afterInst)
        .createDestroyAddr(DestroyLoc, CurrentDef);
    HasChanged = true;
    return true;
  };

  auto SI = DestroyPoint->getIterator(), SE = BB->begin();
  while (SI != SE) {
    --SI;
    SILInstruction *Inst = &*SI;
    if (!SrcUserInsts.count(Inst)) {
      if (StoredValueUserInsts.count(Inst)) {
        // The current definition may take ownership of a value stored into its
        // address. Its lifetime cannot end before the last use of that stored
        // value.
        // CurrentDef = ...
        // Copy = copy_addr CurrentDef to ...
        // store StoredValue to CurrentDef
        // ...                    // no access to CurrentDef
        // retain StoredValue
        // destroy_addr CurrentDef
        LLVM_DEBUG(llvm::dbgs() << "  Cannot hoist above stored value use:"
                                << *Inst);
        return tryToInsertHoistedDestroyAfter(Inst);
      }
      if (!ShouldHoist && isa<ApplyInst>(Inst))
        ShouldHoist = true;
      continue;
    }
    if (auto *CopyInst = dyn_cast<CopyAddrInst>(Inst)) {
      if (!CopyInst->isTakeOfSrc() && CopyInst->getSrc() == CurrentDef) {
        // This use is a copy of CurrentDef. Attempt to forward CurrentDef to
        // all uses of the copy's value.
        if (propagateCopy(CopyInst, /*hoistingDestroy=*/true))
          return true;
      }
    }
    return tryToInsertHoistedDestroyAfter(Inst);
  }
  if (!DoGlobalHoisting) {
    // If DoGlobalHoisting is set, then we should never mark a DeadInBlock, so
    // MustHoist should be false.
    assert(!MustHoist &&
           "Cannot hoist above a terminator with global hoisting disabled.");
    return false;
  }
  DeadInBlocks.insert(BB);
  return true;
}

/// Perform CopyForwarding on the current Def.
void CopyForwarding::forwardCopiesOf(SILValue Def, SILFunction *F) {
  reset(F);
  CurrentDef = Def;
  LLVM_DEBUG(llvm::dbgs() << "Analyzing copies of Def: " << Def);
  CopySrcUserVisitor visitor(*this);
  if (!visitAddressUsers(Def, nullptr, visitor))
    return;

  // First forward any copies that implicitly destroy CurrentDef. There is no
  // need to hoist Destroy for these.
  for (auto *CopyInst : TakePoints) {
    propagateCopy(CopyInst, /*hoistingDestroy=*/false);
  }
  // If the copied address is also loaded from, then destroy hoisting is unsafe.
  //
  // TODO: Record all loads during collectUsers. Implement findRetainPoints to
  // peek though projections of the load, like unchecked_enum_data to find the
  // true extent of the lifetime including transitively referenced objects.
  if (IsSrcLoadedFrom || HasUnknownStoredValue)
    return;

  bool HoistedDestroyFound = false;
  SILLocation HoistedDestroyLoc = F->getLocation();
  const SILDebugScope *HoistedDebugScope = nullptr;

  for (auto *Destroy : DestroyPoints) {
    // If hoistDestroy returns false, it was not worth hoisting.
    if (hoistDestroy(Destroy, Destroy->getLoc())) {
      // Propagate DestroyLoc for any destroy hoisted above a block.
      if (DeadInBlocks.count(Destroy->getParent())) {
        HoistedDestroyLoc = Destroy->getLoc();
        HoistedDebugScope = Destroy->getDebugScope();
        HoistedDestroyFound = true;
      }
      // We either just created a new destroy, forwarded a copy, or will
      // continue propagating from this dead-in block. In any case, erase the
      // original Destroy.
      Destroy->eraseFromParent();
      assert(HasChanged || !DeadInBlocks.empty() && "HasChanged should be set");
    }
  }
  // Any blocks containing a DestroyPoints where hoistDestroy did not find a use
  // are now marked in DeadInBlocks.
  if (DeadInBlocks.empty())
    return;

  assert(HoistedDestroyFound && "Hoisted destroy should have been found");

  DestroyPoints.clear();

  // Propagate dead-in blocks upward via PostOrder traversal.
  // TODO: We could easily handle hoisting above loops if LoopInfo is available.
  //
  for (auto *BB : PostOrder->get(F)->getPostOrder()) {
    SmallVector<unsigned, 4> DeadInSuccs;
    ArrayRef<SILSuccessor> Succs = BB->getSuccessors();
    if (Succs.empty())
      continue;

    for (unsigned EdgeIdx = 0, End = Succs.size(); EdgeIdx != End; ++EdgeIdx) {
      if (DeadInBlocks.count(Succs[EdgeIdx].getBB()))
        DeadInSuccs.push_back(EdgeIdx);
    }
    if (DeadInSuccs.size() == Succs.size() &&
        !SrcUserInsts.count(BB->getTerminator())) {
      // All successors are dead, so continue hoisting.
      bool WasHoisted = hoistDestroy(BB->getTerminator(), HoistedDestroyLoc);
      (void)WasHoisted;
      assert(WasHoisted && "should always hoist above a terminator");
      continue;
    }
    // Emit a destroy on each CFG edge leading to a dead-in block. This requires
    // splitting critical edges and will naturally handle redundant branch
    // targets.
    for (unsigned EdgeIdx : DeadInSuccs) {
      SILBasicBlock *SuccBB = splitCriticalEdge(BB->getTerminator(), EdgeIdx);
      if (SuccBB)
        HasChangedCFG = true;
      else
        SuccBB = BB->getSuccessors()[EdgeIdx];

      // We make no attempt to use the best DebugLoc, because in all known
      // cases, we only have one.
      SILBuilder B(SuccBB->begin());
      B.setCurrentDebugScope(HoistedDebugScope);
      B.createDestroyAddr(HoistedDestroyLoc, CurrentDef);
      HasChanged = true;
    }
  }
}

//===----------------------------------------------------------------------===//
//                    Named Return Value Optimization
//===----------------------------------------------------------------------===//

/// Return true if this copy can be eliminated through Named Return Value
/// Optimization (NRVO).
///
/// Simple NRVO cases are handled naturally via backwardPropagateCopy. However,
/// general NRVO is not handled via local propagation without global data
/// flow. Nonetheless, NRVO is a simple pattern that can be detected using a
/// different technique from propagation.
///
/// Example:
/// func nrvo<T : P>(z : Bool) -> T {
///   var rvo : T
///   if (z) {
///     rvo = T(10)
///   }
///   else {
///     rvo = T(1)
///   }
///   return rvo
/// }
///
/// Because of the control flow, backward propagation with a block will fail to
/// find the initializer for the copy at "return rvo". Instead, we directly
/// check for an NRVO pattern by observing a copy in a return block that is the
/// only use of the copy's dest, which must be an @out arg. If there are no
/// instructions between the copy and the return that may write to the copy's
/// source, we simply replace the source's local stack address with the @out
/// address.
///
/// The following SIL pattern will be detected:
///
/// sil @foo : $@convention(thin) <T> (@out T) -> () {
/// bb0(%0 : $*T):
///   %2 = alloc_stack $T
/// ... // arbitrary control flow, but no other uses of %0
/// bbN:
///   copy_addr [take] %2 to [initialization] %0 : $*T
///   ... // no writes
///   return
static bool canNRVO(CopyAddrInst *CopyInst) {
  // Don't perform NRVO unless the copy is a [take]. This is the easiest way
  // to determine that the local variable has ownership of its value and ensures
  // that removing a copy is a reference count neutral operation. For example,
  // this copy can't be trivially eliminated without adding a retain.
  //   sil @f : $@convention(thin) (@guaranteed T) -> @out T
  //   bb0(%in : $*T, %out : $T):
  //     %local = alloc_stack $T
  //     store %in to %local : $*T
  //     copy_addr %local to [initialization] %out : $*T
  if (!CopyInst->isTakeOfSrc())
    return false;

  if (!isa<AllocStackInst>(CopyInst->getSrc()))
    return false;

  // The copy's dest must be an indirect SIL argument. Otherwise, it may not
  // dominate all uses of the source. Worse, it may be aliased. This
  // optimization will early-initialize the copy dest, so we can't allow aliases
  // to be accessed between the initialization and the return.
  auto OutArg = dyn_cast<SILFunctionArgument>(CopyInst->getDest());
  if (!OutArg)
    return false;

  if (!OutArg->isIndirectResult())
    return false;

  SILBasicBlock *BB = CopyInst->getParent();
  if (!isa<ReturnInst>(BB->getTerminator()))
    return false;

  SILValue CopyDest = CopyInst->getDest();
  if (!hasOneNonDebugUse(CopyDest))
    return false;

  auto SI = CopyInst->getIterator(), SE = BB->end();
  for (++SI; SI != SE; ++SI) {
    if (SI->mayWriteToMemory() && !isa<DeallocationInst>(SI))
      return false;
  }
  return true;
}

/// Replace all uses of \p ASI by \p RHS, except the dealloc_stack.
static void replaceAllUsesExceptDealloc(AllocStackInst *ASI, ValueBase *RHS) {
  llvm::SmallVector<Operand *, 8> Uses;
  for (Operand *Use : ASI->getUses()) {
    if (!isa<DeallocStackInst>(Use->getUser()))
      Uses.push_back(Use);
  }
  for (Operand *Use : Uses) {
    Use->set(RHS);
  }
}

/// Remove a copy for which canNRVO returned true.
static void performNRVO(CopyAddrInst *CopyInst) {
  LLVM_DEBUG(llvm::dbgs() << "NRVO eliminates copy" << *CopyInst);
  ++NumCopyNRVO;
  replaceAllUsesExceptDealloc(cast<AllocStackInst>(CopyInst->getSrc()),
                              CopyInst->getDest());
  assert(CopyInst->getSrc() == CopyInst->getDest() && "bad NRVO");
  CopyInst->eraseFromParent();
}

//===----------------------------------------------------------------------===//
//                         CopyForwardingPass
//===----------------------------------------------------------------------===//

namespace {
#ifndef NDEBUG
static llvm::cl::opt<int> ForwardStart("copy-forward-start",
                                       llvm::cl::init(0), llvm::cl::Hidden);
static llvm::cl::opt<int> ForwardStop("copy-forward-stop",
                                      llvm::cl::init(-1), llvm::cl::Hidden);
#endif

class CopyForwardingPass : public SILFunctionTransform
{
  void run() override {
    if (!EnableCopyForwarding && !EnableDestroyHoisting)
      return;

    // FIXME: We should be able to support [ossa].
    if (getFunction()->hasOwnership())
      return;

    LLVM_DEBUG(llvm::dbgs() << "Copy Forwarding in Func "
                            << getFunction()->getName() << "\n");

    // Collect a set of identified objects (@in arg or alloc_stack) that are
    // copied in this function.
    // Collect a separate set of copies that can be removed via NRVO.
    llvm::SmallSetVector<SILValue, 16> CopiedDefs;
    llvm::SmallVector<CopyAddrInst*, 4> NRVOCopies;
    for (auto &BB : *getFunction())
      for (auto II = BB.begin(), IE = BB.end(); II != IE; ++II) {
        if (auto *CopyInst = dyn_cast<CopyAddrInst>(&*II)) {
          if (EnableDestroyHoisting && canNRVO(CopyInst)) {
            NRVOCopies.push_back(CopyInst);
            continue;
          }
          SILValue Def = CopyInst->getSrc();
          if (isIdentifiedSourceValue(Def))
            CopiedDefs.insert(Def);
          else {
            LLVM_DEBUG(llvm::dbgs() << "  Skipping Def: " << Def
                                    << "    not an argument or local var!\n");
          }
        }
      }

    // Perform NRVO
    for (auto Copy : NRVOCopies) {
      performNRVO(Copy);
      invalidateAnalysis(SILAnalysis::InvalidationKind::CallsAndInstructions);
    }

    // Perform Copy Forwarding.
    if (CopiedDefs.empty())
      return;

    auto *PO = getAnalysis<PostOrderAnalysis>();
    auto *DA = getAnalysis<DominanceAnalysis>();
    auto *RCIA = getAnalysis<RCIdentityAnalysis>();
    auto Forwarding = CopyForwarding(PO, DA, RCIA);

    for (SILValue Def : CopiedDefs) {
#ifndef NDEBUG
      static unsigned NumDefs = 0;
      ++NumDefs;
      if ((int)NumDefs < ForwardStart || NumDefs >= (unsigned)ForwardStop)
        continue;
#endif
      // Iterate to forward through chains of copies.
      do {
        Forwarding.forwardCopiesOf(Def, getFunction());
      } while (Forwarding.hasForwardedToCopy());
    }
    if (Forwarding.hasChangedCFG()) {
      // We've split critical edges so we can't preserve CFG.
      invalidateAnalysis(SILAnalysis::InvalidationKind::FunctionBody);
    } else {
      invalidateAnalysis(SILAnalysis::InvalidationKind::CallsAndInstructions);
      }
  }
};

/// Temporary RValue Optimization
///
/// Peephole optimization to eliminate short-lived immutable temporary copies.
/// This handles a common pattern generated by SILGen where temporary RValues
/// are emitted as copies...
///
///   %temp = alloc_stack $T
///   copy_addr %src to [initialization] %temp : $*T
///   // no writes to %src and %temp
///   destroy_addr %temp : $*T
///   dealloc_stack %temp : $*T
///
/// This differs from the copy forwarding algorithm because it handles
/// copy source and dest lifetimes that are unavoidably overlappying. Instead,
/// it finds cases in which it is easy to determine that the source is
/// unmodified during the copy destination's lifetime. Thus, the destination can
/// be viewed as a short-lived "rvalue".
class TempRValueOptPass : public SILFunctionTransform {
  AliasAnalysis *AA = nullptr;

  bool collectLoads(Operand *UserOp, SILInstruction *UserInst,
                           SingleValueInstruction *Addr,
                           SILValue srcObject,
                           llvm::SmallPtrSetImpl<SILInstruction *> &LoadInsts);

  bool checkNoSourceModification(CopyAddrInst *copyInst,
                       const llvm::SmallPtrSetImpl<SILInstruction *> &useInsts);

  bool tryOptimizeCopyIntoTemp(CopyAddrInst *copyInst);

  void run() override;
};

/// The main entry point of the pass.
void TempRValueOptPass::run() {
  if (getFunction()->hasOwnership())
    return;

  LLVM_DEBUG(llvm::dbgs() << "Copy Peephole in Func "
                          << getFunction()->getName() << "\n");

  AA = PM->getAnalysis<AliasAnalysis>();
  bool Changed = false;

  // Find all copy_addr instructions.
  for (auto &BB : *getFunction()) {
    auto II = BB.begin();
    while (II != BB.end()) {
      auto *CopyInst = dyn_cast<CopyAddrInst>(&*II);

      if (CopyInst) {
        // In case of success, this may delete instructions, but not the
        // CopyInst itself.
        Changed |= tryOptimizeCopyIntoTemp(CopyInst);
      }

      // Increment the instruction iterator here. We can't do it at the begin of
      // the loop because the instruction after CopyInst might be deleted in
      // in tryOptimizeCopyIntoTemp. We can't do it at the end of the loop
      // because the CopyInst might be deleted in the following code.
      ++II;

      // Remove identity copies which are a result of this optimization.
      if (CopyInst && CopyInst->getSrc() == CopyInst->getDest()) {
        // This is either the CopyInst which just got optimized or it is a
        // follow-up from an earlier iteration, where another copy_addr copied
        // the temporary back to the source location.
        CopyInst->eraseFromParent();
      }
    }
  }

  if (Changed) {
    invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }
}

/// Transitively explore all data flow uses of the given \p address until
/// reaching a load or returning false.
bool TempRValueOptPass::collectLoads(
    Operand *userOp, SILInstruction *user, SingleValueInstruction *address,
    SILValue srcObject,
    llvm::SmallPtrSetImpl<SILInstruction *> &loadInsts) {
  // All normal uses (loads) must be in the initialization block.
  // (The destroy and dealloc are commonly in a different block though.)
  if (user->getParent() != address->getParent())
    return false;

  // Only allow uses that cannot destroy their operand. We need to be sure
  // that replacing all this temporary's uses with the copy source doesn't
  // destroy the source. This way, we know that the destroy_addr instructions
  // that we recorded cover all the temporary's lifetime termination points.
  //
  // Currently this includes address projections, loads, and in_guaranteed uses
  // by an apply.
  //
  // TODO: handle non-destructive projections of enums
  // (unchecked_take_enum_data_addr of Optional is nondestructive.)
  switch (user->getKind()) {
  default:
    LLVM_DEBUG(llvm::dbgs() << "  Temp use may write/destroy its source"
                            << *user);
    return false;

  case SILInstructionKind::ApplyInst: {
    ApplySite apply(user);

    // Check if the function can just read from userOp.
    auto Convention = apply.getArgumentConvention(*userOp);
    if (!Convention.isGuaranteedConvention()) {
      LLVM_DEBUG(llvm::dbgs() << "  Temp consuming use may write/destroy "
                 "its source" << *user);
      return false;
    }

    // Check if there is another function argument, which is inout which might
    // modify the source of the copy_addr.
    //
    // When a use of the temporary is an apply, then we need to prove that the
    // function called by the apply cannot modify the temporary's source
    // value. By design, this should be handled by
    // `checkNoSourceModification`. However, this would be too conservative
    // since it's common for the apply to have an @out argument, and alias
    // analysis cannot prove that the @out does not alias with `src`. Instead,
    // `checkNoSourceModification` always avoids analyzing the current use, so
    // applies need to be handled here. We already know that an @out cannot
    // alias with `src` because the `src` value must be initialized at the point
    // of the call. Hence, it is sufficient to check specifically for another
    // @inout that might alias with `src`.
    auto calleeConv = apply.getSubstCalleeConv();
    unsigned calleeArgIdx = apply.getCalleeArgIndexOfFirstAppliedArg();
    for (Operand &operand : apply.getArgumentOperands()) {
      auto argConv = calleeConv.getSILArgumentConvention(calleeArgIdx);
      if (argConv.isInoutConvention()) {
        if (!AA->isNoAlias(operand.get(), srcObject)) {
          return false;
        }
      }
      ++calleeArgIdx;
    }

    // Everything is okay with the function call. Register it as a "load".
    loadInsts.insert(user);
    return true;
  }
  case SILInstructionKind::OpenExistentialAddrInst: {
    // We only support open existential addr if the access is immutable.
    auto *oeai = cast<OpenExistentialAddrInst>(user);
    if (oeai->getAccessKind() != OpenedExistentialAccess::Immutable) {
      LLVM_DEBUG(llvm::dbgs() << "  Temp consuming use may write/destroy "
                 "its source" << *user);
      return false;
    }
    return true;
  }
  case SILInstructionKind::StructElementAddrInst:
  case SILInstructionKind::TupleElementAddrInst: {
    // Transitively look through projections on stack addresses.
    auto proj = cast<SingleValueInstruction>(user);
    for (auto *projUseOper : proj->getUses()) {
      auto *user = projUseOper->getUser();
      if (user->isTypeDependentOperand(*projUseOper))
        continue;

      if (!collectLoads(projUseOper, user, proj, srcObject,
                        loadInsts))
        return false;
    }
    return true;
  }

  case SILInstructionKind::LoadInst:
  case SILInstructionKind::LoadBorrowInst: {
    // Loads are the end of the data flow chain. The users of the load can't
    // access the temporary storage.
    loadInsts.insert(user);
    return true;
  }

  case SILInstructionKind::CopyAddrInst: {
    // copy_addr which read from the temporary are like loads.
    auto *copyFromTmp = cast<CopyAddrInst>(user);
    if (copyFromTmp->getDest() == address) {
      LLVM_DEBUG(llvm::dbgs() << "  Temp written or taken" << *user);
      return false;
    }
    loadInsts.insert(copyFromTmp);
    return true;
  }
  }
}

/// Checks if the copy's source can be modified within the temporary's lifetime.
///
/// Unfortunately, we cannot simply use the destroy points as the lifetime end,
/// because they can be in a different basic block (that's what SILGen
/// generates). Instead we guarantee that all normal uses are within the block
/// of the temporary and look for the last use, which effectively ends the
/// lifetime.
bool TempRValueOptPass::checkNoSourceModification(CopyAddrInst *copyInst,
                     const llvm::SmallPtrSetImpl<SILInstruction *> &useInsts) {
  unsigned NumLoadsFound = 0;
  auto iter = std::next(copyInst->getIterator());
  // We already checked that the useful lifetime of the temporary ends in
  // the initialization block.
  auto iterEnd = copyInst->getParent()->end();
  for (; iter != iterEnd; ++iter) {
    SILInstruction *I = &*iter;

    if (useInsts.count(I))
      NumLoadsFound++;

    // If this is the last use of the temp we are ok. After this point,
    // modifications to the source don't matter anymore.
    if (NumLoadsFound == useInsts.size())
      return true;

    if (AA->mayWriteToMemory(I, copyInst->getSrc())) {
      LLVM_DEBUG(llvm::dbgs() << "  Source modified by" << *iter);
      return false;
    }
  }
  // For some reason, not all normal uses have been seen between the copy and
  // the end of the initialization block. We should never reach here.
  return false;
}

/// Tries to perform the temporary rvalue copy elimination for \p copyInst
bool TempRValueOptPass::tryOptimizeCopyIntoTemp(CopyAddrInst *copyInst) {
  if (!copyInst->isInitializationOfDest())
    return false;

  auto *tempObj = dyn_cast<AllocStackInst>(copyInst->getDest());
  if (!tempObj)
    return false;

  assert(tempObj != copyInst->getSrc() &&
           "can't initialize temporary with itself");

  // Scan all uses of the temporary storage (tempObj) to verify they all refer
  // to the value initialized by this copy. It is sufficient to check that the
  // only users that modify memory are the copy_addr [initialization] and
  // destroy_addr.
  llvm::SmallPtrSet<SILInstruction *, 8> loadInsts;
  for (auto *useOper : tempObj->getUses()) {
    SILInstruction *user = useOper->getUser();

    if (user == copyInst)
      continue;

    // Destroys and deallocations are allowed to be in a different block.
    if (isa<DestroyAddrInst>(user) || isa<DeallocStackInst>(user))
      continue;

    if (!collectLoads(useOper, user, tempObj, copyInst->getSrc(), loadInsts))
      return false;
  }

  // Check if the source is modified within the lifetime of the temporary.
  if (!checkNoSourceModification(copyInst, loadInsts))
    return false;

  LLVM_DEBUG(llvm::dbgs() << "  Success: replace temp" << *tempObj);

  // Do a "replaceAllUses" by either deleting the users or replacing them with
  // the source address. Note: we must not delete the original copyInst because
  // it would crash the instruction iteration in run(). Instead the copyInst
  // gets identical Src and Dest operands.
  while (!tempObj->use_empty()) {
    Operand *use = *tempObj->use_begin();
    SILInstruction *user = use->getUser();
    switch (user->getKind()) {
    case SILInstructionKind::DestroyAddrInst:
      if (copyInst->isTakeOfSrc()) {
        use->set(copyInst->getSrc());
      } else {
        user->eraseFromParent();
      }
      break;
    case SILInstructionKind::DeallocStackInst:
      user->eraseFromParent();
      break;
    case SILInstructionKind::CopyAddrInst: {
      auto *CAI = cast<CopyAddrInst>(user);
      if (CAI != copyInst) {
        assert(CAI->getSrc() == tempObj);
        if (CAI->isTakeOfSrc() && !copyInst->isTakeOfSrc())
          CAI->setIsTakeOfSrc(IsNotTake);
      }
      use->set(copyInst->getSrc());
      break;
    }
    case SILInstructionKind::StructElementAddrInst:
    case SILInstructionKind::TupleElementAddrInst:
    case SILInstructionKind::LoadInst:
    case SILInstructionKind::LoadBorrowInst:
    case SILInstructionKind::ApplyInst:
    case SILInstructionKind::OpenExistentialAddrInst:
      use->set(copyInst->getSrc());
      break;

    default:
      llvm_unreachable("unhandled instruction");
    }
  }
  tempObj->eraseFromParent();
  return true;
}

} // end anonymous namespace

SILTransform *swift::createCopyForwarding() {
  return new CopyForwardingPass();
}

SILTransform *swift::createTempRValueOpt() {
  return new TempRValueOptPass();
}
