//===--- CopyForwarding.cpp - Forward local copies from caller to callee --===//
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

#define DEBUG_TYPE "copy-forwarding"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILAnalysis/PostOrderAnalysis.h"
#include "swift/SILPasses/Passes.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILPasses/Utils/CFG.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"

STATISTIC(NumCopyNRVO, "Number of copies removed via named return value opt.");
STATISTIC(NumCopyForward, "Number of copies removed via forward propagation.");
STATISTIC(NumCopyBackward,
          "Number of copies removed via backward propagation.");

using namespace swift;

// Temporary debugging flag until this pass is better tested.
static llvm::cl::opt<bool> EnableCopyForwarding("enable-copyforwarding",
                                                llvm::cl::init(false));
static llvm::cl::opt<bool> EnableDestroyHoisting("enable-destroyhoisting",
                                                llvm::cl::init(false));

/// \return true of the given object can only be accessed via the given def
/// (this def uniquely identifies the object).
///
/// (1) An "in" argument.
///
/// (2) A local alloc_stack variable.
static bool isIdentifiedObject(SILValue Def, SILFunction *F) {
  if (SILArgument *Arg = dyn_cast<SILArgument>(Def)) {
    // Check that the argument is passed as an in type. This means there are
    // no aliases accessible within this function scope. We may be able to just
    // assert this.
    ParameterConvention Conv =  Arg->getParameterInfo().getConvention();
    switch (Conv) {
    case ParameterConvention::Indirect_In:
    case ParameterConvention::Indirect_In_Guaranteed:
    case ParameterConvention::Indirect_Inout:
      return true;
    default:
      DEBUG(llvm::dbgs() << "  Skipping Def: Not an @in argument!\n");
      return false;
    }
  }
  else if (isa<AllocStackInst>(Def))
    return true;

  return false;
}

/// Return the parameter convention used by Apply to pass an argument
/// indirectly via Address.
///
/// Set Oper to the Apply operand that passes Address.
static ParameterConvention getAddressArgConvention(ApplyInst *Apply,
                                                   SILValue Address,
                                                   Operand *&Oper) {
  Oper = nullptr;
  ParameterConvention Conv;
  auto Params = Apply->getSubstCalleeType()->getParameters();
  auto Args = Apply->getArgumentOperands();
  for (unsigned ArgIdx = 0, ArgE = Params.size(); ArgIdx != ArgE; ++ArgIdx) {
    if (Args[ArgIdx].get() != Address)
      continue;

    Conv = Params[ArgIdx].getConvention();
    assert(isIndirectParameter(Conv) && "Address not passed as an indirection");

    assert(!Oper && "Address can only be passed once as an indirection.");
    Oper = &Args[ArgIdx];
#ifndef NDEBUG
    break;
#endif
  }
  assert(Oper && "Address value not passed as an argument to this call.");
  return Conv;
}

//===----------------------------------------------------------------------===//
//                 Forward and backward copy propagation
//===----------------------------------------------------------------------===//

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
    case ParameterConvention::Indirect_In:
      return true;
    case ParameterConvention::Indirect_In_Guaranteed:
    case ParameterConvention::Indirect_Inout:
      return false;
    case ParameterConvention::Indirect_Out:
      llvm_unreachable("copy_addr not released before reinitialization");
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
  bool visitOpenExistentialInst(OpenExistentialInst *UserInst) {
    Oper = &UserInst->getOperandRef();
    return false;
  }
  bool visitStructElementAddrInst(StructElementAddrInst *UserInst) {
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
    case ParameterConvention::Indirect_Out:
      return true;
    case ParameterConvention::Indirect_Inout:
      return false;
    case ParameterConvention::Indirect_In:
    case ParameterConvention::Indirect_In_Guaranteed:
      llvm_unreachable("copy_addr not destroyed before reinitialization");
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
  bool visitOpenExistentialInst(OpenExistentialInst *UserInst) {
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
  bool visitSILInstruction(SILInstruction *UserInst) {
    return false;
  }
};

class CopyForwarding {
  // Per-function state.
  PostOrderAnalysis *PostOrder;
  bool DoGlobalHoisting;
  bool HasChanged;
  bool HasChangedCFG;

  // Transient state for the current Def valid during forwardCopiesOf.
  SILValue CurrentDef;
  bool HasForwardedToCopy;
  SmallPtrSet<SILInstruction*, 16> SrcUserInsts;
  SmallVector<CopyAddrInst*, 4> TakePoints;
  SmallVector<DestroyAddrInst*, 4> DestroyPoints;
  SmallPtrSet<SILBasicBlock*, 32> DeadInBlocks;
public:
  CopyForwarding(PostOrderAnalysis *PO):
    PostOrder(PO), DoGlobalHoisting(false), HasChanged(false),
    HasChangedCFG(false), HasForwardedToCopy(false) {}

  void reset(SILFunction *F) {
    // Don't hoist destroy_addr globally in transparent functions. Avoid cloning
    // destroy_addr instructions and splitting critical edges before mandatory
    // diagnostic passes. For example, PredictableMemOps can no longer remove
    // some alloc_stack cases after global destroy hoisting. CopyForwarding will
    // be reapplied after the transparent function is inlined at which point
    // global hoisting will be done.
    DoGlobalHoisting = !F->isTransparent();
    if (HasChangedCFG)
      PostOrder->invalidate(F, SILAnalysis::InvalidationKind::CFG);
    CurrentDef = SILValue();
    HasForwardedToCopy = false;
    SrcUserInsts.clear();
    TakePoints.clear();
    DestroyPoints.clear();
    DeadInBlocks.clear();
  }

  bool hasChanged() const { return HasChanged; }
  bool hasChangedCFG() const { return HasChangedCFG; }

  /// Return true if CurrentDef has been forwarded through one copy into
  /// another. This means we should iterate.
  bool hasForwardedToCopy() const { return HasForwardedToCopy; }

  void forwardCopiesOf(SILValue Def, SILFunction *F);

protected:
  bool collectUsers();
  bool propagateCopy(CopyAddrInst *CopyInst);
  bool forwardPropagateCopy(CopyAddrInst *CopyInst,
                            SmallPtrSetImpl<SILInstruction*> &DestUserInsts);
  bool backwardPropagateCopy(CopyAddrInst *CopyInst,
                             SmallPtrSetImpl<SILInstruction*> &DestUserInsts);
  bool hoistDestroy(SILInstruction *DestroyPoint, SILLocation DestroyLoc);
};
} // namespace

/// Gather all instructions that use CurrentDef:
/// - DestroyPoints records 'destroy'
/// - TakePoints records 'copy_addr [take] src'
/// - SrcUserInsts records other users.
///
/// If we are unable to find all uses, for example, because we don't look
/// through struct_element_addr, then return false.
///
/// The collected use points will be consulted during forward and backward
/// copy propagation.
bool CopyForwarding::collectUsers() {
  for (auto UI : CurrentDef.getUses()) {
    SILInstruction *UserInst = UI->getUser();
    if (auto *Apply = dyn_cast<ApplyInst>(UserInst)) {
      /// A call to materializeForSet exposes an address within the parent
      /// object. However, we can rely on a subsequent mark_dependent
      /// instruction to take that object as an operand, causing it to escape
      /// for the purpose of this analysis.
      auto Params = Apply->getSubstCalleeType()->getParameters();
      (void)Params;
      assert(Params[UI->getOperandNumber() - Apply->getArgumentOperandNumber()]
             .isIndirect() && "copy_addr location should be passed indirect");
      SrcUserInsts.insert(Apply);
      continue;
    }
    if (auto *CopyInst = dyn_cast<CopyAddrInst>(UserInst)) {
      if (CopyInst->getSrc() == UI->get() && CopyInst->isTakeOfSrc())
        TakePoints.push_back(CopyInst);
      else
        SrcUserInsts.insert(CopyInst);
      continue;
    }
    if (auto *Destroy = dyn_cast<DestroyAddrInst>(UserInst)) {
      DestroyPoints.push_back(Destroy);
      continue;
    }
    switch (UserInst->getKind()) {
    case ValueKind::UncheckedTakeEnumDataAddrInst:
    case ValueKind::ExistentialMetatypeInst:
    case ValueKind::InjectEnumAddrInst:
    case ValueKind::LoadInst:
    case ValueKind::StoreInst:
    case ValueKind::InitEnumDataAddrInst:
    case ValueKind::DebugValueAddrInst:
      SrcUserInsts.insert(UserInst);
      break;
    default:
      // Likely to be OpenExistentialInst or StructElementAddrInst.
      // TODO: we could peak through struct element users like COWArrayOpts.
      DEBUG(llvm::dbgs() << "  Skipping copy: use exposes def" << *UserInst);
      return false;
    }
  }
  return true;
}

/// Attempt to forward, then backward propagate this copy.
///
/// The caller has already proven that lifetime of the value being copied ends
/// at the copy. (Either it is a [take] or is immediately destroyed).
///
/// If the forwarded copy is not an [init], then insert a destroy of the copy's
/// dest.
bool CopyForwarding::propagateCopy(CopyAddrInst *CopyInst) {
  if (!EnableCopyForwarding)
    return false;

  SILValue CopyDest = CopyInst->getDest();
  SILBasicBlock *BB = CopyInst->getParent();

  // Gather a list of CopyDest users in this block.
  SmallPtrSet<SILInstruction*, 16> DestUserInsts;
  for (auto UI : CopyDest.getUses()) {
    SILInstruction *UserInst = UI->getUser();
    if (UserInst != CopyInst && UI->getUser()->getParent() == BB)
      DestUserInsts.insert(UI->getUser());
  }
  // Note that DestUserInsts is likely empty when the dest is an 'out' argument,
  // allowing us to go straight to backward propagation.
  if (forwardPropagateCopy(CopyInst, DestUserInsts)) {
    DEBUG(llvm::dbgs() << "  Forwarding Copy:" << *CopyInst);
    if (!CopyInst->isInitializationOfDest()) {
      // Replace the original copy with a destroy. We may be able to hoist it
      // more in another pass but don't currently iterate.
      SILBuilder(CopyInst).createDestroyAddr(CopyInst->getLoc(),
                                             CopyInst->getDest())
        ->setDebugScope(CopyInst->getDebugScope());
    }
    CopyInst->eraseFromParent();
    HasChanged = true;
    ++NumCopyForward;
    return true;
  }
  // Forward propagation failed. Attempt to backward propagate.
  if (CopyInst->isInitializationOfDest()
      && backwardPropagateCopy(CopyInst, DestUserInsts)) {
    DEBUG(llvm::dbgs() << "  Reversing Copy:" << *CopyInst);
    CopyInst->eraseFromParent();
    HasChanged = true;
    ++NumCopyBackward;
    return true;
  }
  return false;
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
/// copy_addr %arg to [initialization] %copy#1 : $*T
/// ...
/// %ret = apply %callee<T>(%copy#1) : $@thin <τ_0_0> (@in τ_0_0) -> ()
/// \endcode
///
/// If the last use (deinit) is a copy, replace it with a destroy+copy[init].
///
/// The caller has already guaranteed that the lifetime of the copy's source
/// ends at this copy. Either the copy is a [take] or a destroy can be hoisted
/// to the copy.
bool CopyForwarding::forwardPropagateCopy(
  CopyAddrInst *CopyInst,
  SmallPtrSetImpl<SILInstruction*> &DestUserInsts) {

  if (DestUserInsts.empty())
    return false;

  SILValue CopyDest = CopyInst->getDest();
  SILInstruction *DefDealloc = nullptr;
  if (isa<AllocStackInst>(CurrentDef)) {
    SILValue StackAddr(CurrentDef.getDef(), 0);
    if (!StackAddr.hasOneUse()) {
      DEBUG(llvm::dbgs() << "  Skipping copy" << *CopyInst
            << "  stack address has multiple uses.\n");
      return false;
    }
    DefDealloc = StackAddr.use_begin()->getUser();
  }

  // Scan forward recording all operands that use CopyDest until we see the
  // next deinit of CopyDest.
  SmallVector<Operand*, 16> ValueUses;
  SILBasicBlock::iterator SI = CopyInst, SE = CopyInst->getParent()->end();
  for (++SI; SI != SE; ++SI) {
    SILInstruction *UserInst = &*SI;
    // If we see another use of Src, then the source location is reinitialized
    // before the Dest location is deinitialized. So we really need the copy.
    if (SrcUserInsts.count(UserInst)) {
      DEBUG(llvm::dbgs() << "  Skipping copy" << *CopyInst
            << "  source used by" << *UserInst);
      return false;
    }
    if (UserInst == DefDealloc) {
      DEBUG(llvm::dbgs() << "  Skipping copy" << *CopyInst
            << "    dealloc_stack before dest use.\n");
      return false;
    }
    // Early check to avoid scanning unrelated instructions.
    if (!DestUserInsts.count(UserInst))
      continue;

    AnalyzeForwardUse AnalyzeUse(CopyDest);
    bool seenDeinit = AnalyzeUse.visit(UserInst);
    // If this use cannot be anlayzed, then abort.
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
  // later be able to elimimate this initialization copy.
  if (auto Copy = dyn_cast<CopyAddrInst>(&*SI)) {
    if (Copy->getDest() == CopyDest) {
      assert(!Copy->isInitializationOfDest() && "expected a deinit");

      DestroyAddrInst *Destroy =
        SILBuilderWithScope<1>(SI, Copy->getDebugScope())
          .createDestroyAddr(Copy->getLoc(), CopyDest);
      Copy->setIsInitializationOfDest(IsInitialization);

      assert(ValueUses.back()->getUser() == Copy && "bad value use");
      ValueUses.back() = &Destroy->getOperandRef();
    }
  }
  // Now that a deinit was found, it is safe to substitute all recorded uses
  // with the copy's source.
  for (auto *Oper : ValueUses) {
    Oper->set(CopyInst->getSrc());
    if (isa<CopyAddrInst>(Oper->getUser()))
      HasForwardedToCopy = true;
  }
  return true;
}

/// Perform backward copy-propagation. Find the initialization point of the
/// copy's source and replace the initializer's address with the copy's dest.
bool CopyForwarding::backwardPropagateCopy(
  CopyAddrInst *CopyInst,
  SmallPtrSetImpl<SILInstruction*> &DestUserInsts) {

  SILValue CopySrc = CopyInst->getSrc();
  ValueBase *CopyDestDef = CopyInst->getDest().getDef();

  // Scan backward recording all operands that use CopySrc until we see the
  // most recent init of CopySrc.
  bool seenInit = false;
  SmallVector<Operand*, 16> ValueUses;
  SILBasicBlock::iterator SI = CopyInst, SE = CopyInst->getParent()->begin();
  while (SI != SE) {
    --SI;
    SILInstruction *UserInst = &*SI;
    // If we see another use of Dest, then Dest is live after the Src location
    // is initialized, so we really need the copy.
    if (DestUserInsts.count(UserInst) || UserInst == CopyDestDef) {
      DEBUG(llvm::dbgs() << "  Skipping copy" << *CopyInst
            << "    dest used by " << *UserInst);
      return false;
    }
    // Early check to avoid scanning unrelated instructions.
    if (!SrcUserInsts.count(UserInst))
      continue;

    AnalyzeBackwardUse AnalyzeUse(CopySrc);
    seenInit = AnalyzeUse.visit(UserInst);
    // If this use cannot be anlayzed, then abort.
    if (!AnalyzeUse.Oper)
      return false;
    // Otherwise record the operand.
    ValueUses.push_back(AnalyzeUse.Oper);
    // If this is an init, we're done searching.
    if (seenInit)
      break;
  }
  if (!seenInit)
    return false;

  // Convert a reinitialization of this address into a destroy, followed by an
  // initialization. Replacing a copy with a destroy+init is not by itself
  // profitable. However, it does allow us to eliminate the later copy, and the
  // init copy may be eliminater later.
  if (auto Copy = dyn_cast<CopyAddrInst>(&*SI)) {
    if (Copy->getDest() == CopySrc && !Copy->isInitializationOfDest()) {
      SILBuilder(SI).createDestroyAddr(Copy->getLoc(), CopySrc)
        ->setDebugScope(Copy->getDebugScope());
      Copy->setIsInitializationOfDest(IsInitialization);
    }
  }
  // Now that an init was found, it is safe to substitute all recorded uses
  // with the copy's dest.
  for (auto *Oper : ValueUses) {
    Oper->set(CopyInst->getDest());
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
/// Def = <uniquely identified> // no aliases
/// ...
/// Copy = copy_addr [init] Def
/// ...                    // no access to Def
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

  bool IsWorthHoisting = MustHoist;
  SILBasicBlock::iterator SI = DestroyPoint, SE = BB->begin();
  while (SI != SE) {
    --SI;
    SILInstruction *Inst = &*SI;
    if (!SrcUserInsts.count(Inst)) {
      if (!IsWorthHoisting && isa<ApplyInst>(Inst))
        IsWorthHoisting = true;
      continue;
    }
    if (auto *CopyInst = dyn_cast<CopyAddrInst>(Inst)) {
      if (!CopyInst->isTakeOfSrc() && CopyInst->getSrc() == CurrentDef) {
        // This use is a copy of CurrentDef. Attempt to forward CurrentDef to
        // all uses of the copy's value.
        if (propagateCopy(CopyInst))
          return true;
      }
    }
    // We reached a user of CurrentDef. If we haven't seen anything significant,
    // avoid useless hoisting.
    if (!IsWorthHoisting)
      return false;

    DEBUG(llvm::dbgs() << "  Hoisting to Use:" << *Inst);
    SILBuilder(std::next(SI)).createDestroyAddr(DestroyLoc, CurrentDef)
      ->setDebugScope(Inst->getDebugScope());
    HasChanged = true;
    return true;
  }
  if (!DoGlobalHoisting)
    return false;
  DeadInBlocks.insert(BB);
  return true;
}

/// Perform CopyForwarding on the current Def.
void CopyForwarding::forwardCopiesOf(SILValue Def, SILFunction *F) {
  reset(F);
  CurrentDef = Def;
  DEBUG(llvm::dbgs() << "Analyzing copies of Def: " << Def);
  if (!collectUsers())
    return;

  // First forward any copies that implicitly destroy CurrentDef. There is no
  // need to hoist Destroy for these.
  for (auto *CopyInst : TakePoints)
    propagateCopy(CopyInst);

  SILInstruction *HoistedDestroy = nullptr;
  for (auto *Destroy : DestroyPoints) {
    // If hoistDestroy returns false, it was not worth hoisting.
    if (hoistDestroy(Destroy, Destroy->getLoc())) {
      // Propagate DestroyLoc for any destroy hoisted above a block.
      if (DeadInBlocks.count(Destroy->getParent()))
        HoistedDestroy = Destroy;
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

  SILLocation DestroyLoc = HoistedDestroy->getLoc();
  DestroyPoints.clear();

  // Propagate dead-in blocks upward via PostOrder traversal.
  // TODO: We could easily handle hoisting above loops if LoopInfo is available.
  //
  for (auto *BB : PostOrder->getPostOrder(F)) {
    SmallVector<unsigned, 4> DeadInSuccs;
    ArrayRef<SILSuccessor> Succs = BB->getSuccs();
    if (Succs.size() == 0)
      continue;

    for (unsigned EdgeIdx = 0, End = Succs.size(); EdgeIdx != End; ++EdgeIdx) {
      if (DeadInBlocks.count(Succs[EdgeIdx].getBB()))
        DeadInSuccs.push_back(EdgeIdx);
    }
    if (DeadInSuccs.size() == Succs.size() &&
        !SrcUserInsts.count(BB->getTerminator())) {
      // All successors are dead, so continue hoisting.
      bool WasHoisted = hoistDestroy(BB->getTerminator(), DestroyLoc);
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
        SuccBB = BB->getSuccs()[EdgeIdx];

      // We make no attempt to use the best DebugLoc, because in all known
      // cases, we only have one.
      SILBuilder(SuccBB->begin()).createDestroyAddr(DestroyLoc, CurrentDef)
        ->setDebugScope(HoistedDestroy->getDebugScope());
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
/// sil @foo : $@thin <T> (@out T) -> () {
/// bb0(%0 : $*T):
///   %2 = alloc_stack $T
/// ... // arbitrary control flow, but no other uses of %0
/// bbN:
///   copy_addr [take] %2#1 to [initialization] %0 : $*T
///   ... // no writes
///   return
static bool canNRVO(CopyAddrInst *CopyInst) {
  if (!isa<AllocStackInst>(CopyInst->getSrc()))
    return false;

  // The copy's dest must be an indirect SIL argument. Otherwise, it may not
  // dominate all uses of the source. Worse, it may be aliased. This
  // optimization will early-initialize the copy dest, so we can't allow aliases
  // to be accessed between the initialization and the return.
  auto OutArg = dyn_cast<SILArgument>(CopyInst->getDest());
  if (!OutArg || !OutArg->getParameterInfo().isIndirect())
    return false;

  SILBasicBlock *BB = CopyInst->getParent();
  if (!isa<ReturnInst>(BB->getTerminator()))
    return false;

  SILValue CopyDest = CopyInst->getDest();
  if (!CopyDest->hasOneUse())
    return false;

  SILBasicBlock::iterator SI = CopyInst, SE = BB->end();
  for (++SI; SI != SE; ++SI) {
    if (SI->mayWriteToMemory() && !isa<DeallocationInst>(SI))
      return false;
  }
  return true;
}

/// Remove a copy for which canNRVO returned true.
static void performNRVO(CopyAddrInst *CopyInst) {
  DEBUG(llvm::dbgs() << "NRVO eliminates copy" << *CopyInst);
  ++NumCopyNRVO;
  CopyInst->getSrc().replaceAllUsesWith(CopyInst->getDest());
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

    DEBUG(llvm::dbgs() << "Copy Forwarding in Func " << getFunction()->getName()
          << "\n");

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
          if (isIdentifiedObject(Def, getFunction()))
            CopiedDefs.insert(Def);
          else {
            DEBUG(llvm::dbgs() << "  Skipping Def: " << Def
                  << "    not an argument or local var!\n");
          }
        }
      }

    // Perform NRVO
    for (auto Copy : NRVOCopies) {
      performNRVO(Copy);
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
    }

    // Perform Copy Forwarding.
    if (CopiedDefs.empty())
      return;

    auto *PO = getAnalysis<PostOrderAnalysis>();
    auto Forwarding = CopyForwarding(PO);

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
    if (Forwarding.hasChangedCFG())
      invalidateAnalysis(SILAnalysis::InvalidationKind::CFG);
    else
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }

  StringRef getName() override { return "Copy Forwarding"; }
};
} // anonymous

SILTransform *swift::createCopyForwarding() {
  return new CopyForwardingPass();
}
