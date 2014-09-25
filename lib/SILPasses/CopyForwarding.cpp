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
#include "swift/SILAnalysis/PostOrderAnalysis.h"
#include "swift/SILPasses/Passes.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILPasses/Utils/CFG.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"

using namespace swift;

// Temporary debugging flag until this pass is better tested.
static llvm::cl::opt<bool> EnableCopyForwarding("enable-copyforwarding",
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
    if (Arg->getParameterInfo().getConvention()
        != ParameterConvention::Indirect_In) {
      DEBUG(llvm::dbgs() << "  Skipping Def: Not an @in argument!\n");
      return false;
    }
    return true;
  }
  else if (isa<AllocStackInst>(Def))
    return true;

  DEBUG(llvm::dbgs()
        << "  Skipping Def: Not an argument or local variable!\n");
  return false;
}

/// \return true if this operand destroys its value, false if it does not
/// destroy its value, and an empty Optional in cases that can't be determined.
///
/// We currently check for the following cases of deinit:
/// - 'in' argument
/// - copy_addr [take] src
/// - copy_addr [!init] dest
/// - destroy_addr
static Optional<bool> isDeinit(Operand* Oper) {
  SILInstruction *UserInst = Oper->getUser();
  if (auto Apply = dyn_cast<ApplyInst>(UserInst)) {
    auto Params = Apply->getSubstCalleeType()->getParameters();
    // TODO: Provide and ApplyInst API for this.
    assert(Oper->getOperandNumber() >= 1 && "calling copy_addr operand");
    switch (Params[Oper->getOperandNumber()-1].getConvention()) {
    case ParameterConvention::Indirect_In:
    case ParameterConvention::Indirect_Inout:
      return true;
    case ParameterConvention::Indirect_Out:
      llvm_unreachable("copy_addr must be released before reinitialization");
    default:
      llvm_unreachable("unexpected calling convention for copy_addr user");
    }
  }
  else if (auto *CopyInst = dyn_cast<CopyAddrInst>(UserInst)) {
    if (CopyInst->getSrc() == Oper->get())
      return CopyInst->isTakeOfSrc();

    assert(!CopyInst->isInitializationOfDest() && "illegal reinitialization");
    return true;
  }
  switch (UserInst->getKind()) {
  case ValueKind::DestroyAddrInst:
  case ValueKind::UncheckedTakeEnumDataAddrInst:
    return true;
  case ValueKind::ExistentialMetatypeInst:
  case ValueKind::InjectEnumAddrInst:
  case ValueKind::LoadInst:
  case ValueKind::ProjectExistentialInst:
  case ValueKind::ProtocolMethodInst:
  case ValueKind::StoreInst:
  case ValueKind::StructElementAddrInst:
    return false;
  case ValueKind::InitEnumDataAddrInst:
    llvm_unreachable("illegal reinitialization");
  default:
    return Nothing;
  }
}

static Optional<bool> isInit(Operand* Oper) {
  SILInstruction *UserInst = Oper->getUser();
  if (auto Apply = dyn_cast<ApplyInst>(UserInst)) {
    auto Params = Apply->getSubstCalleeType()->getParameters();
    // TODO: Provide and ApplyInst API for this.
    assert(Oper->getOperandNumber() >= 1 && "calling copy_addr operand");
    switch (Params[Oper->getOperandNumber()-1].getConvention()) {
    case ParameterConvention::Indirect_Out:
    case ParameterConvention::Indirect_Inout:
      return true;
    case ParameterConvention::Indirect_In:
      llvm_unreachable("copy_addr location must be initialized before use");
    default:
      llvm_unreachable("unexpected calling convention for copy_addr user");
    }
  }
  else if (auto *CopyInst = dyn_cast<CopyAddrInst>(UserInst)) {
    if (CopyInst->getDest() == Oper->get())
      return CopyInst->isInitializationOfDest();

    assert(!CopyInst->isTakeOfSrc() && "illegal deinitialization");
    return true;
  }
  switch (UserInst->getKind()) {
  case ValueKind::InitEnumDataAddrInst:
    return true;
  case ValueKind::ExistentialMetatypeInst:
  case ValueKind::InjectEnumAddrInst:
  case ValueKind::LoadInst:
  case ValueKind::ProjectExistentialInst:
  case ValueKind::ProtocolMethodInst:
  case ValueKind::StoreInst:
  case ValueKind::StructElementAddrInst:
    return false;
  case ValueKind::DestroyAddrInst:
  case ValueKind::UncheckedTakeEnumDataAddrInst:
    llvm_unreachable("illegal deinitialization");
  default:
    return Nothing;
  }
}

namespace {
class CopyForwarding {
  // Per-function state.
  PostOrderAnalysis *PostOrder;
  bool HasChanged;
  bool HasChangedCFG;

  // Transient state for the current Def.
  SILValue CurrentDef;
  bool HasForwardedToCopy;
  SmallPtrSet<SILInstruction*, 16> SrcUserInsts;
  SmallVector<CopyAddrInst*, 4> TakePoints;
  SmallVector<DestroyAddrInst*, 4> DestroyPoints;
  SmallPtrSet<SILBasicBlock*, 32> DeadInBlocks;
public:
  CopyForwarding(PostOrderAnalysis *PO):
    PostOrder(PO), HasChanged(false), HasChangedCFG(false),
    HasForwardedToCopy(false) {}

  void reset() {
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
  bool checkUsers();
  bool propagateCopy(CopyAddrInst *CopyInst);
  bool forwardPropagateCopy(CopyAddrInst *CopyInst);
  bool backwardPropagateCopy(CopyAddrInst *CopyInst);
  bool hoistDestroy(SILInstruction *DestroyPoint, SILLocation DestroyLoc);
};
} // namespace

/// Visit all uses of CurrentDef. Return false if any unsafe uses are found.
/// Safe uses are those that access the CurrentDef without capturing or exposing
/// it to any other instructions.
///
/// As a side-effect, record DestroyPoints and TakePoints.
///
/// TODO: This should be guaranteed to return true if we can assume that
/// address-only types are always accessed by-value.
bool CopyForwarding::checkUsers() {
  for (auto UI : CurrentDef.getUses()) {
    SILInstruction *UserInst = UI->getUser();
    if (auto *Apply = dyn_cast<ApplyInst>(UserInst)) {
      auto Params = Apply->getSubstCalleeType()->getParameters();
      (void)Params;
      // TODO: Provide and ApplyInst API for this.
      assert(UI->getOperandNumber() >= 1 && "calling copy_addr operand");
      assert(Params[UI->getOperandNumber()-1].isIndirect() &&
             "copy_addr location should be passed as indeirect");
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
    case ValueKind::ProtocolMethodInst:
    case ValueKind::StoreInst:
    case ValueKind::InitEnumDataAddrInst:
    case ValueKind::DebugValueAddrInst:
      SrcUserInsts.insert(UserInst);
      break;
    default:
      // TODO: we could peak through struct element users like COWArrayOpts.
      // ProjectExistentialInst
      // StructElementAddrInst
      DEBUG(llvm::dbgs() << "  Skipping copy: use exposes def " << *UserInst);
      return false;
    }
  }
  return true;
}

/// If the copy's dest is an @out argument, make no attempt to forward
/// propagate, because it will fail.
bool CopyForwarding::propagateCopy(CopyAddrInst *CopyInst) {
  SILValue CopyDest = CopyInst->getDest();
  if (!isa<SILArgument>(CopyDest.getDef()))
    if (forwardPropagateCopy(CopyInst))
      return true;
  return backwardPropagateCopy(CopyInst);
}

/// Perform copy-propagation. Find a set of uses that the given copy can forward
/// to and replace them with the copy's source.
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
/// TODO: If the copy's dest is an @out argument, or if we fail to find a deinit
/// in the local scan, then propagate backward instead of forward by rewriting
/// the copy source's reaching "init" to initialize the copy's dest instead.
bool CopyForwarding::forwardPropagateCopy(CopyAddrInst *CopyInst) {
  SILValue CopyDest = CopyInst->getDest();
  SILBasicBlock *BB = CopyInst->getParent();
  // Gather a list of CopyDest users in this block.
  SmallPtrSet<SILInstruction*, 16> DestUserInsts;
  for (auto UI : CopyDest.getUses()) {
    if (UI->getUser()->getParent() == BB)
      DestUserInsts.insert(UI->getUser());
  }
  // Scan forward recording all operands that use CopyDest until we see the
  // next deinit of CopyDest.
  SmallVector<Operand*, 16> ValueUses;
  bool seenDeinit = false;
  SILBasicBlock::iterator SI = CopyInst, SE = CopyInst->getParent()->end();
  for (++SI; SI != SE; ++SI) {
    // If we see another use of Src, then the source location is reinitialized
    // before the Dest location is deinitialized. So we really need the copy.
    if (SrcUserInsts.count(&*SI)) {
      DEBUG(llvm::dbgs() << "  Skipping copy " << *CopyInst
            << ", source used by " << *SI);
      return false;
    }
    // Early check to avoid scanning unrelated instructions.
    if (!DestUserInsts.count(&*SI))
      continue;

    for (auto &Oper : SI->getAllOperands()) {
      if (Oper.get() == CopyDest) {
        ValueUses.push_back(&Oper);
        // If we see a deinit we're done searching.
        // If we can't be sure, then abort.
        // Otherwise continue searching for uses.
        if (Optional<bool> IsDeinit = isDeinit(&Oper))
          seenDeinit |= *IsDeinit;
        else
          return false;
      }
    }
    if (seenDeinit)
      break;
  }
  if (!seenDeinit)
    return false;

  // Now that a deinit was found, it is safe to substitute all recorded uses
  // with the copy's source.
  for (auto *Oper : ValueUses) {
    Oper->set(CopyInst->getSrc());
    if (isa<CopyAddrInst>(Oper->getUser()))
      HasForwardedToCopy = true;
  }
  return true;
}

bool CopyForwarding::backwardPropagateCopy(CopyAddrInst *CopyInst) {
  // FIXME: unimplemented. scan backward until we find all reaching
  // inits. Replace all references to the copy's source with the copy's dest.
  DEBUG(llvm::dbgs() << "  Backward propagating " << *CopyInst);

  SILValue CopySrc = CopyInst->getSrc();
  SILValue CopyDest = CopyInst->getDest();
  SILBasicBlock *BB = CopyInst->getParent();
  // Gather a list of CopyDest users in this block.
  SmallPtrSet<SILInstruction*, 16> DestUserInsts;
  for (auto UI : CopyDest.getUses()) {
    if (UI->getUser()->getParent() == BB)
      DestUserInsts.insert(UI->getUser());
  }

  // Scan backward recording all operands that use CopySrc until we see the
  // most recent init of CopySrc.
  SmallVector<Operand*, 16> ValueUses;
  bool seenInit = false;
  SILBasicBlock::iterator SI = CopyInst, SE = CopyInst->getParent()->begin();
  while (SI != SE) {
    --SI;
    // If we see another use of Dest, then the dest location is deinitialized
    // after the Src location is initialized. So we really need the copy.
    if (DestUserInsts.count(&*SI)) {
      DEBUG(llvm::dbgs() << "  Skipping copy " << *CopyInst
            << ", dest used by " << *SI);
      return false;
    }
    // Early check to avoid scanning unrelated instructions.
    if (!SrcUserInsts.count(&*SI))
      continue;

    for (auto &Oper : SI->getAllOperands()) {
      if (Oper.get() == CopySrc) {
        ValueUses.push_back(&Oper);
        // If we see a deinit we're done searching.
        // If we can't be sure, then abort.
        // Otherwise continue searching for uses.
        if (Optional<bool> IsInit = isInit(&Oper))
          seenInit |= *IsInit;
        else
          return false;
      }
    }
    if (seenInit)
      break;
  }
  if (!seenInit)
    return false;

  // Now that a deinit was found, it is safe to substitute all recorded uses
  // with the copy's source.
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
      if (!IsWorthHoisting) {
        if (const ApplyInst *AI = dyn_cast<ApplyInst>(Inst))
          if (!isa<BuiltinFunctionRefInst>(AI->getCallee()))
            IsWorthHoisting = true;
      }
      continue;
    }
    // We reached a user of CurrentDef. If we haven't seen anything significant,
    // avoid useless hoisting.
    if (!IsWorthHoisting)
      return false;

    if (auto *CopyInst = dyn_cast<CopyAddrInst>(Inst)) {
      if (!CopyInst->isTakeOfSrc() && CopyInst->isInitializationOfDest() &&
          CopyInst->getSrc() == CurrentDef) {
        // This use is a copy of CurrentDef. Attempt to forward CurrentDef to
        // all uses of the copy's value.
        if (propagateCopy(CopyInst)) {
          DEBUG(llvm::dbgs() << "  Forwarding Copy: " << *CopyInst);
          CopyInst->eraseFromParent();
          HasChanged = true;
          return true;
        }
      }
    }
    DEBUG(llvm::dbgs() << "  Hoisting to Use: " << *Inst);
    SILBuilder(std::next(SI)).createDestroyAddr(DestroyLoc, CurrentDef);
    HasChanged = true;
    return true;
  }
  DeadInBlocks.insert(BB);
  return true;
}

/// Perform CopyForwarding on the current Def.
void CopyForwarding::forwardCopiesOf(SILValue Def, SILFunction *F) {
  reset();
  CurrentDef = Def;
  DEBUG(llvm::dbgs() << "Analyzing copies of Def: " << Def);
  if (!checkUsers())
    return;

  // First forward any copies that implicitly destroy CurrentDef. There is no
  // need to hoist Destroy for these.
  for (auto *CopyInst : TakePoints) {
    if (propagateCopy(CopyInst)) {
      DEBUG(llvm::dbgs() << "  Forwarding Copy: " << *CopyInst);
      CopyInst->eraseFromParent();
      HasChanged = true;
    }
  }
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
      SILBuilder(SuccBB->begin()).createDestroyAddr(DestroyLoc, CurrentDef);
      HasChanged = true;
    }
  }
}

namespace {
class CopyForwardingPass : public SILFunctionTransform
{
  void run() override {
    if (!EnableCopyForwarding)
      return;

    DEBUG(llvm::dbgs() << "Copy Forwarding in Func " << getFunction()->getName()
          << "\n");

    // Collect a set of identified objects (@in arg or alloc_stack) that are
    // copied in this function.
    llvm::SmallSetVector<SILValue, 16> CopiedDefs;
    for (auto &BB : *getFunction())
      for (auto II = BB.begin(), IE = BB.end(); II != IE; ++II)
        if (auto *CopyInst = dyn_cast<CopyAddrInst>(&*II))
          if (CopyInst->isInitializationOfDest()) {
            SILValue Def = CopyInst->getSrc();
            if (isIdentifiedObject(Def, getFunction()))
              CopiedDefs.insert(Def);
          }

    if (CopiedDefs.empty())
      return;

    auto *PO = getAnalysis<PostOrderAnalysis>();
    auto Forwarding = CopyForwarding(PO);

    for (SILValue Def : CopiedDefs) {
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
