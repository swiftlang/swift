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
#include "swift/SILAnalysis/DominanceAnalysis.h"
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
    auto Params = F->getLoweredFunctionType()->getParameters();
    ArrayRef<SILArgument*> FunctionArgs = F->begin()->getBBArgs();
    for (unsigned ArgIdx = 0, ArgEnd = Params.size();
         ArgIdx != ArgEnd; ++ArgIdx) {
      if (FunctionArgs[ArgIdx] != Arg)
        continue;

      if (Params[ArgIdx].getConvention() != ParameterConvention::Indirect_In) {
        DEBUG(llvm::dbgs() << "  Skipping Def: Not an @in argument!\n");
        return false;
      }
    }
    return true;
  }
  else if (isa<AllocStackInst>(Def))
    return true;

  DEBUG(llvm::dbgs()
        << "  Skipping Def: Not an argument or local variable!\n");
  return false;
}

namespace {
class CopyForwarding {
  // Per-function state.
  PostOrderAnalysis *PostOrder;
  DominanceInfo *DomTree;
  bool HasChanged;
  bool HasChangedCFG;

  // Transient state for the current Def.
  SILValue CurrentDef;
  bool HasForwardedToCopy;
  SmallPtrSet<SILInstruction*, 16> UserInsts;
  SmallVector<DestroyAddrInst*, 4> DestroyPoints;
  SmallPtrSet<SILBasicBlock*, 32> DeadInBlocks;
public:
  CopyForwarding(PostOrderAnalysis *PO, DominanceInfo *DT):
    PostOrder(PO), DomTree(DT), HasChanged(false), HasChangedCFG(false),
    HasForwardedToCopy(false) {}

  void reset() {
    CurrentDef = SILValue();
    HasForwardedToCopy = false;
    UserInsts.clear();
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
  void forwardCopy(CopyAddrInst *CopyInst);
  bool hoistDestroy(SILInstruction *DestroyPoint, SILLocation DestroyLoc);
};
} // namespace

/// Safe uses are those that access the CurrentDef without capturing or exposing
/// it to any other instructions.
bool CopyForwarding::checkUsers() {
  for (auto UI : CurrentDef.getUses()) {
    SILInstruction *UserInst = UI->getUser();
    switch (UserInst->getKind()) {
    case ValueKind::CopyAddrInst:
    case ValueKind::DebugValueAddrInst:
      UserInsts.insert(UserInst);
      break;
    case ValueKind::DestroyAddrInst:
      DestroyPoints.push_back(cast<DestroyAddrInst>(UserInst));
      break;
    default:
      DEBUG(llvm::dbgs() << "  Skipping copy: use exposes def " << *UserInst);
      return false;
    }
  }
  return true;
}

/// Find a set of uses that the given copy can forward to and replace them with
/// the copy's source.
void CopyForwarding::forwardCopy(CopyAddrInst *CopyInst) {
  SmallVector<Operand*, 16> Uses;
  for (auto UI : CopyInst->getDest().getUses()) {
    if (DomTree->properlyDominates(CopyInst, UI->getUser()))
      Uses.push_back(&*UI);
  }
  for (auto *Oper : Uses) {
    Oper->set(CopyInst->getSrc());
    if (isa<CopyAddrInst>(Oper->getUser()))
      HasForwardedToCopy = true;
  }
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
  assert(!UserInsts.count(DestroyPoint) && "caller should check terminator");
  SILBasicBlock *BB = DestroyPoint->getParent();

  // If DestroyPoint is a block terminator, we must hoist.
  bool MustHoist = (DestroyPoint == BB->getTerminator());

  bool IsWorthHoisting = MustHoist;
  SILBasicBlock::iterator SI = DestroyPoint, SE = BB->begin();
  while (SI != SE) {
    --SI;
    SILInstruction *Inst = &*SI;
    if (!UserInsts.count(Inst)) {
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

    HasChanged = true;
    if (auto *CopyInst = dyn_cast<CopyAddrInst>(Inst)) {
      if (!CopyInst->isTakeOfSrc() && CopyInst->isInitializationOfDest() &&
          CopyInst->getSrc() == CurrentDef) {
        // This use is a copy of CurrentDef. Attempt to forward CurrentDef to
        // all dominated uses of the copy's dest.
        DEBUG(llvm::dbgs() << "  Forwarding Copy: " << *CopyInst);
        forwardCopy(CopyInst);
        CopyInst->eraseFromParent();
        return true;
      }
    }
    DEBUG(llvm::dbgs() << "  Hoisting to Use: " << *Inst);
    SILBuilder(std::next(SI)).createDestroyAddr(DestroyLoc, CurrentDef);
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
        !UserInsts.count(BB->getTerminator())) {
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
          if (!CopyInst->isTakeOfSrc() && CopyInst->isInitializationOfDest()) {
            SILValue Def = CopyInst->getSrc();
            if (isIdentifiedObject(Def, getFunction()))
              CopiedDefs.insert(Def);
          }

    if (CopiedDefs.empty())
      return;

    auto *PO = getAnalysis<PostOrderAnalysis>();
    auto *DA = getAnalysis<DominanceAnalysis>();
    auto *DT = DA->getDomInfo(getFunction());
    auto Forwarding = CopyForwarding(PO, DT);

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
