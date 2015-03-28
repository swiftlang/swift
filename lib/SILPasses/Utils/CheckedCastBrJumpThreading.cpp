#define DEBUG_TYPE "sil-simplify-cfg"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILAnalysis/DominanceAnalysis.h"
#include "swift/SILPasses/Utils/CFG.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SILPasses/Utils/SILInliner.h"

using namespace swift;

namespace {
/// This is a class implementing a dominator-based jump-threading
/// for checked_cast_br [exact].
class CheckedCastBrJumpThreading {
  // The checked_cast_br instruction, which
  // we try to jump-thread
  CheckedCastBranchInst *CCBI;
  // Basic block of the current checked_cast_br instruction.
  SILBasicBlock *BB;
  // Condition used by the current checked_cast_br instruction.
  SILValue Condition;
  // Success branch of the current checked_cast_br instruction.
  SILBasicBlock *SuccessBB;
  // Failure branch of the current checked_cast_br instruction.
  SILBasicBlock *FailureBB;


  // Current dominating checked_cast_br instruction.
  CheckedCastBranchInst *DomCCBI;
  // Basic block of the dominating checked_cast_br instruction.
  SILBasicBlock *DomBB;
  // Condition used by the dominating checked_cast_br instruction.
  SILValue DomCondition;
  // Success branch of the dominating checked_cast_br instruction.
  SILBasicBlock *DomSuccessBB;
  // Failure branch of the dominating checked_cast_br instruction.
  SILBasicBlock *DomFailureBB;


  // Current dominator tree node where we look for a dominating
  // checked_cast_br instruction.
  llvm::DomTreeNodeBase<SILBasicBlock> *Node;
  SILBasicBlock *ArgBB;
  // Dominator information to be used.
  DominanceInfo *DT;
  // Basic block created as a landing BB for all failure predecessors.
  SILBasicBlock *TargetFailureBB;
  // Basic block created as a landing BB for all success predecessors.
  SILBasicBlock *TargetSuccessBB;

  // Cloner used to clone the BB to FailureSuccessBB.
  Optional<BasicBlockCloner> FailureBBCloner;

  // Cloner used to clone the BB to TargetSuccessBB.
  Optional<BasicBlockCloner> SuccessBBCloner;

  // Predecessors reached only via a path along the
  // success branch of the dominating checked_cast_br.
  SmallVector<SILBasicBlock *, 8> SuccessPreds;

  // Predecessors reached only via a path along the
  // failure branch of the dominating checked_cast_br.
  SmallVector<SILBasicBlock *, 8> FailurePreds;

  // All other predecessors, where the outcome of the
  // checked_cast_br along the path is not known.
  SmallVector<SILBasicBlock *, 8> UnknownPreds;

  // Basic blocks to be added to for reprocessing
  // after jump-threading is done.
  SmallVectorImpl<SILBasicBlock *> &BlocksForWorklist;

  bool areEquivalentConditionsAlongPaths();
  bool areEquivalentConditionsAlongSomePaths();
  bool handleArgBBIsEntryBlock(SILBasicBlock *ArgBB);
  bool checkCloningConstraints();
  void modifyCFGForUnknownPreds();
  void modifyCFGForFailurePreds();
  void modifyCFGForSuccessPreds();
  void updateDominatorTree();
  void updateSSA();
  void addBlockToSimplifyCFGWorklist(SILBasicBlock *BB);
  void addBlocksToWorklist();

  void classifyPredecessor(
      SILBasicBlock *Pred,
      SmallVectorImpl<SILBasicBlock *> &SuccessPreds,
      SmallVectorImpl<SILBasicBlock *> &FailurePreds,
      SmallVectorImpl<SILBasicBlock *> &UnknownPreds,
      bool SuccessDominates,
      bool FailureDominates);

  SILValue isArgValueEquivalentToCondition(SILValue Value,
                               SILBasicBlock *DomBB,
                               SILValue DomValue,
                               DominanceInfo *DT);

public:
  CheckedCastBrJumpThreading(DominanceInfo *DT,
                             SmallVectorImpl<SILBasicBlock *> &BBs)
      : DT(DT), BlocksForWorklist(BBs) { }

  bool trySimplify(TermInst *Term);

  ArrayRef<SILBasicBlock*> getBlocksForWorklist() {
    return BlocksForWorklist;
  }
};
} // end anonymous namespace

/// Find a nearest common dominator for a given set of basic blocks.
static DominanceInfoNode *findCommonDominator(ArrayRef<SILBasicBlock *> BBs,
                                              DominanceInfo *DT) {
  DominanceInfoNode *CommonDom = nullptr;
  for (auto *BB : BBs) {
    if (!CommonDom) {
      CommonDom = DT->getNode(BB);
    } else {
      CommonDom = DT->getNode(
          DT->findNearestCommonDominator(CommonDom->getBlock(), BB));
    }
  }
  return CommonDom;
}

/// Find a nearest common dominator for all predecessors of
/// a given basic block.
static DominanceInfoNode *findCommonDominator(SILBasicBlock *BB,
                                              DominanceInfo *DT) {
  SmallVector<SILBasicBlock *, 8> Preds;
  for (auto *Pred: BB->getPreds())
    Preds.push_back(Pred);

  return findCommonDominator(Preds, DT);
}

/// Estimate the cost of inlining a given basic block.
static unsigned basicBlockInlineCost(SILBasicBlock *BB, unsigned Cutoff) {
  unsigned Cost = 0;
  for (auto &I : *BB) {
    auto ICost = instructionInlineCost(I);
    Cost += unsigned(ICost);
    if (Cost > Cutoff)
      return Cost;
  }
  return Cost;
}

/// We can not duplicate blocks with AllocStack instructions (they need to be
/// FIFO). Other instructions can be duplicated.
static bool canDuplicateBlock(SILBasicBlock *BB) {
  for (auto &I : *BB) {
    if (!I.isTriviallyDuplicatable())
      return false;
  }
  return true;
}

void CheckedCastBrJumpThreading::addBlockToSimplifyCFGWorklist(SILBasicBlock *BB) {
  BlocksForWorklist.push_back(BB);
}

/// Add affected blocks for re-processing by simplifyCFG
void CheckedCastBrJumpThreading::addBlocksToWorklist() {
  if (TargetFailureBB) {
    if (!TargetFailureBB->pred_empty())
      addBlockToSimplifyCFGWorklist(TargetFailureBB);
  }

  if (TargetSuccessBB) {
    if (!TargetSuccessBB->pred_empty())
      addBlockToSimplifyCFGWorklist(TargetSuccessBB);
  }

  if (!BB->pred_empty())
    addBlockToSimplifyCFGWorklist(BB);
}

/// Classify a predecessor of a BB containing checked_cast_br as being
/// reachable via success or failure branches of a dominating checked_cast_br
/// or as unknown if it can be reached via success or failure branches
/// at the same time.
void CheckedCastBrJumpThreading::classifyPredecessor(
    SILBasicBlock *Pred,
    SmallVectorImpl<SILBasicBlock *> &SuccessPreds,
    SmallVectorImpl<SILBasicBlock *> &FailurePreds,
    SmallVectorImpl<SILBasicBlock *> &UnknownPreds,
    bool SuccessDominates,
    bool FailureDominates) {
  if (SuccessDominates && FailureDominates) {
    UnknownPreds.push_back(Pred);
    return;
  }

  if (SuccessDominates) {
    SuccessPreds.push_back(Pred);
    return;
  }

  if (FailureDominates) {
    FailurePreds.push_back(Pred);
    return;
  }

  UnknownPreds.push_back(Pred);
}

/// Check if the root value for Value that comes
/// along the path from DomBB is equivalent to the
/// DomCondition.
SILValue CheckedCastBrJumpThreading::isArgValueEquivalentToCondition(
    SILValue Value, SILBasicBlock *DomBB, SILValue DomValue,
    DominanceInfo *DT) {
  SmallPtrSet<ValueBase *, 16> SeenValues;
  DomValue = DomValue.stripClassCasts();

  while (true) {
    Value = Value.stripClassCasts();
    if (Value == DomValue)
      return Value;

    // We know how to propagate through BBArgs only.
    auto *V = dyn_cast<SILArgument>(Value);
    if (!V)
      return SILValue();

    // Have we visited this BB already?
    if (!SeenValues.insert(Value.getDef()).second)
      return SILValue();

    if (SeenValues.size() > 10)
      return SILValue();

    SmallVector<SILValue, 4> IncomingValues;
    if (!V->getIncomingValues(IncomingValues) || IncomingValues.empty())
      return SILValue();

    ValueBase *Def = nullptr;
    for (auto IncomingValue : IncomingValues) {
      // Each incoming value should be either from a block
      // dominated by DomBB or it should be the value used in
      // condition in DomBB
      Value = IncomingValue.stripClassCasts();
      if (Value == DomValue)
        continue;

      // Values should be the same
      if (!Def)
        Def = Value.getDef();

      if (Def != Value.getDef())
        return SILValue();

      if (!DT->dominates(DomBB, Value.getDef()->getParentBB()))
        return SILValue();
      // OK, this value is a potential candidate
    }

    Value = IncomingValues[0];
  }
}

/// Update the SSA form after all changes.
void CheckedCastBrJumpThreading::updateSSA() {
  assert(!(SuccessBBCloner.hasValue() && FailureBBCloner.hasValue()) &&
         "Both cloners cannot be used at the same time yet");

  // Now update the SSA form.
  if (!FailurePreds.empty() && FailureBBCloner.hasValue() &&
      !SuccessBBCloner.hasValue())
    updateSSAAfterCloning(*FailureBBCloner.getPointer(), TargetFailureBB, BB);

  if (SuccessBBCloner.hasValue() && !FailureBBCloner.hasValue()) {
    updateSSAAfterCloning(*SuccessBBCloner.getPointer(), TargetSuccessBB, BB);
  }
}

/// Update the SSA form after all changes.
void CheckedCastBrJumpThreading::updateDominatorTree() {
  // Update the dominator tree.

  // If BB was IDom of something, then PredCBBI becomes the IDOM
  // of this after jump-threading.
  auto *BBDomNode = DT->getNode(BB);
  auto &Children = BBDomNode->getChildren();
  if (Children.size() > 1) {
    SmallVector<DominanceInfoNode *, 16> ChildrenCopy;
    std::copy(Children.begin(), Children.end(),
              std::back_inserter(ChildrenCopy));
    for (auto *Child : ChildrenCopy) {
      DT->changeImmediateDominator(Child, Node);
    }
  }

  DominanceInfoNode *CommonDom;

  // Find a common dominator for all unknown preds.
  if (!UnknownPreds.empty()) {
    // Find a new IDom for FailureBB
    CommonDom = findCommonDominator(FailureBB, DT);
    if (CommonDom)
      DT->changeImmediateDominator(FailureBB, CommonDom->getBlock());

    CommonDom = findCommonDominator(UnknownPreds, DT);
    // This common dominator dominates the BB now.
    if (CommonDom) {
      DT->changeImmediateDominator(BB, CommonDom->getBlock());
    }
  }

  // Find a common dominator for all failure preds.
  CommonDom = findCommonDominator(FailurePreds, DT);
  // This common dominator dominates the TargetFailureBB now.
  if (CommonDom) {
    DT->addNewBlock(TargetFailureBB, CommonDom->getBlock());
    // Find a new IDom for FailureBB
    CommonDom = findCommonDominator(FailureBB, DT);
    if (CommonDom)
      DT->changeImmediateDominator(FailureBB, CommonDom->getBlock());
  }


  // Find a common dominator for all success preds.
  CommonDom = findCommonDominator(SuccessPreds, DT);
  // This common dominator of all success preds dominates the BB now.
  if (CommonDom) {
    if (TargetSuccessBB) {
      DT->addNewBlock(TargetSuccessBB, CommonDom->getBlock());
    } else {
      DT->changeImmediateDominator(BB, CommonDom->getBlock());
    }
    CommonDom = findCommonDominator(SuccessBB, DT);
    if (CommonDom)
      DT->changeImmediateDominator(SuccessBB, CommonDom->getBlock());
  }
  // End of dominator tree update.
}

void CheckedCastBrJumpThreading::modifyCFGForUnknownPreds() {
  if (UnknownPreds.empty())
    return;
  // Check the FailureBB if it is a BB that contains a class_method
  // referring to the same value as a condition. This pattern is typical
  // for method chaining code like obj.method1().method2().etc()
  SILInstruction *Inst = FailureBB->begin();
  if (ClassMethodInst *CMI = dyn_cast<ClassMethodInst>(Inst)) {
    if (CMI->getOperand() == Condition) {
      // Replace checked_cast_br by branch to FailureBB.
      auto &InsnList = BB->getInstList();
      auto *InsertedBI =
          BranchInst::create(CCBI->getLoc(), FailureBB, *BB->getParent());
      CCBI->eraseFromParent();
      InsnList.insert(InsnList.end(), InsertedBI);
    }
  }
}

/// Create a copy of the BB as a landing BB
/// for all FailurePreds.
void CheckedCastBrJumpThreading::modifyCFGForFailurePreds() {
  if (FailurePreds.empty())
    return;

  FailureBBCloner.emplace(BasicBlockCloner(BB));
  FailureBBCloner->clone();
  TargetFailureBB = FailureBBCloner->getDestBB();
  auto *TI = TargetFailureBB->getTerminator();
  SILBuilderWithScope<1> Builder(TI);
  // This BB copy branches to a FailureBB.
  Builder.createBranch(TI->getLoc(), FailureBB);
  TI->eraseFromParent();

  // Redirect all FailurePreds to the copy of BB.
  for (auto *Pred : FailurePreds) {
    TermInst *TI = Pred->getTerminator();
    // Replace branch to BB by branch to TargetFailureBB.
    // FIXME: Why is it valid to assume EdgeIdx=0 here?
    changeBranchTarget(TI, 0, TargetFailureBB, /*PreserveArgs=*/true);
    Pred = nullptr;
  }
}

/// Create a copy of the BB or reuse BB as
/// a landing basic block for all FailurePreds.
void CheckedCastBrJumpThreading::modifyCFGForSuccessPreds() {
  if (!UnknownPreds.empty()) {
    if (!SuccessPreds.empty()) {
      // Create a copy of the BB as a landing BB.
      // for all SuccessPreds.
      SuccessBBCloner.emplace(BasicBlockCloner(BB));
      SuccessBBCloner->clone();
      TargetSuccessBB = SuccessBBCloner->getDestBB();
      auto *TI = TargetSuccessBB->getTerminator();
      SILBuilderWithScope<1> Builder(TI);
      SmallVector<SILValue, 8> SuccessBBArgs;
      // Take argument value from the dominating BB.
      SuccessBBArgs.push_back(DomSuccessBB->getBBArg(0));
      // This BB copy branches to SuccessBB.
      Builder.createBranch(TI->getLoc(), SuccessBB, SuccessBBArgs);
      TI->eraseFromParent();

      // Redirect all SuccessPreds to the copy of BB.
      for (auto *Pred : SuccessPreds) {
        TermInst *TI = Pred->getTerminator();
        // Replace branch to BB by branch to TargetSuccessBB.
        // FIXME: Why is it valid to assume EdgeIdx=0 here?
        changeBranchTarget(TI, 0, TargetSuccessBB, /*PreserveArgs=*/true);
        SuccessBBArgs.push_back(DomSuccessBB->getBBArg(0));
        Pred = nullptr;
      }
    }
  } else {
    // There are no predecessors where it is not clear
    // if they are dominated by a success or failure branch
    // of DomBB. Therefore, there is no need to clone
    // the BB for SuccessPreds. Current BB can be re-used
    // instead as their target.

    // Add an unconditional jump at the end of the block.
    auto &InsnList = BB->getInstList();
    SmallVector<SILValue, 8> SuccessBBArgs;
    // Take argument value from the dominating BB
    SuccessBBArgs.push_back(DomSuccessBB->getBBArg(0));
    auto *InsertedBI = BranchInst::create(CCBI->getLoc(), SuccessBB,
                                          SuccessBBArgs, *BB->getParent());
    CCBI->eraseFromParent();
    InsnList.insert(InsnList.end(), InsertedBI);
  }
}

/// Handle a special case, where ArgBB is the entry block.
bool CheckedCastBrJumpThreading::handleArgBBIsEntryBlock(SILBasicBlock *ArgBB) {
  if (ArgBB->getPreds().empty()) {
    // It must be the entry block
    // See if it is reached over Success or Failure path.
    bool SuccessDominates = DomSuccessBB == BB;
    bool FailureDominates = DomFailureBB == BB;

    classifyPredecessor(ArgBB, SuccessPreds, FailurePreds, UnknownPreds,
                        SuccessDominates, FailureDominates);
    return true;
  }
  return false;
}

// Returns false if cloning required by jump threading cannot
// be performed, because some of the constraints are violated.
bool CheckedCastBrJumpThreading::checkCloningConstraints() {
  // Check some cloning related constraints.

  // If this argument from a different BB, then jump-threading
  // may require too much code duplication.
  if (ArgBB && ArgBB != BB)
    return false;

  // Bail out if current BB cannot be duplicated.
  if (!canDuplicateBlock(BB))
    return false;

  // Check if code-bloat would be too big when this BB
  // is jump-threaded.
  // TODO: Make InlineCostCutoff parameter configurable?
  // Dec 1, 2014:
  // We looked at the inline costs of BBs from our benchmark suite
  // and found that currently the highest inline cost for the
  // whole benchmark suite is 12. In 95% of all cases it is <=3.
  const unsigned InlineCostCutoff = 20;
  if (basicBlockInlineCost(BB, InlineCostCutoff) >= InlineCostCutoff)
    return false;

  return true;
}

/// If conditions are not equivalent along all paths, try harder
/// to check if they are actually equivalent along a subset of paths.
/// To do it, try to back-propagate the Condition
/// backwards and see if it is actually equivalent to DomCondition.
/// along some of the paths.
bool CheckedCastBrJumpThreading::areEquivalentConditionsAlongSomePaths() {
  auto *Arg = dyn_cast<SILArgument>(Condition);
  if (!Arg)
    return false;

  ArgBB = Arg->getParent();
  if (!DT->dominates(DomBB, ArgBB))
    return false;

  // Incoming values for the BBArg.
  SmallVector<SILValue, 4> IncomingValues;

  if (ArgBB != ArgBB->getParent()->begin() &&
      (!Arg->getIncomingValues(IncomingValues) || IncomingValues.empty()))
    return false;

  // Check for each predecessor, if the incoming value coming from it
  // is equivalent to the DomCondition. If this is the case, it is
  // possible to try jump-threading along this path.
  if (!handleArgBBIsEntryBlock(ArgBB)) {
    // ArgBB is not the entry block and has predecessors.
    unsigned idx = 0;
    for (auto *PredBB : ArgBB->getPreds()) {
      auto IncomingValue = IncomingValues[idx];
      SILValue ReachingValue = isArgValueEquivalentToCondition(
          IncomingValue, DomBB, DomCondition, DT);

      if (ReachingValue == SILValue()) {
        UnknownPreds.push_back(PredBB);
        idx++;
        continue;
      }

      // Condition is the same if BB is reached over a pass through Pred.
      DEBUG(llvm::dbgs() << "Condition is the same if reached over ");
      DEBUG(PredBB->print(llvm::dbgs()));

      // See if it is reached over Success or Failure path.
      bool SuccessDominates = DT->dominates(DomSuccessBB, PredBB) ||
                              DT->dominates(DomSuccessBB, BB) ||
                              DomSuccessBB == BB;
      bool FailureDominates = DT->dominates(DomFailureBB, PredBB) ||
                              DT->dominates(DomFailureBB, BB) ||
                              DomFailureBB == BB;

      classifyPredecessor(
          PredBB, SuccessPreds, FailurePreds, UnknownPreds,
          SuccessDominates, FailureDominates);
      idx++;
    }
  }


  // At this point we know for each predecessor of ArgBB if its reached
  // over the success, failure or unknown path from DomBB.

  // Now we can generate a new BB for preds reaching BB over the success
  // path and a new BB for preds reaching BB over the failure path.
  // Then we redirect those preds to those new basic blocks.
  return true;
}

/// Check if conditions of CCBI and DomCCBI are equivalent along
/// all or at least  some paths.
bool CheckedCastBrJumpThreading::areEquivalentConditionsAlongPaths() {
  // Are conditions equivalent along all paths?
  if (areEquivalentConditions(DomCondition, Condition)) {
    // Conditions are exactly the same, without any restrictions.
    // They are equivalent along all paths.

    // Figure out for each predecessor which branch of
    // the dominating checked_cast_br is used to reach it.
    for (auto *PredBB : BB->getPreds()) {
      // All predecessors should either unconditionally branch
      // to the current BB or be another checked_cast_br instruction.
      if (!dyn_cast<CheckedCastBranchInst>(PredBB->getTerminator()) &&
          !dyn_cast<BranchInst>(PredBB->getTerminator()))
        return false;

      bool SuccessDominates =
          DT->dominates(DomSuccessBB, PredBB) || DomSuccessBB == BB;
      bool FailureDominates =
          DT->dominates(DomFailureBB, PredBB) || DomFailureBB == BB;

      classifyPredecessor(PredBB, SuccessPreds, FailurePreds, UnknownPreds,
                          SuccessDominates, FailureDominates);
    }
    return true;
  }

  // Check if conditions are equivalent along a subset of reaching paths.
  return areEquivalentConditionsAlongSomePaths();
}

/// Try performing a dominator-based jump-threading for
/// checked_cast_br instructions.
bool CheckedCastBrJumpThreading::trySimplify(TermInst *Term) {
  CCBI = cast<CheckedCastBranchInst>(Term);
  if (!CCBI)
    return false;

  // Init information about the checked_cast_br we try to
  // jump-thread.
  BB = Term->getParent();
  Condition = Term->getOperand(0).stripClassCasts();
  SuccessBB = CCBI->getSuccessBB();
  FailureBB = CCBI->getFailureBB();

  // Find a dominating checked_cast_br, which performs the same check.
  for (Node = DT->getNode(BB)->getIDom(); Node; Node = Node->getIDom()) {
    // Get current dominating block.
    DomBB = Node->getBlock();
    auto *DomTerm = DomBB->getTerminator();

    if (!DomTerm->getNumOperands())
      continue;

    // Check that it is a dominating checked_cast_br.
    DomCCBI = dyn_cast<CheckedCastBranchInst>(DomTerm);
    if (!DomCCBI)
      continue;

    // We need to verify that the result type is the same in the
    // dominating checked_cast_br.
    if (DomCCBI->getCastType() != CCBI->getCastType())
      continue;


    // Conservatively check that both checked_cast_br instructions
    // are either exact or non-exact. This is very conservative,
    // but safe.
    //
    // TODO:
    // If the dominating checked_cast_br is non-exact, then
    // it is in general not safe to assume that current exact cast
    // would have the same outcome. But if the the dominating
    // non-exact checked_cast_br fails, then the current exact cast
    // would always fail as well.
    //
    // If the dominating checked_cast_br is exact then then
    // it is in general not safe to assume that the current non-exact
    // cast would have the same outcome. But if the the dominating
    // exact checked_cast_br succeeds, then the current non-exact
    // cast would always succeed as well.
    //
    // TODO: In some specific cases, it is possible to prove that
    // success or failure of the dominating cast is equivalent to
    // the success or failure of the current cast, even if one
    // of them is exact and the other not. This is the case
    // e.g. if the class has no subclasses.
    if (DomCCBI->isExact() != CCBI->isExact())
      continue;

    // Initialize state variables for the current round of checks
    // based on the found dominating checked_cast_br.
    DomSuccessBB = DomCCBI->getSuccessBB();
    DomFailureBB = DomCCBI->getFailureBB();
    DomCondition = DomTerm->getOperand(0).stripClassCasts();

    // Init state variables for paths analysis
    SuccessPreds.clear();
    FailurePreds.clear();
    UnknownPreds.clear();
    ArgBB = nullptr;

    // Init state variables for jump-threading transformation.
    TargetFailureBB = nullptr;
    TargetSuccessBB = nullptr;

    // Are conditions of CCBI and DomCCBI equivalent along (some) paths?
    // If this is the case, classify all incoming paths into SuccessPreds,
    // FailurePreds or UnknownPreds depending on how they reach CCBI.
    if (!areEquivalentConditionsAlongPaths())
      continue;

    // Check if any jump-threding is required and possible.
    if (SuccessPreds.empty() && FailurePreds.empty())
      return false;

    unsigned TotalPreds =
        SuccessPreds.size() + FailurePreds.size() + UnknownPreds.size();

    // We only need to clone the BB if not all of its
    // predecessors are in the same group.
    if (TotalPreds != SuccessPreds.size() &&
        TotalPreds != UnknownPreds.size()) {
      // Check some cloning related constraints.
      if (!checkCloningConstraints())
        return false;
    }

    // If we have predecessors, where it is not known if they are reached over
    // success or failure path, we cannot eliminate a checked_cast_br.
    // We have to generate new dedicated BBs as landing BBs for all
    // FailurePreds and all SuccessPreds.

    // Since we are going to change the BB,
    // add its successors and predecessors
    // for re-processing.
    for (auto *B : BB->getPreds()) {
      addBlockToSimplifyCFGWorklist(B);
    }

    for (auto &B : BB->getSuccessors()) {
      addBlockToSimplifyCFGWorklist(B.getBB());
    }

    // Create a copy of the BB as a landing BB
    // for all FailurePreds.
    modifyCFGForFailurePreds();

    // Create a copy of the BB or reuse BB as
    // a landing basic block for all SuccessPreds.
    modifyCFGForSuccessPreds();

    // Handle unknown preds.
    modifyCFGForUnknownPreds();

    // Update the dominator tree after all changes.
    updateDominatorTree();

    // Update the SSA form after all changes.
    updateSSA();

    // Since a few BBs were changed now, add them for re-processing.
    addBlocksToWorklist();

    return true;
  }

  // Jump-threading was not possible.
  return false;
}

namespace swift {

bool tryCheckedCastBrJumpThreading(TermInst *Term, DominanceInfo *DT,
                                   SmallVectorImpl<SILBasicBlock *> &BBs) {
  CheckedCastBrJumpThreading CCBJumpThreading(DT, BBs);
  return CCBJumpThreading.trySimplify(Term);
}

} // end namespace swift
