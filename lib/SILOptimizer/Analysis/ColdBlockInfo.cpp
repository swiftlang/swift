//===--- ColdBlockInfo.cpp - Fast/slow path analysis for the SIL CFG ------===//
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

#include "swift/SILOptimizer/Analysis/ColdBlockInfo.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SIL/SILArgument.h"

using namespace swift;

/// Peek through an extract of Bool.value.
static SILValue getCondition(SILValue C) {
  if (auto *SEI = dyn_cast<StructExtractInst>(C)) {
    if (auto *Struct = dyn_cast<StructInst>(SEI->getOperand()))
      return Struct->getFieldValue(SEI->getField());
    return SEI->getOperand();
  }
  return C;
}

/// \return a BranchHint if this call is a builtin branch hint.
ColdBlockInfo::BranchHint ColdBlockInfo::getBranchHint(SILValue Cond,
                                                       int recursionDepth) {
  // Handle the fully inlined Builtin.
  if (auto *BI = dyn_cast<BuiltinInst>(Cond)) {
    if (BI->getIntrinsicInfo().ID == llvm::Intrinsic::expect) {
      // peek through an extract of Bool.value.
      SILValue ExpectedValue = getCondition(BI->getArguments()[1]);
      if (auto *Literal = dyn_cast<IntegerLiteralInst>(ExpectedValue)) {
        return (Literal->getValue() == 0)
          ? BranchHint::LikelyFalse : BranchHint::LikelyTrue;
      }
    }
    return BranchHint::None;
  }

  if (auto *Arg = dyn_cast<SILArgument>(Cond)) {
    llvm::SmallVector<std::pair<SILBasicBlock *, SILValue>, 4> InValues;
    if (!Arg->getIncomingValues(InValues))
      return BranchHint::None;

    if (recursionDepth > RecursionDepthLimit)
      return BranchHint::None;

    BranchHint Hint = BranchHint::None;

    // Check all predecessor values which come from non-cold blocks.
    for (auto Pair : InValues) {
      if (isCold(Pair.first, recursionDepth + 1))
        continue;

      auto *IL = dyn_cast<IntegerLiteralInst>(Pair.second);
      if (!IL)
        return BranchHint::None;
      // Check if we have a consistent value for all non-cold predecessors.
      if (IL->getValue().getBoolValue()) {
        if (Hint == BranchHint::LikelyFalse)
          return BranchHint::None;
        Hint = BranchHint::LikelyTrue;
      } else {
        if (Hint == BranchHint::LikelyTrue)
          return BranchHint::None;
        Hint = BranchHint::LikelyFalse;
      }
    }
    return Hint;
  }
  
  // Handle the @semantic function used for branch hints. The generic
  // fast/slowPath calls are frequently only inlined one level down to
  // _branchHint before inlining the call sites that they guard.
  auto AI = dyn_cast<ApplyInst>(Cond);
  if (!AI)
    return BranchHint::None;

  if (auto *F = AI->getReferencedFunction()) {
    if (F->hasSemanticsAttrs()) {
      if (F->hasSemanticsAttr("branchhint")) {
        // A "branchint" model takes a Bool expected value as the second
        // argument.
        if (auto *SI = dyn_cast<StructInst>(AI->getArgument(1))) {
          assert(SI->getElements().size() == 1 && "Need Bool.value");
          if (auto *Literal =
              dyn_cast<IntegerLiteralInst>(SI->getElements()[0])) {
            return (Literal->getValue() == 0)
              ? BranchHint::LikelyFalse : BranchHint::LikelyTrue;
          }
        }
      }
      // fastpath/slowpath attrs are untested because the inliner luckily
      // inlines them before the downstream calls.
      else if (F->hasSemanticsAttr("slowpath"))
        return BranchHint::LikelyFalse;
      else if (F->hasSemanticsAttr("fastpath"))
        return BranchHint::LikelyTrue;
    }
  }
  return BranchHint::None;
}

/// \return true if the CFG edge FromBB->ToBB is directly gated by a _slowPath
/// branch hint.
bool ColdBlockInfo::isSlowPath(const SILBasicBlock *FromBB,
                               const SILBasicBlock *ToBB,
                               int recursionDepth) {
  auto *CBI = dyn_cast<CondBranchInst>(FromBB->getTerminator());
  if (!CBI)
    return false;

  SILValue C = getCondition(CBI->getCondition());

  BranchHint hint = getBranchHint(C, recursionDepth);
  if (hint == BranchHint::None)
    return false;

  const SILBasicBlock *ColdTarget =
    (hint == BranchHint::LikelyTrue) ? CBI->getFalseBB() : CBI->getTrueBB();

  return ToBB == ColdTarget;
}

/// \return true if the given block is dominated by a _slowPath branch hint.
///
/// Cache all blocks visited to avoid introducing quadratic behavior.
bool ColdBlockInfo::isCold(const SILBasicBlock *BB, int recursionDepth) {
  auto I = ColdBlockMap.find(BB);
  if (I != ColdBlockMap.end())
    return I->second;

  typedef llvm::DomTreeNodeBase<SILBasicBlock> DomTreeNode;
  DominanceInfo *DT = DA->get(const_cast<SILFunction*>(BB->getParent()));
  DomTreeNode *Node = DT->getNode(const_cast<SILBasicBlock*>(BB));
  // Always consider unreachable code cold.
  if (!Node)
    return true;

  std::vector<const SILBasicBlock*> DomChain;
  DomChain.push_back(BB);
  bool IsCold = false;
  Node = Node->getIDom();
  while (Node) {
    if (isSlowPath(Node->getBlock(), DomChain.back(), recursionDepth)) {
      IsCold = true;
      break;
    }
    auto I = ColdBlockMap.find(Node->getBlock());
    if (I != ColdBlockMap.end()) {
      IsCold = I->second;
      break;
    }
    DomChain.push_back(Node->getBlock());
    Node = Node->getIDom();
  }
  for (auto *ChainBB : DomChain)
    ColdBlockMap[ChainBB] = IsCold;
  return IsCold;
}
