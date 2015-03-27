//===----- ColdBlocks.cpp - Fast/slow path analysis for the SIL CFG -------===//
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

#include "swift/SILAnalysis/ColdBlockInfo.h"
#include "swift/SILAnalysis/DominanceAnalysis.h"

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

namespace {
/// Tri-value return code for checking branch hints.
enum BranchHint : unsigned {
  None,
  LikelyTrue,
  LikelyFalse
};
} // namespace

/// \return a BranHint if this call is a builtin branch hint.
static BranchHint getBranchHint(SILValue Cond) {
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
  
  // Handle the @semantic function used for branch hints. The generic
  // fast/slowPath calls are frequently only inlined one level down to
  // _branchHint before inlining the call sites that they guard.
  auto AI = dyn_cast<ApplyInst>(Cond);
  if (!AI)
    return BranchHint::None;
  
  if (auto *FunctionRef = dyn_cast<FunctionRefInst>(AI->getCallee())) {
    SILFunction *F = FunctionRef->getReferencedFunction();
    if (F->hasDefinedSemantics()) {
      if (F->getSemanticsString() == "branchhint") {
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
      else if (F->getSemanticsString() == "slowpath")
        return BranchHint::LikelyFalse;
      else if (F->getSemanticsString() == "fastpath")
        return BranchHint::LikelyTrue;
    }
  }
  return BranchHint::None;
}

/// \return true if the CFG edge FromBB->ToBB is directly gated by a _slowPath
/// branch hint.
bool ColdBlockInfo::isSlowPath(const SILBasicBlock *FromBB, const SILBasicBlock *ToBB) {
  auto *CBI = dyn_cast<CondBranchInst>(FromBB->getTerminator());
  if (!CBI)
    return false;

  SILValue C = getCondition(CBI->getCondition());

  BranchHint hint = getBranchHint(C);
  if (hint == BranchHint::None)
    return false;

  const SILBasicBlock *ColdTarget =
    (hint == BranchHint::LikelyTrue) ? CBI->getFalseBB() : CBI->getTrueBB();

  return ToBB == ColdTarget;
}

/// \return true if the given block is dominated by a _slowPath branch hint.
///
/// Cache all blocks visited to avoid introducing quadratic behavior.
bool ColdBlockInfo::isCold(const SILBasicBlock *BB) {
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
    if (isSlowPath(Node->getBlock(), DomChain.back())) {
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
