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
#include "swift/SIL/Dominance.h"

using namespace swift;

/// @return true if the CFG edge FromBB->ToBB is directly gated by a _slowPath
/// branch hint.
static bool isSlowPath(const SILBasicBlock *FromBB, const SILBasicBlock *ToBB) {
  auto *CBI = dyn_cast<CondBranchInst>(FromBB->getTerminator());
  if (!CBI)
    return false;

  auto *AI = dyn_cast<ApplyInst>(CBI->getCondition());
  if (!AI)
    return false;

  auto *BFRI = dyn_cast<BuiltinFunctionRefInst>(AI->getCallee());
  if (!BFRI || BFRI->getIntrinsicInfo().ID != llvm::Intrinsic::expect)
    return false;

  auto *Literal = dyn_cast<IntegerLiteralInst>(AI->getArgument(0));
  if (!Literal)
    return false;

  // This is the slow path if the condition's value is expected to be true and
  // our target is false or vice-versa.
  return
    ToBB == (Literal->getValue() == 0 ? CBI->getFalseBB() : CBI->getTrueBB());
}

/// @return true if the given block is dominated by a _slowPath branch hint.
///
/// Cache all blocks visited to avoid introducing quadratic behavior.
bool ColdBlockInfo::isCold(const SILBasicBlock *BB) {
  auto I = ColdBlockMap.find(BB);
  if (I != ColdBlockMap.end())
    return I->second;

  typedef llvm::DomTreeNodeBase<SILBasicBlock> DomTreeNode;
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
    DomChain.push_back(Node->getBlock());
    Node = Node->getIDom();
  }
  for (auto *ChainBB : DomChain)
    ColdBlockMap[ChainBB] = IsCold;
  return IsCold;
}
