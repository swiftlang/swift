//===--- PerformanceInlinerUtils.cpp - Performance inliner utilities. -----===//
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

#include "swift/SILOptimizer/Utils/PerformanceInlinerUtils.h"

//===----------------------------------------------------------------------===//
//                               ConstantTracker
//===----------------------------------------------------------------------===//

void ConstantTracker::trackInst(SILInstruction *inst) {
  if (auto *LI = dyn_cast<LoadInst>(inst)) {
    SILValue baseAddr = scanProjections(LI->getOperand());
    if (SILInstruction *loadLink = getMemoryContent(baseAddr))
      links[LI] = loadLink;
  } else if (StoreInst *SI = dyn_cast<StoreInst>(inst)) {
    SILValue baseAddr = scanProjections(SI->getOperand(1));
    memoryContent[baseAddr] = SI;
  } else if (CopyAddrInst *CAI = dyn_cast<CopyAddrInst>(inst)) {
    if (!CAI->isTakeOfSrc()) {
      // Treat a copy_addr as a load + store
      SILValue loadAddr = scanProjections(CAI->getOperand(0));
      if (SILInstruction *loadLink = getMemoryContent(loadAddr)) {
        links[CAI] = loadLink;
        SILValue storeAddr = scanProjections(CAI->getOperand(1));
        memoryContent[storeAddr] = CAI;
      }
    }
  }
}

SILValue ConstantTracker::scanProjections(SILValue addr,
                                          SmallVectorImpl<Projection> *Result) {
  for (;;) {
    if (Projection::isAddressProjection(addr)) {
      SILInstruction *I = cast<SILInstruction>(addr);
      if (Result) {
        Result->push_back(Projection(I));
      }
      addr = I->getOperand(0);
      continue;
    }
    if (SILValue param = getParam(addr)) {
      // Go to the caller.
      addr = param;
      continue;
    }
    // Return the base address = the first address which is not a projection.
    return addr;
  }
}

SILValue ConstantTracker::getStoredValue(SILInstruction *loadInst,
                                         ProjectionPath &projStack) {
  SILInstruction *store = links[loadInst];
  if (!store && callerTracker)
    store = callerTracker->links[loadInst];
  if (!store) return SILValue();

  assert(isa<LoadInst>(loadInst) || isa<CopyAddrInst>(loadInst));

  // Push the address projections of the load onto the stack.
  SmallVector<Projection, 4> loadProjections;
  scanProjections(loadInst->getOperand(0), &loadProjections);
  for (const Projection &proj : loadProjections) {
    projStack.push_back(proj);
  }

  //  Pop the address projections of the store from the stack.
  SmallVector<Projection, 4> storeProjections;
  scanProjections(store->getOperand(1), &storeProjections);
  for (auto iter = storeProjections.rbegin(); iter != storeProjections.rend();
       ++iter) {
    const Projection &proj = *iter;
    // The corresponding load-projection must match the store-projection.
    if (projStack.empty() || projStack.back() != proj)
      return SILValue();
    projStack.pop_back();
  }

  if (isa<StoreInst>(store))
    return store->getOperand(0);

  // The copy_addr instruction is both a load and a store. So we follow the link
  // again.
  assert(isa<CopyAddrInst>(store));
  return getStoredValue(store, projStack);
}

// Get the aggregate member based on the top of the projection stack.
static SILValue getMember(SILInstruction *inst, ProjectionPath &projStack) {
  if (!projStack.empty()) {
    const Projection &proj = projStack.back();
    return proj.getOperandForAggregate(inst);
  }
  return SILValue();
}

SILInstruction *ConstantTracker::getDef(SILValue val,
                                        ProjectionPath &projStack) {

  // Track the value up the dominator tree.
  for (;;) {
    if (SILInstruction *inst = dyn_cast<SILInstruction>(val)) {
      if (Projection::isObjectProjection(inst)) {
        // Extract a member from a struct/tuple/enum.
        projStack.push_back(Projection(inst));
        val = inst->getOperand(0);
        continue;
      } else if (SILValue member = getMember(inst, projStack)) {
        // The opposite of a projection instruction: composing a struct/tuple.
        projStack.pop_back();
        val = member;
        continue;
      } else if (SILValue loadedVal = getStoredValue(inst, projStack)) {
        // A value loaded from memory.
        val = loadedVal;
        continue;
      } else if (isa<ThinToThickFunctionInst>(inst)) {
        val = inst->getOperand(0);
        continue;
      }
      return inst;
    } else if (SILValue param = getParam(val)) {
      // Continue in the caller.
      val = param;
      continue;
    }
    return nullptr;
  }
}

ConstantTracker::IntConst ConstantTracker::getBuiltinConst(BuiltinInst *BI, int depth) {
  const BuiltinInfo &Builtin = BI->getBuiltinInfo();
  OperandValueArrayRef Args = BI->getArguments();
  switch (Builtin.ID) {
    default: break;

      // Fold comparison predicates.
#define BUILTIN(id, name, Attrs)
#define BUILTIN_BINARY_PREDICATE(id, name, attrs, overload) \
case BuiltinValueKind::id:
#include "swift/AST/Builtins.def"
    {
      IntConst lhs = getIntConst(Args[0], depth);
      IntConst rhs = getIntConst(Args[1], depth);
      if (lhs.isValid && rhs.isValid) {
        return IntConst(constantFoldComparison(lhs.value, rhs.value,
                                               Builtin.ID),
                        lhs.isFromCaller || rhs.isFromCaller);
      }
      break;
    }

    case BuiltinValueKind::SAddOver:
    case BuiltinValueKind::UAddOver:
    case BuiltinValueKind::SSubOver:
    case BuiltinValueKind::USubOver:
    case BuiltinValueKind::SMulOver:
    case BuiltinValueKind::UMulOver: {
      IntConst lhs = getIntConst(Args[0], depth);
      IntConst rhs = getIntConst(Args[1], depth);
      if (lhs.isValid && rhs.isValid) {
        bool IgnoredOverflow;
        return IntConst(constantFoldBinaryWithOverflow(lhs.value, rhs.value,
                        IgnoredOverflow,
                        getLLVMIntrinsicIDForBuiltinWithOverflow(Builtin.ID)),
                          lhs.isFromCaller || rhs.isFromCaller);
      }
      break;
    }

    case BuiltinValueKind::SDiv:
    case BuiltinValueKind::SRem:
    case BuiltinValueKind::UDiv:
    case BuiltinValueKind::URem: {
      IntConst lhs = getIntConst(Args[0], depth);
      IntConst rhs = getIntConst(Args[1], depth);
      if (lhs.isValid && rhs.isValid && rhs.value != 0) {
        bool IgnoredOverflow;
        return IntConst(constantFoldDiv(lhs.value, rhs.value,
                                        IgnoredOverflow, Builtin.ID),
                        lhs.isFromCaller || rhs.isFromCaller);
      }
      break;
    }

    case BuiltinValueKind::And:
    case BuiltinValueKind::AShr:
    case BuiltinValueKind::LShr:
    case BuiltinValueKind::Or:
    case BuiltinValueKind::Shl:
    case BuiltinValueKind::Xor: {
      IntConst lhs = getIntConst(Args[0], depth);
      IntConst rhs = getIntConst(Args[1], depth);
      if (lhs.isValid && rhs.isValid) {
        return IntConst(constantFoldBitOperation(lhs.value, rhs.value,
                                                 Builtin.ID),
                        lhs.isFromCaller || rhs.isFromCaller);
      }
      break;
    }

    case BuiltinValueKind::Trunc:
    case BuiltinValueKind::ZExt:
    case BuiltinValueKind::SExt:
    case BuiltinValueKind::TruncOrBitCast:
    case BuiltinValueKind::ZExtOrBitCast:
    case BuiltinValueKind::SExtOrBitCast: {
      IntConst val = getIntConst(Args[0], depth);
      if (val.isValid) {
        return IntConst(constantFoldCast(val.value, Builtin), val.isFromCaller);
      }
      break;
    }
  }
  return IntConst();
}

// Tries to evaluate the integer constant of a value. The \p depth is used
// to limit the complexity.
ConstantTracker::IntConst ConstantTracker::getIntConst(SILValue val, int depth) {

  // Don't spend too much time with constant evaluation.
  if (depth >= 10)
    return IntConst();
  
  SILInstruction *I = getDef(val);
  if (!I)
    return IntConst();
  
  if (auto *IL = dyn_cast<IntegerLiteralInst>(I)) {
    return IntConst(IL->getValue(), IL->getFunction() != F);
  }
  if (auto *BI = dyn_cast<BuiltinInst>(I)) {
    if (constCache.count(BI) != 0)
      return constCache[BI];
    
    IntConst builtinConst = getBuiltinConst(BI, depth + 1);
    constCache[BI] = builtinConst;
    return builtinConst;
  }
  return IntConst();
}

// Returns the taken block of a terminator instruction if the condition turns
// out to be constant.
SILBasicBlock *ConstantTracker::getTakenBlock(TermInst *term) {
  if (CondBranchInst *CBI = dyn_cast<CondBranchInst>(term)) {
    IntConst condConst = getIntConst(CBI->getCondition());
    if (condConst.isFromCaller) {
      return condConst.value != 0 ? CBI->getTrueBB() : CBI->getFalseBB();
    }
    return nullptr;
  }
  if (SwitchValueInst *SVI = dyn_cast<SwitchValueInst>(term)) {
    IntConst switchConst = getIntConst(SVI->getOperand());
    if (switchConst.isFromCaller) {
      for (unsigned Idx = 0; Idx < SVI->getNumCases(); ++Idx) {
        auto switchCase = SVI->getCase(Idx);
        if (auto *IL = dyn_cast<IntegerLiteralInst>(switchCase.first)) {
          if (switchConst.value == IL->getValue())
            return switchCase.second;
        } else {
          return nullptr;
        }
      }
      if (SVI->hasDefault())
        return SVI->getDefaultBB();
    }
    return nullptr;
  }
  if (SwitchEnumInst *SEI = dyn_cast<SwitchEnumInst>(term)) {
    if (SILInstruction *def = getDefInCaller(SEI->getOperand())) {
      if (EnumInst *EI = dyn_cast<EnumInst>(def)) {
        for (unsigned Idx = 0; Idx < SEI->getNumCases(); ++Idx) {
          auto enumCase = SEI->getCase(Idx);
          if (enumCase.first == EI->getElement())
            return enumCase.second;
        }
        if (SEI->hasDefault())
          return SEI->getDefaultBB();
      }
    }
    return nullptr;
  }
  if (CheckedCastBranchInst *CCB = dyn_cast<CheckedCastBranchInst>(term)) {
    if (SILInstruction *def = getDefInCaller(CCB->getOperand())) {
      if (UpcastInst *UCI = dyn_cast<UpcastInst>(def)) {
        SILType castType = UCI->getOperand()->getType();
        if (CCB->getCastType().isExactSuperclassOf(castType)) {
          return CCB->getSuccessBB();
        }
        if (!castType.isBindableToSuperclassOf(CCB->getCastType())) {
          return CCB->getFailureBB();
        }
      }
    }
  }
  return nullptr;
}

//===----------------------------------------------------------------------===//
//                           Shortest path analysis
//===----------------------------------------------------------------------===//

int ShortestPathAnalysis::getEntryDistFromPreds(const SILBasicBlock *BB,
                                                int LoopDepth) {
  int MinDist = InitialDist;
  for (SILBasicBlock *Pred : BB->getPredecessorBlocks()) {
    BlockInfo *PredInfo = getBlockInfo(Pred);
    Distances &PDists = PredInfo->getDistances(LoopDepth);
    int DistFromEntry = PDists.DistFromEntry + PredInfo->Length +
                          PDists.LoopHeaderLength;
    assert(DistFromEntry >= 0);
    if (DistFromEntry < MinDist)
      MinDist = DistFromEntry;
  }
  return MinDist;
}

int ShortestPathAnalysis::getExitDistFromSuccs(const SILBasicBlock *BB,
                                               int LoopDepth) {
  int MinDist = InitialDist;
  for (const SILSuccessor &Succ : BB->getSuccessors()) {
    BlockInfo *SuccInfo = getBlockInfo(Succ);
    Distances &SDists = SuccInfo->getDistances(LoopDepth);
    if (SDists.DistToExit < MinDist)
      MinDist = SDists.DistToExit;
  }
  return MinDist;
}

/// Detect an edge from the loop pre-header's predecessor to the loop exit
/// block. Such an edge "short-cuts" a loop if it is never iterated. But usually
/// it is the less frequent case and we want to ignore it.
/// E.g. it handles the case of N==0 for
///     for i in 0..<N { ... }
/// If the \p Loop has such an edge the source block of this edge is returned,
/// which is the predecessor of the loop pre-header.
static SILBasicBlock *detectLoopBypassPreheader(SILLoop *Loop) {
  SILBasicBlock *Pred = Loop->getLoopPreheader();
  if (!Pred)
    return nullptr;

  SILBasicBlock *PredPred = Pred->getSinglePredecessorBlock();
  if (!PredPred)
    return nullptr;

  auto *CBR = dyn_cast<CondBranchInst>(PredPred->getTerminator());
  if (!CBR)
    return nullptr;

  SILBasicBlock *Succ = (CBR->getTrueBB() == Pred ? CBR->getFalseBB() :
                                                    CBR->getTrueBB());

  for (SILBasicBlock *PredOfSucc : Succ->getPredecessorBlocks()) {
    SILBasicBlock *Exiting = PredOfSucc->getSinglePredecessorBlock();
    if (!Exiting)
      Exiting = PredOfSucc;
    if (Loop->contains(Exiting))
      return PredPred;
  }
  return nullptr;
}

void ShortestPathAnalysis::analyzeLoopsRecursively(SILLoop *Loop, int LoopDepth) {
  if (LoopDepth >= MaxNumLoopLevels)
    return;

  // First dive into the inner loops.
  for (SILLoop *SubLoop : Loop->getSubLoops()) {
    analyzeLoopsRecursively(SubLoop, LoopDepth + 1);
  }

  BlockInfo *HeaderInfo = getBlockInfo(Loop->getHeader());
  Distances &HeaderDists = HeaderInfo->getDistances(LoopDepth);

  // Initial values for the entry (== header) and exit-predecessor (== header as
  // well).
  HeaderDists.DistFromEntry = 0;
  HeaderDists.DistToExit = 0;

  solveDataFlow(Loop->getBlocks(), LoopDepth);

  int LoopLength = getExitDistFromSuccs(Loop->getHeader(), LoopDepth) +
  HeaderInfo->getLength(LoopDepth);
  HeaderDists.DistToExit = LoopLength;

  // If there is a loop bypass edge, add the loop length to the loop pre-pre-
  // header instead to the header. This actually let us ignore the loop bypass
  // edge in the length calculation for the loop's parent scope.
  if (SILBasicBlock *Bypass = detectLoopBypassPreheader(Loop))
    HeaderInfo = getBlockInfo(Bypass);

  // Add the full loop length (= assumed-iteration-count * length) to the loop
  // header so that it is considered in the parent scope.
  HeaderInfo->getDistances(LoopDepth - 1).LoopHeaderLength =
    LoopCount * LoopLength;
}

ShortestPathAnalysis::Weight ShortestPathAnalysis::
getWeight(SILBasicBlock *BB, Weight CallerWeight) {
  assert(BB->getParent() == F);

  SILLoop *Loop = LI->getLoopFor(BB);
  if (!Loop) {
    // We are not in a loop. So just account the length of our function scope
    // in to the length of the CallerWeight.
    return Weight(CallerWeight.ScopeLength + getScopeLength(BB, 0),
                  CallerWeight.LoopWeight);
  }
  int LoopDepth = Loop->getLoopDepth();
  // Deal with the corner case of having more than 4 nested loops.
  while (LoopDepth >= MaxNumLoopLevels) {
    --LoopDepth;
    Loop = Loop->getParentLoop();
  }
  Weight W(getScopeLength(BB, LoopDepth), SingleLoopWeight);

  // Add weights for all the loops BB is in.
  while (Loop) {
    assert(LoopDepth > 0);
    BlockInfo *HeaderInfo = getBlockInfo(Loop->getHeader());
    int InnerLoopLength = HeaderInfo->getScopeLength(LoopDepth) *
                            ShortestPathAnalysis::LoopCount;
    int OuterLoopWeight = SingleLoopWeight;
    int OuterScopeLength = HeaderInfo->getScopeLength(LoopDepth - 1);

    // Reaching the outermost loop, we use the CallerWeight to get the outer
    // length+loopweight.
    if (LoopDepth == 1) {
      // If the apply in the caller is not in a significant loop, just stop with
      // what we have now.
      if (CallerWeight.LoopWeight < 4)
        return W;

      // If this function is part of the caller's scope length take the caller's
      // scope length. Note: this is not the case e.g. if the apply is in a
      // then-branch of an if-then-else in the caller and the else-branch is
      // the short path.
      if (CallerWeight.ScopeLength > OuterScopeLength)
        OuterScopeLength = CallerWeight.ScopeLength;
      OuterLoopWeight = CallerWeight.LoopWeight;
    }
    assert(OuterScopeLength >= InnerLoopLength);

    // If the current loop is only a small part of its outer loop, we don't
    // take the outer loop that much into account. Only if the current loop is
    // actually the "main part" in the outer loop we add the full loop weight
    // for the outer loop.
    if (OuterScopeLength < InnerLoopLength * 2) {
      W.LoopWeight += OuterLoopWeight - 1;
    } else if (OuterScopeLength < InnerLoopLength * 3) {
      W.LoopWeight += OuterLoopWeight - 2;
    } else if (OuterScopeLength < InnerLoopLength * 4) {
      W.LoopWeight += OuterLoopWeight - 3;
    } else {
      return W;
    }
    --LoopDepth;
    Loop = Loop->getParentLoop();
  }
  assert(LoopDepth == 0);
  return W;
}

void ShortestPathAnalysis::dump() {
  printFunction(llvm::errs());
}

void ShortestPathAnalysis::printFunction(llvm::raw_ostream &OS) {
  OS << "SPA @" << F->getName() << "\n";
  for (SILBasicBlock &BB : *F) {
    printBlockInfo(OS, &BB, 0);
  }
  for (SILLoop *Loop : *LI) {
    printLoop(OS, Loop, 1);
  }
}

void ShortestPathAnalysis::printLoop(llvm::raw_ostream &OS, SILLoop *Loop,
                                     int LoopDepth) {
  if (LoopDepth >= MaxNumLoopLevels)
    return;
  assert(LoopDepth == (int)Loop->getLoopDepth());

  OS << "Loop bb" << Loop->getHeader()->getDebugID() << ":\n";
  for (SILBasicBlock *BB : Loop->getBlocks()) {
    printBlockInfo(OS, BB, LoopDepth);
  }
  for (SILLoop *SubLoop : Loop->getSubLoops()) {
    printLoop(OS, SubLoop, LoopDepth + 1);
  }
}

void ShortestPathAnalysis::printBlockInfo(llvm::raw_ostream &OS,
                                          SILBasicBlock *BB, int LoopDepth) {
  BlockInfo *BBInfo = getBlockInfo(BB);
  Distances &D = BBInfo->getDistances(LoopDepth);
  OS << "  bb" << BB->getDebugID() << ": length=" << BBInfo->Length << '+'
     << D.LoopHeaderLength << ", d-entry=" << D.DistFromEntry
     << ", d-exit=" << D.DistToExit << '\n';
}

void ShortestPathAnalysis::Weight::updateBenefit(int &Benefit,
                                                 int Importance) const {
  assert(isValid());
  int newBenefit = 0;

  // Use some heuristics. The basic idea is: length is bad, loops are good.
  if (ScopeLength > 320) {
    newBenefit = Importance;
  } else if (ScopeLength > 160) {
    newBenefit = Importance + LoopWeight * 4;
  } else if (ScopeLength > 80) {
    newBenefit = Importance + LoopWeight * 8;
  } else if (ScopeLength > 40) {
    newBenefit = Importance + LoopWeight * 12;
  } else if (ScopeLength > 20) {
    newBenefit = Importance + LoopWeight * 16;
  } else {
    newBenefit = Importance + 20 + LoopWeight * 16;
  }
  // We don't accumulate the benefit instead we max it.
  if (newBenefit > Benefit)
    Benefit = newBenefit;
}
