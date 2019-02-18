//===--- AccessEnforcementDom.cpp - dominated access removal opt ---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// This pass contains two optimizations:
///
/// 1) DominatedAccessRemoval:
/// This function pass removes dynamic access enforcement based on dominance.
///
/// General case:
/// begin_access A (may or may not have no_nested_conflict)
/// load/store
/// end_access
/// ...
/// begin_access A [no_nested_conflict] // dominated by the first access
/// load/store
/// end_access A
///
/// The second access scope does not need to be emitted.
///
/// 2) LoopDominatingAccessAdder:
/// This function pass adds new dominating accesses to loop's dynamic accesses
///
/// General case:
/// <loop preheader>
/// A = ref_element_addr
/// <loop>
/// begin_access A [dynamic] [no_nested_conflict]
///
/// Adding an empty begin_access A in the preheader would allow us to
/// turn the loop's access to [static]
///
/// Note 1: This optimization must be aware of all possible access to a Class or
/// Global address. This includes unpaired access instructions and keypath
/// entry points. Ignoring any access pattern would weaken enforcement.
///
/// Note 2: We either need to also flag the dominating access specially, or,
/// we need to do this in the last-chance pipeline, with a guarantee that no,
/// access marker removal is done after it.
///
/// Note 3: LoopDominatedAccessAdder makes some key assumptions:
/// 1) Precondition: There are no unpaired accesses (or keypath accesses)
///    anywhere in the function.
/// 2) Invariant: An access scope cannot begin outside the loop and end on any
///    path between the loop header and the original dominated access.
/// 3) Expectation: Accesses will be hoisted across nested loops during
///    bottom-up processing.
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "access-enforcement-dom"

#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/MemAccessUtils.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/Analysis/LoopAnalysis.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/Local.h"

#define _MAX_ACCESS_DOM_OPT_RECURSION_DEPTH 100
#define _MAX_ACCESS_DOM_OPT_UNIQUE_STORAGE_LOCS 42

using namespace swift;

namespace {
class DominatedAccessRemoval {
public:
  using AccessedStoragePair = std::pair<BeginAccessInst *, AccessedStorage>;
  using AccessedStorageInfo = llvm::SmallVector<AccessedStoragePair, 32>;
  using DominatedInstVec = llvm::SmallVector<BeginAccessInst *, 32>;
  using KeyPathEntryPointsSet = llvm::SmallSet<SILInstruction *, 8>;

public:
  DominatedAccessRemoval(SILFunction &func, DominanceInfo *domInfo)
      : func(func), domInfo(domInfo) {}

  bool perform();

protected:
  bool visitInstruction(SILInstruction *instr,
                        AccessedStorageInfo &visitedDomAccessesToStorageInfo);
  void visitBeginAccess(BeginAccessInst *beginAccess, AccessedStorage storage,
                        AccessedStorageInfo &visitedDomAccessesToStorageInfo);
  bool analyzeDomSubTree(SILBasicBlock *block,
                         AccessedStorageInfo &visitedDomAccessesToStorageInfo,
                         size_t recursionDepth);

  bool analyze();
  void optimize();

private:
  SILFunction &func;
  DominanceInfo *domInfo;
  // domInstrs is a vector of Dominated begin_access instructions.
  // the accesses are dynamic, and the Dominated one has no nested conflict
  // we can turn the dominated to static during the optimize() phase
  DominatedInstVec domInstrs;
};
} // namespace

// Returns a bool: If we should bail on this function
// we return false - else true
// See the discussion in DominatedAccessRemoval::analyze() below
bool DominatedAccessRemoval::visitInstruction(
    SILInstruction *instr,
    AccessedStorageInfo &visitedDomAccessesToStorageInfo) {
  if (auto *BAI = dyn_cast<BeginAccessInst>(instr)) {
    if (BAI->getEnforcement() != SILAccessEnforcement::Dynamic) {
      return true;
    }
    AccessedStorage storage = findAccessedStorageNonNested(BAI->getSource());
    if (!storage) {
      return true;
    }

    visitBeginAccess(BAI, storage, visitedDomAccessesToStorageInfo);
  } else if (auto fullApply = FullApplySite::isa(instr)) {
    SILFunction *callee = fullApply.getReferencedFunction();
    if (!callee)
      return true;
    if (!callee->hasSemanticsAttr("keypath.entry"))
      return true;
    // we can't eliminate dominated checks even when we can prove that
    // the dominated scope has no internal nested conflicts.
    // We just bail on these functions.
    // Aprevious commit handled them, you can see the full support there,
    // but, to simplify the code, assuming key-paths are rare for now, bail.
    return false;
  } else if (auto *BUAI = dyn_cast<BeginUnpairedAccessInst>(instr)) {
    // We have an Implementation that handles this in the analyzer
    // and optimizer in a previous commit, However,
    // In order to simplify the code, at least in the initial version,
    // We decided to just bail on these extremely rare cases
    return false;
  }
  return true;
}

void DominatedAccessRemoval::visitBeginAccess(
    BeginAccessInst *beginAccess, AccessedStorage storage,
    AccessedStorageInfo &visitedDomAccessesToStorageInfo) {
  if (!storage.isUniquelyIdentifiedOrClass()) {
    // No cannot do anything about this location -
    // bail on trying to turn it to static
    return;
  }

  // checks if the current storage has never been seen before
  auto predNewStorageLoc = [&](AccessedStoragePair it) {
    auto currStorage = it.second;
    return !currStorage.isDistinctFrom(storage);
  };
  // The size of this list should be tiny: number of storage locations
  auto visitedAccessesIt =
      std::find_if(visitedDomAccessesToStorageInfo.begin(),
                   visitedDomAccessesToStorageInfo.end(), predNewStorageLoc);

  if (visitedAccessesIt == visitedDomAccessesToStorageInfo.end()) {
    // We've never seen this one before - just add it and return
    visitedDomAccessesToStorageInfo.push_back(
        std::make_pair(beginAccess, storage));
    return;
  }

  // If the currnet access has nested conflict,
  // we can't remove it by finding a dominating access
  if (!beginAccess->hasNoNestedConflict()) {
    return;
  }

  // check that the storage we found is identical to the
  // one we found in order to enable optimization
  auto parentStorage = visitedAccessesIt->second;
  if (!parentStorage.hasIdenticalBase(storage)) {
    return;
  }

  auto *parentBegin = visitedAccessesIt->first;
  if (beginAccess->getAccessKind() > parentBegin->getAccessKind()) {
    // we can't change to static: dominating access has a > access kind
    // change parent's access kind so we would be able to do so
    parentBegin->setAccessKind(beginAccess->getAccessKind());
  }

  domInstrs.push_back(beginAccess);
}

bool DominatedAccessRemoval::analyzeDomSubTree(
    SILBasicBlock *block, AccessedStorageInfo &visitedDomAccessesToStorageInfo,
    size_t recursionDepth) {
  if (recursionDepth > _MAX_ACCESS_DOM_OPT_RECURSION_DEPTH) {
    return false;
  }
  // We will modify the incoming visitedDomAccessesToStorageInfo,
  // after finishing with the sub-tree,
  // we need to restore it to its previous state.
  // the state should be small because the number of unique storage
  // locations, which is the size of the data structure,
  // should be quite small. a handful at most.
  auto numOfElems = visitedDomAccessesToStorageInfo.size();
  if (numOfElems > _MAX_ACCESS_DOM_OPT_UNIQUE_STORAGE_LOCS) {
    return false;
  }

  // analyze the current block:
  for (auto &instr : *block) {
    if (!visitInstruction(&instr, visitedDomAccessesToStorageInfo))
      return false;
  }
  // do the same for each child:
  auto *domNode = domInfo->getNode(block);
  for (auto *child : *domNode) {
    if (!analyzeDomSubTree(child->getBlock(), visitedDomAccessesToStorageInfo,
                           recursionDepth + 1))
      return false;
  }

  // Restore the sets to their previous state as described above,
  // removing all "new" elements
  assert(visitedDomAccessesToStorageInfo.size() >= numOfElems &&
         "Expected the size of visitedStorageLocs to be the same or higher");
  auto rmInfoStart = visitedDomAccessesToStorageInfo.begin() + numOfElems;
  visitedDomAccessesToStorageInfo.erase(rmInfoStart,
                                        visitedDomAccessesToStorageInfo.end());
  assert(visitedDomAccessesToStorageInfo.size() == numOfElems &&
         "Number of elems should stay the same");
  return true;
}

// Finds domPairs for which we can change the dominated instruction to static
// NOTE: We might not be able to optimize some the pairs due to other
// restrictions Such as key-path or unpaired begin access. We only traverse the
// function once, if we find a pattern that *might* prevent optimization, we
// just add it to appropriate data structures which will be analyzed later.
// If we should bail on this function we return false - else true
// we bail to simplify the code instead of handling unpaired accesses
// in the optimize phase
bool DominatedAccessRemoval::analyze() {
  SILBasicBlock *entry = &func.front();
  AccessedStorageInfo visitedDomAccessesToStorageInfo;

  return analyzeDomSubTree(entry, visitedDomAccessesToStorageInfo,
                           0 /*recursion-depth*/);
}

// Sets the dominated instruction to static.
// Goes through the data structures initialized by the analysis method
// and makes sure we are not Weakening enforcement
void DominatedAccessRemoval::optimize() {
  for (BeginAccessInst *dominatedInstr : domInstrs) {
    LLVM_DEBUG(llvm::dbgs() << "Processing optimizable dominated instruction: "
                            << dominatedInstr << "\n");
    LLVM_DEBUG(llvm::dbgs() << "Setting " << *dominatedInstr
                            << " access enforcement to static\n");
    dominatedInstr->setEnforcement(SILAccessEnforcement::Static);
  }
}

// Returns a bool: If we should bail on this function
// we return false - else true
bool DominatedAccessRemoval::perform() {
  if (func.empty())
    return false;

  if (!analyze()) {
    LLVM_DEBUG(llvm::dbgs()
               << "Bailed on function: " << func.getName() << "\n");
    return false;
  }
  optimize();
  return true;
}

namespace {
class LoopDominatingAccessAdder {
  SmallVector<SILLoop *, 8> bottomUpWorkList;
  DominanceInfo *domInfo;

public:
  LoopDominatingAccessAdder(SILLoop *topLevelLoop, DominanceInfo *domInfo)
      : domInfo(domInfo) {
    // Collect loops for a recursive bottom-up traversal in the loop tree.
    bottomUpWorkList.push_back(topLevelLoop);
    for (unsigned i = 0; i < bottomUpWorkList.size(); ++i) {
      auto *loop = bottomUpWorkList[i];
      for (auto *subLoop : *loop) {
        bottomUpWorkList.push_back(subLoop);
      }
    }
  }

  void perform();

protected:
  /// Collect a set of instructions that can be dominated
  void
  analyzeCurrentLoop(SILLoop *currentLoop,
                     SmallVectorImpl<BeginAccessInst *> &dominatableInstrVec);

  /// Optimize the current loop nest.
  void optimizeLoop(SILLoop *currnetLoop,
                    SmallVectorImpl<BeginAccessInst *> &dominatableInstrVec);
};
} // namespace

void LoopDominatingAccessAdder::analyzeCurrentLoop(
    SILLoop *currentLoop,
    SmallVectorImpl<BeginAccessInst *> &dominatableInstrVec) {
  auto *preheader = currentLoop->getLoopPreheader();
  if (!preheader) {
    // Can't dominate this loop - bail
    return;
  }
  for (auto *block : currentLoop->getBlocks()) {
    for (auto &instr : *block) {
      auto *beginAccess = dyn_cast<BeginAccessInst>(&instr);
      if (!beginAccess) {
        continue;
      }
      if (beginAccess->getEnforcement() != SILAccessEnforcement::Dynamic) {
        continue;
      }
      if (!beginAccess->hasNoNestedConflict()) {
        continue;
      }
      auto operand = beginAccess->getOperand();
      auto *parentBB = operand->getParentBlock();
      if (currentLoop->contains(parentBB)) {
        // Loop varaint argument
        continue;
      }
      if (!domInfo->properlyDominates(parentBB, preheader) &&
          parentBB != preheader) {
        // Argument not dominated by the preheader
        // TODO: Should this be a fatal error? Invalid control flow?
        continue;
      }
      dominatableInstrVec.push_back(beginAccess);
    }
  }
}

void LoopDominatingAccessAdder::optimizeLoop(
    SILLoop *currnetLoop,
    SmallVectorImpl<BeginAccessInst *> &dominatableInstrVec) {
  auto *preheader = currnetLoop->getLoopPreheader();
  if (!preheader) {
    // Can't dominate this loop - bail
    return;
  }
  // Get the last instruction in the pre-header, this is
  // We insert new access scopes before it.
  // We also use it for our currently-optimizeable begin_access:
  // We can only create a dominating scope if the
  // dominatableInstr's operand dominates this instruction:
  TermInst *preheaderTerm = preheader->getTerminator();

  // Same operand may appear more than once in dominatableInstrVec
  // No reason to create multiple dominating instructions
  // The following set keeps track of operands we can safely skip
  // i.e. we already created a dominating begin_access for
  // We also need to keep track of the new dominating begin_access:
  // First inner loop access might be [read] while a second one
  // Might be [modify], the dominating access should be [modify]
  // in that case
  llvm::SmallDenseMap<SILValue, BeginAccessInst *> domOpToBeginAccess;

  while (!dominatableInstrVec.empty()) {
    BeginAccessInst *beginAccess = dominatableInstrVec.pop_back_val();
    auto operand = beginAccess->getOperand();
    auto domIt = domOpToBeginAccess.find(operand);
    if (domIt != domOpToBeginAccess.end()) {
      BeginAccessInst *domBegin = domIt->getSecond();
      if (domBegin->getAccessKind() < beginAccess->getAccessKind()) {
        LLVM_DEBUG(llvm::dbgs()
                   << "Changing " << *domBegin << " access kind to "
                   << *beginAccess << " access kind\n");
        domBegin->setAccessKind(beginAccess->getAccessKind());
      }
      LLVM_DEBUG(llvm::dbgs()
                 << "Already have a dominating access scope for: "
                 << *beginAccess
                 << ", settting the inner access' enforcement to static\n");
      beginAccess->setEnforcement(SILAccessEnforcement::Static);
      continue;
    }
    // We can go ahead with the optimization - do so:
    SILBuilderWithScope scopeBuilder(preheaderTerm);
    BeginAccessInst *newBegin = scopeBuilder.createBeginAccess(
        beginAccess->getLoc(), operand, beginAccess->getAccessKind(),
        SILAccessEnforcement::Dynamic, true /*no nested conflict*/,
        beginAccess->isFromBuiltin());
    scopeBuilder.createEndAccess(beginAccess->getLoc(), newBegin, false);
    domOpToBeginAccess.insert(std::make_pair(operand, newBegin));
    LLVM_DEBUG(llvm::dbgs()
               << "Created a dominating access scope for: " << *beginAccess
               << ", settting the inner access' enforcement to static\n");
    beginAccess->setEnforcement(SILAccessEnforcement::Static);
  }
}

void LoopDominatingAccessAdder::perform() {
  // Process loops bottom up in the loop tree.
  while (!bottomUpWorkList.empty()) {
    SILLoop *currnetLoop = bottomUpWorkList.pop_back_val();
    LLVM_DEBUG(llvm::dbgs() << "Processing loop " << *currnetLoop);

    llvm::SmallVector<BeginAccessInst *, 8> dominatableInstrVec;
    analyzeCurrentLoop(currnetLoop, dominatableInstrVec);
    optimizeLoop(currnetLoop, dominatableInstrVec);
  }
}

namespace {
struct AccessEnforcementDom : public SILFunctionTransform {
  void run() override {
    SILFunction *func = getFunction();

    DominanceAnalysis *domAnalysis = getAnalysis<DominanceAnalysis>();
    DominanceInfo *domInfo = domAnalysis->get(func);
    DominatedAccessRemoval eliminationPass(*func, domInfo);
    if (!eliminationPass.perform()) {
      // Bailed on the function due to key-path and/or other reasons
      // Too risky to do anything else, don't repeat
      // the analysis in subsequent optimizations
      return;
    }

    SILLoopAnalysis *loopAnalysis = PM->getAnalysis<SILLoopAnalysis>();
    SILLoopInfo *loopInfo = loopAnalysis->get(func);
    if (loopInfo->empty()) {
      LLVM_DEBUG(llvm::dbgs() << "No loops in " << func->getName() << "\n");
      return;
    }

    LLVM_DEBUG(llvm::dbgs() << "LoopDominatingAccessAdder: Processing loops in "
                            << func->getName() << "\n");

    for (auto *topLevelLoop : *loopInfo) {
      LoopDominatingAccessAdder additionPass(topLevelLoop, domInfo);
      additionPass.perform();
    }

    // TODO: Do we need to rerun the normal dominated check removal here?
    // I don’t think we need to do so: there shouldn’t be any redundant
    // accesses in the preheader that we can eliminate:
    // We only create a single access no matter how many accesses to
    // the same storage are inside the loop.
    // If there was an access outside of the loop then no new potential
    // would have been exposed by LoopDominatingAccessAdder?
  }
};
} // namespace

SILTransform *swift::createAccessEnforcementDom() {
  return new AccessEnforcementDom();
}
