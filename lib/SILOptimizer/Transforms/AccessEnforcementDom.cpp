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
/// The second access scope does not need to be emitted.
///
/// Note: This optimization must be aware of all possible access to a Class or
/// Global address. This includes unpaired access instructions and keypath
/// entry points. Ignoring any access pattern would weaken enforcement.
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "access-enforcement-dom"

#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/MemAccessUtils.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
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

  void perform();

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

void DominatedAccessRemoval::perform() {
  if (func.empty())
    return;

  if (!analyze()) {
    LLVM_DEBUG(llvm::dbgs()
               << "Bailed on function: " << func.getName() << "\n");
    return;
  }
  optimize();
}

namespace {
struct AccessEnforcementDom : public SILFunctionTransform {
  void run() override {
    DominanceAnalysis *domAnalysis = getAnalysis<DominanceAnalysis>();
    DominanceInfo *domInfo = domAnalysis->get(getFunction());
    DominatedAccessRemoval eliminationPass(*getFunction(), domInfo);
    eliminationPass.perform();
  }
};
} // namespace

SILTransform *swift::createAccessEnforcementDom() {
  return new AccessEnforcementDom();
}
