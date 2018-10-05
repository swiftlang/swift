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

using namespace swift;

namespace {
class DominatedAccessRemoval {
public:
  using AccessedStoragePair = std::pair<BeginAccessInst *, AccessedStorage>;
  using AccessedStorageInfo = llvm::SmallVector<AccessedStoragePair, 32>;
  using DominatorToDominatedPair =
      std::pair<BeginAccessInst *, BeginAccessInst *>;
  using DomPairSet = llvm::SmallVector<DominatorToDominatedPair, 32>;
  using KeyPathEntryPointsSet = llvm::SmallSet<SILInstruction *, 8>;
  using UnpairedAccessToStoragePair =
      std::pair<BeginUnpairedAccessInst *, AccessedStorage>;
  using UnpairedAccessToStorageInfo =
      llvm::SmallVector<UnpairedAccessToStoragePair, 8>;

public:
  DominatedAccessRemoval(SILFunction &func, DominanceInfo *domInfo)
      : func(func), domInfo(domInfo) {}

  void perform();

protected:
  void visitInstruction(SILInstruction *instr);
  void visitBeginAccess(BeginAccessInst *beginAccess, AccessedStorage storage);
  bool domByKeyPath(BeginAccessInst *dominatedInstr);
  bool domByRelevantUnpairedAccess(DominatorToDominatedPair pair);
  void analyze();
  void optimize();

private:
  SILFunction &func;
  DominanceInfo *domInfo;
  AccessedStorageInfo accessInfo;
  DomPairSet domPairs;
  KeyPathEntryPointsSet keypathEntries;
  UnpairedAccessToStorageInfo unpairedEntries;
};
} // namespace

bool DominatedAccessRemoval::domByKeyPath(BeginAccessInst *dominatedInstr) {
  for (SILInstruction *keyPathEntry : keypathEntries) {
    if (domInfo->properlyDominates(keyPathEntry, dominatedInstr)) {
      return true;
    }
  }
  return false;
}

bool DominatedAccessRemoval::domByRelevantUnpairedAccess(
    DominatorToDominatedPair pair) {
  BeginAccessInst *parentBegin = pair.first;
  BeginAccessInst *dominatedInstr = pair.second;
  auto predEqual = [&](AccessedStoragePair it) {
    auto currInstr = it.first;
    return currInstr == parentBegin;
  };
  auto currStorageIt =
      std::find_if(accessInfo.begin(), accessInfo.end(), predEqual);
  assert(currStorageIt != accessInfo.end() && "Expected storage in accessInfo");
  AccessedStorage currStorage = currStorageIt->second;
  for (UnpairedAccessToStoragePair unpairedEntry : unpairedEntries) {
    auto *instr = unpairedEntry.first;
    if (!domInfo->properlyDominates(instr, dominatedInstr)) {
      continue;
    }
    auto entryStorage = unpairedEntry.second;
    if (!currStorage.isDistinctFrom(entryStorage)) {
      return true;
    }
  }
  return false;
}

void DominatedAccessRemoval::visitInstruction(SILInstruction *instr) {
  if (auto *BAI = dyn_cast<BeginAccessInst>(instr)) {
    if (BAI->getEnforcement() != SILAccessEnforcement::Dynamic) {
      return;
    }
    AccessedStorage storage = findAccessedStorageNonNested(BAI->getSource());
    if (!storage) {
      return;
    }
    visitBeginAccess(BAI, storage);
  } else if (auto fullApply = FullApplySite::isa(instr)) {
    SILFunction *callee = fullApply.getReferencedFunction();
    if (!callee)
      return;
    if (!callee->hasSemanticsAttr("_keyPathEntryPoint"))
      return;
    // we can't eliminate dominated checks even when we can prove that
    // the dominated scope has no internal nested conflicts.
    keypathEntries.insert(fullApply.getInstruction());
  } else if (auto *BUAI = dyn_cast<BeginUnpairedAccessInst>(instr)) {
    AccessedStorage storage = findAccessedStorageNonNested(BUAI->getSource());
    unpairedEntries.push_back(std::make_pair(BUAI, storage));
  }
}

void DominatedAccessRemoval::visitBeginAccess(BeginAccessInst *beginAccess,
                                              AccessedStorage storage) {
  auto predEqual = [&](AccessedStoragePair it) {
    auto currStorage = it.second;
    return currStorage.hasIdenticalBase(storage);
  };

  // If the currnet access has nested conflict, just add it to map
  // we can't remove it by finding a dominating access
  if (!beginAccess->hasNoNestedConflict()) {
    accessInfo.push_back(std::make_pair(beginAccess, storage));
    return;
  }

  auto it = std::find_if(accessInfo.begin(), accessInfo.end(), predEqual);
  while (it != accessInfo.end()) {
    BeginAccessInst *parentBeginAccess = it->first;
    if (!domInfo->properlyDominates(parentBeginAccess, beginAccess)) {
      ++it;
      it = std::find_if(it, accessInfo.end(), predEqual);
      continue;
    }
    // Found a pair that can potentially be optimized
    domPairs.push_back(std::make_pair(parentBeginAccess, beginAccess));
    return;
  }

  // Did not find a dominating access to same storage
  accessInfo.push_back(std::make_pair(beginAccess, storage));
}

// Finds domPairs for which we can change the dominated instruction to static
// NOTE: We might not be able to optimize some the pairs due to other
// restrictions Such as key-path or unpaired begin access We only traverse the
// function once, if we find a pattern that *might* prevent optimization, we
// just add it to appropriate data structures which will be analyzed later.
void DominatedAccessRemoval::analyze() {
  SILBasicBlock *entry = &func.front();
  DominanceOrder domOrder(entry, domInfo, func.size());
  while (SILBasicBlock *block = domOrder.getNext()) {
    for (auto &instr : *block) {
      visitInstruction(&instr);
    }
    domOrder.pushChildren(block);
  }
}

// Sets the dominated instruction to static.
// Goes through the data structures initialized by the analysis method
// and makes sure we are not Weakening enforcement
void DominatedAccessRemoval::optimize() {
  for (DominatorToDominatedPair pair : domPairs) {
    LLVM_DEBUG(llvm::dbgs()
               << "Processing optimizable pair - Dominator: " << *pair.first
               << " , Dominated: " << *pair.second << "\n");
    BeginAccessInst *dominatedInstr = pair.second;
    // look through keypathEntries to see if dominatedInstr
    // can no longer be optimized
    if (domByKeyPath(dominatedInstr)) {
      LLVM_DEBUG(llvm::dbgs()
                 << "Can not set " << *dominatedInstr
                 << " access enforcement to static - it is properly dominated "
                    "by a key-path entry point\n");
      continue;
    }
    if (domByRelevantUnpairedAccess(pair)) {
      LLVM_DEBUG(llvm::dbgs()
                 << "Can not set " << *dominatedInstr
                 << " access enforcement to static - there's an unpaired "
                    "access that is not distinct from it in the way\n");
      continue;
    }
    LLVM_DEBUG(llvm::dbgs() << "Setting " << *dominatedInstr
                            << " access enforcement to static\n");
    dominatedInstr->setEnforcement(SILAccessEnforcement::Static);
  }
}

void DominatedAccessRemoval::perform() {
  if (func.empty())
    return;

  analyze();
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
