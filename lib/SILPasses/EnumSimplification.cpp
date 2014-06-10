//===--- EnumSimplification.cpp - Propagate enum case ---------------------===//
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

#define DEBUG_TYPE "sil-enum-simplification"
#include "swift/SILPasses/Passes.h"
#include "swift/Basic/BlotMapVector.h"
#include "swift/Basic/PreallocatedMap.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/CFG.h"
#include "swift/AST/Decl.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/Support/Allocator.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"

STATISTIC(NumRefCountOpsSimplified, "number of enum ref count ops simplified.");
STATISTIC(NumReleasesMovedIntoSwitches, "number of release moved into switch "
          "regions");

using namespace swift;

//===----------------------------------------------------------------------===//
//                                  Utility
//===----------------------------------------------------------------------===//

static void createRefCountOpForPayload(SILBuilder &Builder, SILInstruction *I,
                                       EnumElementDecl *EnumDecl) {
  ++NumRefCountOpsSimplified;

  SILModule &Mod = I->getModule();
  SILType ArgType =
      I->getOperand(0).getType().getEnumElementType(EnumDecl, Mod);
  auto *UEDI = Builder.createUncheckedEnumData(I->getLoc(), I->getOperand(0),
                                               EnumDecl, ArgType);
  if (isa<RetainValueInst>(I)) {
    Builder.createRetainValue(I->getLoc(), UEDI);
    return;
  }

  Builder.createReleaseValue(I->getLoc(), UEDI);
}

namespace {

/// Class that performs enum tag state dataflow on the given BB.
class BBEnumTagDataflowState
    : public SILInstructionVisitor<BBEnumTagDataflowState, bool> {
  NullablePtr<SILBasicBlock> BB;

  using ValueToCaseSmallBlotMapVectorTy =
    BlotMapVector<SILValue, EnumElementDecl *,
                  llvm::SmallDenseMap<SILValue, unsigned>,
                  llvm::SmallVector<std::pair<SILValue,
                                              EnumElementDecl *>, 4>>;
  ValueToCaseSmallBlotMapVectorTy ValueToCaseMap;

  using PredCase = llvm::SmallVector<std::pair<SILBasicBlock *,
                                               EnumElementDecl *>, 2>;
  llvm::DenseMap<SILValue, PredCase> EnumToPredCaseMap;

public:
  BBEnumTagDataflowState() : BB(nullptr), ValueToCaseMap() {}
  BBEnumTagDataflowState(const BBEnumTagDataflowState &Other)
      : BB(const_cast<SILBasicBlock *>(Other.BB.getPtrOrNull())),
        ValueToCaseMap() {}
  ~BBEnumTagDataflowState() = default;

  bool init(SILBasicBlock *NewBB) {
    if (BB.isNonNull())
      return false;
    BB = NewBB;
    return true;
  }

  SILBasicBlock *getBB() { return BB.get(); }

  using iterator = decltype(ValueToCaseMap)::iterator;
  iterator begin() { return ValueToCaseMap.getItems().begin(); }
  iterator end() { return ValueToCaseMap.getItems().begin(); }
  Range<iterator> currentTrackedState() {
    return ValueToCaseMap.getItems();
  }

  void clear() { ValueToCaseMap.clear(); }

  bool visitValueBase(ValueBase *V) { return false; }

  bool visitEnumInst(EnumInst *EI) {
    DEBUG(llvm::dbgs() << "    Storing enum into map: " << *EI);
    ValueToCaseMap[SILValue(EI)] = EI->getElement();
    return false;
  }

  bool visitUncheckedEnumDataInst(UncheckedEnumDataInst *UEDI) {
    DEBUG(
        llvm::dbgs() << "    Storing unchecked enum data into map: " << *UEDI);
    ValueToCaseMap[SILValue(UEDI->getOperand())] = UEDI->getElement();
    return false;
  }

  bool visitRetainValueInst(RetainValueInst *RVI);
  bool visitReleaseValueInst(ReleaseValueInst *RVI);
  bool process();
  void
  mergePredecessorStates(PreallocatedMap<SILBasicBlock *,
                                         BBEnumTagDataflowState> &BBToStateMap);
  bool
  moveReleasesUpCFGIfCasesCovered();
  void handlePredSwitchEnum(TermInst *TI);
};

} // end anonymous namespace

void BBEnumTagDataflowState::handlePredSwitchEnum(TermInst *TI) {
  // Check if the predecessor BB ends in a switch_enum statement...
  auto *S = dyn_cast<SwitchEnumInst>(TI);

  // If it does not, we are done...
  if (!S)
    return;

  // Otherwise, find the tag associated with our BB and set the state of the
  // enum we switch on to that value. This is important so we can determine
  // covering switches for enums that have cases without payload.

  // Next check if we are the target of a default switch_eum case. If we are,
  // no interesting information can be extracted, so bail...
  if (S->hasDefault() && S->getDefaultBB() == getBB())
    return;

  // Otherwise, attempt to find the tag associated with this BB in the switch
  // enum...
  for (unsigned i = 0, e = S->getNumCases(); i != e; ++i) {
    auto P = S->getCase(i);
    // If we don't match our BB, skip this interation.
    if (P.second != getBB())
      continue;
    // Ok, we have a matching BB. If we don't have a case (which can happen if
    // we have a default statement), return, there is nothing more we can do.
    if (!P.first)
      return;
    // Ok, we have a matching BB and a matching case. Set the state and
    // return.
    ValueToCaseMap[S->getOperand()] = P.first;
    return;
  }
  llvm_unreachable("A successor of a switch_enum terminated BB should be in "
                   "the switch_enum.");
}

void BBEnumTagDataflowState::mergePredecessorStates(
    PreallocatedMap<SILBasicBlock *, BBEnumTagDataflowState> &BBToStateMap) {
  // If we have no precessors, there is nothing to do so return early...
  if (getBB()->pred_empty()) {
    DEBUG(llvm::dbgs() << "            No Preds.\n");
    return;
  }

  auto PI = getBB()->pred_begin(), PE = getBB()->pred_end();

  if (*PI == getBB()) {
    DEBUG(llvm::dbgs() << "            Found a self loop. Bailing!\n");
    return;
  }

  // Initialize our state with our first predecessor...
  SILBasicBlock *BB = *PI;
  BBEnumTagDataflowState &FirstPredState = BBToStateMap[BB];
  FirstPredState.init(BB);
  ++PI;
  ValueToCaseMap = FirstPredState.ValueToCaseMap;

  // If we are predecessors only successor, we can potentially hoist releases
  // into it.
  if (FirstPredState.getBB()->getSingleSuccessor()) {
    for (auto P : ValueToCaseMap.getItems())
      EnumToPredCaseMap[P.first].push_back({BB, P.second});
  }

  // If we only have one predecessor...
  if (PI == PE) {
    // Grab the terminator of that successor and if it is a switch enum, mix it
    // into this state.
    TermInst *PredTerm = FirstPredState.getBB()->getTerminator();
    if (auto *S = dyn_cast<SwitchEnumInst>(PredTerm))
      handlePredSwitchEnum(S);

    // There are no other predecessors to merge in. return.
    return;
  }

  DEBUG(llvm::dbgs() << "            Merging in rest of perdecessors...\n");

  // And for each remaining predecessor...
  do {
    // If we loop on ourselves, bail...
    if (*PI == getBB()) {
      DEBUG(llvm::dbgs() << "            Found a self loop. Bailing!\n");
      return;
    }

    // Grab the predecessors state...
    SILBasicBlock *OtherBB = *PI;
    BBEnumTagDataflowState &PredState = BBToStateMap[OtherBB];
    PredState.init(*PI);
    ++PI;

    // Then for each (SILValue, Enum Tag) that we are tracking...
    for (auto P : ValueToCaseMap.getItems()) {
      // If the entry was blotted, skip it...
      if (!P.first)
        continue;

      // Then attempt to look up the enum state for our SILValue in the other
      // predecessor.
      auto OtherValue = PredState.ValueToCaseMap.find(P.first);

      // If we find the state and it was not blotted...
      if (OtherValue != PredState.ValueToCaseMap.end() && OtherValue->first) {

        // Check if out predecessor has any other successors. If that is true we
        // clear all the state since we can not hoist safely.
        if (!PredState.getBB()->getSingleSuccessor()) {
          EnumToPredCaseMap.clear();
        } else {
          // Otherwise, add this case to our predecessor case list. We will unique
          // this after we have finished processing all predecessors.
          auto Case = std::make_pair(PredState.getBB(), OtherValue->second);
          EnumToPredCaseMap[OtherValue->first].push_back(Case);
        }

        // And the states match, the enum state propagates to this BB.
        if (OtherValue->second == P.second)
          continue;
      } else {
        // If we fail to find any state, we can not cover the switch along every
        // BB path... Clear all predecessor cases that we are tracking so we
        // don't attempt to perform that optimization.
        EnumToPredCaseMap.clear();
      }

      // Otherwise, we are conservative and do not forward the EnumTag that we
      // are tracking. Blot it!
      ValueToCaseMap.blot(P.first);
    }
  } while (PI != PE);
}

bool BBEnumTagDataflowState::visitRetainValueInst(RetainValueInst *RVI) {
  auto FindResult = ValueToCaseMap.find(RVI->getOperand());
  if (FindResult == ValueToCaseMap.end())
    return false;

  // If we do not have any argument, kill the retain_value.
  if (!FindResult->second->hasArgumentType()) {
    RVI->eraseFromParent();
    return true;
  }

  DEBUG(llvm::dbgs() << "    Found RetainValue: " << *RVI);
  DEBUG(llvm::dbgs() << "        Paired to Enum Oracle: " << FindResult->first);

  SILBuilder Builder(RVI);
  createRefCountOpForPayload(Builder, RVI, FindResult->second);
  RVI->eraseFromParent();
  return true;
}

bool BBEnumTagDataflowState::visitReleaseValueInst(ReleaseValueInst *RVI) {
  auto FindResult = ValueToCaseMap.find(RVI->getOperand());
  if (FindResult == ValueToCaseMap.end())
    return false;

  // If we do not have any argument, just delete the release value.
  if (!FindResult->second->hasArgumentType()) {
    RVI->eraseFromParent();
    return true;
  }

  DEBUG(llvm::dbgs() << "    Found ReleaseValue: " << *RVI);
  DEBUG(llvm::dbgs() << "        Paired to Enum Oracle: " << FindResult->first);

  SILBuilder Builder(RVI);
  createRefCountOpForPayload(Builder, RVI, FindResult->second);
  RVI->eraseFromParent();
  return true;
}

bool BBEnumTagDataflowState::process() {
  bool Changed = false;

  auto SI = getBB()->begin(), SE = getBB()->end();
  while (SI != SE) {
    SILInstruction *I = &*SI;
    ++SI;
    Changed |= visit(I);
  }

  return Changed;
}

bool
BBEnumTagDataflowState::moveReleasesUpCFGIfCasesCovered() {
  bool Changed = false;
  unsigned NumPreds = std::distance(getBB()->pred_begin(), getBB()->pred_end());
  for (auto II = getBB()->begin(), IE = getBB()->end(); II != IE;) {
    auto *RVI = dyn_cast<ReleaseValueInst>(&*II);
    ++II;

    // For now we stop whenever we see a non-release pointer. We can extend this
    // later to use can use from ARCAnalysis.
    if (!RVI)
      break;

    SILValue Op = RVI->getOperand();
    auto &PredCase = EnumToPredCaseMap[Op];

    // If we don't have an enum decl for each predecessor, bail...
    if (PredCase.size() != NumPreds)
      continue;

    // Otherwise perform the transformation.
    for (auto P : PredCase) {
      // If we don't have an argument for this case, there is nothing to
      // do... continue...
      if (!P.second->hasArgumentType())
        continue;

      // Otherwise create the release_value before the terminator of the
      // predecessor.
      assert(P.first->getSingleSuccessor() &&
             "Can not hoist release into BB that has multiple successors");
      SILBuilder Builder(P.first->getTerminator());
      createRefCountOpForPayload(Builder, RVI, P.second);
    }

    RVI->eraseFromParent();
    ++NumReleasesMovedIntoSwitches;
    Changed = true;
  }
  return Changed;
}

static bool processFunction(SILFunction &F) {

  DEBUG(llvm::dbgs() << "**** Enum Simplification: " << F.getName() << " ****\n");

  bool Changed = false;
  std::vector<SILBasicBlock *> PostOrder;
  std::copy(po_begin(&F), po_end(&F), std::back_inserter(PostOrder));

  using PairTy = std::pair<SILBasicBlock *, BBEnumTagDataflowState>;
  auto SortFn = [](const PairTy &P1,
                   const PairTy &P2) { return P1.first < P2.first; };

  PreallocatedMap<SILBasicBlock *, BBEnumTagDataflowState> BBToStateMap(
      PostOrder.size(), SortFn);

#ifndef NDEBUG
  llvm::DenseMap<SILBasicBlock *, unsigned> BBToRPOTNumMap;
  unsigned RPOTId = 0;
#endif

  for (int i = PostOrder.size() - 1; i >= 0; --i) {
    SILBasicBlock *BB = PostOrder[i];
    BBToStateMap[i].first = BB;
    BBToStateMap[i].second.init(BB);
#ifndef NDEBUG
    BBToRPOTNumMap[BB] = RPOTId++;
#endif
  }
  BBToStateMap.sort();

  for (auto BI = PostOrder.rbegin(), BE = PostOrder.rend(); BI != BE; ++BI) {
    SILBasicBlock *BB = *BI;
    DEBUG(llvm::dbgs() << "Visiting BB#" << BBToRPOTNumMap[BB] << "\n");

    // We do this here to avoid issues with inserting, finding into BBToStateMap
    // while using memory that it owns (State below).
    DEBUG(llvm::dbgs() << "    Predecessors (empty if no predecessors):\n");
    DEBUG(for (SILBasicBlock *Pred : BB->getPreds()) {
        llvm::dbgs() << "        BB#" << BBToRPOTNumMap[Pred]
                     << "; Ptr: " << Pred << "\n";
    });

    // Now that we have made sure that our predecessors all have initialized
    // states, grab the state for this BB. If we don't have a state for this BB,
    // this creates the state.
    BBEnumTagDataflowState &State = BBToStateMap[BB];
    DEBUG(llvm::dbgs() << "    State Addr: " << &State << "\n");

    // Then merge in our predecessor states. We relook up our the states for our
    // predecessors to avoid memory invalidation issues due to copying in the
    // dense map.
    DEBUG(llvm::dbgs() << "    Merging predecessors!\n");
    State.mergePredecessorStates(BBToStateMap);

    // If our predecessors cover any of our enum values, attempt to move
    // releases up the CFG onto the enum payloads (if they exist) or eliminate
    // them entirely.
    Changed |= State.moveReleasesUpCFGIfCasesCovered();

    // Then perform the dataflow.
    Changed |= State.process();
  }

  return Changed;
}

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

namespace {
class EnumSimplification : public SILFunctionTransform {
  /// The entry point to the transformation.
  void run() {
    if (processFunction(*getFunction()))
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }

  StringRef getName() override { return "Enum Simplification"; }
};
} // end anonymous namespace

SILTransform *swift::createEnumSimplification() {
  return new EnumSimplification();
}
