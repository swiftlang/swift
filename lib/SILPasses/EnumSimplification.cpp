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

  using SmallBlotMapVector = BlotMapVector<
      SILValue, EnumElementDecl *, llvm::SmallDenseMap<SILValue, unsigned>,
      llvm::SmallVector<std::pair<SILValue, EnumElementDecl *>, 4>>;

  SmallBlotMapVector ValueToCaseMap;

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
  Range<SmallBlotMapVector::iterator> currentTrackedState() {
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
  void mergePredecessorStates(
      PreallocatedMap<SILBasicBlock *, BBEnumTagDataflowState> &BBToStateMap);
  void handlePredSwitchEnum(TermInst *TI);
};

} // end anonymous namespace

namespace llvm {

raw_ostream &operator<<(raw_ostream &OS, BBEnumTagDataflowState &State) {
  auto TS = State.currentTrackedState();
  if (TS.begin() == TS.end())
    return OS << "            No stored state.\n";
  for (auto P : TS) {
    OS << "                Key: " << P.first;
    OS << "                    Value: ";
    P.second->dump(OS);
  }
  return OS;
}

} // end namespace llvm

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

  DEBUG(llvm::dbgs() << "FirstPred: " << *PI << "\n");

  // Initialize our state with our first predecessor...
  SILBasicBlock *BB = *PI;
  BBEnumTagDataflowState &FirstPredState = BBToStateMap[BB];
  DEBUG(llvm::dbgs() << "FirstPredState: " << FirstPredState);
  DEBUG(llvm::dbgs() << "FirstPredState Addr: " << &FirstPredState << "\n");
  assert(FirstPredState.getBB() == *PI);
  FirstPredState.init(*PI);
  DEBUG(llvm::dbgs() << "FirstPredState: " << FirstPredState);
  ValueToCaseMap = FirstPredState.ValueToCaseMap;
  DEBUG(llvm::dbgs() << "Self: " << *this);
  ++PI;

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

    // Then for each (silvalue, enum tag) that we are tracking...
    for (auto P : ValueToCaseMap.getItems()) {
      // If the entry was blotted, skip it...
      if (!P.first)
        continue;

      // Then attempt to look up the enum state for our SILValue in the other
      // predecessor.
      auto OtherValue = PredState.ValueToCaseMap.find(P.first);

      // If we find the state and it was not blotted...
      if (OtherValue != PredState.ValueToCaseMap.end() && OtherValue->first) {
        // Add the enum tag to the list of predecessor tags so we can check if
        // our predecessors cover this value... If we do so, we can move
        // release_values on the enum up the CFG into the predecessors and place
        // the release_values on payloads if the enum has them or completely
        // eliminate them if no payload is present.
        //
        // TODO: Implement this here.

        // And the states match, the enum state propagates to this BB.
        if (OtherValue->second == P.second)
          continue;
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

static bool processFunction(SILFunction &F) {
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
    for (SILBasicBlock *Pred : BB->getPreds()) {
      DEBUG(llvm::dbgs() << "        BB#" << BBToRPOTNumMap[Pred]
                         << "; Ptr: " << Pred << "\n");
    }

    // Now that we have made sure that our predecessors all have initialized
    // states, grab the state for this BB. If we don't have a state for this BB,
    // this creates the state.
    auto P = BBToStateMap.find(BB);
    assert(P != BBToStateMap.end() && "Every BB should have state now.");
    BBEnumTagDataflowState &State = P->second;
    DEBUG(llvm::dbgs() << "    State Addr: " << &State << "\n");

    // Then merge in our predecessor states. We relook up our the states for our
    // predecessors to avoid memory invalidation issues due to copying in the
    // dense map.
    DEBUG(llvm::dbgs() << "    Merging predecessors!\n");
    State.mergePredecessorStates(BBToStateMap);

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
