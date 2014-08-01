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
#include "swift/SILAnalysis/AliasAnalysis.h"
#include "swift/SILAnalysis/PostOrderAnalysis.h"
#include "swift/SILAnalysis/ARCAnalysis.h"
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

  SILType UEDITy = UEDI->getType();

  // If our payload is trivial, we do not need to insert any retain or release
  // operations.
  if (UEDITy.isTrivial(Mod))
    return;

  // If we have a retain value...
  if (isa<RetainValueInst>(I)) {
    // And our payload has reference semantics, insert a strong_retain onto the
    // payload.
    if (UEDITy.hasReferenceSemantics()) {
      Builder.createStrongRetain(I->getLoc(), UEDI);
      return;
    }

    // Otherwise, insert a retain_value on the payload.
    Builder.createRetainValue(I->getLoc(), UEDI);
    return;
  }

  // At this point we know that we must have a release_value and a non-trivial
  // payload.
  assert(isa<ReleaseValueInst>(I) && "If I is not a retain value here, it must "
         "be a release value since enums do not have reference semantics.");

  // If our payload has reference semantics, insert the strong release.
  if (UEDITy.hasReferenceSemantics()) {
    Builder.createStrongRelease(I->getLoc(), UEDI);
    return;
  }

  // Otherwise if our payload is non-trivial but lacking reference semantics,
  // insert the release_value.
  Builder.createReleaseValue(I->getLoc(), UEDI);
}

namespace {

/// Class that performs enum tag state dataflow on the given BB.
class BBEnumTagDataflowState
    : public SILInstructionVisitor<BBEnumTagDataflowState, bool> {
  NullablePtr<SILBasicBlock> BB;
  NullablePtr<AliasAnalysis> AA;

  using ValueToCaseSmallBlotMapVectorTy =
    BlotMapVector<SILValue, EnumElementDecl *,
                  llvm::SmallDenseMap<SILValue, unsigned>,
                  llvm::SmallVector<std::pair<SILValue,
                                              EnumElementDecl *>, 4>>;
  ValueToCaseSmallBlotMapVectorTy ValueToCaseMap;

  using EnumBBCaseList = llvm::SmallVector<std::pair<SILBasicBlock *,
                                                     EnumElementDecl *>, 2>;
  llvm::DenseMap<SILValue, EnumBBCaseList> EnumToEnumBBCaseListMap;

public:
  using PreallocatedMap = PreallocatedMap<SILBasicBlock *,
                                          BBEnumTagDataflowState>;

  BBEnumTagDataflowState() = default;
  BBEnumTagDataflowState(const BBEnumTagDataflowState &Other)
      : BB(const_cast<SILBasicBlock *>(Other.BB.getPtrOrNull())),
        AA(const_cast<AliasAnalysis *>(Other.AA.getPtrOrNull())),
        ValueToCaseMap() {}
  ~BBEnumTagDataflowState() = default;

  bool init(SILBasicBlock *NewBB,
            AliasAnalysis *NewAA) {
    assert(NewBB && "NewBB should not be null");
    assert(NewAA && "NewAA should not be null");
    BB = NewBB;
    AA = NewAA;
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
  mergePredecessorStates(PreallocatedMap &BBToStateMap);
  bool
  moveReleasesUpCFGIfCasesCovered();
  void handlePredSwitchEnum(SwitchEnumInst *S);
  void handlePredCondEnumIsTag(CondBranchInst *CondBr);
};

} // end anonymous namespace

void BBEnumTagDataflowState::handlePredSwitchEnum(SwitchEnumInst *S) {

  // Find the tag associated with our BB and set the state of the
  // enum we switch on to that value. This is important so we can determine
  // covering switches for enums that have cases without payload.

  // Next check if we are the target of a default switch_enum case. If we are,
  // no interesting information can be extracted, so bail...
  if (S->hasDefault() && S->getDefaultBB() == getBB())
    return;

  // Otherwise, attempt to find the tag associated with this BB in the switch
  // enum...
  for (unsigned i = 0, e = S->getNumCases(); i != e; ++i) {
    auto P = S->getCase(i);

    // If this case of the switch is not matched up with this BB, skip the
    // case...
    if (P.second != getBB())
      continue;

    // Ok, we found the case for our BB. If we don't have an enum tag (which can
    // happen if we have a default statement), return. There is nothing more we
    // can do.
    if (!P.first)
      return;

    // Ok, we have a matching BB and a matching enum tag. Set the state and
    // return.
    ValueToCaseMap[S->getOperand()] = P.first;
    return;
  }
  llvm_unreachable("A successor of a switch_enum terminated BB should be in "
                   "the switch_enum.");
}

void BBEnumTagDataflowState::handlePredCondEnumIsTag(CondBranchInst *CondBr) {

  EnumIsTagInst *EITI = dyn_cast<EnumIsTagInst>(CondBr->getCondition());
  if (!EITI)
    return;

  // Find the tag associated with our BB and set the state of the
  // enum we switch on to that value. This is important so we can determine
  // covering switches for enums that have cases without payload.

  // Check if we are the true case, ie, we know that we are the given tag.
  const auto &Operand = EITI->getOperand();
  if (CondBr->getTrueBB() == getBB()) {
    ValueToCaseMap[Operand] = EITI->getElement();
    return;
  }

  // If the enum only has 2 values and its tag isn't the true branch, then we
  // know the true branch must be the other tag.
  if (EnumDecl *E = Operand.getType().getEnumOrBoundGenericEnum()) {
    // Look for a single other element on this enum.
    EnumElementDecl *OtherElt = nullptr;
    for (EnumElementDecl *Elt : E->getAllElements()) {
      // Skip the case where we find the enum_is_tag element
      if (Elt == EITI->getElement())
        continue;
      // If we find another element, then we must have more than 2, so bail.
      if (OtherElt)
        return;
      OtherElt = Elt;
    }
    // Only a single enum element?  How would this even get here?  We should
    // handle it in SILCombine.
    if (!OtherElt)
      return;
    // FIXME: Can we ever not be the false BB here?
    if (CondBr->getTrueBB() != getBB()) {
      ValueToCaseMap[Operand] = OtherElt;
      return;
    }
  }
}

void
BBEnumTagDataflowState::
mergePredecessorStates(PreallocatedMap &BBToStateMap) {

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
  SILBasicBlock *OtherBB = *PI;
  auto Iter = BBToStateMap.find(OtherBB);
  if (Iter == BBToStateMap.end()) {
    DEBUG(llvm::dbgs() << "        Found an unreachable block!\n");
    return;
  }
  BBEnumTagDataflowState &FirstPredState = Iter->second;
  ++PI;
  ValueToCaseMap = FirstPredState.ValueToCaseMap;

  // If we are predecessors only successor, we can potentially hoist releases
  // into it.
  if (OtherBB->getSingleSuccessor()) {
    for (auto P : ValueToCaseMap.getItems())
      EnumToEnumBBCaseListMap[P.first].push_back({OtherBB, P.second});
  }

  // If we only have one predecessor...
  if (PI == PE) {
    // Grab the terminator of that successor and if it is a switch enum, mix it
    // into this state.
    TermInst *PredTerm = OtherBB->getTerminator();
    if (auto *S = dyn_cast<SwitchEnumInst>(PredTerm))
      handlePredSwitchEnum(S);
    else if (auto *CondBr = dyn_cast<CondBranchInst>(PredTerm))
      handlePredCondEnumIsTag(CondBr);

    // There are no other predecessors to merge in. return.
    return;
  }

  DEBUG(llvm::dbgs() << "            Merging in rest of perdecessors...\n");

  llvm::SmallVector<SILValue, 4> ValuesToBlot;

  // And for each remaining predecessor...
  do {
    // If we loop on ourselves, bail...
    if (*PI == getBB()) {
      DEBUG(llvm::dbgs() << "            Found a self loop. Bailing!\n");
      return;
    }

    // Grab the predecessors state...
    OtherBB = *PI;

    auto OtherIter = BBToStateMap.find(OtherBB);
    if (OtherIter == BBToStateMap.end()) {
      DEBUG(llvm::dbgs() << "            Found an unreachable block!\n");
      return;
    }

    BBEnumTagDataflowState &PredState = OtherIter->second;
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
        if (!OtherBB->getSingleSuccessor()) {
          EnumToEnumBBCaseListMap.clear();
          DEBUG(llvm::dbgs() << "                Predecessor has other "
                "successors. Clearing BB cast list map.\n");
        } else {
          // Otherwise, add this case to our predecessor case list. We will unique
          // this after we have finished processing all predecessors.
          auto Case = std::make_pair(OtherBB, OtherValue->second);
          EnumToEnumBBCaseListMap[OtherValue->first].push_back(Case);
        }

        // And the states match, the enum state propagates to this BB.
        if (OtherValue->second == P.second)
          continue;
      } else {
        // If we fail to find any state, we can not cover the switch along every
        // BB path... Clear all predecessor cases that we are tracking so we
        // don't attempt to perform that optimization.
        EnumToEnumBBCaseListMap.clear();
        DEBUG(llvm::dbgs() << "                Failed to find state. Clearing "
              "BB cast list map.\n");
      }

      // Otherwise, we are conservative and do not forward the EnumTag that we
      // are tracking. Blot it!
      DEBUG(llvm::dbgs() << "                Blotting: " << P.first);
      ValuesToBlot.push_back(P.first);
    }
  } while (PI != PE);

  for (SILValue V : ValuesToBlot) {
    ValueToCaseMap.blot(V);
  }
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

    // If this instruction is not a release, skip it...
    if (!RVI)
      continue;

    DEBUG(llvm::dbgs() << "        Visiting release: " << *RVI);

    // Grab the operand of the release value inst.
    SILValue Op = RVI->getOperand();

    // Lookup the [(BB, EnumTag)] list for this operand.
    auto R = EnumToEnumBBCaseListMap.find(Op);
    // If we don't have one, skip this release value inst.
    if (R == EnumToEnumBBCaseListMap.end()) {
      DEBUG(llvm::dbgs() << "            Could not find [(BB, EnumTag)] "
            "list for release_value's operand. Bailing!\n");
      continue;
    }

    auto &EnumBBCaseList = R->second;
    // If we don't have an enum tag for each predecessor of this BB, bail since
    // we do not know how to handle that BB.
    if (EnumBBCaseList.size() != NumPreds) {
      DEBUG(llvm::dbgs() << "            Found [(BB, EnumTag)] "
            "list for release_value's operand, but we do not have an enum tag "
            "for each predecessor. Bailing!\n");
      DEBUG(llvm::dbgs() << "            List:\n");
      DEBUG(for (auto P : EnumBBCaseList) {
          llvm::dbgs() << "                "; P.second->dump(llvm::dbgs());
        });
      continue;
    }

    // Finally ensure that we have no users of this operand preceding the
    // release_value in this BB. If we have users like that we can not hoist the
    // release past them unless we know that there is an additional set of
    // releases that together post-dominate this release. If we can not do this,
    // skip this release.
    //
    // TODO: We need information from the ARC optimizer to prove that property
    // if we are going to use it.
    if (arc::valueHasARCUsesInInstructionRange(Op, getBB()->begin(),
                                               SILBasicBlock::iterator(RVI),
                                               AA.get())) {
      DEBUG(llvm::dbgs() << "            Release value has use that stops "
            "hoisting! Bailing!\n");
      continue;
    }

    DEBUG(llvm::dbgs() << "            Its safe to perform the "
          "transformation!\n");

    // Otherwise perform the transformation.
    for (auto P : EnumBBCaseList) {
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

static bool processFunction(SILFunction &F, AliasAnalysis *AA,
                            PostOrderAnalysis *POTA) {

  DEBUG(llvm::dbgs() << "**** Enum Simplification: " << F.getName()
                     << " ****\n");

  bool Changed = false;

  auto ReversePostOrder = POTA->getReversePostOrder(&F);
  int PostOrderSize = std::distance(ReversePostOrder.begin(),
                                    ReversePostOrder.end());

  using PairTy = std::pair<SILBasicBlock *, BBEnumTagDataflowState>;
  auto SortFn = [](const PairTy &P1,
                   const PairTy &P2) { return P1.first < P2.first; };

  BBEnumTagDataflowState::PreallocatedMap BBToStateMap(PostOrderSize,
                                                       SortFn);

#ifndef NDEBUG
  llvm::DenseMap<SILBasicBlock *, unsigned> BBToRPOTNumMap;
  unsigned RPOTId = 0;
#endif

  unsigned i = 0;
  for (SILBasicBlock *BB : ReversePostOrder) {
    BBToStateMap[i].first = BB;
    BBToStateMap[i].second.init(BB, AA);
#ifndef NDEBUG
    BBToRPOTNumMap[BB] = RPOTId++;
#endif
    i++;
  }
  BBToStateMap.sort();

  for (SILBasicBlock *BB : ReversePostOrder) {
    DEBUG(llvm::dbgs() << "Visiting BB#" << BBToRPOTNumMap[BB] << "\n");

    // We do this here to avoid issues with inserting, finding into BBToStateMap
    // while using memory that it owns (State below).
    DEBUG(llvm::dbgs() << "    Predecessors (empty if no predecessors):\n");
    DEBUG(for (SILBasicBlock *Pred : BB->getPreds()) {
        llvm::dbgs() << "        BB#" << BBToRPOTNumMap[Pred]
                     << "; Ptr: " << Pred << "\n";
    });

    // Now that we have made sure that our predecessors all have initialized
    // states, grab the state for this BB.
    auto Iter = BBToStateMap.find(BB);
    if (Iter == BBToStateMap.end()) {
      assert(0 && "Found a BB in the post order without a state!\n");
      return Changed;
    }
    BBEnumTagDataflowState &State = Iter->second;

    DEBUG(llvm::dbgs() << "    State Addr: " << &State << "\n");

    // Then merge in our predecessor states. We relook up our the states for our
    // predecessors to avoid memory invalidation issues due to copying in the
    // dense map.
    DEBUG(llvm::dbgs() << "    Merging predecessors!\n");
    State.mergePredecessorStates(BBToStateMap);

    // If our predecessors cover any of our enum values, attempt to move
    // releases up the CFG onto the enum payloads (if they exist) or eliminate
    // them entirely.
    DEBUG(llvm::dbgs() << "    Attempting to move releases into "
          "predecessors!\n");
    Changed |= State.moveReleasesUpCFGIfCasesCovered();

    // Then perform the dataflow.
    DEBUG(llvm::dbgs() << "    Performing the dataflow!\n");
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
    auto *AA = getAnalysis<AliasAnalysis>();
    auto *POTA = getAnalysis<PostOrderAnalysis>();

    if (processFunction(*getFunction(), AA, POTA))
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }

  StringRef getName() override { return "Enum Simplification"; }
};
} // end anonymous namespace

SILTransform *swift::createEnumSimplification() {
  return new EnumSimplification();
}
