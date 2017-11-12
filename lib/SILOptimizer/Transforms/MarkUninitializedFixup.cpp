//===--- MarkUninitializedFixup.cpp ---------------------------------------===//
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

#define DEBUG_TYPE "sil-ownership-model-eliminator"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                           Top Level Entry Point
//===----------------------------------------------------------------------===//

static ProjectBoxInst *
getInitialProjectBox(MarkUninitializedInst *MUI,
                     ArrayRef<ProjectBoxInst *> Projections) {
  assert(!Projections.empty());
  if (Projections.size() == 1) {
    auto *PBI = Projections[0];
    assert(PBI->getParent() == MUI->getParent());
    return PBI;
  }

  // Otherwise, we want to select the earliest project box. There should
  // only be one.
  ProjectBoxInst *PBI = Projections[0];

  // Otherwise, we need to find the one that is closest to the
  // mark_uninitialized. It should be in the same block.
  for (auto *I : makeArrayRef(Projections).slice(1)) {
    // If the new instruction is in a different block than the
    // mark_uninitialized, it can not be a good solution, so skip it.
    if (I->getParent() != MUI->getParent()) {
      continue;
    }

    // If PBI is not in the same block as the MUI, but I is, we picked a
    // bad initial PBI, set PBI to I.
    if (PBI->getParent() != MUI->getParent()) {
      // Otherwise, I is a better candidate than PBI so set PBI to I.
      PBI = I;
      continue;
    }

    // Otherwise, we have that PBI and I are both in the same block. See
    // which one is first.
    auto *BB = PBI->getParent();
    if (BB->end() != std::find_if(PBI->getIterator(), BB->end(),
                                  [&I](const SILInstruction &InnerI) -> bool {
                                    return I == &InnerI;
                                  })) {
      continue;
    }

    PBI = I;
  }

  assert(PBI->getParent() == MUI->getParent());
  return PBI;
}

namespace {

struct MarkUninitializedFixup : SILModuleTransform {
  void run() override {
    bool MadeChange = false;

    for (auto &F : *getModule()) {
      for (auto &BB : F) {
        for (auto II = BB.begin(), IE = BB.end(); II != IE;) {
          // Grab our given instruction and advance the iterator. This is
          // important since we may be destroying the given instruction.
          auto *MUI = dyn_cast<MarkUninitializedInst>(&*II);
          ++II;

          // If we do not have a mark_uninitialized or we have a
          // mark_uninitialized of an alloc_box, continue. These are not
          // interesting to us.
          if (!MUI)
            continue;

          auto *Box = dyn_cast<AllocBoxInst>(MUI->getOperand());
          if (!Box)
            continue;

          // We expect there to be in most cases exactly one project_box. That
          // being said, it is not impossible for there to be multiple. In such
          // a case, we assume that the correct project_box is the one that is
          // nearest to the mark_uninitialized in the same block. This preserves
          // the existing behavior.
          llvm::TinyPtrVector<ProjectBoxInst *> Projections;
          for (auto *Op : MUI->getUses()) {
            if (auto *PBI = dyn_cast<ProjectBoxInst>(Op->getUser())) {
              Projections.push_back(PBI);
            }
          }
          assert(!Projections.empty() && "SILGen should never emit a "
                                         "mark_uninitialized by itself");

          // First replace all uses of the mark_uninitialized with the box.
          MUI->replaceAllUsesWith(Box);

          // That means now our project box now has the alloc_box as its
          // operand. Grab that project_box.
          auto *PBI = getInitialProjectBox(MUI, Projections);

          // Then create the new mark_uninitialized and force all uses of the
          // project_box to go through the new mark_uninitialized.
          SILBuilder B(std::next(PBI->getIterator()));
          SILValue Undef = SILUndef::get(PBI->getType(), PBI->getModule());
          auto *NewMUI = B.createMarkUninitialized(PBI->getLoc(), Undef,
                                                   MUI->getKind());
          PBI->replaceAllUsesWith(NewMUI);
          NewMUI->setOperand(PBI);

          // Finally, remove the old mark_uninitialized.
          MUI->eraseFromParent();
          MadeChange = true;
        }
      }

      if (MadeChange) {
        auto InvalidKind =
            SILAnalysis::InvalidationKind::BranchesAndInstructions;
        invalidateAnalysis(&F, InvalidKind);
      }
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createMarkUninitializedFixup() {
  return new MarkUninitializedFixup();
}
