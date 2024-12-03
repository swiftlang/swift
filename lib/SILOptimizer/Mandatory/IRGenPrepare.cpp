//===--- IRGenPrepare.cpp -------------------------------------------------===//
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
/// \file
///
/// Cleanup SIL to make it suitable for IRGen.
///
/// We perform the following canonicalizations:
///
/// 1. We remove calls to Builtin.poundAssert() and Builtin.staticReport(),
///    which are not needed post SIL.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-cleanup"
#include "swift/AST/SemanticAttrs.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/InstructionDeleter.h"
#include "swift/SILOptimizer/Analysis/ColdBlockInfo.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/Strings.h"

using namespace swift;

static bool cleanFunction(SILFunction &fn) {
  bool madeChange = false;

  for (auto &bb : fn) {
    for (auto i = bb.begin(), e = bb.end(); i != e;) {
      // Make sure there is no iterator invalidation if the inspected
      // instruction gets removed from the block.
      SILInstruction *inst = &*i;
      ++i;

      // Remove calls to Builtin.poundAssert() and Builtin.staticReport().
      auto *bi = dyn_cast<BuiltinInst>(inst);
      if (!bi) {
        continue;
      }

      switch (bi->getBuiltinInfo().ID) {
        case BuiltinValueKind::CondFailMessage: {
          SILBuilderWithScope Builder(bi);
          Builder.createCondFail(bi->getLoc(), bi->getOperand(0),
            "unknown program error");
          LLVM_FALLTHROUGH;
        }
        case BuiltinValueKind::PoundAssert:
        case BuiltinValueKind::StaticReport: {
          // The call to the builtin should get removed before we reach
          // IRGen.
          InstructionDeleter deleter;
          deleter.forceDelete(bi);
          // StaticReport only takes trivial operands, and therefore doesn't
          // require fixing the lifetime of its operands.
          deleter.cleanupDeadInstructions();
          madeChange = true;
          break;
        }
        default:
          break;
      }
    }
  }

  return madeChange;
}

/// Embed information about cold edges into the SIL via ProfileCounters
/// so that it's available in LLVM.
static bool lowerColdBlockInfo(DominanceAnalysis *DA,
                               PostDominanceAnalysis *PDA,
                               SILFunction &fn) {
  bool invalidate = false;

  ColdBlockInfo CBI(DA, PDA);
  CBI.analyze(&fn);

  // If the entry block is cold, then the whole function is cold.
  if (CBI.isCold(fn.getEntryBlock())) {
    fn.addSemanticsAttr(semantics::COLD);
    return true;
  }

  SmallVector<SILSuccessor*, 8> coldSuccs;
  SmallVector<SILSuccessor*, 8> warmSuccs;
  for (auto &block : fn) {
    if (CBI.isCold(&block) || block.getNumSuccessors() < 2)
      continue;

    coldSuccs.clear(); warmSuccs.clear();

    // Partition the successors.
    bool hasExistingProfileData = false;
    for (SILSuccessor &succ : block.getSuccessors()) {
      if (succ.getCount().hasValue()) {
        hasExistingProfileData = true;
        break;
      }

      if (CBI.isCold(succ))
        coldSuccs.push_back(&succ);
      else
        warmSuccs.push_back(&succ);
    }

    if (hasExistingProfileData)
      continue;

    // Nothing to annotate if everything's warm.
    if (coldSuccs.empty())
      continue;

    ASSERT(!warmSuccs.empty() && "all succs are cold, yet the block isn't?");
    invalidate = true;

    for (auto *coldSucc : coldSuccs)
      coldSucc->setCount(ProfileCounter(1));

    for (auto *warmSucc : warmSuccs)
      warmSucc->setCount(ProfileCounter(2000));
  }

  return invalidate;
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

namespace {

class IRGenPrepare : public SILFunctionTransform {
  void run() override {
    SILFunction *F = getFunction();
    auto *DA = PM->getAnalysis<DominanceAnalysis>();
    auto *PDA = PM->getAnalysis<PostDominanceAnalysis>();

    if (getOptions().EmbeddedSwift) {
      // In embedded swift all the code is generated in the top-level module.
      // Even de-serialized functions must be code-gen'd.
      SILLinkage linkage = F->getLinkage();
      if (isAvailableExternally(linkage)) {
        F->setLinkage(stripExternalFromLinkage(linkage));
      }
    }

    bool shouldInvalidate = cleanFunction(*F);

    shouldInvalidate |= lowerColdBlockInfo(DA, PDA, *F);

    if (shouldInvalidate)
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }
};

} // end anonymous namespace


SILTransform *swift::createIRGenPrepare() {
  return new IRGenPrepare();
}
