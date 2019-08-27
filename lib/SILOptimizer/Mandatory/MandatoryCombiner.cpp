//===------- MandatoryCombiner.cpp ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
///  \file
///
///  Defines the MandatoryCombiner function transform.  The pass contains basic
///  instruction combines to be performed at the begining of both the Onone and
///  also the performance pass pipelines, after the diagnostics passes have been
///  run.  It is intended to be run before and to be independent of other
///  transforms.
///
///  The intention of this pass is to be a place for mandatory peepholes that
///  are not needed for diagnostics. Please put any such peepholes here instead
///  of in the diagnostic passes.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-mandatory-combiner"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <vector>

using namespace swift;

/// \returns whether all the values are of trivial type in the provided
///          function.
template <typename Values>
static bool areAllValuesTrivial(Values values, SILFunction &function) {
  return llvm::all_of(values, [&](SILValue value) -> bool {
    return value->getType().isTrivial(function);
  });
}

/// Replaces all full applies of the specified partial apply with full applies
/// of the underlying function ref and attempts to delete the partial apply.
///
/// The transformation of the full applies only takes place if all the arguments
/// to the full applies are trivial.
///
/// TODO: Apply this transformation to partial applies not all of whose
/// arguments are trivial.
///
/// \param pai the partial apply to attempt to delete
///
/// \returns a pair of bools.  The first bool indicates whether the a full apply
///          of the partial apply was deleted, meaning that analyses must be
///          invalidated.  The second bool indicates that the provided partial
///          apply instruction itself was deleted, with consequences for the
///          iterator.
static std::pair<bool, bool> processPartialApply(PartialApplyInst *pai) {
  auto callee = pai->getCallee();
  LLVM_DEBUG(llvm::dbgs() << "Callee: " << *callee);

  // Apply this pass only to partial applies all of whose arguments are
  // trivial.
  auto *function = pai->getReferencedFunctionOrNull();
  if (!function) {
    return {false, false};
  }

  auto paiArgs = ApplySite(pai).getArguments();
  if (!areAllValuesTrivial(paiArgs, *function)) {
    return {false, false};
  }

  bool erasedFullApply = false;
  for (auto *use : pai->getUses()) {
    auto fas = FullApplySite::isa(use->getUser());
    if (!fas) {
      continue;
    }
    LLVM_DEBUG(llvm::dbgs() << "Partial Apply: " << *pai);
    LLVM_DEBUG(llvm::dbgs() << "Full Apply Site: " << *fas.getInstruction());
    auto *fasi = dyn_cast<ApplyInst>(fas.getInstruction());
    if (!fasi) {
      continue;
    }

    auto fasArgs = fas.getArguments();
    if (!areAllValuesTrivial(fasArgs, *function)) {
      continue;
    }

    SmallVector<SILValue, 8> argsVec;
    llvm::copy(paiArgs, std::back_inserter(argsVec));
    llvm::copy(fasArgs, std::back_inserter(argsVec));

    SILBuilderWithScope builder(fasi);
    ApplyInst *appInst = builder.createApply(
        /*Loc=*/fasi->getDebugLocation().getLocation(), /*Fn=*/callee,
        /*Subs=*/pai->getSubstitutionMap(),
        /*Args*/ argsVec,
        /*isNonThrowing=*/fasi->isNonThrowing(),
        /*SpecializationInfo=*/pai->getSpecializationInfo());
    fasi->replaceAllUsesWith(appInst);
    fasi->eraseFromParent();
    erasedFullApply = true;
  }
  bool deletedDeadClosure = tryDeleteDeadClosure(pai);
  return {erasedFullApply, deletedDeadClosure};
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

namespace {

struct MandatoryCombiner : SILFunctionTransform {
  void run() override {
    bool madeChange = false;
    auto *f = getFunction();
    for (auto &block : *f) {
      for (auto ii = block.begin(), ie = block.end(); ii != ie;) {
        auto *inst = &*ii;
        // If any action is taken, the current instruction will be deleted.
        // That instruction would be the definition of a partial apply.  Because
        // the definition dominates all its uses, the previous instruction will
        // be unaffected by the removal of the instruction and its uses.  So
        // move the iterator back a step.  If no action is taken, it will be
        // advanced twice.  If an action is taken, it will be advanced once to a
        // new instruction (since the current one will have been deleted).
        ii = prev_or_default(ii, block.begin(), block.end());

        bool deleted = false;
        if (auto *pai = dyn_cast<PartialApplyInst>(inst)) {
          auto result = processPartialApply(pai);
          deleted = result.second;
          madeChange |= result.first || result.second;
        }

        if (deleted) {
          // The deletion succeeded.  The iterator has already been moved back
          // a step so as to avoid invalidation.  Advancing it one step will
          // not advance it to the current instruction since that has been
          // deleted.  Instead, it will advance it to the next *remaining*
          // instruction after the previous instruction as desired.
          ii = next_or_default(ii, /*end*/block.end(), /*defaultIter*/block.begin());
          continue;
        }

        // No action was taken.  The iterator has already been moved back a
        // step.  Consequently it is insufficient to advance it once--it must
        // be advanced twice.
        ii = next_or_default(ii, /*end*/block.end(), /*defaultIter*/block.begin());
        ++ii;
      }
    }

    if (madeChange) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createMandatoryCombiner() {
  return new MandatoryCombiner();
}
