//===--- UnsafeGuaranteedPeephole.cpp - UnsafeGuaranteed Peephole ---------===//
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
//
// Optimize retain/release pairs based on Builtin.unsafeGuaranteed
//
//   strong_retain %0 : $Foo
//   %4 = builtin "unsafeGuaranteed"<Foo>(%0 : $Foo) : $(Foo, Builtin.Int8)
//   %5 = tuple_extract %4 : $(Foo, Builtin.Int8), 0
//   %6 = tuple_extract %4 : $(Foo, Builtin.Int8), 1
//   %9 = function_ref @beep : $@convention(method) (@guaranteed Foo) -> ()
//   %10 = apply %9(%0) : $@convention(method) (@guaranteed Foo) -> ()
//   strong_release %5 : $Foo
//   %12 = builtin "unsafeGuaranteedEnd"(%6 : $Builtin.Int8) : $()
//
// Based on the assertion that there is another reference to "%0" that keeps
// "%0" alive for the scope between the two builtin calls we can remove the
// retain/release pair and the builtins.
//
//   %9 = function_ref @beep : $@convention(method) (@guaranteed Foo) -> ()
//   %10 = apply %9(%0) : $@convention(method) (@guaranteed Foo) -> ()
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "unsafe-guaranteed-peephole"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILOptimizer/Analysis/ARCAnalysis.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/Analysis/RCIdentityAnalysis.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/Local.h"

using namespace swift;

/// Remove retain/release pairs around builtin "unsafeGuaranteed" instruction
/// sequences.
static bool removeGuaranteedRetainReleasePairs(SILFunction &F,
                                               RCIdentityFunctionInfo &RCIA,
                                               PostDominanceAnalysis *PDA) {
  DEBUG(llvm::dbgs() << "Running on function " << F.getName() << "\n");
  bool Changed = false;

  // Lazily compute post-dominance info only when we really need it.
  PostDominanceInfo *PDI = nullptr;

  for (auto &BB : F) {
    auto It = BB.begin(), End = BB.end();
    llvm::DenseMap<SILValue, SILInstruction *> LastRetain;
    while (It != End) {
      auto *CurInst = &*It;
      ++It;

      // Memorize the last retain.
      if (isa<StrongRetainInst>(CurInst) || isa<RetainValueInst>(CurInst)) {
        LastRetain[RCIA.getRCIdentityRoot(CurInst->getOperand(0))] = CurInst;
        continue;
      }

      // Look for a builtin "unsafeGuaranteed" instruction.
      auto *UnsafeGuaranteedI = dyn_cast<BuiltinInst>(CurInst);
      if (!UnsafeGuaranteedI || !UnsafeGuaranteedI->getBuiltinKind() ||
          *UnsafeGuaranteedI->getBuiltinKind() !=
              BuiltinValueKind::UnsafeGuaranteed)
        continue;

      auto Opd = UnsafeGuaranteedI->getOperand(0);
      auto RCIdOpd = RCIA.getRCIdentityRoot(UnsafeGuaranteedI->getOperand(0));
      if (!LastRetain.count(RCIdOpd)) {
        DEBUG(llvm::dbgs() << "LastRetain failed\n");
        continue;
      }

      // This code is very conservative. Check that there is a matching retain
      // before the unsafeGuaranteed builtin with only retains inbetween.
      auto *LastRetainInst = LastRetain[RCIdOpd];
      auto NextInstIter = std::next(SILBasicBlock::iterator(LastRetainInst));
      while (NextInstIter != BB.end() && &*NextInstIter != CurInst &&
             (isa<RetainValueInst>(*NextInstIter) ||
              isa<StrongRetainInst>(*NextInstIter) ||
              !NextInstIter->mayHaveSideEffects() ||
              isa<DebugValueInst>(*NextInstIter) ||
              isa<DebugValueAddrInst>(*NextInstIter)))
       ++NextInstIter;
      if (&*NextInstIter != CurInst) {
        DEBUG(llvm::dbgs() << "Last retain right before match failed\n");
        continue;
      }

      DEBUG(llvm::dbgs() << "Saw " << *UnsafeGuaranteedI);
      DEBUG(llvm::dbgs() << "  with operand " << *Opd);

      // Match the reference and token result.
      //  %4 = builtin "unsafeGuaranteed"<Foo>(%0 : $Foo)
      //  %5 = tuple_extract %4 : $(Foo, Builtin.Int8), 0
      //  %6 = tuple_extract %4 : $(Foo, Builtin.Int8), 1
      SILInstruction *UnsafeGuaranteedValue;
      SILInstruction *UnsafeGuaranteedToken;
      std::tie(UnsafeGuaranteedValue, UnsafeGuaranteedToken) =
          getSingleUnsafeGuaranteedValueResult(UnsafeGuaranteedI);

      if (!UnsafeGuaranteedValue) {
        DEBUG(llvm::dbgs() << "  no single unsafeGuaranteed value use\n");
        continue;
      }

      // Look for a builtin "unsafeGuaranteedEnd" instruction that uses the
      // token.
      //   builtin "unsafeGuaranteedEnd"(%6 : $Builtin.Int8) : $()
      auto *UnsafeGuaranteedEndI =
          getUnsafeGuaranteedEndUser(UnsafeGuaranteedToken);
      if (!UnsafeGuaranteedEndI) {
        DEBUG(llvm::dbgs() << "  no single unsafeGuaranteedEnd use found\n");
        continue;
      }

      if (!PDI)
        PDI = PDA->get(&F);

      // It needs to post-dominated the end instruction, since we need to remove
      // the release along all paths to exit.
      if (!PDI->properlyDominates(UnsafeGuaranteedEndI, UnsafeGuaranteedI))
        continue;


      // Find the release to match with the unsafeGuaranteedValue.
      auto &UnsafeGuaranteedEndBB = *UnsafeGuaranteedEndI->getParent();
      auto LastRelease = findReleaseToMatchUnsafeGuaranteedValue(
          UnsafeGuaranteedEndI, UnsafeGuaranteedI, UnsafeGuaranteedValue,
          UnsafeGuaranteedEndBB, RCIA);
      if (!LastRelease) {
        DEBUG(llvm::dbgs() << "  no release before/after unsafeGuaranteedEnd found\n");
        continue;
      }

      // Restart iteration before the earliest instruction we remove.
      bool RestartAtBeginningOfBlock = false;
      auto LastRetainIt = SILBasicBlock::iterator(LastRetainInst);
      if (LastRetainIt != BB.begin()) {
        It = std::prev(LastRetainIt);
      } else RestartAtBeginningOfBlock = true;

      // Okay we found a post dominating release. Let's remove the
      // retain/unsafeGuaranteed/release combo.
      //
      LastRetainInst->eraseFromParent();
      LastRelease->eraseFromParent();
      UnsafeGuaranteedEndI->eraseFromParent();
      deleteAllDebugUses(UnsafeGuaranteedValue);
      deleteAllDebugUses(UnsafeGuaranteedToken);
      deleteAllDebugUses(UnsafeGuaranteedI);
      UnsafeGuaranteedValue->replaceAllUsesWith(Opd);
      UnsafeGuaranteedValue->eraseFromParent();
      UnsafeGuaranteedToken->eraseFromParent();
      UnsafeGuaranteedI->replaceAllUsesWith(Opd);
      UnsafeGuaranteedI->eraseFromParent();

      if (RestartAtBeginningOfBlock)
        It = BB.begin();

      Changed = true;
    }
  }
  return Changed;
}

namespace {
class UnsafeGuaranteedPeephole : public swift::SILFunctionTransform {

  void run() override {
    auto &RCIA = *getAnalysis<RCIdentityAnalysis>()->get(getFunction());
    auto *PostDominance = getAnalysis<PostDominanceAnalysis>();
    if (removeGuaranteedRetainReleasePairs(*getFunction(), RCIA, PostDominance))
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }

  StringRef getName() override { return "UnsafeGuaranteed Peephole"; }
};
} // end anonymous namespace

SILTransform *swift::createUnsafeGuaranteedPeephole() {
  return new UnsafeGuaranteedPeephole();
}
