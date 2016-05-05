//===--- UnsafeGuaranteedPeephole.cpp - UnsafeGuaranteed Peephole ---------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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
#include "swift/SILOptimizer/Analysis/RCIdentityAnalysis.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/Local.h"

using namespace swift;

/// Get the (GuaranteedValue, Token) tuple from a call to "unsafeGuaranteed"
/// if the tuple elements are identified by a single tuple_extract use.
/// Otherwise, return a (nullptr, nullptr) tuple.
static std::pair<SILInstruction *, SILInstruction *>
getSingleUnsafeGuaranteedValueResult(BuiltinInst *BI) {
  assert(BI->getBuiltinKind() &&
         *BI->getBuiltinKind() == BuiltinValueKind::UnsafeGuaranteed &&
         "Expecting a unsafeGuaranteed builtin");

  SILInstruction *GuaranteedValue = nullptr;
  SILInstruction *Token = nullptr;

  auto Failed = std::make_pair(nullptr, nullptr);

  for (auto *Operand : getNonDebugUses(BI)) {
    auto *Usr = Operand->getUser();
    if (isa<ReleaseValueInst>(Usr) || isa<RetainValueInst>(Usr))
      continue;

    auto *TE = dyn_cast<TupleExtractInst>(Usr);
    if (!TE || TE->getOperand() != BI)
      return Failed;

    if (TE->getFieldNo() == 0 && !GuaranteedValue) {
      GuaranteedValue = TE;
      continue;
    }
    if (TE->getFieldNo() == 1 && !Token) {
      Token = TE;
      continue;
    }
    return Failed;
  }

  if (!GuaranteedValue || !Token)
    return Failed;

  return std::make_pair(GuaranteedValue, Token);
}

static bool hasUnsafeGuaranteedOperand(SILValue UnsafeGuaranteedValue,
                                       SILValue UnsafeGuaranteedValueOperand,
                                       RCIdentityFunctionInfo &RCII,
                                       SILBasicBlock::iterator ReleaseIt) {
  assert(isa<StrongReleaseInst>(ReleaseIt) ||
         isa<ReleaseValueInst>(ReleaseIt) && "Expecting a release");

  auto RCRoot =
      RCII.getRCIdentityRoot(cast<SILInstruction>(ReleaseIt)->getOperand(0));

  return RCRoot == UnsafeGuaranteedValue ||
         RCRoot == UnsafeGuaranteedValueOperand;
}

/// Walk backwards from an unsafeGuaranteedEnd builtin instruction looking for a
/// release on the reference returned by the matching unsafeGuaranteed builtin
/// ignoring releases on the way.
///
///    %4 = builtin "unsafeGuaranteed"<Foo>(%0 : $Foo) : $(Foo, Builtin.Int8)
///    %5 = tuple_extract %4 : $(Foo, Builtin.Int8), 0
///    %6 = tuple_extract %4 : $(Foo, Builtin.Int8), 1
///    strong_release %5 : $Foo // <-- Matching release.
///    strong_release %6 : $Foo // Ignore.
///    %12 = builtin "unsafeGuaranteedEnd"(%6 : $Builtin.Int8) : $()
///
/// Alternatively, look for the release after the unsafeGuaranteedEnd.
static SILBasicBlock::iterator findReleaseToMatchUnsafeGuaranteedValue(
    SILInstruction *UnsafeGuaranteedEndI, SILInstruction *UnsafeGuaranteedI,
    SILValue UnsafeGuaranteedValue, SILBasicBlock &BB,
    RCIdentityFunctionInfo &RCIA) {

  auto UnsafeGuaranteedEndIIt = SILBasicBlock::iterator(UnsafeGuaranteedEndI);
  if (UnsafeGuaranteedEndIIt == BB.begin())
    return BB.end();
  auto LastReleaseIt = std::prev(UnsafeGuaranteedEndIIt);
  auto UnsafeGuaranteedRoot = RCIA.getRCIdentityRoot(UnsafeGuaranteedValue);
  auto UnsafeGuaranteedOpdRoot =
      RCIA.getRCIdentityRoot(UnsafeGuaranteedI->getOperand(0));

  // Look before the "unsafeGuaranteedEnd".
  while (LastReleaseIt != BB.begin() &&
         (((isa<StrongReleaseInst>(*LastReleaseIt) ||
            isa<ReleaseValueInst>(*LastReleaseIt)) &&
           !hasUnsafeGuaranteedOperand(UnsafeGuaranteedRoot,
                                       UnsafeGuaranteedOpdRoot, RCIA,
                                       LastReleaseIt)) ||
          !LastReleaseIt->mayHaveSideEffects() ||
          isa<DebugValueInst>(*LastReleaseIt) ||
          isa<DebugValueInst>(*LastReleaseIt)))
    --LastReleaseIt;
  if ((isa<StrongReleaseInst>(*LastReleaseIt) ||
       isa<ReleaseValueInst>(*LastReleaseIt)) &&
      hasUnsafeGuaranteedOperand(UnsafeGuaranteedRoot, UnsafeGuaranteedOpdRoot,
                                 RCIA, LastReleaseIt))
    return LastReleaseIt;

  // Otherwise, try finding it after the "unsafeGuaranteedEnd".
  LastReleaseIt = std::next(SILBasicBlock::iterator(UnsafeGuaranteedEndI));
  while (LastReleaseIt != BB.end() &&
         (((isa<StrongReleaseInst>(*LastReleaseIt) ||
            isa<ReleaseValueInst>(*LastReleaseIt)) &&
           !hasUnsafeGuaranteedOperand(UnsafeGuaranteedRoot,
                                       UnsafeGuaranteedOpdRoot, RCIA,
                                       LastReleaseIt)) ||
          !LastReleaseIt->mayHaveSideEffects() ||
          isa<DebugValueInst>(*LastReleaseIt) ||
          isa<DebugValueInst>(*LastReleaseIt)))
    ++LastReleaseIt;
  if (LastReleaseIt == BB.end())
    return LastReleaseIt;
  if ((!isa<StrongReleaseInst>(*LastReleaseIt) &&
       !isa<ReleaseValueInst>(*LastReleaseIt)) ||
      !hasUnsafeGuaranteedOperand(UnsafeGuaranteedRoot, UnsafeGuaranteedOpdRoot,
                                  RCIA, LastReleaseIt))
    return BB.end();

  return LastReleaseIt;
}

/// Remove retain/release pairs around builtin "unsafeGuaranteed" instruction
/// sequences.
static bool removeGuaranteedRetainReleasePairs(SILFunction &F,
                                               RCIdentityFunctionInfo &RCIA) {
  DEBUG(llvm::dbgs() << "Running on function " << F.getName() << "\n");
  bool Changed = false;
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
      BuiltinInst *UnsafeGuaranteedEndI = nullptr;
      for (auto *Operand : getNonDebugUses(UnsafeGuaranteedToken)) {
        if (UnsafeGuaranteedEndI) {
          DEBUG(llvm::dbgs() << "  multiple unsafeGuaranteedEnd users\n");
          UnsafeGuaranteedEndI = nullptr;
          break;
        }
        auto *BI = dyn_cast<BuiltinInst>(Operand->getUser());
        if (!BI || !BI->getBuiltinKind() ||
            *BI->getBuiltinKind() != BuiltinValueKind::UnsafeGuaranteedEnd) {
          DEBUG(llvm::dbgs() << "  wrong unsafeGuaranteed token user "
                             << *Operand->getUser());
          break;
        }

        UnsafeGuaranteedEndI = BI;
      }

      if (!UnsafeGuaranteedEndI) {
        DEBUG(llvm::dbgs() << "  no single unsafeGuaranteedEnd use found\n");
        continue;
      }

      if (SILBasicBlock::iterator(UnsafeGuaranteedEndI) ==
          UnsafeGuaranteedEndI->getParent()->end())
        continue;

      // Find the release to match with the unsafeGuaranteedValue.
      auto &UnsafeGuaranteedEndBB = *UnsafeGuaranteedEndI->getParent();
      auto LastReleaseIt = findReleaseToMatchUnsafeGuaranteedValue(
          UnsafeGuaranteedEndI, UnsafeGuaranteedI, UnsafeGuaranteedValue,
          UnsafeGuaranteedEndBB, RCIA);
      if (LastReleaseIt == UnsafeGuaranteedEndBB.end()) {
        DEBUG(llvm::dbgs() << "  no release before/after unsafeGuaranteedEnd found\n");
        continue;
      }
      SILInstruction *LastRelease = &*LastReleaseIt;

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
        ++It = BB.begin();

      Changed = true;
    }
  }
  return Changed;
}

namespace {
class UnsafeGuaranteedPeephole : public swift::SILFunctionTransform {

  void run() override {
    auto &RCIA = *getAnalysis<RCIdentityAnalysis>()->get(getFunction());
    if (removeGuaranteedRetainReleasePairs(*getFunction(), RCIA))
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }

  StringRef getName() override { return "UnsafeGuaranteed Peephole"; }
};
} // end anonymous namespace.

SILTransform *swift::createUnsafeGuaranteedPeephole() {
  return new UnsafeGuaranteedPeephole();
}
