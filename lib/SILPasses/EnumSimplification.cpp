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
#include "swift/SILPasses/Transforms.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/AST/Decl.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"

STATISTIC(NumRefCountOpsSimplified, "number of enum ref count ops simplified.");

using namespace swift;

static void createRefCountOpForPayload(SILBuilder &Builder,
                                       SILInstruction *I,
                                       EnumElementDecl *EnumDecl) {
  ++NumRefCountOpsSimplified;

  SILModule &Mod = I->getModule();
  SILType ArgType = I->getOperand(0).getType().getEnumElementType(EnumDecl,
                                                                  Mod);
  auto *UEDI = Builder.createUncheckedEnumData(I->getLoc(), I->getOperand(0),
                                               EnumDecl, ArgType);
  if (isa<RetainValueInst>(I)) {
    Builder.createRetainValue(I->getLoc(), UEDI);
    return;
  }

  Builder.createReleaseValue(I->getLoc(), UEDI);
}

static bool
processBasicBlock(SILBasicBlock *BB,
                  llvm::DenseMap<SILValue, EnumElementDecl *> &ValueToCaseMap) {
  bool Changed = false;
  SILBuilder Builder(BB);

  auto SI = BB->begin(), SE = BB->end();
  while (SI != SE) {
    SILInstruction *I = &*SI;
    ++SI;

    if (auto *E = dyn_cast<EnumInst>(I)) {
      DEBUG(llvm::dbgs() << "Storing enum into map: " << *E);
      ValueToCaseMap[SILValue(E)] = E->getElement();
      continue;
    }

    if (auto *UEDI = dyn_cast<UncheckedEnumDataInst>(I)) {
      DEBUG(llvm::dbgs() << "Storing unchecked enum data into map: " << *UEDI);
      ValueToCaseMap[SILValue(UEDI->getOperand())] = UEDI->getElement();
      continue;
    }

    if (auto *RetainValue = dyn_cast<RetainValueInst>(I)) {
      auto FindResult = ValueToCaseMap.find(I->getOperand(0));
      if (FindResult == ValueToCaseMap.end())
        continue;

      // If we do not have any argument, kill the retain_value.
      if (!FindResult->second->hasArgumentType()) {
        RetainValue->eraseFromParent();
        Changed = true;
        continue;
      }

      DEBUG(llvm::dbgs() << "Found RetainValue: " << *RetainValue);
      DEBUG(llvm::dbgs() << "    Paired to Enum Oracle: " << FindResult->first);

      Builder.setInsertionPoint(RetainValue);
      createRefCountOpForPayload(Builder, RetainValue, FindResult->second);
      RetainValue->eraseFromParent();
      Changed = true;
      continue;
    }

    if (auto *ReleaseValue = dyn_cast<ReleaseValueInst>(I)) {
      auto FindResult = ValueToCaseMap.find(I->getOperand(0));
      if (FindResult == ValueToCaseMap.end())
        continue;

      // If we do not have any argument, just delete the release value.
      if (!FindResult->second->hasArgumentType()) {
        ReleaseValue->eraseFromParent();
        Changed = true;
        continue;
      }

      DEBUG(llvm::dbgs() << "Found ReleaseValue: " << *ReleaseValue);
      DEBUG(llvm::dbgs() << "    Paired to Enum Oracle: " << FindResult->first);

      Builder.setInsertionPoint(ReleaseValue);
      createRefCountOpForPayload(Builder, ReleaseValue, FindResult->second);
      Changed = true;
      ReleaseValue->eraseFromParent();
    }
  }

  return Changed;
}

static bool processFunction(SILFunction &F) {
  bool Changed = false;
  llvm::DenseMap<SILValue, EnumElementDecl *> ValueToCaseMap;

  for (auto &BB : F) {
    ValueToCaseMap.clear();
    Changed |= processBasicBlock(&BB, ValueToCaseMap);
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
