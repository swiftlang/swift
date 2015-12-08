//===--- ProgramTerminationAnalysis.cpp -----------------------------------===//
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

#define DEBUG_TYPE "program-termination-analysis"
#include "swift/SILAnalysis/ProgramTerminationAnalysis.h"
#include "swift/SIL/SILFunction.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                                  Utility
//===----------------------------------------------------------------------===//

static bool ignoreableApplyInstInUnreachableBlock(const ApplyInst *AI) {
  const char *fatalName = "_TFs18_fatalErrorMessageFTVs12StaticStringS_S_Su_T_";
  const auto *Fn = AI->getCalleeFunction();

  // We use endswith here since if we specialize fatal error we will always
  // prepend the specialization records to fatalName.
  if (!Fn || !Fn->getName().endswith(fatalName))
    return false;

  return true;
}

static bool ignoreableBuiltinInstInUnreachableBlock(const BuiltinInst *BI) {
  const BuiltinInfo &BInfo = BI->getBuiltinInfo();
  if (BInfo.ID == BuiltinValueKind::CondUnreachable)
    return true;

  const IntrinsicInfo &IInfo = BI->getIntrinsicInfo();
  if (IInfo.ID == llvm::Intrinsic::trap)
    return true;

  return false;
}

/// Match a call to a trap BB with no ARC relevant side effects.
static bool isARCInertTrapBB(const SILBasicBlock *BB) {
  // Do a quick check at the beginning to make sure that our terminator is
  // actually an unreachable. This ensures that in many cases this function will
  // exit early and quickly.
  auto II = BB->rbegin();
  if (!isa<UnreachableInst>(*II))
    return false;

  auto IE = BB->rend();
  while (II != IE) {
    // Ignore any instructions without side effects.
    if (!II->mayHaveSideEffects()) {
      ++II;
      continue;
    }

    // Ignore cond fail.
    if (isa<CondFailInst>(*II)) {
      ++II;
      continue;
    }

    // Check for apply insts that we can ignore.
    if (auto *AI = dyn_cast<ApplyInst>(&*II)) {
      if (ignoreableApplyInstInUnreachableBlock(AI)) {
        ++II;
        continue;
      }
    }

    // Check for builtins that we can ignore.
    if (auto *BI = dyn_cast<BuiltinInst>(&*II)) {
      if (ignoreableBuiltinInstInUnreachableBlock(BI)) {
        ++II;
        continue;
      }
    }

    // If we can't ignore the instruction, return false.
    return false;
  }

  // Otherwise, we have an unreachable and every instruction is inert from an
  // ARC perspective in an unreachable BB.
  return true;
}

//===----------------------------------------------------------------------===//
//                       ProgramTerminationFunctionInfo
//===----------------------------------------------------------------------===//

ProgramTerminationFunctionInfo::ProgramTerminationFunctionInfo(
    const SILFunction *F) {
  for (const auto &BB : *F) {
    if (!isARCInertTrapBB(&BB))
      continue;
    ProgramTerminatingBlocks.insert(&BB);
  }
}

//===----------------------------------------------------------------------===//
//                            Top level Entrypoint
//===----------------------------------------------------------------------===//

SILAnalysis *swift::createProgramTerminationAnalysis(SILModule *M) {
  return new ProgramTerminationAnalysis(M);
}
