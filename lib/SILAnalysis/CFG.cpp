//===--- CFG.cpp ----------------------------------------------------------===//
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

#include "swift/SILAnalysis/CFG.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILValue.h"
#include "llvm/ADT/TinyPtrVector.h"

using namespace swift;

static bool isSafeNonExitTerminator(TermInst *TI) {
  switch (TI->getKind()) {
  case ValueKind::BranchInst:
  case ValueKind::CondBranchInst:
  case ValueKind::SwitchValueInst:
  case ValueKind::SwitchEnumInst:
  case ValueKind::SwitchEnumAddrInst:
  case ValueKind::DynamicMethodBranchInst:
  case ValueKind::CheckedCastBranchInst:
  case ValueKind::CheckedCastAddrBranchInst:
    return true;
  default:
    return false;
  }
}

static bool isTrapNoReturnFunction(ApplyInst *AI) {
  const char *fatalName =
      "_TFSs18_fatalErrorMessageFTVSs12StaticStringS_S_Su_T_";
  auto *FRI = dyn_cast<FunctionRefInst>(AI->getCallee());

  // We use endswith here since if we specialize fatal error we will always
  // prepend the specialization records to fatalName.
  if (!FRI || !FRI->getReferencedFunction()->getName().endswith(fatalName))
    return false;

  return true;
}

/// TODO: Add support for autorelease_return. This is not implemented now to
/// cause this to always fail in functions with objc calling convention.
bool
swift::
findAllNonFailureExitBBs(SILFunction *F,
                         llvm::TinyPtrVector<SILBasicBlock *> &BBs) {
  for (SILBasicBlock &BB : *F) {
    TermInst *TI = BB.getTerminator();

    // If we know that this terminator is not an exit terminator, continue.
    if (isSafeNonExitTerminator(TI))
      continue;

    // A return inst is always a non-failure exit bb.
    if (isa<ReturnInst>(TI)) {
      BBs.push_back(&BB);
      continue;
    }

    // If we don't have an unreachable inst at this point, this is a terminator
    // we don't understand. Be conservative and return false.
    if (!isa<UnreachableInst>(TI))
      return false;

    // Ok, at this point we know we have a terminator. If it is the only
    // instruction in our BB, it is a failure BB. continue...
    if (TI == &*BB.begin())
      continue;

    // If the unreachable is preceded by a no-return apply inst, then it is a
    // non-failure exit BB. Add it to our list and continue.
    auto PrevIter = std::prev(SILBasicBlock::iterator(TI));
    if (auto *AI = dyn_cast<ApplyInst>(&*PrevIter)) {
      if (AI->getSubstCalleeType()->isNoReturn() &&
          !isTrapNoReturnFunction(AI)) {
        BBs.push_back(&BB);
        continue;
      }
    }

    // Otherwise, it must be a failure BB where we leak, continue.
    continue;
  }

  // We understood all terminators, return true.
  return true;
}
