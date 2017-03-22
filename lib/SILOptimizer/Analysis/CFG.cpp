//===--- CFG.cpp ----------------------------------------------------------===//
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

#include "swift/SILOptimizer/Analysis/CFG.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILValue.h"
#include "swift/Demangling/ManglingMacros.h"
#include "llvm/ADT/TinyPtrVector.h"

using namespace swift;

static bool isSafeNonExitTerminator(TermInst *TI) {
  switch (TI->getTermKind()) {
  case TermKind::BranchInst:
  case TermKind::CondBranchInst:
  case TermKind::SwitchValueInst:
  case TermKind::SwitchEnumInst:
  case TermKind::SwitchEnumAddrInst:
  case TermKind::DynamicMethodBranchInst:
  case TermKind::CheckedCastBranchInst:
  case TermKind::CheckedCastValueBranchInst:
  case TermKind::CheckedCastAddrBranchInst:
    return true;
  case TermKind::UnreachableInst:
  case TermKind::ReturnInst:
  case TermKind::ThrowInst:
  case TermKind::TryApplyInst:
    return false;
  }

  llvm_unreachable("Unhandled TermKind in switch.");
}

static bool isTrapNoReturnFunction(ApplyInst *AI) {
  const char *fatalName =
    MANGLE_AS_STRING(MANGLE_SYM(s18_fatalErrorMessageys12StaticStringV_AcCSutF));
  auto *Fn = AI->getReferencedFunction();

  // We use endswith here since if we specialize fatal error we will always
  // prepend the specialization records to fatalName.
  if (!Fn || !Fn->getName().endswith(fatalName))
    return false;

  return true;
}

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
      if (AI->isCalleeNoReturn() &&
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
