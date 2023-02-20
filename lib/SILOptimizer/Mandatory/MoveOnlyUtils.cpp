//===--- MoveOnlyUtils.cpp ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-move-only-checker"

#include "swift/AST/AccessScope.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/Basic/Debug.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/FrozenMultiMap.h"
#include "swift/Basic/SmallBitVector.h"
#include "swift/SIL/ApplySite.h"
#include "swift/SIL/BasicBlockBits.h"
#include "swift/SIL/BasicBlockData.h"
#include "swift/SIL/BasicBlockDatastructures.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/Consumption.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/FieldSensitivePrunedLiveness.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/MemAccessUtils.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/PrunedLiveness.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILArgumentConvention.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILUndef.h"
#include "swift/SIL/SILValue.h"
#include "swift/SILOptimizer/Analysis/ClosureScope.h"
#include "swift/SILOptimizer/Analysis/DeadEndBlocksAnalysis.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/Analysis/NonLocalAccessBlockAnalysis.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CanonicalizeOSSALifetime.h"
#include "swift/SILOptimizer/Utils/InstructionDeleter.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallBitVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"

#include "MoveOnlyDiagnostics.h"
#include "MoveOnlyUtils.h"

using namespace swift;
using namespace swift::siloptimizer;

//===----------------------------------------------------------------------===//
//                        MARK: Missed Copy Diagnostic
//===----------------------------------------------------------------------===//

/// A small diagnostic helper that causes us to emit a diagnostic error upon any
/// copies we did not eliminate and ask the user for a test case.
void swift::siloptimizer::emitCheckerMissedCopyOfNonCopyableTypeErrors(
    SILFunction *fn, DiagnosticEmitter &diagnosticEmitter) {
  for (auto &block : *fn) {
    for (auto &inst : block) {
      if (auto *cvi = dyn_cast<CopyValueInst>(&inst)) {
        if (cvi->getOperand()->getType().isMoveOnly()) {
          LLVM_DEBUG(llvm::dbgs()
                     << "Emitting missed copy error for: " << *cvi);
          diagnosticEmitter.emitCheckedMissedCopyError(cvi);
        }
        continue;
      }

      if (auto *li = dyn_cast<LoadInst>(&inst)) {
        if (li->getOwnershipQualifier() == LoadOwnershipQualifier::Copy &&
            li->getType().isMoveOnly()) {
          LLVM_DEBUG(llvm::dbgs() << "Emitting missed copy error for: " << *li);
          diagnosticEmitter.emitCheckedMissedCopyError(li);
        }
        continue;
      }

      if (auto *copyAddr = dyn_cast<CopyAddrInst>(&inst)) {
        if (!copyAddr->isTakeOfSrc() &&
            copyAddr->getSrc()->getType().isMoveOnly()) {
          LLVM_DEBUG(llvm::dbgs()
                     << "Emitting missed copy error for: " << *copyAddr);
          diagnosticEmitter.emitCheckedMissedCopyError(copyAddr);
        }
        continue;
      }
    }
  }
}

//===----------------------------------------------------------------------===//
//                  MARK: Cleanup After Emitting Diagnostic
//===----------------------------------------------------------------------===//

bool swift::siloptimizer::cleanupNonCopyableCopiesAfterEmittingDiagnostic(
    SILFunction *fn) {
  bool changed = false;
  for (auto &block : *fn) {
    for (auto ii = block.begin(), ie = block.end(); ii != ie;) {
      auto *inst = &*ii;
      ++ii;

      // Convert load [copy] -> load_borrow + explicit_copy_value.
      if (auto *li = dyn_cast<LoadInst>(inst)) {
        if (li->getOwnershipQualifier() == LoadOwnershipQualifier::Copy) {
          SILBuilderWithScope builder(li);
          auto *lbi = builder.createLoadBorrow(li->getLoc(), li->getOperand());
          auto *cvi = builder.createExplicitCopyValue(li->getLoc(), lbi);
          builder.createEndBorrow(li->getLoc(), lbi);
          li->replaceAllUsesWith(cvi);
          li->eraseFromParent();
          changed = true;
        }
      }

      // Convert copy_addr !take of src to its explicit value form so we don't
      // error.
      if (auto *copyAddr = dyn_cast<CopyAddrInst>(inst)) {
        if (!copyAddr->isTakeOfSrc()) {
          SILBuilderWithScope builder(copyAddr);
          builder.createExplicitCopyAddr(
              copyAddr->getLoc(), copyAddr->getSrc(), copyAddr->getDest(),
              IsTake_t(copyAddr->isTakeOfSrc()),
              IsInitialization_t(copyAddr->isInitializationOfDest()));
          copyAddr->eraseFromParent();
          changed = true;
        }
      }

      // Convert any copy_value of move_only type to explicit copy value.
      if (auto *cvi = dyn_cast<CopyValueInst>(inst)) {
        if (!cvi->getOperand()->getType().isMoveOnly())
          continue;
        SILBuilderWithScope b(cvi);
        auto *expCopy =
            b.createExplicitCopyValue(cvi->getLoc(), cvi->getOperand());
        cvi->replaceAllUsesWith(expCopy);
        cvi->eraseFromParent();
        changed = true;
        continue;
      }

      if (auto *mmci = dyn_cast<MarkMustCheckInst>(inst)) {
        mmci->replaceAllUsesWith(mmci->getOperand());
        mmci->eraseFromParent();
        changed = true;
        continue;
      }
    }
  }

  return changed;
}
