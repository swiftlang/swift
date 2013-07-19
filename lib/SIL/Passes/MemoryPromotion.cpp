//===--- MemoryPromotion.cpp - Promote heap memory to registers and stack -===//
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

#define DEBUG_TYPE "memory-promotion"
#include "swift/Subsystems.h"
#include "swift/SIL/SILModule.h"
#include "llvm/ADT/Statistic.h"
//#include "swift/SIL/Dominance.h"
using namespace swift;

STATISTIC(NumStackPromoted, "Number of heap allocations promoted to the stack");
STATISTIC(NumRegPromoted, "Number of heap allocations promoted to registers");

enum class AllocationUseKind {
  Register,   // Value can be promoted to live in an SSA register.
  Stack,      // Value can be promoted to living on the stack.
  Heap        // Value must remain on the heap.
};

static bool optimizeAllocBox(AllocBoxInst *ABI) {
  return false;
}

static bool optimizeAllocStack(AllocStackInst *ASI) {
  return false;
}


void swift::performSILMemoryPromotion(SILModule *M) {
  for (auto &Fn : *M)
    for (auto &BB : Fn) {
      auto I = BB.begin(), E = BB.end();
      while (I != E) {
        SILInstruction *Inst = I;

        if (auto *ABI = dyn_cast<AllocBoxInst>(Inst)) {
          if (optimizeAllocBox(ABI)) {
            ++NumStackPromoted;
            // Carefully move iterator to avoid invalidation problems.
            ++I;
            Inst->eraseFromParent();
            continue;
          }
        } else if (auto *ASI = dyn_cast<AllocStackInst>(Inst)) {
          if (optimizeAllocStack(ASI)) {
            ++NumRegPromoted;

            // Carefully move iterator to avoid invalidation problems.
            ++I;
            Inst->eraseFromParent();
            continue;
          }
        }

        // Increment the iterator.
        ++I;
      }
    }
}


