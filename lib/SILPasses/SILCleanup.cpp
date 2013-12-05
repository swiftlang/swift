//===-- SILCleanup.cpp - Removes diagnostics instructions -----------------===//
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
//
// Cleanup SIL to make it suitable for IRGen. Specifically, removes the calls to
// Builtin.staticReport(), which are not needed post SIL.
//
//===----------------------------------------------------------------------===//

#include "swift/SILPasses/Passes.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILPasses/Utils/Local.h"

void swift::performSILCleanup(SILModule *M) {
  for (auto &Fn : *M)
    for (auto &BB : Fn) {
      auto I = BB.begin(), E = BB.end();
      while (I != E) {
        // Make sure there is no iterator invalidation if the inspected
        // instruction gets removed from the block.
        SILInstruction *Inst = I++;

        // Remove calls to Builtin.staticReport().
        if (ApplyInst *AI = dyn_cast<ApplyInst>(Inst))
          if (BuiltinFunctionRefInst *FR =
              dyn_cast<BuiltinFunctionRefInst>(AI->getCallee().getDef())) {
            const BuiltinInfo &B = FR->getBuiltinInfo();
            if (B.ID == BuiltinValueKind::StaticReport) {
              // The call to the builtin should get removed before we reach
              // IRGen.
              recursivelyDeleteTriviallyDeadInstructions(AI, /* Force */true);
            }
          }
      }
    }
}
