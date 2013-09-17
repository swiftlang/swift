//===--- Link.cpp - Link in transparent SILFunctions from module ----------===//
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

#define DEBUG_TYPE "link"
#include "swift/AST/Module.h"
#include "swift/Serialization/SerializedSILLoader.h"
#include "swift/SIL/SILModule.h"
#include "swift/Subsystems.h"
#include "llvm/ADT/Statistic.h"

using namespace swift;

STATISTIC(NumFuncLinked, "Number of SIL functions linked");

//===----------------------------------------------------------------------===//
//                          Top Level Driver
//===----------------------------------------------------------------------===//

void swift::performSILLinking(SILModule *M) {
  
  SerializedSILLoader *SILLoader = SerializedSILLoader::create(
                                     M->getASTContext(), M);
  SmallVector<SILFunction*, 128> Worklist;
  for (auto &Fn : *M)
    Worklist.push_back(&Fn);

  while (!Worklist.empty()) {
    auto Fn = Worklist.pop_back_val();
    for (auto &BB : *Fn) {
      for (auto I = BB.begin(), E = BB.end(); I != E; I++) {
        // Handles ApplyInst only.
        auto *AI = dyn_cast<ApplyInst>(I);
        if (!AI || !AI->isTransparent())
          continue;

        SILValue Callee = AI->getCallee();
        // Handles FunctionRefInst only.
        FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(Callee.getDef());
        if (!FRI)
          continue;

        SILFunction *CalleeFunction = FRI->getFunction();
        if (CalleeFunction->empty()) {
          // Try to find the definition in a serialized module when callee is
          // currently empty and the ApplyInst is transparent.
          auto NewFn = SILLoader->lookupSILFunction(CalleeFunction);
          if (NewFn) {
            Worklist.push_back(NewFn);
            ++NumFuncLinked;
          }
        }
      }
    }
  }
}
