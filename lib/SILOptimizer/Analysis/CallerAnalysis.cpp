//===--- CallerAnalysis.cpp - Determine callsites to a function  ----------===//
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

#include "swift/SILOptimizer/Analysis/CallerAnalysis.h"

#include "swift/SIL/SILModule.h"
#include "swift/SILOptimizer/Utils/Local.h"

using namespace swift;

void CallerAnalysis::processFunctionCallSites(SILFunction *F) {
  // Scan the whole module and search Apply sites.
  for (auto &BB : *F) {
    for (auto &II : BB) {
      if (auto Apply = FullApplySite::isa(&II)) {
        SILFunction *CalleeFn = Apply.getCalleeFunction();
        if (!CalleeFn)
          continue;

        // Update the callee information for this function.
        FunctionInfo &CallerInfo = FuncInfos[F];
        CallerInfo.Callees.insert(CalleeFn);
        
        // Update the callsite information for the callee.
        FunctionInfo &CalleeInfo = FuncInfos[CalleeFn];
        CalleeInfo.Callers.insert(F);
        continue;
      }
      if (auto *PAI = dyn_cast<PartialApplyInst>(&II)) {
        SILFunction *CalleeFn = PAI->getCalleeFunction();
        if (!CalleeFn)
          continue;

        // Update the callee information for this function.
        FunctionInfo &CallerInfo = FuncInfos[F];
        CallerInfo.Callees.insert(CalleeFn);
        
        // Update the partial-apply information for the callee.
        FunctionInfo &CalleeInfo = FuncInfos[CalleeFn];
        int &minAppliedArgs = CalleeInfo.PartialAppliers[F];
        int numArgs = (int)PAI->getNumArguments();
        if (minAppliedArgs == 0 || numArgs < minAppliedArgs) {
          minAppliedArgs = numArgs;
        }
        continue;
      }
    }   
  }   
}

void CallerAnalysis::invalidateExistingCalleeRelation(SILFunction *F) {
  FunctionInfo &CallerInfo = FuncInfos[F];
  for (auto Callee : CallerInfo.Callees) {
    FunctionInfo &CalleeInfo = FuncInfos[Callee];
    CalleeInfo.Callers.erase(F);
    CalleeInfo.PartialAppliers.erase(F);
  }
}

//===----------------------------------------------------------------------===//
//                              Main Entry Point
//===----------------------------------------------------------------------===//
SILAnalysis *swift::createCallerAnalysis(SILModule *M) {
  return new CallerAnalysis(M);
}
