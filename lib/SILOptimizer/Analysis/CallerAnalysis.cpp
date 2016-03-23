//===--- CallerAnalysis.cpp - Determine callsites to a function  ----------===//
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

#include "swift/SILOptimizer/Analysis/CallerAnalysis.h"

#include "swift/Basic/Fallthrough.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILOptimizer/Utils/Local.h"

using namespace swift;

void CallerAnalysis::processFunctionCallSites(SILFunction *F) {
  // Scan the whole module and search Apply sites.
  CallerAnalysisFunctionInfo &CallerInfo = CallInfo.FindAndConstruct(F).second;
  for (auto &BB : *F) {
    for (auto &II : BB) {
      if (auto Apply = FullApplySite::isa(&II)) {
        SILFunction *CalleeFn = Apply.getCalleeFunction();
        if (!CalleeFn)
          continue;
        // Update the callee information for this function.
        CallerInfo.Callees.push_back(CalleeFn);
        
        // Update the callsite information for the callee.
        CallerAnalysisFunctionInfo &CalleeInfo
                           = CallInfo.FindAndConstruct(CalleeFn).second;

        // Record it if this is the first time we see this caller.
        if (CalleeInfo.CallSites.find(F) == CalleeInfo.CallSites.end())
          CalleeInfo.Callers.push_back(F);

        // Record the callsite. 
        CalleeInfo.CallSites[F].push_back(Apply);
      }   
    }   
  }   
}

void CallerAnalysis::invalidateExistingCalleeRelation(SILFunction *F) {
  CallerAnalysisFunctionInfo &CallerInfo = CallInfo.FindAndConstruct(F).second;
  for (auto Callee : CallerInfo.Callees) {
    CallerAnalysisFunctionInfo &CalleeInfo = CallInfo.find(Callee)->second;
    CalleeInfo.CallSites[F].clear();
  }
}

//===----------------------------------------------------------------------===//
//                              Main Entry Point
//===----------------------------------------------------------------------===//
SILAnalysis *swift::createCallerAnalysis(SILModule *M) {
  return new CallerAnalysis(M);
}
