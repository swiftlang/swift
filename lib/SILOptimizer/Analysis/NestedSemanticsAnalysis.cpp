//===--- NestedSemanticsAnalysis.cpp - Determine callsites to a function
//----------===//
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

#include "swift/SILOptimizer/Analysis/NestedSemanticsAnalysis.h"
#include "swift/SILOptimizer/Utils/PerformanceInlinerUtils.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILVisitor.h"

using namespace swift;

bool NestedSemanticsAnalysis::isNestedSemanticFunction(SILFunction *f) {
  if (!f)
    return false;
  auto it = callsSemanticFunctions.find(f);
  if (it != callsSemanticFunctions.end()) {
    return it->second;
  }
  if (!isOptimizableSemanticFunction(f)) {
    return false;
  }
  SmallPtrSet<SILFunction*, 4> visited;
  return hasCallsToSemanticFunctions(f, visited);
}

bool NestedSemanticsAnalysis::hasCallsToSemanticFunctions(
    SILFunction *f, SmallPtrSetImpl<SILFunction *> &visited) {
  if (!visited.insert(f).second)
    return false;

  auto it = callsSemanticFunctions.find(f);
  if (it != callsSemanticFunctions.end())
    return it->second;

  for (auto &bb : *f) {
    for (auto &i : bb) {
      auto applySite = ApplySite::isa(&i);
      if (!applySite)
        continue;
      auto callee = applySite.getReferencedFunctionOrNull();
      if (!callee)
        continue;
      if (isOptimizableSemanticFunction(callee)) {
        callsSemanticFunctions.insert({f, true});
        return true;
      }
      if (!hasCallsToSemanticFunctions(callee, visited))
        continue;

      callsSemanticFunctions.insert({f, true});
      return true;
    }
  }
  callsSemanticFunctions.insert({f, false});
  return false;
}

//===----------------------------------------------------------------------===//
//                              Main Entry Point
//===----------------------------------------------------------------------===//

SILAnalysis *swift::createNestedSemanticsAnalysis(SILModule *m) {
  return new NestedSemanticsAnalysis();
}
