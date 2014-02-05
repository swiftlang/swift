//===----- Analysis.cpp - Swift Analysis ----------------------------------===//
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

#define DEBUG_TYPE "sil-analysis"
#include "swift/SILAnalysis/Analysis.h"
#include "swift/AST/Module.h"
#include "swift/AST/SILOptions.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILFunction.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
#include "swift/SILPasses/Utils/Local.h"

using namespace swift;

void
CallGraphAnalysis::bottomUpCallGraphOrder(std::vector<SILFunction*> &order) {
  // TODO: cache this calculation.
  swift::bottomUpCallGraphOrder(M, order);
}

void
CallGraphAnalysis::topDownCallGraphOrder(std::vector<SILFunction*> &order) {
  // TODO: cache this calculation.
  swift::topDownCallGraphOrder(M, order);
}


SILAnalysis *swift::createCallGraphAnalysis(SILModule *M) {
  return new CallGraphAnalysis(M);
}

