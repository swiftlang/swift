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
#include "swift/SILAnalysis/DominanceAnalysis.h"
#include "swift/AST/Module.h"
#include "swift/AST/SILOptions.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILFunction.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
#include "swift/SILPasses/Utils/Local.h"

using namespace swift;

/// \brief return a bottom-up function order.
const std::vector<SILFunction*> &CallGraphAnalysis::bottomUpCallGraphOrder() {
  // If we haven't calculated the order before do it now.
  if (!BottomUpFunctionOrder.size())
    swift::bottomUpCallGraphOrder(M, BottomUpFunctionOrder);

  return BottomUpFunctionOrder;
}

SILAnalysis *swift::createCallGraphAnalysis(SILModule *M) {
  return new CallGraphAnalysis(M);
}

SILAnalysis *swift::createDominanceAnalysis(SILModule *M) {
  return new DominanceAnalysis(M);
}
