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
#include "swift/SILAnalysis/IVAnalysis.h"
#include "swift/SILAnalysis/PostOrderAnalysis.h"
#include "swift/SILAnalysis/CallGraphAnalysis.h"
#include "swift/SILAnalysis/ClassHierarchyAnalysis.h"
#include "swift/AST/Module.h"
#include "swift/AST/SILOptions.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILFunction.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
#include "swift/SILPasses/Utils/Local.h"

using namespace swift;

// anchor for virtual D'tor
CompleteFunctions::~CompleteFunctions() {}

void CompleteFunctions::setComplete() {
  CompleteFuncs.clear();
  if (!IsModulePending)
    for (auto &F : *M)
      if (!PendingFuncs.count(&F))
        CompleteFuncs.insert(&F);

  PendingFuncs.clear();
  IsModulePending = false;
}

SILAnalysis *swift::createCallGraphAnalysis(SILModule *M) {
  return new CallGraphAnalysis(M);
}

SILAnalysis *swift::createDominanceAnalysis(SILModule *) {
  return new DominanceAnalysis();
}

SILAnalysis *swift::createPostDominanceAnalysis(SILModule *) {
  return new PostDominanceAnalysis();
}

SILAnalysis *swift::createInductionVariableAnalysis(SILModule *M) {
  return new IVAnalysis(M);
}

SILAnalysis *swift::createPostOrderAnalysis(SILModule *M) {
  return new PostOrderAnalysis(M);
}


SILAnalysis *swift::createClassHierarchyAnalysis(SILModule *M) {
  return new ClassHierarchyAnalysis(M);
}
