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

SILAnalysis *swift::createCallGraphAnalysis(SILModule *M, SILPassManager *) {
  return new CallGraphAnalysis(M);
}

SILAnalysis *swift::createDominanceAnalysis(SILModule *, SILPassManager *) {
  return new DominanceAnalysis();
}

SILAnalysis *swift::createPostDominanceAnalysis(SILModule *, SILPassManager *) {
  return new PostDominanceAnalysis();
}

SILAnalysis *swift::createInductionVariableAnalysis(SILModule *M,
                                                    SILPassManager *) {
  return new IVAnalysis(M);
}

SILAnalysis *swift::createPostOrderAnalysis(SILModule *M, SILPassManager *) {
  return new PostOrderAnalysis();
}

SILAnalysis *swift::createClassHierarchyAnalysis(SILModule *M,
                                                 SILPassManager *) {
  return new ClassHierarchyAnalysis(M);
}
