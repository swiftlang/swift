//===--- PassManagerVerifierAnalysis.cpp ----------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-passmanager-verifier-analysis"
#include "swift/SILOptimizer/Analysis/PassManagerVerifierAnalysis.h"
#include "swift/SIL/SILModule.h"
#include "llvm/Support/CommandLine.h"

static llvm::cl::opt<bool>
    EnableVerifier("enable-sil-passmanager-verifier-analysis",
                   llvm::cl::desc("Enable verification of the passmanagers "
                                  "function notification infrastructure"),
                   llvm::cl::init(false));

using namespace swift;

PassManagerVerifierAnalysis::PassManagerVerifierAnalysis(SILModule *mod)
    : SILAnalysis(SILAnalysisKind::PassManagerVerifier), mod(*mod) {
#ifndef NDEBUG
  if (!EnableVerifier)
    return;
  for (auto &fn : *mod) {
    LLVM_DEBUG(llvm::dbgs() << "PMVerifierAnalysis. Add: " << fn.getName()
                            << '\n');
    liveFunctions.insert(&fn);
  }
#endif
}

/// Validate that the analysis is able to look up all functions and that those
/// functions are live.
void PassManagerVerifierAnalysis::invalidate() {}

/// Validate that the analysis is able to look up the given function.
void PassManagerVerifierAnalysis::invalidate(SILFunction *f,
                                             InvalidationKind k) {}

/// If a function has not yet been seen start tracking it.
void PassManagerVerifierAnalysis::notifyAddedOrModifiedFunction(
    SILFunction *f) {
#ifndef NDEBUG
  if (!EnableVerifier)
    return;
  LLVM_DEBUG(llvm::dbgs() << "PMVerifierAnalysis. Add|Mod: " << f->getName()
                          << '\n');
  liveFunctions.insert(f);
#endif
}

/// Stop tracking a function.
void PassManagerVerifierAnalysis::notifyWillDeleteFunction(SILFunction *f) {
#ifndef NDEBUG
  if (!EnableVerifier)
    return;
  LLVM_DEBUG(llvm::dbgs() << "PMVerifierAnalysis. Delete: " << f->getName()
                          << '\n');
  assert(liveFunctions.count(f) &&
         "Tried to delete function that analysis was not aware of?!");
  liveFunctions.erase(f);
#endif
}

/// Make sure that when we invalidate a function table, make sure we can find
/// all functions for all witness tables.
void PassManagerVerifierAnalysis::invalidateFunctionTables() {}

/// Run the entire verification.
void PassManagerVerifierAnalysis::verify() const {
#ifndef NDEBUG
  if (!EnableVerifier)
    return;

  // We check that all functions in the module are in liveFunctions /and/ then
  // make sure that liveFunctions has the same number of elements. If we have
  // too many elements, this means we missed a delete event.
  unsigned funcCount = 0;
  for (auto &fn : mod) {
    ++funcCount;
    assert(liveFunctions.count(&fn) &&
           "Found function in module that verifier is not aware of?!");
  }
  assert(liveFunctions.size() == funcCount &&
         "Analysis has state for deleted functions?!");
#endif
}

//===----------------------------------------------------------------------===//
//                              Main Entry Point
//===----------------------------------------------------------------------===//

SILAnalysis *swift::createPassManagerVerifierAnalysis(SILModule *m) {
  return new PassManagerVerifierAnalysis(m);
}
