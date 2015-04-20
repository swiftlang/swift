//===----- PassManager.cpp - Swift Pass Manager ---------------------------===//
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

#define DEBUG_TYPE "sil-passmanager"

#include "swift/SILPasses/PassManager.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILPasses/PrettyStackTrace.h"
#include "swift/SILPasses/Transforms.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/TimeValue.h"

using namespace swift;

STATISTIC(NumOptzIterations, "Number of optimization iterations");

llvm::cl::opt<std::string>
    SILPrintOnlyFun("sil-print-only-function", llvm::cl::init(""),
                    llvm::cl::desc("Only print out the sil for this function"));

llvm::cl::opt<std::string>
    SILPrintOnlyFuns("sil-print-only-functions", llvm::cl::init(""),
                    llvm::cl::desc("Only print out the sil for the functions whose name contains this substring"));

llvm::cl::list<std::string>
    SILPrintBefore("sil-print-before",
                   llvm::cl::desc("Print out the sil before passes which "
                                  "contain a string from this list."));

llvm::cl::list<std::string>
    SILPrintAfter("sil-print-after",
                  llvm::cl::desc("Print out the sil after passes which contain "
                                 "a string from this list."));

llvm::cl::list<std::string>
    SILPrintAround("sil-print-around",
                   llvm::cl::desc("Print out the sil before and after passes "
                                  "which contain a string from this list"));

llvm::cl::list<std::string>
    SILDisablePass("sil-disable-pass",
                     llvm::cl::desc("Disable passes "
                                    "which contain a string from this list"));

llvm::cl::opt<bool> SILValidateAnalyses(
    "sil-validate-analyses", llvm::cl::init(false),
    llvm::cl::desc("Validate analyses when running with -sil-verify-all"));

static bool doPrintBefore(SILTransform *T, SILFunction *F) {
  if (!SILPrintOnlyFun.empty() && F && F->getName() != SILPrintOnlyFun)
    return false;

  if (!SILPrintOnlyFuns.empty() && F &&
      F->getName().find(SILPrintOnlyFuns, 0) == StringRef::npos)
    return false;

  auto MatchFun = [&](const std::string &Str) -> bool {
    return T->getName().find(Str) != StringRef::npos;
  };

  if (SILPrintBefore.end() !=
      std::find_if(SILPrintBefore.begin(), SILPrintBefore.end(), MatchFun))
    return true;

  if (SILPrintAround.end() !=
      std::find_if(SILPrintAround.begin(), SILPrintAround.end(), MatchFun))
    return true;

  return false;
}

static bool doPrintAfter(SILTransform *T, SILFunction *F, bool Default) {
  if (!SILPrintOnlyFun.empty() && F && F->getName() != SILPrintOnlyFun)
    return false;

  if (!SILPrintOnlyFuns.empty() && F &&
      F->getName().find(SILPrintOnlyFuns, 0) == StringRef::npos)
    return false;

  auto MatchFun = [&](const std::string &Str) -> bool {
    return T->getName().find(Str) != StringRef::npos;
  };

  if (SILPrintAfter.end() !=
      std::find_if(SILPrintAfter.begin(), SILPrintAfter.end(), MatchFun))
    return true;

  if (SILPrintAround.end() !=
      std::find_if(SILPrintAround.begin(), SILPrintAround.end(), MatchFun))
    return true;

  return Default;
}

static bool isDisabled(SILTransform *T) {
  for (const std::string &NamePattern : SILDisablePass) {
    if (T->getName().find(NamePattern) != StringRef::npos)
      return true;
  }
  return false;
}

static void printModule(SILModule *Mod) {
  if (SILPrintOnlyFun.empty() && SILPrintOnlyFuns.empty()) {
    Mod->dump();
    return;
  }
  for (auto &F : *Mod) {
    if (!SILPrintOnlyFun.empty() && F.getName().str() == SILPrintOnlyFun)
      F.dump();

    if (!SILPrintOnlyFuns.empty() &&
        F.getName().find(SILPrintOnlyFuns, 0) != StringRef::npos)
      F.dump();
  }
}

bool SILPassManager::
runFunctionPasses(llvm::ArrayRef<SILFunctionTransform*> FuncTransforms) {
  const SILOptions &Options = getOptions();

  for (auto &F : *Mod) {
    if (F.empty())
      continue;

    // Don't optimize functions that are marked with the opt.never attribute.
    if (F.hasSemanticsString("optimize.never"))
      continue;

    CompletedPasses &completedPasses = CompletedPassesMap[&F];

    for (auto SFT : FuncTransforms) {
      PrettyStackTraceSILFunctionTransform X(SFT);
      SFT->injectPassManager(this);
      SFT->injectFunction(&F);
      
      // If nothing changed since the last run of this pass, we can skip this
      // pass.
      if (completedPasses.test((size_t)SFT->getPassKind()))
        continue;

      if (isDisabled(SFT))
        continue;

      currentPassHasInvalidated = false;

      if (Options.PrintPassName)
        llvm::dbgs() << "#" << NumPassesRun << " Stage: " << StageName
                     << " Pass: " << SFT->getName()
                     << ", Function: " << F.getName() << "\n";

      if (doPrintBefore(SFT, &F)) {
        llvm::dbgs() << "*** SIL function before " << StageName << " "
                     << SFT->getName() << " (" << NumOptimizationIterations
                     << ") ***\n";
        F.dump();
      }

      llvm::sys::TimeValue StartTime = llvm::sys::TimeValue::now();
      SFT->run();

      if (Options.TimeTransforms) {
        auto Delta = llvm::sys::TimeValue::now().nanoseconds() -
          StartTime.nanoseconds();
        llvm::dbgs() << Delta << " (" << SFT->getName() << "," << F.getName()
                     << ")\n";
      }

      // If this pass invalidated anything, print and verify.
      if (doPrintAfter(SFT, &F,
                       currentPassHasInvalidated && Options.PrintAll)) {
        llvm::dbgs() << "*** SIL function after " << StageName << " "
                     << SFT->getName() << " (" << NumOptimizationIterations
                     << ") ***\n";
        F.dump();
      }

      // Remember if this pass didn't change anything.
      if (!currentPassHasInvalidated)
        completedPasses.set((size_t)SFT->getPassKind());

      if (currentPassHasInvalidated && Options.VerifyAll) {
        F.verify();
        if (SILValidateAnalyses)
          verifyAnalyses();
      }

      ++NumPassesRun;
      if (Mod->getStage() == SILStage::Canonical
          && NumPassesRun >= Options.NumOptPassesToRun)
        return false;
    }
  }

  return true;
}

void SILPassManager::runOneIteration() {
  // Verify that all analysis were properly unlocked.
  for (auto A : Analysis) {
    assert(!A->isLocked() &&
           "Deleting a locked analysis. Did we forget to unlock ?");
    (void)A;
  }

  const SILOptions &Options = getOptions();

  DEBUG(llvm::dbgs() << "*** Optimizing the module (" << StageName
        << ") *** \n");
  if (Options.PrintAll && NumOptimizationIterations == 0) {
    llvm::dbgs() << "*** SIL module before "  << StageName
                 << " transformation (" << NumOptimizationIterations
                 << ") ***\n";
    printModule(Mod);
  }
  NumOptzIterations++;
  NumOptimizationIterations++;
  SmallVector<SILFunctionTransform*, 16> PendingFuncTransforms;

  // For each transformation:
  for (SILTransform *ST : Transformations) {
    // Bail out if we've hit the optimization pass limit.
    if (Mod->getStage() == SILStage::Canonical
        && NumPassesRun >= Options.NumOptPassesToRun)
      return;

    // Run module transformations on the module.
    if (SILModuleTransform *SMT = llvm::dyn_cast<SILModuleTransform>(ST)) {
      // Run all function passes that we've seen since the last module pass. If
      // one of the passes asked us to stop the pass pipeline, return false.
      if (!runFunctionPasses(PendingFuncTransforms))
        return;

      PendingFuncTransforms.clear();

      if (isDisabled(SMT))
        continue;
      
      PrettyStackTraceSILModuleTransform X(SMT);

      SMT->injectPassManager(this);
      SMT->injectModule(Mod);

      currentPassHasInvalidated = false;

      if (Options.PrintPassName)
        llvm::dbgs() << "#" << NumPassesRun << " Stage: " << StageName
                     << " Pass: " << SMT->getName() << " (module pass)\n";

      if (doPrintBefore(SMT, nullptr)) {
        llvm::dbgs() << "*** SIL module before " << StageName << " "
                     << SMT->getName() << " (" << NumOptimizationIterations
                     << ") ***\n";
        printModule(Mod);
      }

      llvm::sys::TimeValue StartTime = llvm::sys::TimeValue::now();
      SMT->run();

      if (Options.TimeTransforms) {
        auto Delta = llvm::sys::TimeValue::now().nanoseconds() -
          StartTime.nanoseconds();
        llvm::dbgs() << Delta << " (" << SMT->getName() << ",Module)\n";
      }

      // If this pass invalidated anything, print and verify.
      if (doPrintAfter(SMT, nullptr,
                       currentPassHasInvalidated && Options.PrintAll)) {
        llvm::dbgs() << "*** SIL module after " << StageName << " "
                     << SMT->getName() << " (" << NumOptimizationIterations
                     << ") ***\n";
        printModule(Mod);
      }

      if (currentPassHasInvalidated && Options.VerifyAll) {
        Mod->verify();
        if (SILValidateAnalyses)
          verifyAnalyses();
      }

      ++NumPassesRun;
      if (Mod->getStage() == SILStage::Canonical
          && NumPassesRun >= Options.NumOptPassesToRun) {
        return;
      }
      
      continue;
    }

    // Run function transformation on all functions.
    if (SILFunctionTransform *SFT = llvm::dyn_cast<SILFunctionTransform>(ST)) {
      PendingFuncTransforms.push_back(SFT);      
      continue;
    }

    llvm_unreachable("Unknown pass kind.");
  }

  runFunctionPasses(PendingFuncTransforms);
}

void SILPassManager::run() {
  const SILOptions &Options = getOptions();
  if (Options.PrintAll) {
    if (SILPrintOnlyFun.empty() && SILPrintOnlyFuns.empty()) {
      llvm::dbgs() << "*** SIL module before transformation ("
                   << NumOptimizationIterations << ") ***\n";
      Mod->dump();
    } else {
      for (auto &F : *Mod) {
        if (!SILPrintOnlyFun.empty() && F.getName().str() == SILPrintOnlyFun) {
          llvm::dbgs() << "*** SIL function before transformation ("
                       << NumOptimizationIterations << ") ***\n";
          F.dump();
        }
        if (!SILPrintOnlyFuns.empty() &&
            F.getName().find(SILPrintOnlyFuns, 0) != StringRef::npos)
          llvm::dbgs() << "*** SIL function before transformation ("
                       << NumOptimizationIterations << ") ***\n";
          F.dump();
      }
    }
  }
  // Keep optimizing the module until no pass requested another iteration
  // of the pass or we reach the maximum.
  const unsigned IterationLimit = 20;
  do {
    anotherIteration = false;
    runOneIteration();
  } while (anotherIteration && NumOptimizationIterations < IterationLimit);
}

/// D'tor.
SILPassManager::~SILPassManager() {
  // Free all transformations.
  for (auto T : Transformations)
    delete T;

  // delete the analyis.
  for (auto A : Analysis) {
    assert(!A->isLocked() &&
           "Deleting a locked analysis. Did we forget to unlock ?");
    delete A;
  }
}

/// \brief Reset the state of the pass manager and remove all transformation
/// owned by the pass manager. Anaysis passes will be kept.
void SILPassManager::resetAndRemoveTransformations() {
  for (auto T : Transformations)
    delete T;

  Transformations.clear();
  NumOptimizationIterations = 0;
  anotherIteration = false;
}

void SILPassManager::setStageName(llvm::StringRef NextStage) {
  StageName = NextStage;
}

const SILOptions &SILPassManager::getOptions() const {
  return Mod->getOptions();
}

// Define the add-functions for all passes.

#define PASS(Id)                           \
void SILPassManager::add##Id() {           \
  SILTransform *T = swift::create##Id();   \
  T->setPassKind(PassKind::Id);            \
  Transformations.push_back(T);            \
}
#include "swift/SILPasses/Passes.def"

void SILPassManager::addPass(PassKind Kind) {
  assert(unsigned(PassKind::AllPasses_Last) >= unsigned(Kind) &&
         "Invalid pass kind");
  switch (Kind) {
#define PASS(ID)                           \
  case PassKind::ID:                       \
    add##ID();                             \
    break;
#include "swift/SILPasses/Passes.def"
  case PassKind::invalidPassKind:
    llvm_unreachable("invalid pass kind");
  }
}

void SILPassManager::addPassForName(StringRef Name) {
  auto P = llvm::StringSwitch<PassKind>(Name)
#define PASS(Id) .Case(#Id, PassKind::Id)
#include "swift/SILPasses/Passes.def"
  ;
  addPass(P);
}


