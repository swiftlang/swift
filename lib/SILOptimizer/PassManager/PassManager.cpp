//===--- PassManager.cpp - Swift Pass Manager -----------------------------===//
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

#define DEBUG_TYPE "sil-passmanager"

#include "swift/SILOptimizer/PassManager/PassManager.h"
#include "swift/Basic/DemangleWrappers.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILOptimizer/Analysis/BasicCalleeAnalysis.h"
#include "swift/SILOptimizer/Analysis/FunctionOrder.h"
#include "swift/SILOptimizer/PassManager/PrettyStackTrace.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/GraphWriter.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/TimeValue.h"

using namespace swift;

STATISTIC(NumOptzIterations, "Number of optimization iterations");

llvm::cl::opt<bool> SILPrintAll(
    "sil-print-all", llvm::cl::init(false),
    llvm::cl::desc("Print SIL after each pass"));

llvm::cl::opt<bool> SILPrintPassName(
    "sil-print-pass-name", llvm::cl::init(false),
    llvm::cl::desc("Print the name of each SIL pass before it runs"));

llvm::cl::opt<bool> SILPrintPassTime(
    "sil-print-pass-time", llvm::cl::init(false),
    llvm::cl::desc("Print the execution time of each SIL pass"));

llvm::cl::opt<unsigned> SILNumOptPassesToRun(
    "sil-opt-pass-count", llvm::cl::init(UINT_MAX),
    llvm::cl::desc("Stop optimizing after <N> optimization passes"));

llvm::cl::opt<unsigned> SILFunctionPassPipelineLimit("sil-pipeline-limit",
                                                     llvm::cl::init(10),
                                                     llvm::cl::desc(""));

llvm::cl::opt<std::string> SILBreakOnFun(
    "sil-break-on-function", llvm::cl::init(""),
    llvm::cl::desc(
        "Break before running each function pass on a particular function"));

llvm::cl::opt<std::string> SILBreakOnPass(
    "sil-break-on-pass", llvm::cl::init(""),
    llvm::cl::desc("Break before running a particular function pass"));

llvm::cl::opt<std::string>
    SILPrintOnlyFun("sil-print-only-function", llvm::cl::init(""),
                    llvm::cl::desc("Only print out the sil for this function"));

llvm::cl::opt<std::string>
    SILPrintOnlyFuns("sil-print-only-functions", llvm::cl::init(""),
                     llvm::cl::desc("Only print out the sil for the functions "
                                    "whose name contains this substring"));

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

llvm::cl::opt<bool> SILVerifyWithoutInvalidation(
    "sil-verify-without-invalidation", llvm::cl::init(false),
    llvm::cl::desc("Verify after passes even if the pass has not invalidated"));

llvm::cl::opt<bool> SILDisableSkippingPasses(
    "sil-disable-skipping-passes", llvm::cl::init(false),
    llvm::cl::desc("Do not skip passes even if nothing was changed"));

static llvm::ManagedStatic<std::vector<unsigned>> DebugPassNumbers;

namespace {

struct DebugOnlyPassNumberOpt {
  void operator=(const std::string &Val) const {
    if (Val.empty())
      return;
    SmallVector<StringRef, 8> dbgPassNumbers;
    StringRef(Val).split(dbgPassNumbers, ',', -1, false);
    for (auto dbgPassNumber : dbgPassNumbers) {
      int PassNumber;
      if (dbgPassNumber.getAsInteger(10, PassNumber) || PassNumber < 0)
        llvm_unreachable("The pass number should be an integer number >= 0");
      DebugPassNumbers->push_back(static_cast<unsigned>(PassNumber));
    }
  }
};

}

static DebugOnlyPassNumberOpt DebugOnlyPassNumberOptLoc;

static llvm::cl::opt<DebugOnlyPassNumberOpt, true,
                     llvm::cl::parser<std::string>>
    DebugOnly("debug-only-pass-number",
              llvm::cl::desc("Enable a specific type of debug output (comma "
                             "separated list pass numbers)"),
              llvm::cl::Hidden, llvm::cl::ZeroOrMore,
              llvm::cl::value_desc("pass number"),
              llvm::cl::location(DebugOnlyPassNumberOptLoc),
              llvm::cl::ValueRequired);

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

static void printModule(SILModule *Mod, bool EmitVerboseSIL) {
  if (SILPrintOnlyFun.empty() && SILPrintOnlyFuns.empty()) {
    Mod->dump();
    return;
  }
  for (auto &F : *Mod) {
    if (!SILPrintOnlyFun.empty() && F.getName().str() == SILPrintOnlyFun)
      F.dump(EmitVerboseSIL);

    if (!SILPrintOnlyFuns.empty() &&
        F.getName().find(SILPrintOnlyFuns, 0) != StringRef::npos)
      F.dump(EmitVerboseSIL);
  }
}

class DebugPrintEnabler {
#ifndef NDEBUG
  bool OldDebugFlag;
#endif
public:
  DebugPrintEnabler(unsigned PassNumber) {
#ifndef NDEBUG
    OldDebugFlag = llvm::DebugFlag;
    if (llvm::DebugFlag)
      return;
    if (DebugPassNumbers->empty())
      return;
    // Enable debug printing if the pass number matches
    // one of the pass numbers provided as a command line option.
    for (auto DebugPassNumber : *DebugPassNumbers) {
      if (DebugPassNumber == PassNumber) {
        llvm::DebugFlag = true;
        return;
      }
    }
#endif
  }

  ~DebugPrintEnabler() {
#ifndef NDEBUG
    llvm::DebugFlag = OldDebugFlag;
#endif
  }
};


SILPassManager::SILPassManager(SILModule *M, llvm::StringRef Stage) :
  Mod(M), StageName(Stage) {
  
#define ANALYSIS(NAME) \
  Analysis.push_back(create##NAME##Analysis(Mod));
#include "swift/SILOptimizer/Analysis/Analysis.def"

  for (SILAnalysis *A : Analysis) {
    A->initialize(this);
    M->registerDeleteNotificationHandler(A);
  }
}

bool SILPassManager::continueTransforming() {
  return Mod->getStage() == SILStage::Raw ||
         NumPassesRun < SILNumOptPassesToRun;
}

bool SILPassManager::analysesUnlocked() {
  for (auto A : Analysis)
    if (A->isLocked())
      return false;

  return true;
}

// Test the function and pass names we're given against the debug
// options that force us to break prior to a given pass and/or on a
// given function.
static bool breakBeforeRunning(StringRef fnName, StringRef passName) {
  if (SILBreakOnFun.empty() && SILBreakOnPass.empty())
    return false;

  if (SILBreakOnFun.empty() && passName == SILBreakOnPass)
    return true;

  if (SILBreakOnPass.empty() && fnName == SILBreakOnFun)
    return true;

  return fnName == SILBreakOnFun && passName == SILBreakOnPass;
}

void SILPassManager::runPassesOnFunction(PassList FuncTransforms,
                                         SILFunction *F,
                                         bool runToCompletion) {

  const SILOptions &Options = getOptions();

  CompletedPasses &completedPasses = CompletedPassesMap[F];

  assert(analysesUnlocked() && "Expected all analyses to be unlocked!");

  for (auto SFT : FuncTransforms) {
    PrettyStackTraceSILFunctionTransform X(SFT, NumPassesRun);
    DebugPrintEnabler DebugPrint(NumPassesRun);

    SFT->injectPassManager(this);
    SFT->injectFunction(F);

    // If nothing changed since the last run of this pass, we can skip this
    // pass.
    if (completedPasses.test((size_t)SFT->getPassKind()) &&
        !SILDisableSkippingPasses) {
      if (SILPrintPassName)
        llvm::dbgs() << "(Skip) Stage: " << StageName
                     << " Pass: " << SFT->getName()
                     << ", Function: " << F->getName() << "\n";
      continue;
    }

    if (isDisabled(SFT)) {
      if (SILPrintPassName)
        llvm::dbgs() << "(Disabled) Stage: " << StageName
                     << " Pass: " << SFT->getName()
                     << ", Function: " << F->getName() << "\n";
      continue;
    }

    CurrentPassHasInvalidated = false;

    if (SILPrintPassName)
      llvm::dbgs() << "#" << NumPassesRun << " Stage: " << StageName
                   << " Pass: " << SFT->getName()
                   << ", Function: " << F->getName() << "\n";

    if (doPrintBefore(SFT, F)) {
      llvm::dbgs() << "*** SIL function before " << StageName << " "
                   << SFT->getName() << " (" << NumOptimizationIterations
                   << ") ***\n";
      F->dump(Options.EmitVerboseSIL);
    }

    llvm::sys::TimeValue StartTime = llvm::sys::TimeValue::now();
    Mod->registerDeleteNotificationHandler(SFT);
    if (breakBeforeRunning(F->getName(), SFT->getName()))
      LLVM_BUILTIN_DEBUGTRAP;
    SFT->run();
    assert(analysesUnlocked() && "Expected all analyses to be unlocked!");
    Mod->removeDeleteNotificationHandler(SFT);

    // Did running the transform result in new functions being added
    // to the top of our worklist?
    bool newFunctionsAdded = (F != FunctionWorklist.back());

    if (SILPrintPassTime) {
      auto Delta =
          llvm::sys::TimeValue::now().nanoseconds() - StartTime.nanoseconds();
      llvm::dbgs() << Delta << " (" << SFT->getName() << "," << F->getName()
                   << ")\n";
    }

    // If this pass invalidated anything, print and verify.
    if (doPrintAfter(SFT, F, CurrentPassHasInvalidated && SILPrintAll)) {
      llvm::dbgs() << "*** SIL function after " << StageName << " "
                   << SFT->getName() << " (" << NumOptimizationIterations
                   << ") ***\n";
      F->dump(Options.EmitVerboseSIL);
    }

    // Remember if this pass didn't change anything.
    if (!CurrentPassHasInvalidated)
      completedPasses.set((size_t)SFT->getPassKind());

    if (Options.VerifyAll &&
        (CurrentPassHasInvalidated || SILVerifyWithoutInvalidation)) {
      F->verify();
      verifyAnalyses(F);
    }

    ++NumPassesRun;

    if (!continueTransforming())
      return;

    if (runToCompletion)
      continue;

    // If running the transform resulted in new functions on the top
    // of the worklist, we'll return so that we can begin processing
    // those new functions.
    if (shouldRestartPipeline() || newFunctionsAdded)
      return;
  }
}

void SILPassManager::runFunctionPasses(PassList FuncTransforms) {
  BasicCalleeAnalysis *BCA = getAnalysis<BasicCalleeAnalysis>();
  BottomUpFunctionOrder BottomUpOrder(*Mod, BCA);
  auto BottomUpFunctions = BottomUpOrder.getFunctions();

  assert(FunctionWorklist.empty() && "Expected empty function worklist!");

  FunctionWorklist.reserve(BottomUpFunctions.size());
  for (auto I = BottomUpFunctions.rbegin(), E = BottomUpFunctions.rend();
       I != E; ++I) {
    auto &F = **I;

    // Only include functions that are definitions, and which have not
    // been intentionally excluded from optimization.
    if (F.isDefinition() && F.shouldOptimize())
      FunctionWorklist.push_back(*I);
  }

  // Used to track how many times a given function has been
  // (partially) optimized by the function pass pipeline in this
  // invocation.
  llvm::DenseMap<SILFunction *, unsigned> CountOptimized;

  // Count of how many iterations we've had since any function was
  // popped off the function worklist. This is used to ensure progress
  // and eliminate the chance of going into an infinite loop in cases
  // where (for example) we have recursive type-based specialization
  // happening.
  unsigned IterationsWithoutProgress = 0;

  // The maximum number of functions we'll optimize without popping
  // any off the worklist. This is expected to non-zero.
  const unsigned MaxIterationsWithoutProgress = 20;

  // Pop functions off the worklist, and run all function transforms
  // on each of them.
  while (!FunctionWorklist.empty() && continueTransforming()) {
    auto *F = FunctionWorklist.back();

    // If we've done many iterations without progress, pop the current
    // function and any other function we've run any optimizations on
    // on from the stack and then continue.
    if (IterationsWithoutProgress == (MaxIterationsWithoutProgress - 1)) {
      // Pop the current (potentially not-yet-optimized) function off.
      FunctionWorklist.pop_back();
      IterationsWithoutProgress = 0;

      // Pop any remaining functions that have been optimized (at
      // least through some portion of the pipeline).
      while (!FunctionWorklist.empty() &&
             CountOptimized[FunctionWorklist.back()] > 0)
        FunctionWorklist.pop_back();

      continue;
    }

    if (CountOptimized[F] > SILFunctionPassPipelineLimit) {
      DEBUG(llvm::dbgs() << "*** Hit limit optimizing: " << F->getName()
                         << '\n');
      FunctionWorklist.pop_back();
      IterationsWithoutProgress = 0;
      continue;
    }

    assert(
        !shouldRestartPipeline() &&
        "Did not expect function pipeline set up to restart from beginning!");

    assert(CountOptimized[F] <= SILFunctionPassPipelineLimit &&
           "Function optimization count exceeds limit!");
    auto runToCompletion = CountOptimized[F] == SILFunctionPassPipelineLimit;

    runPassesOnFunction(FuncTransforms, F, runToCompletion);
    ++CountOptimized[F];
    ++IterationsWithoutProgress;

    if (runToCompletion) {
      FunctionWorklist.pop_back();
      IterationsWithoutProgress = 0;
      clearRestartPipeline();
      continue;
    }


    // If running the function transforms did not result in new
    // functions being added to the top of the worklist, then we're
    // done with this function and can pop it off and continue.
    // Otherwise, we'll return to this function and reoptimize after
    // processing the new functions that were added.
    if (F == FunctionWorklist.back() && !shouldRestartPipeline()) {
      FunctionWorklist.pop_back();
      IterationsWithoutProgress = 0;
    }

    clearRestartPipeline();
  }
}

void SILPassManager::runModulePass(SILModuleTransform *SMT) {
  if (isDisabled(SMT))
    return;

  const SILOptions &Options = getOptions();

  PrettyStackTraceSILModuleTransform X(SMT, NumPassesRun);
  DebugPrintEnabler DebugPrint(NumPassesRun);

  SMT->injectPassManager(this);
  SMT->injectModule(Mod);

  CurrentPassHasInvalidated = false;

  if (SILPrintPassName)
    llvm::dbgs() << "#" << NumPassesRun << " Stage: " << StageName
                 << " Pass: " << SMT->getName() << " (module pass)\n";

  if (doPrintBefore(SMT, nullptr)) {
    llvm::dbgs() << "*** SIL module before " << StageName << " "
                 << SMT->getName() << " (" << NumOptimizationIterations
                 << ") ***\n";
    printModule(Mod, Options.EmitVerboseSIL);
  }

  llvm::sys::TimeValue StartTime = llvm::sys::TimeValue::now();
  assert(analysesUnlocked() && "Expected all analyses to be unlocked!");
  Mod->registerDeleteNotificationHandler(SMT);
  SMT->run();
  Mod->removeDeleteNotificationHandler(SMT);
  assert(analysesUnlocked() && "Expected all analyses to be unlocked!");

  if (SILPrintPassTime) {
    auto Delta = llvm::sys::TimeValue::now().nanoseconds() -
      StartTime.nanoseconds();
    llvm::dbgs() << Delta << " (" << SMT->getName() << ",Module)\n";
  }

  // If this pass invalidated anything, print and verify.
  if (doPrintAfter(SMT, nullptr,
                   CurrentPassHasInvalidated && SILPrintAll)) {
    llvm::dbgs() << "*** SIL module after " << StageName << " "
                 << SMT->getName() << " (" << NumOptimizationIterations
                 << ") ***\n";
    printModule(Mod, Options.EmitVerboseSIL);
  }

  if (Options.VerifyAll &&
      (CurrentPassHasInvalidated || !SILVerifyWithoutInvalidation)) {
    Mod->verify();
    verifyAnalyses();
  }
}

void SILPassManager::runOneIteration() {
  const SILOptions &Options = getOptions();

  DEBUG(llvm::dbgs() << "*** Optimizing the module (" << StageName
        << ") *** \n");
  if (SILPrintAll && NumOptimizationIterations == 0) {
    llvm::dbgs() << "*** SIL module before "  << StageName
                 << " transformation (" << NumOptimizationIterations
                 << ") ***\n";
    printModule(Mod, Options.EmitVerboseSIL);
  }
  NumOptzIterations++;
  NumOptimizationIterations++;
  SmallVector<SILFunctionTransform*, 16> PendingFuncTransforms;

  // Run the transforms by alternating between function transforms and
  // module transforms. We'll queue up all the function transforms
  // that we see in a row and then run the entire group of transforms
  // on each function in turn. Then we move on to running the next set
  // of consecutive module transforms.
  auto It = Transformations.begin();
  auto End = Transformations.end();

  while (It != End && continueTransforming()) {
    assert((isa<SILFunctionTransform>(*It) || isa<SILModuleTransform>(*It)) &&
           "Unexpected pass kind!");

    while (It != End && isa<SILFunctionTransform>(*It))
      PendingFuncTransforms.push_back(cast<SILFunctionTransform>(*It++));

    runFunctionPasses(PendingFuncTransforms);
    PendingFuncTransforms.clear();

    while (It != End && isa<SILModuleTransform>(*It) &&
           continueTransforming()) {
      runModulePass(cast<SILModuleTransform>(*It));

      ++It;
      ++NumPassesRun;
    }
  }
}

void SILPassManager::run() {
  const SILOptions &Options = getOptions();
  (void) Options;

  if (SILPrintAll) {
    if (SILPrintOnlyFun.empty() && SILPrintOnlyFuns.empty()) {
      llvm::dbgs() << "*** SIL module before transformation ("
                   << NumOptimizationIterations << ") ***\n";
      Mod->dump(Options.EmitVerboseSIL);
    } else {
      for (auto &F : *Mod) {
        if (!SILPrintOnlyFun.empty() && F.getName().str() == SILPrintOnlyFun) {
          llvm::dbgs() << "*** SIL function before transformation ("
                       << NumOptimizationIterations << ") ***\n";
          F.dump(Options.EmitVerboseSIL);
        }
        if (!SILPrintOnlyFuns.empty() &&
            F.getName().find(SILPrintOnlyFuns, 0) != StringRef::npos) {
          llvm::dbgs() << "*** SIL function before transformation ("
                       << NumOptimizationIterations << ") ***\n";
          F.dump(Options.EmitVerboseSIL);
        }
      }
    }
  }
  runOneIteration();
}

/// D'tor.
SILPassManager::~SILPassManager() {
  // Free all transformations.
  for (auto T : Transformations)
    delete T;

  // delete the analysis.
  for (auto A : Analysis) {
    Mod->removeDeleteNotificationHandler(A);
    assert(!A->isLocked() &&
           "Deleting a locked analysis. Did we forget to unlock ?");
    delete A;
  }
}

void SILPassManager::restartWithCurrentFunction(SILTransform *T) {
  assert(isa<SILFunctionTransform>(T) &&
         "Can only restart the pipeline from function passes");
  RestartPipeline = true;
}

/// \brief Reset the state of the pass manager and remove all transformation
/// owned by the pass manager. Analysis passes will be kept.
void SILPassManager::resetAndRemoveTransformations() {
  for (auto T : Transformations)
    delete T;

  Transformations.clear();
  NumOptimizationIterations = 0;
}

void SILPassManager::setStageName(llvm::StringRef NextStage) {
  StageName = NextStage;
}

const SILOptions &SILPassManager::getOptions() const {
  return Mod->getOptions();
}

// Define the add-functions for all passes.

#define PASS(ID, NAME, DESCRIPTION)                                            \
  void SILPassManager::add##ID() {                                             \
    SILTransform *T = swift::create##ID();                                     \
    T->setPassKind(PassKind::ID);                                              \
    Transformations.push_back(T);                                              \
  }
#include "swift/SILOptimizer/PassManager/Passes.def"

void SILPassManager::addPass(PassKind Kind) {
  assert(unsigned(PassKind::AllPasses_Last) >= unsigned(Kind) &&
         "Invalid pass kind");
  switch (Kind) {
#define PASS(ID, NAME, DESCRIPTION)                                            \
  case PassKind::ID:                                                           \
    add##ID();                                                                 \
    break;
#include "swift/SILOptimizer/PassManager/Passes.def"
  case PassKind::invalidPassKind:
    llvm_unreachable("invalid pass kind");
  }
}

void SILPassManager::addPassForName(StringRef Name) {
  PassKind P = llvm::StringSwitch<PassKind>(Name)
#define PASS(ID, NAME, DESCRIPTION) .Case(#ID, PassKind::ID)
#include "swift/SILOptimizer/PassManager/Passes.def"
  ;
  addPass(P);
}

//===----------------------------------------------------------------------===//
//                          View Call-Graph Implementation
//===----------------------------------------------------------------------===//

#ifndef NDEBUG

namespace {

  /// An explicit graph data structure for the call graph.
  /// Used for viewing the callgraph as dot file with llvm::ViewGraph.
  struct CallGraph {

    struct Node;

    struct Edge {
      FullApplySite FAS;
      Node *Child;
      bool Incomplete;
    };

    struct Node {
      SILFunction *F;
      CallGraph *CG;
      int NumCallSites = 0;
      SmallVector<Edge, 8> Children;
    };

    struct child_iterator
        : public std::iterator<std::random_access_iterator_tag, Node *,
                               ptrdiff_t> {
      SmallVectorImpl<Edge>::iterator baseIter;

      child_iterator(SmallVectorImpl<Edge>::iterator baseIter) :
      baseIter(baseIter)
      { }

      child_iterator &operator++() { baseIter++; return *this; }
      child_iterator operator++(int) {
        auto tmp = *this;
        baseIter++;
        return tmp;
      }
      Node *operator*() const { return baseIter->Child; }
      bool operator==(const child_iterator &RHS) const {
        return baseIter == RHS.baseIter;
      }
      bool operator!=(const child_iterator &RHS) const {
        return baseIter != RHS.baseIter;
      }
      difference_type operator-(const child_iterator &RHS) const {
        return baseIter - RHS.baseIter;
      }
    };

    CallGraph(SILModule *M, BasicCalleeAnalysis *BCA);

    std::vector<Node> Nodes;

    /// The SILValue IDs which are printed as edge source labels.
    llvm::DenseMap<const ValueBase *, unsigned> InstToIDMap;

    typedef std::vector<Node>::iterator iterator;
  };

  CallGraph::CallGraph(SILModule *M, BasicCalleeAnalysis *BCA) {
    Nodes.resize(M->getFunctionList().size());
    llvm::DenseMap<SILFunction *, Node *> NodeMap;
    int idx = 0;
    for (SILFunction &F : *M) {
      Node &Nd = Nodes[idx++];
      Nd.F = &F;
      Nd.CG = this;
      NodeMap[&F] = &Nd;

      F.numberValues(InstToIDMap);
    }

    for (Node &Nd : Nodes) {
      for (SILBasicBlock &BB : *Nd.F) {
        for (SILInstruction &I : BB) {
          if (FullApplySite FAS = FullApplySite::isa(&I)) {
            auto CList = BCA->getCalleeList(FAS);
            for (SILFunction *Callee : CList) {
              Node *CalleeNode = NodeMap[Callee];
              Nd.Children.push_back({FAS, CalleeNode,CList.isIncomplete()});
            }
          }
        }
      }
    }
  }

} // end swift namespace

namespace llvm {

  /// Wraps a dot node label string to multiple lines. The \p NumEdgeLabels
  /// gives an estimate on the minimum width of the node shape.
  static void wrap(std::string &Str, int NumEdgeLabels) {
    unsigned ColNum = 0;
    unsigned LastSpace = 0;
    unsigned MaxColumns = std::max(60, NumEdgeLabels * 8);
    for (unsigned i = 0; i != Str.length(); ++i) {
      if (ColNum == MaxColumns) {
        if (!LastSpace)
          LastSpace = i;
        Str.insert(LastSpace + 1, "\\l");
        ColNum = i - LastSpace - 1;
        LastSpace = 0;
      } else
        ++ColNum;
      if (Str[i] == ' ' || Str[i] == '.')
        LastSpace = i;
    }
  }

  /// CallGraph GraphTraits specialization so the CallGraph can be
  /// iterable by generic graph iterators.
  template <> struct GraphTraits<CallGraph::Node *> {
    typedef CallGraph::Node NodeType;
    typedef CallGraph::child_iterator ChildIteratorType;

    static NodeType *getEntryNode(NodeType *N) { return N; }
    static inline ChildIteratorType child_begin(NodeType *N) {
      return N->Children.begin();
    }
    static inline ChildIteratorType child_end(NodeType *N) {
      return N->Children.end();
    }
  };

  template <> struct GraphTraits<CallGraph *>
  : public GraphTraits<CallGraph::Node *> {
    typedef CallGraph *GraphType;

    static NodeType *getEntryNode(GraphType F) { return nullptr; }

    typedef CallGraph::iterator nodes_iterator;
    static nodes_iterator nodes_begin(GraphType CG) {
      return CG->Nodes.begin();
    }
    static nodes_iterator nodes_end(GraphType CG) { return CG->Nodes.end(); }
    static unsigned size(GraphType CG) { return CG->Nodes.size(); }
  };

  /// This is everything the llvm::GraphWriter needs to write the call graph in
  /// a dot file.
  template <>
  struct DOTGraphTraits<CallGraph *> : public DefaultDOTGraphTraits {

    DOTGraphTraits(bool isSimple = false) : DefaultDOTGraphTraits(isSimple) {}

    std::string getNodeLabel(const CallGraph::Node *Node,
                             const CallGraph *Graph) {
      std::string Label = Node->F->getName();
      wrap(Label, Node->NumCallSites);
      return Label;
    }

    std::string getNodeDescription(const CallGraph::Node *Node,
                                   const CallGraph *Graph) {
      std::string Label = demangle_wrappers::
      demangleSymbolAsString(Node->F->getName());
      wrap(Label, Node->NumCallSites);
      return Label;
    }

    static std::string getEdgeSourceLabel(const CallGraph::Node *Node,
                                          CallGraph::child_iterator I) {
      std::string Label;
      raw_string_ostream O(Label);
      SILInstruction *Inst = I.baseIter->FAS.getInstruction();
      O << '%' << Node->CG->InstToIDMap[Inst];
      return Label;
    }

    static std::string getEdgeAttributes(const CallGraph::Node *Node,
                                         CallGraph::child_iterator I,
                                         const CallGraph *Graph) {
      CallGraph::Edge *Edge = I.baseIter;
      if (Edge->Incomplete)
        return "color=\"red\"";
      return "";
    }
  };
} // end llvm namespace
#endif

void SILPassManager::viewCallGraph() {
  /// When asserts are disabled, this should be a NoOp.
#ifndef NDEBUG
  CallGraph OCG(getModule(), getAnalysis<BasicCalleeAnalysis>());
  llvm::ViewGraph(&OCG, "callgraph");
#endif
}
