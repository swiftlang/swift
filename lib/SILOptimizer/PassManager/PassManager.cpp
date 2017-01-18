//===--- PassManager.cpp - Swift Pass Manager -----------------------------===//
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
#include "llvm/Support/Chrono.h"

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

} // end anonymous namespace

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

SILPassManager::SILPassManager(SILModule *M, irgen::IRGenModule *IRMod,
                               llvm::StringRef Stage)
    : SILPassManager(M, Stage) {
  this->IRMod = IRMod;
}

bool SILPassManager::continueTransforming() {
  return Mod->getStage() == SILStage::Raw ||
         NumPassesRun < SILNumOptPassesToRun;
}

bool SILPassManager::analysesUnlocked() {
  for (auto *A : Analysis)
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

void SILPassManager::runPassOnFunction(SILFunctionTransform *SFT,
                                       SILFunction *F) {

  assert(analysesUnlocked() && "Expected all analyses to be unlocked!");

  PrettyStackTraceSILFunctionTransform X(SFT, NumPassesRun);
  DebugPrintEnabler DebugPrint(NumPassesRun);

  SFT->injectPassManager(this);
  SFT->injectFunction(F);

  // If nothing changed since the last run of this pass, we can skip this
  // pass.
  CompletedPasses &completedPasses = CompletedPassesMap[F];
  if (completedPasses.test((size_t)SFT->getPassKind()) &&
      !SILDisableSkippingPasses) {
    if (SILPrintPassName)
      llvm::dbgs() << "  (Skip) Stage: " << StageName
                   << " Pass: " << SFT->getName()
                   << ", Function: " << F->getName() << "\n";
    return;
  }

  if (isDisabled(SFT)) {
    if (SILPrintPassName)
      llvm::dbgs() << "  (Disabled) Stage: " << StageName
                   << " Pass: " << SFT->getName()
                   << ", Function: " << F->getName() << "\n";
    return;
  }

  CurrentPassHasInvalidated = false;

  if (SILPrintPassName)
    llvm::dbgs() << "  #" << NumPassesRun << " Stage: " << StageName
                 << " Pass: " << SFT->getName()
                 << ", Function: " << F->getName() << "\n";

  if (doPrintBefore(SFT, F)) {
    llvm::dbgs() << "*** SIL function before " << StageName << " "
                 << SFT->getName() << " (" << NumOptimizationIterations
                 << ") ***\n";
    F->dump(getOptions().EmitVerboseSIL);
  }

  llvm::sys::TimePoint<> StartTime = std::chrono::system_clock::now();
  Mod->registerDeleteNotificationHandler(SFT);
  if (breakBeforeRunning(F->getName(), SFT->getName()))
    LLVM_BUILTIN_DEBUGTRAP;
  SFT->run();
  assert(analysesUnlocked() && "Expected all analyses to be unlocked!");
  Mod->removeDeleteNotificationHandler(SFT);

  if (SILPrintPassTime) {
    auto Delta = (std::chrono::system_clock::now() - StartTime).count();
    llvm::dbgs() << Delta << " (" << SFT->getName() << "," << F->getName()
                 << ")\n";
  }

  // If this pass invalidated anything, print and verify.
  if (doPrintAfter(SFT, F, CurrentPassHasInvalidated && SILPrintAll)) {
    llvm::dbgs() << "*** SIL function after " << StageName << " "
                 << SFT->getName() << " (" << NumOptimizationIterations
                 << ") ***\n";
    F->dump(getOptions().EmitVerboseSIL);
  }

  // Remember if this pass didn't change anything.
  if (!CurrentPassHasInvalidated)
    completedPasses.set((size_t)SFT->getPassKind());

  if (getOptions().VerifyAll &&
      (CurrentPassHasInvalidated || SILVerifyWithoutInvalidation)) {
    F->verify();
    verifyAnalyses(F);
  }

  ++NumPassesRun;
}

void SILPassManager::
runFunctionPasses(ArrayRef<SILFunctionTransform *> FuncTransforms) {
  if (FuncTransforms.empty())
    return;

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

  DerivationLevels.clear();

  // The maximum number of times the pass pipeline can be restarted for a
  // function. This is used to ensure we are not going into an infinite loop in
  // cases where (for example) we have recursive type-based specialization
  // happening.
  const unsigned MaxNumRestarts = 20;

  if (SILPrintPassName)
    llvm::dbgs() << "Start function passes at stage: " << StageName << "\n";

  // Run all transforms for all functions, starting at the tail of the worklist.
  while (!FunctionWorklist.empty() && continueTransforming()) {
    unsigned TailIdx = FunctionWorklist.size() - 1;
    unsigned PipelineIdx = FunctionWorklist[TailIdx].PipelineIdx;
    SILFunction *F = FunctionWorklist[TailIdx].F;

    if (PipelineIdx >= FuncTransforms.size()) {
      // All passes did already run for the function. Pop it off the worklist.
      FunctionWorklist.pop_back();
      continue;
    }
    assert(!shouldRestartPipeline() &&
        "Did not expect function pipeline set up to restart from beginning!");

    runPassOnFunction(FuncTransforms[PipelineIdx], F);

    // Note: Don't get entry reference prior to runPassOnFunction().
    // A pass can push a new function to the worklist which may cause a
    // reallocation of the buffer and that would invalidate the reference.
    WorklistEntry &Entry = FunctionWorklist[TailIdx];
    if (shouldRestartPipeline() && Entry.NumRestarts < MaxNumRestarts) {
      ++Entry.NumRestarts;
      Entry.PipelineIdx = 0;
    } else {
      ++Entry.PipelineIdx;
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

  llvm::sys::TimePoint<> StartTime = std::chrono::system_clock::now();
  assert(analysesUnlocked() && "Expected all analyses to be unlocked!");
  Mod->registerDeleteNotificationHandler(SMT);
  SMT->run();
  Mod->removeDeleteNotificationHandler(SMT);
  assert(analysesUnlocked() && "Expected all analyses to be unlocked!");

  if (SILPrintPassTime) {
    auto Delta = (std::chrono::system_clock::now() - StartTime).count();
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
  SmallVector<SILFunctionTransform *, 16> PendingFuncTransforms;

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

/// D'tor.
SILPassManager::~SILPassManager() {
  assert(IRGenPasses.empty() && "Must add IRGen SIL passes that were "
                                "registered to the list of transformations");

  // Free all transformations.
  for (auto *T : Transformations)
    delete T;

  // delete the analysis.
  for (auto *A : Analysis) {
    Mod->removeDeleteNotificationHandler(A);
    assert(!A->isLocked() &&
           "Deleting a locked analysis. Did we forget to unlock ?");
    delete A;
  }
}

void SILPassManager::addFunctionToWorklist(SILFunction *F,
                                           SILFunction *DerivedFrom) {
  assert(F && F->isDefinition() && F->shouldOptimize() &&
         "Expected optimizable function definition!");

  constexpr int MaxDeriveLevels = 10;

  int NewLevel = 1;
  if (DerivedFrom) {
    // When SILVerifyAll is enabled, individual functions are verified after
    // function passes are run upon them. This means that any functions created
    // by a function pass will not be verified after the pass runs. Thus
    // specialization errors that cause the verifier to trip will be
    // misattributed to the first pass that makes a change to the specialized
    // function. This is very misleading and increases triage time.
    //
    // As a result, when SILVerifyAll is enabled, we always verify newly
    // specialized functions as they are added to the worklist.
    //
    // TODO: Currently, all specialized functions are added to the function
    // worklist in this manner. This is all well and good, but we should really
    // add support for verifying that all specialized functions are added via
    // this function to the pass manager to ensure that we perform this
    // verification.
    if (getOptions().VerifyAll) {
      F->verify();
    }

    NewLevel = DerivationLevels[DerivedFrom] + 1;
    // Limit the number of derivations, i.e. don't allow that a pass specializes
    // a specialized function which is itself a specialized function, and so on.
    if (NewLevel >= MaxDeriveLevels)
      return;
  }
  int &StoredLevel = DerivationLevels[F];

  // Only allow a function to be pushed on the worklist a single time
  // (not counting the initial population of the worklist with the bottom-up
  // function order).
  if (StoredLevel > 0)
    return;

  StoredLevel = NewLevel;
  FunctionWorklist.push_back(F);
}

void SILPassManager::restartWithCurrentFunction(SILTransform *T) {
  assert(isa<SILFunctionTransform>(T) &&
         "Can only restart the pipeline from function passes");
  RestartPipeline = true;
}

/// \brief Reset the state of the pass manager and remove all transformation
/// owned by the pass manager. Analysis passes will be kept.
void SILPassManager::resetAndRemoveTransformations() {
  for (auto *T : Transformations)
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

void SILPassManager::addPass(PassKind Kind) {
  assert(unsigned(PassKind::AllPasses_Last) >= unsigned(Kind) &&
         "Invalid pass kind");
  switch (Kind) {
#define PASS(ID, NAME, DESCRIPTION)                                            \
  case PassKind::ID: {                                                         \
    SILTransform *T = swift::create##ID();                                     \
    T->setPassKind(PassKind::ID);                                              \
    Transformations.push_back(T);                                              \
    break;                                                                     \
  }
#define IRGEN_PASS(ID, NAME, DESCRIPTION)                                      \
  case PassKind::ID: {                                                         \
    SILTransform *T = IRGenPasses[unsigned(Kind)];                             \
    assert(T && "Missing IRGen pass?");                                        \
    T->setPassKind(PassKind::ID);                                              \
    Transformations.push_back(T);                                              \
    IRGenPasses.erase(unsigned(Kind));                                         \
    break;                                                                     \
  }
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

} // end anonymous namespace

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
    typedef CallGraph::child_iterator ChildIteratorType;
    typedef CallGraph::Node *NodeRef;

    static NodeRef getEntryNode(NodeRef N) { return N; }
    static inline ChildIteratorType child_begin(NodeRef N) {
      return N->Children.begin();
    }
    static inline ChildIteratorType child_end(NodeRef N) {
      return N->Children.end();
    }
  };

  template <> struct GraphTraits<CallGraph *>
  : public GraphTraits<CallGraph::Node *> {
    typedef CallGraph *GraphType;
    typedef CallGraph::Node *NodeRef;

    static NodeRef getEntryNode(GraphType F) { return nullptr; }

    typedef pointer_iterator<CallGraph::iterator> nodes_iterator;
    static nodes_iterator nodes_begin(GraphType CG) {
      return nodes_iterator(CG->Nodes.begin());
    }
    static nodes_iterator nodes_end(GraphType CG) {
      return nodes_iterator(CG->Nodes.end());
    }
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
} // namespace llvm
#endif

void SILPassManager::viewCallGraph() {
  /// When asserts are disabled, this should be a NoOp.
#ifndef NDEBUG
  CallGraph OCG(getModule(), getAnalysis<BasicCalleeAnalysis>());
  llvm::ViewGraph(&OCG, "callgraph");
#endif
}
