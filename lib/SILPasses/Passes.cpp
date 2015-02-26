//===-------- Passes.cpp - Swift Compiler SIL Pass Entrypoints ------------===//
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
///
///  \file
///  \brief This file provides implementations of a few helper functions
///  which provide abstracted entrypoints to the SILPasses stage.
///
///  \note The actual SIL passes should be implemented in per-pass source files,
///  not in this file.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-optimizer"

#include "swift/SILPasses/Passes.h"
#include "swift/SILPasses/PassManager.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILAnalysis/Analysis.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Module.h"
#include "swift/SIL/SILModule.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorOr.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/YAMLParser.h"

llvm::cl::opt<bool>
    SILViewCFG("sil-view-cfg", llvm::cl::init(false),
               llvm::cl::desc("Enable the sil cfg viewer pass"));

using namespace swift;

// Enumerates the optimization kinds that we do in SIL.
enum OptimizationLevelKind {
  LowLevel,
  MidLevel,
  HighLevel,
};

static void registerAnalysisPasses(SILPassManager &PM) {
  SILModule *Mod = PM.getModule();
  PM.registerAnalysis(createCallGraphAnalysis(Mod));
  PM.registerAnalysis(createAliasAnalysis(Mod));
  PM.registerAnalysis(createDominanceAnalysis(Mod));
  PM.registerAnalysis(createLoopInfoAnalysis(Mod, &PM));
  PM.registerAnalysis(createInductionVariableAnalysis(Mod));
  PM.registerAnalysis(createPostOrderAnalysis(Mod));
  PM.registerAnalysis(createClassHierarchyAnalysis(Mod));
  PM.registerAnalysis(createRCIdentityAnalysis(Mod, &PM));
  PM.registerAnalysis(createDestructorAnalysis(Mod));
}

bool swift::runSILDiagnosticPasses(SILModule &Module) {
  // If we parsed a .sil file that is already in canonical form, don't rerun
  // the diagnostic passes.
  if (Module.getStage() == SILStage::Canonical)
    return false;

  auto &Ctx = Module.getASTContext();

  SILPassManager PM(&Module);
  registerAnalysisPasses(PM);
  // If we are asked do debug serialization, instead of running all diagnostic
  // passes, just run mandatory inlining with dead transparent function cleanup
  // disabled.
  if (Module.getOptions().DebugSerialization) {
    PM.add(createMandatoryInlining());
    PM.run();
    return Ctx.hadError();
  }

  // Otherwise run the rest of diagnostics.
  PM.add(createCapturePromotion());
  PM.add(createAllocBoxToStack());
  PM.add(createInOutDeshadowing());
  PM.add(createNoReturnFolding());
  PM.add(createDefiniteInitialization());

  PM.add(createMandatoryInlining());
  PM.add(createPredictableMemoryOptimizations());
  PM.add(createDiagnosticConstantPropagation());
  PM.add(createDiagnoseUnreachable());
  PM.add(createEmitDFDiagnostics());
  // Canonical swift requires all non cond_br critical edges to be split.
  PM.add(createSplitNonCondBrCriticalEdges());
  PM.run();

  // Generate diagnostics.
  Module.setStage(SILStage::Canonical);

  // If errors were produced during SIL analysis, return true.
  return Ctx.hadError();
}

void AddSimplifyCFGSILCombine(SILPassManager &PM) {
  PM.add(createSimplifyCFG());
  // Jump threading can expose opportunity for silcombine (enum -> is_enum_tag->
  // cond_br).
  PM.add(createSILCombine());
  // Which can expose opportunity for simplifcfg.
  PM.add(createSimplifyCFG());
}

/// Perform semantic annotation/loop base optimizations.
void AddHighLevelLoopOptPasses(SILPassManager &PM) {
  // Perform classsic SSA optimizations for cleanup.
  PM.add(createLowerAggregateInstrs());
  PM.add(createSILCombine());
  PM.add(createSROA());
  PM.add(createMem2Reg());
  PM.add(createDCE());
  PM.add(createSILCombine());
  AddSimplifyCFGSILCombine(PM);

  // Run high-level loop opts.
  PM.add(createLoopRotate());

  // Cleanup.
  PM.add(createDCE());
  PM.add(createCSE());
  PM.add(createSILCombine());
  PM.add(createSimplifyCFG());
  PM.add(createLICM());
  PM.add(createRemovePins());
  PM.add(createABCOpt());
  // Cleanup.
  PM.add(createDCE());
  PM.add(createCOWArrayOpts());
  // Cleanup.
  PM.add(createDCE());
  PM.add(createSwiftArrayOpts());
}

void AddLowLevelLoopOptPasses(SILPassManager &PM) {
  PM.add(createMem2Reg());
  PM.add(createLICM());
  PM.add(createDCE());
  PM.add(createCSE());
  PM.add(createSILCombine());
  PM.add(createSimplifyCFG());
}

void AddSSAPasses(SILPassManager &PM, OptimizationLevelKind OpLevel) {
  AddSimplifyCFGSILCombine(PM);
  PM.add(createAllocBoxToStack());
  PM.add(createCopyForwarding());
  PM.add(createLowerAggregateInstrs());
  PM.add(createSILCombine());
  PM.add(createSROA());
  PM.add(createMem2Reg());

  // Perform classsic SSA optimizations.
  PM.add(createPerformanceConstantPropagation());
  PM.add(createDCE());
  PM.add(createCSE());
  PM.add(createSILCombine());
  AddSimplifyCFGSILCombine(PM);

  // Perform retain/release code motion and run the first ARC optimizer.
  PM.add(createGlobalLoadStoreOpts());
  PM.add(createCodeMotion(false /* HoistReleases */));
  PM.add(createGlobalARCOpts());

  // Devirtualize.
  PM.add(createDevirtualizer());
  PM.add(createGenericSpecializer());
  PM.add(createSILLinker());

  switch (OpLevel) {
    case OptimizationLevelKind::HighLevel:
      // Does not inline functions with defined semantics.
      PM.add(createEarlyInliner());
      break;
    case OptimizationLevelKind::MidLevel:
      // Does inline semantics-functions, but not global-init functions.
      PM.add(createPerfInliner());
      break;
    case OptimizationLevelKind::LowLevel:
      // Inlines everything
      PM.add(createLateInliner());
      break;
  }
  PM.add(createSimplifyCFG());
  // Only hoist releases very late.
  PM.add(createCodeMotion(OpLevel ==
                          OptimizationLevelKind::LowLevel /* HoistReleases */));
  PM.add(createGlobalARCOpts());
  PM.add(createRemovePins());
}


void swift::runSILOptimizationPasses(SILModule &Module) {
  if (Module.getOptions().DebugSerialization) {
    SILPassManager PM(&Module);
    registerAnalysisPasses(PM);
    PM.add(createSILLinker());
    PM.run();
    return;
  }

  SILPassManager PM(&Module, "PreSpecialize");
  registerAnalysisPasses(PM);

  // Start by specializing generics and by cloning functions from stdlib.
  PM.add(createSILLinker());
  PM.add(createGenericSpecializer());
  PM.run();
  PM.resetAndRemoveTransformations();

  // Run two iterations of the high-level SSA passes.
  PM.setStageName("HighLevel");
  AddSSAPasses(PM, OptimizationLevelKind::HighLevel);
  PM.runOneIteration();
  PM.runOneIteration();
  PM.resetAndRemoveTransformations();


  PM.setStageName("EarlyLoopOpt");
  AddHighLevelLoopOptPasses(PM);
  PM.runOneIteration();
  PM.resetAndRemoveTransformations();


  // Run two iterations of the mid-level SSA passes.
  PM.setStageName("MidLevel");
  AddSSAPasses(PM, OptimizationLevelKind::MidLevel);
  PM.runOneIteration();
  PM.runOneIteration();
  PM.resetAndRemoveTransformations();

  // Perform lowering optimizations.
  PM.setStageName("Lower");
  PM.add(createDeadFunctionElimination());
  PM.add(createDeadObjectElimination());

  // Hoist globals out of loops.
  // Global-init functions should not be inlined GlobalOpt is done.
  PM.add(createGlobalOpt());

  // Propagate constants into closures and convert to static dispatch.  This
  // should run after specialization and inlining because we don't want to
  // specialize a call that can be inlined. It should run before
  // ClosureSpecialization, because constant propagation is more effective.  At
  // least one round of SSA optimization and inlining should run after this to
  // take advantage of static dispatch.
  PM.add(createCapturePropagation());

  // Specialize closure.
  PM.add(createClosureSpecializer());

  // Insert inline caches for virtual calls.
  PM.add(createDevirtualizer());
  PM.add(createInlineCaches());

  // Optimize function signatures if we are asked to.
  //
  // We do this late since it is a pass like the inline caches that we only want
  // to run once very late. Make sure to run at least one round of the ARC
  // optimizer after this.
  if (Module.getOptions().EnableFuncSigOpts)
    PM.add(createFunctionSignatureOpts());

  PM.run();
  PM.resetAndRemoveTransformations();

  // Run another iteration of the SSA optimizations to optimize the
  // devirtualized inline caches and constants propagated into closures
  // (CapturePropagation).

  PM.setStageName("LowLevel");
  AddSSAPasses(PM, OptimizationLevelKind::LowLevel);
  PM.runOneIteration();
  PM.resetAndRemoveTransformations();

  PM.setStageName("LateLoopOpt");
  AddLowLevelLoopOptPasses(PM);
  PM.add(createExternalFunctionDefinitionsElimination());
  PM.add(createDeadFunctionElimination());
  PM.runOneIteration();

  // Call the CFG viewer.
  if (SILViewCFG) {
    PM.resetAndRemoveTransformations();
    PM.add(createCFGPrinter());
    PM.runOneIteration();
  }

  DEBUG(Module.verify());
}

SILTransform *swift::createPass(PassKind Kind) {
  assert(unsigned(PassKind::AllPasses_Last) >= unsigned(Kind) &&
         "Invalid pass kind");
  switch (Kind) {
#define PASS(Id, ...)                           \
  case PassKind::Id:                            \
    return create##Id();                        \
    break;
#include "swift/SILPasses/Passes.def"
  }
}

#ifndef NDEBUG

static SILTransform *createPassForName(StringRef Name) {
  auto P = llvm::StringSwitch<PassKind>(Name)
#define PASS(Id, ...) .Case(#Id, PassKind::Id)
#include "swift/SILPasses/Passes.def"
  ;
  return createPass(P);
}

namespace {

struct PMDescriptor {
  llvm::SmallString<32> Id;
  llvm::SmallString<32> ActionName;
  Optional<unsigned> ActionCount;
  std::vector<llvm::SmallString<32>> Passes;

  explicit PMDescriptor(llvm::yaml::SequenceNode *Descriptor);
  ~PMDescriptor() = default;
  PMDescriptor(const PMDescriptor &) = delete;
  PMDescriptor(PMDescriptor &&) = default;

  static void
  descriptorsForFile(StringRef Filename,
                     llvm::SmallVectorImpl<PMDescriptor> &Descriptors);
};

} // end anonymous namespace

void
PMDescriptor::
descriptorsForFile(StringRef Filename,
                   llvm::SmallVectorImpl<PMDescriptor> &Descriptors) {
  namespace yaml = llvm::yaml;

  // Load the input file.
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileBufOrErr =
    llvm::MemoryBuffer::getFileOrSTDIN(Filename);
  if (!FileBufOrErr) {
    llvm_unreachable("Failed to read yaml file");
  }

  StringRef Buffer = FileBufOrErr->get()->getBuffer();
  llvm::SourceMgr SM;
  yaml::Stream Stream(Buffer, SM);
  yaml::document_iterator DI = Stream.begin();
  assert(DI != Stream.end() && "Failed to read a document");
  yaml::Node *N = DI->getRoot();
  assert(N && "Failed to find a root");

  auto *RootList = cast<yaml::SequenceNode>(N);

  for (auto &PMDescriptorIter : make_range(RootList->begin(), RootList->end())) {
    PMDescriptor PM(cast<yaml::SequenceNode>(&PMDescriptorIter));
    Descriptors.push_back(std::move(PM));
  }
}

/// Our general format is as follows:
///
///   [
///     [
///       "PASS_MANAGER_ID",
///       "run_n_times"|"run_to_fixed_point",
///       count,
///       "PASS1", "PASS2", ...
///     ],
///     ...
///   ]
///
/// Where "id" is printed out when we process the action, "action" can be one of
/// "run_n_times", "run_to_fixed_point" and "passes" is a list of passes to
/// run. The names to use are the stringified versions of pass kinds.
PMDescriptor::PMDescriptor(llvm::yaml::SequenceNode *Desc) {
  namespace yaml = llvm::yaml;

  yaml::SequenceNode::iterator DescIter = Desc->begin();
  Id = cast<yaml::ScalarNode>(&*DescIter)->getRawValue();
  ++DescIter;

  ActionName = cast<yaml::ScalarNode>(&*DescIter)->getRawValue();
  unsigned ActionSize = ActionName.size()-2;
  ActionName = ActionName.substr(1, ActionSize);
  ++DescIter;

  auto *ActionCountValue = cast<yaml::ScalarNode>(&*DescIter);
  APInt APCount(64, ActionCountValue->getRawValue(), 10);
  ActionCount = APCount.getLimitedValue(UINT_MAX);
  ++DescIter;

  for (auto DescEnd = Desc->end(); DescIter != DescEnd; ++DescIter) {
    StringRef PassName = cast<yaml::ScalarNode>(&*DescIter)->getRawValue();
    unsigned Size = PassName.size()-2;
    Passes.push_back(PassName.substr(1, Size));
  }
}

#endif

void swift::runSILOptimizationPassesWithFileSpecification(SILModule &Module,
                                                          StringRef FileName) {
#ifndef NDEBUG
  llvm::SmallVector<PMDescriptor, 4> Descriptors;
  PMDescriptor::descriptorsForFile(FileName, Descriptors);

  for (auto &Desc : Descriptors) {
    DEBUG(llvm::dbgs() << "Creating PM: " << Desc.Id << "\n");
    SILPassManager PM(&Module, Desc.Id);
    registerAnalysisPasses(PM);

    for (auto &P : Desc.Passes) {
      DEBUG(llvm::dbgs() << "  Adding Pass: " << P << "\n");
      PM.add(createPassForName(P));
    }

    if (Desc.ActionName.equals("run_n_times")) {
      unsigned Count = Desc.ActionCount.getValue();
      DEBUG(llvm::dbgs() << "    Running " << Count << " iterations...\n");
      for (unsigned i = 0, e = Count; i < e; ++i) {
        PM.runOneIteration();
      }
    } else if (Desc.ActionName.equals("run_to_fixed_point")) {
      DEBUG(llvm::dbgs() << "    Running until fixed point...\n");
      PM.run();
    } else {
      llvm_unreachable("unknown action");
    }
  }
#endif
}
