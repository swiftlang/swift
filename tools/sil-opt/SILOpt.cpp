//===-- SILOpt.cpp - SIL Optimization Driver ------------------------------===//
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
//
// This is a tool for reading sil files and running sil passes on them. The
// targeted usecase is debugging and testing SIL passes.
//
//===----------------------------------------------------------------------===//

#include "swift/Subsystems.h"
#include "swift/AST/SILOptions.h"
#include "swift/Frontend/DiagnosticVerifier.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/SILPasses/Passes.h"
#include "swift/SILPasses/PassManager.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/Serialization/SerializedSILLoader.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Signals.h"
using namespace swift;

namespace {

enum class PassKind {
  AllocBoxToStack,
  CapturePromotion,
  CCP,
  CSE,
  DefiniteInit,
  DCE,
  DataflowDiagnostics,
  InOutDeshadowing,
  MandatoryInlining,
  PredictableMemoryOpt,
  SILCleanup,
  SILMem2Reg,
  SILCombine,
  SILDeadFunctionElimination,
  SILSpecialization,
  SILDevirt,
  SimplifyCFG,
  PerformanceInlining,
  CodeMotion,
  LowerAggregateInstrs,
  SROA,
  ARCOpts,
  StripDebugInfo,
  DeadObjectElimination,
  InstCount,
  AADumper,
  LoadStoreOpts,
};

} // end anonymous namespace

static llvm::cl::opt<std::string>
InputFilename(llvm::cl::desc("input file"), llvm::cl::init("-"),
              llvm::cl::Positional);

static llvm::cl::opt<std::string>
OutputFilename("o", llvm::cl::desc("output filename"), llvm::cl::init("-"));

static llvm::cl::list<std::string>
ImportPaths("I", llvm::cl::desc("add a directory to the import search path"));

static llvm::cl::opt<std::string>
ModuleName("module-name", llvm::cl::desc("The name of the module if processing"
                                         " a module. Necessary for processing "
                                         "stdin."));

static llvm::cl::list<PassKind>
Passes(llvm::cl::desc("Passes:"),
       llvm::cl::values(clEnumValN(PassKind::AllocBoxToStack,
                                   "allocbox-to-stack", "Promote memory"),
                        clEnumValN(PassKind::CapturePromotion,
                                   "capture-promotion",
                                   "Promote closure capture variables"),
                        clEnumValN(PassKind::SILMem2Reg,
                                   "mem2reg",
                                   "Promote stack allocations to registers"),
                        clEnumValN(PassKind::SILCleanup,
                                   "cleanup",
                                   "Cleanup SIL in preparation for IRGen"),
                        clEnumValN(PassKind::CCP,
                                   "constant-propagation",
                                   "Propagate constants"),
                        clEnumValN(PassKind::CSE,
                                   "cse",
                                   "Perform constant subexpression elimination."),
                        clEnumValN(PassKind::DataflowDiagnostics,
                                   "dataflow-diagnostics",
                                   "Emit SIL diagnostics"),
                        clEnumValN(PassKind::DCE,
                                   "dead-code-elimination", "Remove dead code"),
                        clEnumValN(PassKind::DefiniteInit,
                                   "definite-init","definitive initialization"),
                        clEnumValN(PassKind::InOutDeshadowing,
                                   "inout-deshadow",
                                   "Remove inout argument shadow variables"),
                        clEnumValN(PassKind::MandatoryInlining,
                                   "mandatory-inlining",
                                   "Inline transparent functions"),
                        clEnumValN(PassKind::SILSpecialization,
                                   "specialize",
                                   "Specialize generic functions"),
                        clEnumValN(PassKind::SILDevirt,
                                   "devirtualize",
                                   "Devirtualize virtual calls"),
                        clEnumValN(PassKind::PredictableMemoryOpt,
                                   "predictable-memopt",
                                   "Predictable early memory optimization"),
                        clEnumValN(PassKind::SILCombine,
                                   "sil-combine",
                                   "Perform small peepholes and combine"
                                   " operations."),
                        clEnumValN(PassKind::SILDeadFunctionElimination,
                                   "sil-deadfuncelim",
                                   "Remove private unused functions"),
                        clEnumValN(PassKind::SimplifyCFG,
                                   "simplify-cfg",
                                   "Clean up the CFG of SIL functions"),
                        clEnumValN(PassKind::PerformanceInlining,
                                   "inline",
                                   "Inline functions which are determined to be"
                                   " less than a pre-set cost."),
                        clEnumValN(PassKind::CodeMotion,
                                   "codemotion",
                                   "Perform code motion optimizations"),
                        clEnumValN(PassKind::LowerAggregateInstrs,
                                   "lower-aggregate-instrs",
                                   "Perform lower aggregate instrs to scalar "
                                   "instrs."),
                        clEnumValN(PassKind::SROA,
                                   "sroa",
                                   "Perform SIL scalar replacement of "
                                   "aggregates."),
                        clEnumValN(PassKind::ARCOpts,
                                   "arc-opts",
                                   "Perform automatic reference counting "
                                   "optimizations."),
                        clEnumValN(PassKind::StripDebugInfo,
                                   "strip-debug-info",
                                   "Strip debug info."),
                        clEnumValN(PassKind::DeadObjectElimination,
                                   "deadobject-elim",
                                   "Eliminate unused object allocation with no "
                                   "side effect destructors."),
                        clEnumValN(PassKind::InstCount,
                                   "inst-count",
                                   "Count all instructions in the given "
                                   "module."),
                        clEnumValN(PassKind::AADumper,
                                   "aa-dump",
                                   "Dump AA result for all pairs of ValueKinds"
                                   " in all functions."),
                        clEnumValN(PassKind::LoadStoreOpts,
                                   "load-store-opts",
                                   "Remove duplicate loads, dead stores, and "
                                   "perform load forwarding."),
                        clEnumValEnd));

static llvm::cl::opt<bool>
PrintStats("print-stats", llvm::cl::desc("Print various statistics"));

static llvm::cl::opt<bool>
VerifyMode("verify",
           llvm::cl::desc("verify diagnostics against expected-"
                          "{error|warning|note} annotations"));

static llvm::cl::opt<unsigned>
SILInlineThreshold("sil-inline-threshold", llvm::cl::Hidden,
                   llvm::cl::init(50));

static llvm::cl::opt<unsigned>
SILDevirtThreshold("sil-devirt-threshold", llvm::cl::Hidden,
                   llvm::cl::init(0));

static llvm::cl::opt<bool>
EnableSILVerifyAll("enable-sil-verify-all",
                   llvm::cl::Hidden,
                   llvm::cl::init(true),
                   llvm::cl::desc("Run sil verifications after every pass."));

static llvm::cl::opt<bool>
EnableSILPrintAll("-sil-print-all",
                  llvm::cl::Hidden,
                  llvm::cl::init(false),
                  llvm::cl::desc("Print sil after every pass."));

static llvm::cl::opt<bool>
EmitVerboseSIL("emit-verbose-sil",
               llvm::cl::desc("Emit locations during sil emission."));

// This function isn't referenced outside its translation unit, but it
// can't use the "static" keyword because its address is used for
// getMainExecutable (since some platforms don't support taking the
// address of main, and some platforms can't implement getMainExecutable
// without being given the address of a function in the main executable).
void anchorForGetMainExecutable() {}

int main(int argc, char **argv) {
  // Print a stack trace if we signal out.
  llvm::sys::PrintStackTraceOnErrorSignal();
  llvm::PrettyStackTraceProgram X(argc, argv);

  llvm::cl::ParseCommandLineOptions(argc, argv, "Swift SIL optimizer\n");

  // Call llvm_shutdown() on exit to print stats and free memory.
  llvm::llvm_shutdown_obj Y;

  if (PrintStats)
    llvm::EnableStatistics();

  CompilerInvocation Invocation;

  Invocation.setMainExecutablePath(
      llvm::sys::fs::getMainExecutable(argv[0],
          reinterpret_cast<void *>(&anchorForGetMainExecutable)));

  // Give the context the list of search paths to use for modules.
  Invocation.setImportSearchPaths(ImportPaths);

  // Load the input file.
  llvm::OwningPtr<llvm::MemoryBuffer> InputFile;
  if (llvm::MemoryBuffer::getFileOrSTDIN(InputFilename, InputFile)) {
    fprintf(stderr, "Error! Failed to open file: %s\n", InputFilename.c_str());
    exit(-1);
  }

  // If it looks like we have an AST, set the source file kind to SIL and the
  // name of the module to the file's name.
  Invocation.addInputBuffer(InputFile.get());
  bool IsModule = false;
  if (SerializedModuleLoader::isSerializedAST(InputFile.get()->getBuffer())) {
    IsModule = true;
    const StringRef Stem = ModuleName.size() ?
                             StringRef(ModuleName) :
                             llvm::sys::path::stem(InputFilename);
    Invocation.setModuleName(Stem);
    Invocation.setInputKind(SourceFileKind::Library);
  } else {
    Invocation.setModuleName("main");
    Invocation.setInputKind(SourceFileKind::SIL);
  }

  CompilerInstance CI;
  PrintingDiagnosticConsumer PrintDiags;
  CI.addDiagnosticConsumer(&PrintDiags);

  if (CI.setup(Invocation))
    return 1;
  CI.performParse();

  // If parsing produced an error, don't run any passes.
  if (CI.getASTContext().hadError())
    return 1;

  // Load the SIL if we have a module. We have to do this after SILParse
  // creating the unfortunate double if statement.
  if (IsModule) {
    assert(!CI.hasSILModule() &&
           "performParse() should not create a SILModule.");
    CI.setSILModule(SILModule::createEmptyModule(CI.getMainModule()));
    SerializedSILLoader *SL = SerializedSILLoader::create(CI.getASTContext(),
                                                          CI.getSILModule(),
                                                          nullptr);
    SL->getAll();
    delete SL;
  }

  // If we're in verify mode, install a custom diagnostic handling for
  // SourceMgr.
  if (VerifyMode)
    enableDiagnosticVerifier(CI.getSourceMgr());

  SILOptions &SILOpts = Invocation.getSILOptions();
  SILOpts.InlineThreshold = SILInlineThreshold;
  SILOpts.DevirtThreshold = SILDevirtThreshold;
  SILOpts.VerifyAll = EnableSILVerifyAll;
  SILOpts.PrintAll = EnableSILPrintAll;

  SILPassManager PM(CI.getSILModule(), SILOpts);
  PM.registerAnalysis(createCallGraphAnalysis(CI.getSILModule()));
  PM.registerAnalysis(createAliasAnalysis(CI.getSILModule()));
  PM.registerAnalysis(createDominanceAnalysis(CI.getSILModule()));
  PM.registerAnalysis(createSpecializedArgsAnalysis(CI.getSILModule()));
  for (auto Pass : Passes) {
    switch (Pass) {
    case PassKind::AllocBoxToStack:
      PM.add(createAllocBoxToStack());
      break;
    case PassKind::CapturePromotion:
      PM.add(createCapturePromotion());
      break;
    case PassKind::CCP:
      PM.add(createConstantPropagation());
      break;
    case PassKind::CSE:
      PM.add(createCSE());
      break;
    case PassKind::DCE:
      PM.add(createDCE());
      break;
    case PassKind::DefiniteInit:
      PM.add(createDefiniteInitialization());
      break;
    case PassKind::DataflowDiagnostics:
      PM.add(createEmitDFDiagnostics());
      break;
    case PassKind::InOutDeshadowing:
      PM.add(createInOutDeshadowing());
      break;
    case PassKind::MandatoryInlining:
      PM.add(createMandatoryInlining());
      break;
    case PassKind::PredictableMemoryOpt:
      PM.add(createPredictableMemoryOptimizations());
      break;
    case PassKind::SILCleanup:
      PM.add(createSILCleanup());
      break;
    case PassKind::SILMem2Reg:
      PM.add(createMem2Reg());
      break;
    case PassKind::SILCombine:
      PM.add(createSILCombine());
      break;
    case PassKind::SILDeadFunctionElimination:
      PM.add(createDeadFunctionElimination());
      break;
    case PassKind::SILSpecialization:
      PM.add(createGenericSpecializer());
      break;
    case PassKind::SILDevirt:
      PM.add(createDevirtualization());
      break;
    case PassKind::SimplifyCFG:
      PM.add(createSimplifyCFG());
      break;
    case PassKind::PerformanceInlining:
      PM.add(createPerfInliner());
      break;
    case PassKind::CodeMotion:
      PM.add(createCodeMotion());
      break;
    case PassKind::LowerAggregateInstrs:
      PM.add(createLowerAggregate());
      break;
    case PassKind::SROA:
      PM.add(createSROA());
      break;
    case PassKind::ARCOpts:
      PM.add(createARCOpts());
      break;
    case PassKind::StripDebugInfo:
      PM.add(createStripDebug());
      break;
    case PassKind::DeadObjectElimination:
      PM.add(createDeadObjectElimination());
      break;
    case PassKind::InstCount:
      PM.add(createSILInstCount());
      break;
    case PassKind::AADumper:
      PM.add(createSILAADumper());
      break;
    case PassKind::LoadStoreOpts:
      PM.add(createLoadStoreOpts());
      break;
    }
  }

  PM.run();

  std::string ErrorInfo;
  llvm::raw_fd_ostream OS(OutputFilename.c_str(), ErrorInfo);
  if (!ErrorInfo.empty()) {
    llvm::errs() << "while opening '" << OutputFilename << "': "
                 << ErrorInfo << '\n';
    return 1;
  }
  CI.getSILModule()->print(OS, EmitVerboseSIL, CI.getMainModule());

  bool HadError = CI.getASTContext().hadError();

  // If we're in -verify mode, we've buffered up all of the generated
  // diagnostics.  Check now to ensure that they meet our expectations.
  if (VerifyMode)
    HadError = verifyDiagnostics(CI.getSourceMgr(), CI.getInputBufferIDs());


  return HadError;
}
