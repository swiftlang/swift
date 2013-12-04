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
// This is the entry point.
//
//===----------------------------------------------------------------------===//

#include "swift/Subsystems.h"
#include "swift/Frontend/DiagnosticVerifier.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Signals.h"
using namespace swift;

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
  SILCombine,
  SimplifyCFG,
};

static llvm::cl::opt<std::string>
InputFilename(llvm::cl::desc("input file"), llvm::cl::init("-"),
              llvm::cl::Positional);

static llvm::cl::opt<std::string>
OutputFilename("o", llvm::cl::desc("output filename"), llvm::cl::init("-"));

static llvm::cl::list<std::string>
ImportPaths("I", llvm::cl::desc("add a directory to the import search path"));

static llvm::cl::list<PassKind>
Passes(llvm::cl::desc("Passes:"),
       llvm::cl::values(clEnumValN(PassKind::AllocBoxToStack,
                                   "allocbox-to-stack", "Promote memory"),
                        clEnumValN(PassKind::CapturePromotion,
                                   "capture-promotion",
                                   "Promote closure capture variables"),
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
                        clEnumValN(PassKind::PredictableMemoryOpt,
                                   "predictable-memopt",
                                   "Predictable early memory optimization"),
                        clEnumValN(PassKind::SILCombine,
                                   "sil-combine",
                                   "Perform small peepholes/combine operations/"
                                   "dead code elimination"),
                        clEnumValN(PassKind::SimplifyCFG,
                                   "simplify-cfg",
                                   "Clean up the CFG of SIL functions"),
                        clEnumValEnd));

static llvm::cl::opt<bool>
PrintStats("print-stats", llvm::cl::desc("Print various statistics"));

static llvm::cl::opt<bool>
VerifyMode("verify",
           llvm::cl::desc("verify diagnostics against expected-"
                          "{error|warning|note} annotations"));


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

  Invocation.setModuleName("main");
  Invocation.setInputKind(SourceFile::SIL);
  
  Invocation.addInputFilename(InputFilename);

  CompilerInstance CI;
  PrintingDiagnosticConsumer PrintDiags;
  CI.addDiagnosticConsumer(&PrintDiags);

  if (CI.setup(Invocation))
    return 1;
  CI.doIt();
  
  // If parsing produced an error, don't run any passes.
  if (CI.getASTContext().hadError())
    return 1;

  // If we're in verify mode, install a custom diagnostic handling for
  // SourceMgr.
  if (VerifyMode)
    enableDiagnosticVerifier(CI.getSourceMgr());

  for (auto Pass : Passes) {
    switch (Pass) {
    case PassKind::AllocBoxToStack:
      performSILAllocBoxToStackPromotion(CI.getSILModule());
      break;
    case PassKind::CapturePromotion:
      performSILCapturePromotion(CI.getSILModule());
      break;
    case PassKind::CCP:
      performSILConstantPropagation(CI.getSILModule());
      break;
    case PassKind::CSE:
      performSILCSE(CI.getSILModule());
      break;
    case PassKind::DCE:
      performSILDeadCodeElimination(CI.getSILModule());
      break;
    case PassKind::DefiniteInit:
      performSILDefiniteInitialization(CI.getSILModule());
      break;
    case PassKind::DataflowDiagnostics:
      emitSILDataflowDiagnostics(CI.getSILModule());
      break;
    case PassKind::InOutDeshadowing:
      performInOutDeshadowing(CI.getSILModule());
      break;
    case PassKind::MandatoryInlining:
      performSILMandatoryInlining(CI.getSILModule());
      break;
    case PassKind::PredictableMemoryOpt:
      performSILPredictableMemoryOptimizations(CI.getSILModule());
      break;
    case PassKind::SILCleanup:
      performSILCleanup(CI.getSILModule());
      break;
    case PassKind::SILCombine:
      performSILCombine(CI.getSILModule());
      break;
    case PassKind::SimplifyCFG:
      performSimplifyCFG(CI.getSILModule());
      break;
    }

    // Verify the module after every pass.
    CI.getSILModule()->verify();
  }

  std::string ErrorInfo;
  llvm::raw_fd_ostream OS(OutputFilename.c_str(), ErrorInfo);
  if (!ErrorInfo.empty()) {
    llvm::errs() << "while opening '" << OutputFilename << "': "
                 << ErrorInfo << '\n';
    return 1;
  }
  CI.getSILModule()->print(OS, EmitVerboseSIL);

  bool HadError = CI.getASTContext().hadError();

  // If we're in -verify mode, we've buffered up all of the generated
  // diagnostics.  Check now to ensure that they meet our expectations.
  if (VerifyMode)
    HadError = verifyDiagnostics(CI.getSourceMgr(), CI.getInputBufferIDs());


  return HadError;
}
