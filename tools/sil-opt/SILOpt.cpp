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
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Signals.h"
using namespace swift;

enum class PassKind {
  AllocBoxPromotion,
  StackToSSA,
  DataflowDiagnostics
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
       llvm::cl::values(clEnumValN(PassKind::AllocBoxPromotion,
                                   "allocbox-promotion", "Promote memory"),
                        clEnumValN(PassKind::StackToSSA,
                                   "stacktossa", "alloc_stack to SSA"),
                        clEnumValN(PassKind::DataflowDiagnostics,
                                   "dataflow-diagnostics",
                                   "Emit SIL diagnostics"),
                        clEnumValEnd));

static llvm::cl::opt<bool>
PrintStats("print-stats", llvm::cl::desc("Print various statistics"));

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

  if (PrintStats)
    llvm::EnableStatistics();

  llvm::IntrusiveRefCntPtr<CompilerInvocation> Invocation(
      new CompilerInvocation());

  Invocation->setMainExecutablePath(
      llvm::sys::fs::getMainExecutable(argv[0],
          reinterpret_cast<void *>(&anchorForGetMainExecutable)));

  // Give the context the list of search paths to use for modules.
  Invocation->setImportSearchPaths(ImportPaths);

  PrintingDiagnosticConsumer PrintDiags;
  Invocation->addDiagnosticConsumer(&PrintDiags);
  Invocation->setModuleName("main");
  Invocation->setTUKind(TranslationUnit::SIL);
  CompilerInstance CI(Invocation);

  // Open the input file.
  llvm::OwningPtr<llvm::MemoryBuffer> InputFile;
  if (llvm::error_code Err =
        llvm::MemoryBuffer::getFileOrSTDIN(InputFilename, InputFile)) {
    llvm::errs() << "swift: error opening input file: " << Err.message()
                 << '\n';
    return 1;
  }

  // Transfer ownership of the MemoryBuffer to the SourceMgr.
  CI.addBufferID(CI.getSourceMgr().AddNewSourceBuffer(InputFile.take(),
                                                      llvm::SMLoc()));
  CI.setup();
  CI.doIt();

  for (auto Pass : Passes) {
    switch (Pass) {
    case PassKind::AllocBoxPromotion:
      performSILAllocBoxPromotion(CI.getSILModule());
      break;
    case PassKind::StackToSSA:
      performSILStackToSSA(CI.getSILModule());
      break;
    case PassKind::DataflowDiagnostics:
      emitSILDataflowDiagnostics(CI.getSILModule());
      break;
    }
  }

  std::string ErrorInfo;
  llvm::raw_fd_ostream OS(OutputFilename.c_str(), ErrorInfo);
  if (!ErrorInfo.empty()) {
    llvm::errs() << "while opening '" << OutputFilename << "': "
                 << ErrorInfo << '\n';
    return 1;
  }
  CI.getSILModule()->print(OS);

  // Print statistics.
  if (PrintStats)
    llvm::PrintStatistics(llvm::errs());

  return CI.getASTContext().hadError();
}
