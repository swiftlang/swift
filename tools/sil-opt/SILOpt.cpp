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
// This is the entry point to the swift app.
//
//===----------------------------------------------------------------------===//

#include "swift/Subsystems.h"
#include "swift/IRGen/Options.h"
#include "swift/AST/AST.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Diagnostics.h"
#include "swift/Parse/Lexer.h"
#include "swift/SIL/SILModule.h"
#include "swift/Basic/DiagnosticConsumer.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "llvm/PassManager.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Assembly/PrintModulePass.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Target/TargetLibraryInfo.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/system_error.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/ADT/OwningPtr.h"
#include "llvm/ADT/Triple.h"
using namespace swift;

enum class PassKind {
  MemoryPromotion,
  DataflowDiagnostics
};

static llvm::cl::opt<bool>
UseMalloc("use-malloc",
          llvm::cl::desc("allocate internal data structures using malloc "
                         "(for memory debugging)"));

static llvm::cl::opt<std::string>
InputFilename(llvm::cl::desc("input file"), llvm::cl::init("-"));

static llvm::cl::opt<std::string>
OutputFilename("o", llvm::cl::desc("output filename"), llvm::cl::init("-"));

static llvm::cl::list<std::string>
ImportPaths("I", llvm::cl::desc("add a directory to the import search path"));

static llvm::cl::list<PassKind>
Passes(llvm::cl::desc("Passes:"),
       llvm::cl::values(clEnumValN(PassKind::MemoryPromotion,
                                   "memory-promotion", "Promote memory"),
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
    case PassKind::MemoryPromotion:
      // Perform "stable" optimizations that are invariant across compiler
      // version.
      performSILMemoryPromotion(CI.getSILModule());
      break;
    case PassKind::DataflowDiagnostics:
      // Generate diagnostics.
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
