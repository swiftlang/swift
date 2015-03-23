//===-- driver.cpp - Swift Compiler Driver --------------------------------===//
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
// This is the entry point to the swift compiler driver.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/DiagnosticEngine.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Driver/Compilation.h"
#include "swift/Driver/Driver.h"
#include "swift/Driver/FrontendUtil.h"
#include "swift/Driver/Job.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Process.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"

#include <memory>
#include <stdlib.h>

using namespace swift;
using namespace swift::driver;

std::string getExecutablePath(const char *FirstArg) {
  void *P = (void *)(intptr_t)getExecutablePath;
  return llvm::sys::fs::getMainExecutable(FirstArg, P);
}

extern int frontend_main(ArrayRef<const char *> Args, const char *Argv0,
                         void *MainAddr);

/// Run 'swift-update'.
extern int update_main(ArrayRef<const char *> Args, const char *Argv0,
                       void *MainAddr);

/// Run 'swift-autolink-extract'.
extern int autolink_extract_main(ArrayRef<const char *> Args, const char *Argv0,
                                 void *MainAddr);

int main(int argc_, const char **argv_) {
  // Print a stack trace if we signal out.
  llvm::sys::PrintStackTraceOnErrorSignal();
  llvm::PrettyStackTraceProgram X(argc_, argv_);

  // Set up an object which will call llvm::llvm_shutdown() on exit.
  llvm::llvm_shutdown_obj Y;

  llvm::SmallVector<const char *, 256> argv;
  llvm::SpecificBumpPtrAllocator<char> ArgAllocator;
  std::error_code EC = llvm::sys::Process::GetArgumentVector(
      argv, llvm::ArrayRef<const char *>(argv_, argc_), ArgAllocator);
  if (EC) {
    llvm::errs() << "error: couldn't get arguments: " << EC.message() << '\n';
    return 1;
  }

  // Handle integrated tools.
  if (argv.size() > 1){
    StringRef FirstArg(argv[1]);
    if (FirstArg == "-frontend") {
      return frontend_main(llvm::makeArrayRef(argv.data()+2,
                                              argv.data()+argv.size()),
                           argv[0], (void *)(intptr_t)getExecutablePath);
    }
  }

  std::string Path = getExecutablePath(argv[0]);

  PrintingDiagnosticConsumer PDC;

  SourceManager SM;
  DiagnosticEngine Diags(SM);
  Diags.addConsumer(PDC);

  Driver TheDriver(Path, llvm::sys::path::stem(argv[0]), argv, Diags);
  switch (TheDriver.getDriverKind()) {
  case Driver::DriverKind::UpdateCode:
    return update_main(TheDriver.getArgsWithoutProgramNameAndDriverMode(argv),
                       argv[0],
                       (void *)(intptr_t)getExecutablePath);
  case Driver::DriverKind::AutolinkExtract:
    return autolink_extract_main(
      TheDriver.getArgsWithoutProgramNameAndDriverMode(argv),
      argv[0], (void *)(intptr_t)getExecutablePath);
  default:
    break;
  }

  llvm::InitializeAllTargets();

  std::unique_ptr<Compilation> C = TheDriver.buildCompilation(argv);

  if (Diags.hadAnyError())
    return 1;

  if (C) {
    return C->performJobs();
  }

  return 0;
}
