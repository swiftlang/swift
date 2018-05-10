//===--- driver.cpp - Swift Compiler Driver -------------------------------===//
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
//
// This is the entry point to the swift compiler driver.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/DiagnosticEngine.h"
#include "swift/Basic/LLVMInitialize.h"
#include "swift/Basic/Program.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Driver/Compilation.h"
#include "swift/Driver/Driver.h"
#include "swift/Driver/FrontendUtil.h"
#include "swift/Driver/Job.h"
#include "swift/Driver/ToolChain.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/FrontendTool/FrontendTool.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Errno.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Process.h"
#include "llvm/Support/Program.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/StringSaver.h"
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

/// Run 'swift-autolink-extract'.
extern int autolink_extract_main(ArrayRef<const char *> Args, const char *Argv0,
                                 void *MainAddr);

extern int modulewrap_main(ArrayRef<const char *> Args, const char *Argv0,
                           void *MainAddr);

/// Run 'swift-format'
extern int swift_format_main(ArrayRef<const char *> Args, const char *Argv0,
                             void *MainAddr);

/// Determine if the given invocation should run as a subcommand.
///
/// \param ExecName The name of the argv[0] we were invoked as.
/// \param SubcommandName On success, the full name of the subcommand to invoke.
/// \param Args On return, the adjusted program arguments to use.
/// \returns True if running as a subcommand.
static bool shouldRunAsSubcommand(StringRef ExecName,
                                  SmallString<256> &SubcommandName,
                                  SmallVectorImpl<const char *> &Args) {
  assert(!Args.empty());

  // If we are not run as 'swift', don't do anything special. This doesn't work
  // with symlinks with alternate names, but we can't detect 'swift' vs 'swiftc'
  // if we try and resolve using the actual executable path.
  if (ExecName != "swift")
    return false;

  // If there are no program arguments, always invoke as normal.
  if (Args.size() == 1)
    return false;

  // Otherwise, we have a program argument. If it looks like an option or a
  // path, then invoke in interactive mode with the arguments as given.
  StringRef FirstArg(Args[1]);
  if (FirstArg.startswith("-") || FirstArg.find('.') != StringRef::npos ||
      FirstArg.find('/') != StringRef::npos)
    return false;

  // Otherwise, we should have some sort of subcommand. Get the subcommand name
  // and remove it from the program arguments.
  StringRef Subcommand = Args[1];
  Args.erase(&Args[1]);

  // If the subcommand is the "built-in" 'repl', then use the
  // normal driver.
  if (Subcommand == "repl")
    return false;

  // Form the subcommand name.
  SubcommandName.assign("swift-");
  SubcommandName.append(Subcommand);

  return true;
}

extern int apinotes_main(ArrayRef<const char *> Args);

int main(int argc_, const char **argv_) {
  PROGRAM_START(argc_, argv_);

  SmallVector<const char *, 256> argv;
  llvm::SpecificBumpPtrAllocator<char> ArgAllocator;
  std::error_code EC = llvm::sys::Process::GetArgumentVector(
      argv, llvm::ArrayRef<const char *>(argv_, argc_), ArgAllocator);
  if (EC) {
    llvm::errs() << "error: couldn't get arguments: " << EC.message() << '\n';
    return 1;
  }

  // Expand any response files in the command line argument vector - arguments
  // may be passed through response files in the event of command line length
  // restrictions.
  llvm::BumpPtrAllocator Allocator;
  llvm::StringSaver Saver(Allocator);
  llvm::cl::ExpandResponseFiles(
      Saver,
      llvm::Triple(llvm::sys::getProcessTriple()).isOSWindows() ?
      llvm::cl::TokenizeWindowsCommandLine :
      llvm::cl::TokenizeGNUCommandLine,
      argv);

  // Check if this invocation should execute a subcommand.
  StringRef ExecName = llvm::sys::path::stem(argv[0]);
  SmallString<256> SubcommandName;
  if (shouldRunAsSubcommand(ExecName, SubcommandName, argv)) {
    // We are running as a subcommand, try to find the subcommand adjacent to
    // the executable we are running as.
    SmallString<256> SubcommandPath(
      llvm::sys::path::parent_path(getExecutablePath(argv[0])));
    llvm::sys::path::append(SubcommandPath, SubcommandName);

    // If we didn't find the tool there, let the OS search for it.
    if (!llvm::sys::fs::exists(SubcommandPath)) {
      // Search for the program and use the path if found. If there was an
      // error, ignore it and just let the exec fail.
      auto result = llvm::sys::findProgramByName(SubcommandName);
      if (!result.getError())
        SubcommandPath = *result;
    }

    // Rewrite the program argument.
    argv[0] = SubcommandPath.c_str();
    
    // Execute the subcommand.
    argv.push_back(nullptr);
    ExecuteInPlace(SubcommandPath.c_str(), argv.data());

    // If we reach here then an error occurred (typically a missing path).
    std::string ErrorString = llvm::sys::StrError();
    llvm::errs() << "error: unable to invoke subcommand: " << argv[0]
                 << " (" << ErrorString << ")\n";
    return 2;
  }

  // Handle integrated tools.
  if (argv.size() > 1) {
    StringRef FirstArg(argv[1]);
    if (FirstArg == "-frontend") {
      return performFrontend(llvm::makeArrayRef(argv.data()+2,
                                                argv.data()+argv.size()),
                             argv[0], (void *)(intptr_t)getExecutablePath);
    }
    if (FirstArg == "-modulewrap") {
      return modulewrap_main(llvm::makeArrayRef(argv.data()+2,
                                                argv.data()+argv.size()),
                             argv[0], (void *)(intptr_t)getExecutablePath);
    }
    if (FirstArg == "-apinotes") {
      return apinotes_main(llvm::makeArrayRef(argv.data()+1,
                                              argv.data()+argv.size()));
    }
  }

  std::string Path = getExecutablePath(argv[0]);

  PrintingDiagnosticConsumer PDC;

  SourceManager SM;
  DiagnosticEngine Diags(SM);
  Diags.addConsumer(PDC);

  Driver TheDriver(Path, ExecName, argv, Diags);
  switch (TheDriver.getDriverKind()) {
  case Driver::DriverKind::AutolinkExtract:
    return autolink_extract_main(
      TheDriver.getArgsWithoutProgramNameAndDriverMode(argv),
      argv[0], (void *)(intptr_t)getExecutablePath);
  case Driver::DriverKind::SwiftFormat:
    return swift_format_main(
      TheDriver.getArgsWithoutProgramNameAndDriverMode(argv),
      argv[0], (void *)(intptr_t)getExecutablePath);
  default:
    break;
  }

  std::unique_ptr<llvm::opt::InputArgList> ArgList =
    TheDriver.parseArgStrings(ArrayRef<const char*>(argv).slice(1));
  if (Diags.hadAnyError())
    return 1;

  std::unique_ptr<ToolChain> TC = TheDriver.buildToolChain(*ArgList);
  if (Diags.hadAnyError())
    return 1;

  std::unique_ptr<Compilation> C =
      TheDriver.buildCompilation(*TC, std::move(ArgList));
  if (Diags.hadAnyError())
    return 1;

  if (C) {
    return C->performJobs();
  }

  return 0;
}
