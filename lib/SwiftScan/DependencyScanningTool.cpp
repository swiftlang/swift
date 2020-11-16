//===------------ DependencyScanningTool.cpp - Swift Compiler -------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SwiftScan/DependencyScanningTool.h"
#include "swift/SwiftScan/ScanDependencies.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/Basic/LLVMInitialize.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/Frontend/Frontend.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"

#include <sstream>

namespace swift {
namespace dependencies {

DependencyScanningTool::DependencyScanningTool()
: GlobalCache(), Alloc(), Saver(Alloc) {
  

}

std::string
DependencyScanningTool::getFullDependencies(ArrayRef<const char *> Command,
                                            const llvm::StringSet<> &InputFiles,
                                            const llvm::StringSet<> &PlaceholderModules) {
  PrintingDiagnosticConsumer PDC;
  // State unique to an individual scan
  auto Instance = std::make_unique<CompilerInstance>();
  Instance->addDiagnosticConsumer(&PDC);

  // Basic error checking on the arguments
  if (Command.empty()) {
    Instance->getDiags().diagnose(SourceLoc(), diag::error_no_frontend_args);
    return "Error";
  }

  CompilerInvocation Invocation;
  SmallString<128> WorkingDirectory;
  llvm::sys::fs::current_path(WorkingDirectory);

  // Parse arguments.
  std::string CommandString;
  for (const auto *c : Command) {
    CommandString.append(c);
    CommandString.append(" ");
  }
  SmallVector<const char*, 4> Args;
  llvm::cl::TokenizeGNUCommandLine(CommandString, Saver, Args);
  if (Invocation.parseArgs(Args, Instance->getDiags())) {
    return "Error";
  }

  // Setup the instance
  Instance->setup(Invocation);

  std::string JSONOutput;
  llvm::raw_string_ostream Oss(JSONOutput);
  scanDependencies(*Instance, Oss);
  Oss.flush();
  
  return JSONOutput;
}
}
}
