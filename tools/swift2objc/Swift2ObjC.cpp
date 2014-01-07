//===-- Swift2ObjC.cpp - Emit a header file for a Swift module ------------===//
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

#include "swift/Subsystems.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Signals.h"
using namespace swift;

static llvm::cl::opt<std::string>
InputFilename(llvm::cl::desc("input.swiftmodule"), llvm::cl::Positional,
              llvm::cl::Required);

static llvm::cl::opt<std::string>
OutputFilename("o", llvm::cl::desc("Output file"), llvm::cl::init("-"));

static llvm::cl::list<std::string>
ImportPaths("I", llvm::cl::desc("Add a directory to the import search path"));

#ifndef SWIFT_MODULES_SDK
#define SWIFT_MODULES_SDK ""
#endif

static llvm::cl::opt<std::string>
SDK("sdk", llvm::cl::desc("Path to the SDK to build against"),
    llvm::cl::init(SWIFT_MODULES_SDK));

#ifndef SWIFT_MODULE_CACHE_PATH
#define SWIFT_MODULE_CACHE_PATH ""
#endif

static llvm::cl::opt<std::string>
ModuleCachePath("module-cache-path", llvm::cl::desc("Clang module cache path"),
                llvm::cl::init(SWIFT_MODULE_CACHE_PATH));


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
  
  llvm::cl::ParseCommandLineOptions(argc, argv,
                                    "Emit a header file for a Swift module\n");

  // Call llvm_shutdown() on exit to print stats and free memory.
  llvm::llvm_shutdown_obj Y;

  StringRef moduleName = llvm::sys::path::stem(InputFilename);
  if (!Lexer::isIdentifier(moduleName))
    moduleName = "main";

  // If no SDK was specified via -sdk, check environment variable SDKROOT.
  if (SDK.getNumOccurrences() == 0) {
    const char *SDKROOT = getenv("SDKROOT");
    if (SDKROOT)
      SDK = SDKROOT;
  }

  CompilerInvocation invocation;
  invocation.setModuleName(InputFilename);
  invocation.setMainExecutablePath(
      llvm::sys::fs::getMainExecutable(argv[0],
          reinterpret_cast<void *>(&anchorForGetMainExecutable)));

  invocation.setModuleName(moduleName);
  invocation.setImportSearchPaths(ImportPaths);
  invocation.setInputKind(SourceFileKind::Library);
  invocation.addInputFilename(InputFilename);

  invocation.setSDKPath(SDK);
  invocation.getClangImporterOptions().ModuleCachePath = ModuleCachePath;

  CompilerInstance CI;
  PrintingDiagnosticConsumer PrintDiags;
  CI.addDiagnosticConsumer(&PrintDiags);

  if (CI.setup(invocation))
    return 1;
  CI.performParse();

  bool HadError = CI.getASTContext().hadError();

  std::string errorInfo;
  llvm::raw_fd_ostream os(OutputFilename.c_str(), errorInfo);
  if (!errorInfo.empty()) {
    llvm::errs() << "error: " << errorInfo << "\n";
    return 1;
  }

  SmallVector<Decl *, 64> decls;
  CI.getMainModule()->getTopLevelDecls(decls);
  for (const Decl *D : decls) {
    if (auto VD = dyn_cast<ValueDecl>(D))
      if (VD->isObjC())
        os << VD->getName() << "\n";
  }

  return HadError;
}
